
# Data tidying functions
# content of tables:
# 1. data_partition (simple version, 1 split)
# 2. data_resample (advanced version, support CV & bagging) 
# 2. data_dummy (dummy coding)
# 3. normal_colnames
# 4. data_auto_dtype
# 5. data_normalize

# Machine learning functions
# content of tables:
# 1. model_eval (evaluate model performance)
  # mcc: Matthew’s correlation coefficient
# 2. modeling: model construction with caret::train

# 1. This function split data to training and testing set
# return: list with training and testing data OR a vector with the training data index
data_partition <- function(data, y, seed=1, prop=0.7, stratified=FALSE, list=TRUE){
  # param x: input data set
  # param y: vector, the variable stratified for per-class sampling. Usually this is the outcome variable
  # param seed: the seed of set.seed
  # param prop: the proportion of training data; default = 0.7
  # param stratified: conduct stratified/per-class sampling or not
  # param list: return a list or not. If FALSE, return the index of training 
  require(foreach)
  lst <- vector("list")
  set.seed(seed)
  if (stratified){
    k <- sort(unique(y))
    idx <- foreach(j = 1:k, .combine = c) %dopar%      # combine the idx of diff classes
      sample(which(y == j),floor(sum(y == j)*prop)) # stratified sampling
  } else {
    idx <- sample(1:nrow(data), floor(nrow(data)*prop))
  }
  if (list){
    lst[["train"]] <- data[idx,]
    lst[["test"]] <- data[-idx,]
    return(lst)
  }
  return(idx)   # return training index if list=FALSE
}


# 2. dummy coding for factor columns
# :param data: data frame for dummy coding, only factor will be modified
# :param except: vector, the cols that we don't want to do dummy coding. 
#                If numeric -> read as index, if chr -> read as colname
# :return new_data: data.frame with no factors
data_dummy <- function(data, except=NULL){
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  rowc <- nrow(data)
  ori_nms <- colnames(data)  # original colnames
  dtypes <- sapply(data,class)
  if (typeof(except)=="double" && all.equal(except,round(except))) except <- ori_nms[except]
  not_factors <- union(ori_nms[dtypes!="factor"], except)
  lst <- sep_freeze(data = data,except = not_factors) # split the cols into factors & non-factors & except
  factors <- lst$check ; freeze <- lst$freeze
  freeze <- data.frame(1:rowc) %>%      # add a redundant col to avoid cbind with null
    {if (!is.null(freeze)) bind_cols(.,freeze)}
  # apply dummy coding on all factor cols:
  dummys <- lapply(names(factors), function(nm){
    col <- factors[[nm]]
    vals <- sort(unique(col))[-1]
    new_nm <- paste0(nm, "_",vals)
    list(new_nm, sapply(vals, function(v1) ifelse(col==v1, 1, 0)))
  })
  new_nms <- lapply(dummys,function(x) x[[1]]) %>% unlist() # names of dummy cols
  new_cols <- matrix(lapply(dummys,function(x) x[[2]]) %>% unlist(), nrow=rowc,
                     dimnames = list(NULL,new_nms))
  data.frame(freeze[,-1], new_cols)
}

# 3. remove symbols in column names
# any symbols that are not alphabet, digits or dash will be removed or replaced by dash 

normal_colnames <- function(data, replace_by_dash=FALSE){
  new_colnames <- original_colnames <- colnames(data)
  new_colnames <- gsub("\\s", "_", new_colnames)
  if (replace_by_dash){
    new_colnames <- gsub("[^A-Za-z0-9\\_]", "_", new_colnames)
  } else {
    new_colnames <- gsub("[^A-Za-z0-9\\_]", "", new_colnames)
  }
  return(new_colnames)
}

# 4. 
data_auto_dtype <- function(data, except=NULL, fct_threshold=10, date_to_num=TRUE, drop_chr=TRUE){
  # :param data: data.frame, input data
  # :param fct_threshold: int, if distinct count< fct_threshold, transform to factor
  # :param drop_chr: bool, if T drop the remaining chr variables
  # :param date_to_num: bool, if T transform date dtype to numeric
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  ori_nms <- colnames(data)  # original colnames
  lst <- sep_freeze(data = data,except = except)
  check <- lst$check ; freeze <- lst$freeze
  rm(data,lst)                  # save memory
  nms <- colnames(check)
  dtypes <- sapply(check,class)
  counts <- apply(check,2,function(x) length(unique(x)))
  to_fct <- nms[counts<fct_threshold]  # transform to factors
  dt <- nms[dtypes=="Date"] ; chr <- setdiff(nms[dtypes=="character"], to_fct)
  data <- check %>%
    {if (!is.null(freeze)) bind_cols(.,freeze)} %>%  # if no except data, don't cbind
    mutate_at(vars(all_of(to_fct)), as.factor) %>%   # transform to factor
    {if (date_to_num) mutate_at(.,vars(all_of(dt)), as.numeric)} %>% # transform to numeric
    dplyr::select(all_of(ori_nms)) %>%                      # same order as input data
    {if (drop_chr) dplyr::select(.,!all_of(chr))}           # drop chr that didn't transform
  return(data)
}

# 5. normalize numeric variables but not dummy variables
data_normalize <- function(data, except=NULL){
  # normalize the numeric variables
  # won't change the value of factors & dummy variables
  # if the outcome is numeric, must provide outcome_idx or outcome_name to avoid normalization of outcome
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  data_freeze <- NULL
  if (typeof(except)=="character"){
      data_freeze <- data %>% dplyr::select(all_of(except))
      data <- data %>% dplyr::select(!all_of(except))
    } else if(typeof(except) == "double"){
      data_freeze <- data[,except]
      data <- data[,-except]
      }
  data <- apply(data,2,function(col){
    uq <- sort(unique(col))
    if ((class(col[1])=="numeric") && (uq[1]!=0 | uq[2]!=1)){
      scale(col)      
    } else {
      col
    }
    
  })
  if (!is.null(data_freeze)) data <- cbind(data_freeze, data.frame(data))
  return(data)
}

# 6. check number of unique values in each cols:
unique_count <- function(data){
  invisible(sapply(c("dplyr", "DT"), require, character.only=TRUE))
  classes <- sapply(data,class) 
  counts <- apply(data,2,function(x) length(unique(x)))
  examples <- sapply(apply(data,2,function(x) head(unique(x), 10)),  # keep the first 10 unique values as example
                     function(x) paste(x, collapse = ","))           # combine the vals into one
  output <- data.frame(class = classes, 
                       unique_count = counts,
                       values = examples
             ) %>% arrange(desc(counts))
  return(datatable(output))
  
}

## . sep_freeze
# supply function for other data cleaning functions
sep_freeze <- function(data,except){
  nms <- colnames(data)  # colnames
  if (typeof(except)=="character"){
    freeze <- data %>% dplyr::select(all_of(except))
    check <- data %>% dplyr::select(!all_of(except))
  } else if (typeof(except)=="double" && all.equal(except,round(except))){ # check integer
    freeze <- data.frame(data[,except])  # avoid being a vector & couldn't cbind
    colnames(freeze) <- nms[except]
    check <- data[,-except]
  } else {
    check <- data
    freeze <- NULL
  }
  return(list(check=check, freeze=freeze))
}

# 7. data resampling using bagging or cross validation
data_resample <- function(x, fold=10, seed=1, method=c("cv", "bagging"), stratified=TRUE, partition_ratio=0.7){
  require(dplyr)
  # :param x: vector, input response value for data partition
  # :param fold: integer; number of folds in CV or number of bagging iterations, default=10
  # :param method: string, resampling method
  # :param stratified: logical, stratified the response variable or not
  # :param partitoin_ratio: numeric; the proportion of data to split when parameter resample=="bagging"
  # :return idxs: list, index of TESTING data
  method <- match.arg(method)
  x <- as.factor(dense_rank(x))   # turn to integer from 1 to k
  n <- length(x)
  k <- length(unique(x))
  idxs <- vector(mode = "list")
  if (method=="bagging"){
    if (stratified){
      for (i in 1:fold) {
        set.seed(seed+i, kind = "L'Ecuyer-CMRG")
        idxs[[i]] <- foreach(j = 1:k, .combine = c) %do%      
          sample(which(x == j),floor(sum(x == j)*(1-partition_ratio)))
      }
    } else {
      test_cnt <- floor((1-partition_ratio)*n)
      set.seed(seed, kind = "L'Ecuyer-CMRG")
      idxs <- lapply(1:fold, function(idx) sample(n,test_cnt))
      
    }
  } else{  # CV
    if (stratified){
      set.seed(seed, kind = "L'Ecuyer-CMRG")
      idx <- lapply(1:k, function(i) {
        neworder <- sample(which(x==i))       # reorder indices in each class
        split(neworder, f=rep(1:fold,length.out=length(neworder))) # split to folds
      })   
      idxs <- lapply(1:fold,function(i) {     # turn to testing indices
        c(unlist(sapply(idx,function(sub) sub[[i]])))
      })
      } else {
        set.seed(seed, kind = "L'Ecuyer-CMRG")
        idx <- sample(rep(1:fold, length.out = n))        # resample idx with "fold" kinds at length n
        idxs <- lapply(1:fold, function(i) which(idx==i)) 
      }
  }
  return(idxs)
}

# ML functions:

# 1. model_eval()
# TO-DO: returns a summary of model performance

mcc <- function(t1){
  # TO-DO: return the Matthew’s correlation coefficient 
  # :param t1: table(prediction, reference), the confusion matrix 
  k = ncol(t1)
  n = sum(t1)
  if (n==0) return(NaN)  # if all model infeasible
  if (k>2){
    wr = colSums(t1);wp = rowSums(t1)               
    tr = sapply(1:k, function(i) t1[i,i]) 
    cov_xy = n*sum(tr) - sum(wp * wr)                      # MCC numerator
    cov_xxyy = (n**2 - sum(wp*wp)) * (n**2 - sum(wr*wr))   # MCC denominator^2
    mcc = ifelse(cov_xxyy==0,0, cov_xy / sqrt(cov_xxyy))
    return(mcc)
  } else if (k==2){
    # turn sum(t1[2,]) to float -> avoid integer overflow
    mcc = (t1[1,1]*t1[2,2] - t1[2,1]*t1[1,2])/sqrt(as.numeric(sum(t1[2,]))*sum(t1[,2])*sum(t1[,1])*sum(t1[1,]) )
    return(mcc)
  } else {
    warning("The dimension of input table should be >= 2")
  }
}

model_eval <- function(t1, verbose=FALSE){
  # :param x: table(prediction, reference), the confusion matrix 
  # :param verbose: If FALSE, the function will remain silent
  # Note: Use table instead of y values as input to save computation memories 
  #       We can do one simple evaluation when resampling
  n = sum(t1)
  k = ncol(t1) 
  wr = colSums(t1);wp = rowSums(t1)               # weight of reference/prediction
  tr = sapply(1:k, function(i) t1[i,i])          # trace before summation
  # Accuracy:
  acc = sum(tr)/n
  if (k>2){                                       # multiclass
    # F1 score calculation
    rc <- tr/wr ; pc <- tr/wp                     # recall ; precision
    f1 <- 2*rc*pc/(rc+pc)   
    # Balanced accuracy:
    bac <- mean(rc)
    # MCC calculation:
    mcc_val = mcc(t1)
    # Cohen's kappa calculation:
    ex = sum(wr/n*wp/n)                           # expectation if independent
    ag = sum(sapply(1:k, function(i) t1[i,i]))/n  # agreement
    kp = (ag-ex)/(1-ex)
    output <- c(acc,mcc_val,sum(f1*wr/n),mean(f1),kp, bac)
    names(output) <- c("accuracy", "mcc", "microF1", "macroF1", "Kappa", "BAC")
    return(output)
  } else if (k==2){  # binary
    if (verbose) cat("Reference group for binary classification", colnames(t1)[1],"\n")
    rc = tr[2]/wr[2]  # recall
    pc = tr[1]/wp[1]  # precision
    sp = tr[1]/wr[1]  # specificity
    bac = (rc+sp)/2   # balanced accuracy
    # gms = sqrt(rc*pc) # G measure
    f1_val = 2*rc*pc/(rc+pc) # f1
    ex = sum(wr/n*wp/n)                           # expectation if independent
    ag = sum(sapply(1:k, function(i) t1[i,i]))/n  # agreement
    kp = (ag-ex)/(1-ex)
    mcc_val = mcc(t1)
    output <- c(acc,mcc_val,f1_val,rc,pc,sp,kp,bac)
    names(output) <- c("accuracy", "mcc","F1", "recall", "precision", 
                       "specificity", "kappa", "BAC")
    return(output)
  }
}
# 2. modeling with caret train function
modeling <- function(data, classifier, trControl=list(), tuneGrid=list(), seed=1){
  # ---
  # :params data: 
  # :params trControl: list, inputs for resampling in caret::train; default method="none"
  # :params tuneGrid: list, inputs for hyperparameters tuning in caret::train()
  # may be very slow for some methods without resetting the hyperparameters | resampling methods
  # ---
  # trainControl:
  trControl <- do.call(trainControl,trControl) 
  # tuneGrid:
  if (classifier=="logistic") tuneGrid <- list(alpha=0,lambda=0)       # logistic regression
  tuneGrid <- do.call(expand.grid, tuneGrid)
  set.seed(-seed, kind = "L'Ecuyer-CMRG")
  params <- list(as.formula("y~."), data=data, method=classifier, 
                 trControl=trControl, tuneGrid=tuneGrid)
  params <- lapply(params, function(x) if (length(x)!=0) return(x) )
  params <- params[!sapply(params,is.null)]        # remove trControl & tuneGrid if empty
  if (classifier=="gbm") {    # silence some classifier
    params[["verbose"]] <- F
  } else if (classifier=="xgbTree"){
    params[["verbosity"]] <- 0
  }
  fit <- try(do.call(caret::train, params))
  if ("try-error" %in% class(fit)) {
    warning("If you reset any hyperparameter,all required hyperparameters of ",
            classifier," need to be specified by parameter tuneGrid")
  } else {
    return(fit)
  }
}


# auto_relation plots
# visualization of categorical|continuous outcome vs. two kinds of features



# 











