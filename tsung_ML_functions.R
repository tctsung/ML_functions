
# Data tidying functions
# content of tables:
# 1. data_partition
# 2. dummy_coding
# 3. normal_colnames
# 4. auto_dtype_change: drop_chr has bug
# 5. data_normalize
# 6. unique_count


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
dummy_coding <- function(data, except=NULL){
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  
  if (typeof(except)=="character"){
    data_freeze = data %>% dplyr::select(all_of(except))
    data = data %>% dplyr::select(!all_of(except))
  } else if(typeof(except) == "double"){
    data_freeze = data[,except]
    data = data[,-except]
  }
  # split the cols into factors & non-factors:
  factor_col <- data %>% map_lgl(function(x) class(x)=="factor")  # bool that report factor cols
  new_data <- data[,!factor_col]
  factors <- data[,factor_col]
  # apply dummy coding on all factor cols:
  col_lst <- apply(factors, 2, function(col){  
    vals <- sort(unique(col))                  # get the unique value within col
    new_cols <- sapply(vals[-1], function(v1) ifelse(col==v1, 1, 0)) # dummy coding without lowest level 
  })
  # change the col names:
  for (i in 1:length(col_lst)){
    colnames(col_lst[[i]]) <- paste0(names(col_lst)[i], "_", colnames(col_lst[[i]]))
    new_data <- cbind(new_data, col_lst[[i]])
  }
  if (!is.null(data_freeze)) new_data <- cbind(data_freeze, new_data)
  return(data.frame(new_data))
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
auto_dtype_change <- function(data, except=NULL, fct_threshold=10, date_to_num=TRUE, drop_chr=TRUE){
  # :param data: data.frame, input data
  # :param fct_threshold: int, if distinct count< fct_threshold, transform to factor
  # :param drop_chr: bool, if T drop the remaining chr variables
  # :param date_to_num: bool, if T transform date dtype to numeric
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  ori_nms <- colnames(data)  # original colnames
  if (typeof(except)=="character"){
    freeze <- data %>% dplyr::select(all_of(except))
    check <- data %>% dplyr::select(!all_of(except))
  } else if (typeof(except)=="double" && all.equal(except,round(except))){ # check integer
    freeze <- data.frame(data[,except])  # avoid being a vector & couldn't cbind
    colnames(freeze) <- ori_nms[except]
    check <- data[,-except]
  } else {
    check <- data
    freeze <- NULL
  }
  data <- NULL              # save memory
  nms <- colnames(check)
  dtypes <- sapply(check,class)
  counts <- apply(check,2,function(x) length(unique(x)))
  to_fct <- nms[counts<fct_threshold]  # transform to factors
  dt <- nms[dtypes=="Date"] ; chr <- setdiff(nms[dtypes=="character"], to_fct)
  data <- cbind(freeze,check) %>%
    mutate_at(vars(all_of(to_fct)), as.factor) %>%
    {if (date_to_num) mutate_at(.,vars(all_of(dt)), as.numeric)} %>%
    select(all_of(ori_nms)) %>% # same order as the inputs
    {if (drop_chr) select(.,!all_of(chr))}
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
  classes <- apply(data,2,class) 
  counts <- apply(data,2,function(x) length(unique(x)))
  examples <- sapply(apply(data,2,function(x) head(unique(x), 10)),  # keep the first 10 unique values as example
                     function(x) paste(x, collapse = ","))           # combine the vals into one
  output <- data.frame(class = classes, 
                       unique_count = counts,
                       values = examples
             ) %>% arrange(desc(counts))
  return(datatable(output))
  
}

# 7. result tables (accuracy, ....)















