
# Data tidying functions
# content of tables:
# 1. data_partition
# 2. dummy_coding
# 3. normal_colnames
# 4. auto_dtype_change
# 5. data_normalize


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
    idx <- foreach(j = k, .combine = c) %do%      # combine the idx of diff classes
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
# :return new_data: data.frame with no factors
dummy_coding <- function(data, except){
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
  new_data <- cbind(data_freeze, new_data)
  return(new_data)
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
auto_dtype_change <- function(data, chr_to_fct=TRUE, fct_still_fct=TRUE,fct_threshold=Inf, date_to_num=TRUE){
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  dtypes <- data %>% map_chr(class)
  date_nms <- colnames(data)[which(dtypes == "Date")]
  if (!fct_still_fct) {  # if fct_still_fct=F, check the category count of factor columns
    cat_idx <- which(dtypes %in% c("character","factor"))
  } else {               # if fct_still_fct=T, don't change the datatype of var that are already factors
    cat_idx <- which(dtypes=="character")
  }
  class_cnt <- apply(data[,cat_idx],2,function(x) length(unique(x)))  # count of unique classes in each category var
  chr_nms <- colnames(data)[cat_idx][class_cnt<fct_threshold]
  data <- data %>%
    mutate_at(vars(all_of(date_nms)), as.numeric) %>%  # turn date to numeric
    mutate_at(vars(all_of(chr_nms)), as.factor)        # turn characters to factor
  return(data)
}

# 5. normalize numeric variables but not dummy variables
data_normalize <- function(data, outcome_idx, outcome_name){
  # normalize the numeric variables
  # won't change the value of factors & dummy variables
  # if the outcome is numeric, must provide outcome_idx or outcome_name to avoid normalization of outcome
  invisible(sapply(c("dplyr", "purrr"), require, character.only=TRUE))  # load packages without printing
  col_nms <- colnames(data)
  dtypes <- data %>% map_chr(class)
  # 把outcome分出來, 其他normalize
  if (outcome_idx) outcome_name <- col_nms[outcome_idx]   # get outcome name by outcome index
  if (outcome_name) {
    data <- data %>%
      select()
  }
  apply(data,2,function(col){
    if ((class(col[1])==numeric) & (sort(unique(col))==c(0,1))){
      col <- scale(col)      
    } else {
      col
    }
    
  })
  
  
}

# 7. result tables (accuracy, ....)















