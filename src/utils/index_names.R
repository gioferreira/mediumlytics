index_names_tbl <- function(data_tbl){
  as.data.frame(colnames(data_tbl))
}

index_by_name_str <- function(data_tbl,
                          name){
  which(colnames(data_tbl) == name)
}

index_by_name_ptr <- function(data_tbl,
                              pattern){
  grep(pattern, colnames(data_tbl))
}

