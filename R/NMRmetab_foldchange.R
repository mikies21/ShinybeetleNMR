NMRmetab_foldchange <- function(data, groupID, index_col = 3, dividendID, divisorID){
  
  coln = colnames(data)[index_col:ncol(data)]
  
  fold_df = data %>%
    dplyr::select(.data[[groupID]], c(index_col:ncol(data))) %>%
    dplyr::group_by(.data[[groupID]]) %>%
    dplyr::summarise(dplyr::across(everything(), mean)) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(1) %>%
    dplyr::mutate(across(everything(), as.numeric)) %>%
    tibble::rownames_to_column('metabolite') %>%
    tibble::tibble() %>%
    dplyr::mutate(log2FC = log2(.data[[dividendID]]/.data[[divisorID]]))
  
  return(fold_df)
}

