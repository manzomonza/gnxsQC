#' Function to read in watchdog prep_snv.txt files
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
readin_prepfiles = function(filepath){
  prepsnv = readr::read_tsv(filepath)
  prepsnv$filepath = filepath
  prepsnv = prepsnv[which(colnames(prepsnv) %in% c("gene","coding","one_AA", "filepath"))]
  prepsnv = tidyr::unite(prepsnv,col = "variants", c(gene, one_AA, coding), sep = "_" )
  prepsnv$variants = gsub("_NA_","_",  prepsnv$variants)
  prepsnv$variants = gsub("_NA_","_",  prepsnv$variants)
  #prepsnv = tidyr::nest(prepsnv, variants=mtbp)
  #prepsnv = dplyr::mutate(prepsnv, variants = paste(unlist(variants), collapse = "; "))
  return(prepsnv)
}
