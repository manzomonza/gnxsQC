#' Function to read in and file Snvindel.tsv files
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
readin_snvindel = function(filepath){
  snv = readr::read_tsv(filepath)
  snv = janitor::clean_names(snv)
  #snv = dplyr::filter(snv, grepl("PRESENT",call ))
  snv = dplyr::filter(snv, call != "ABSENT")
  snv = dplyr::filter(snv, allele_frequency != 0)
  snv = dplyr::mutate(snv, vartype = ifelse(grepl("=", amino_acid_change), "syn","nonsyn"))
  snv$oncomine_variant_class =  as.character(snv$oncomine_variant_class)
  snv$filepath = filepath
  snv = snv %>% dplyr::select(gene, nucleotide_change, amino_acid_change, allele_frequency, filepath, vartype)
  snv = snv %>% dplyr::filter(gene != "CDKN2A-DT")
  return(snv)
}
