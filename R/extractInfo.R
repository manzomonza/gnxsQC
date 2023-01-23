#' Extract information from Info.csv file
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
extract_info = function(filepath){
  if(!grepl("RNA", filepath) & !grepl("watchdog", filepath)){
    name = grep("Sample Name", readLines(filepath), value = TRUE)
    name = grep("Assay Name", name, value = TRUE, invert = TRUE)
    name = gsub("Sample Name,",'',name)
    name = gsub(",",'',name)
    ##
    tcc = grep("ellularity", readLines(filepath), value = TRUE)
    tcc = gsub("%Cellularity,",'',tcc)
    tcc = gsub(",",'',tcc)
    ##
    cancer_type = grep("Cancer Type", readLines(filepath), value = TRUE)
    cancer_type = gsub("Cancer Type,",'', cancer_type)
    cancer_type = gsub(",",'', cancer_type)

    return(data.frame(id = name,
                      tcc = tcc,
                      cancer_type = cancer_type,
                      PW_gender = NA,
                      CNV_gender = NA))
  }
}
