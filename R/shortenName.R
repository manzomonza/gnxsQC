#' Remove long absolute filepaths and GNXS UID
#'
#' @param filestring
#'
#' @return
#' @export
#'
#' @examples
shorten_name = function(filestring){
  parsestring = dirname(filestring)
  parsestring = strsplit(parsestring, split = "/")[[1]]
  parsestring = grep("20[2|3][0-9]", parsestring, value = TRUE)
  returnstring = stringr::str_extract(parsestring, pattern = "\\w{1,4}_.+?(?=_)")
  return(returnstring)
}
