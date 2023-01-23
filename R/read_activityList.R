#' Function to read in activityList_Entry from 'combined_output.xlsx' file
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
readin_actList_entry = function(filepath){
  entry = readxl::read_xlsx(filepath, sheet = "activityList_Entry")
  entry = entry$activityList_Entry
  return(data.frame(filepath = filepath, mutation = entry))
}
