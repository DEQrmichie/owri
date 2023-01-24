#' Current OWRI Database
#'
#' Returns the path to the most recent SQLite OWRI database export included with this R pacakge.
#'
#' @param owri.db Optional. owri SQLite database file name. Default is the
#'        most recent version. 'OWRIDbExport_011023.db'.
#'
#' @keywords owri, raw_data
#' @export

owri_db <- function(owri.db = "OWRIDbExport_011023.db") {

  owri.db <- system.file("extdata", owri.db, package = "owri", mustWork = TRUE)

  return(owri.db)

}

