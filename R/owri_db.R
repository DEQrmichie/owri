#' Current OWRI Database
#'
#' Returns the path to the most recent SQLite OWRI database export included with this R package.
#' The original OWRI database published by the Oregon Watershed Enhancement Board can be found here:
#' https://hub.oregonexplorer.info/pages/water-planning-enhancing-watersheds-in-oregon#TheOWRIDatabaseandGISdata
#'
#' @param owri.db Optional. owri SQLite database file name. Default is the
#'        most recent version. 'OwriDbExport_010826.db'.
#'
#' @keywords owri, raw_data
#' @export

owri_db <- function(owri.db = "OwriDbExport_010826.db") {

  owri.db <- system.file("extdata", owri.db, package = "owri", mustWork = TRUE)

  return(owri.db)

}

