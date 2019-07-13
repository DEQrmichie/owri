#' Move Access mdb Tables into a SQLite Database.
#'
#' Move tables in a Microsoft Access database into a SQlite database.
#' All tables are moved exactly as named using their existing column datatypes.
#' If a table alredy exists in the SQlite database an error will be returned.
#' This function is to be used to create new tables in a SQlite database rather
#' than append or overwrite them.
#'
#' Microsoft Access 1997 or newer supported with Microsoft Access Driver (*.mdb, *.accdb) installed.
#'
#' @param mdb The path and file name to the Access database.
#' @param sqlite The path and file name to the new SQLite database to be created.
#'    columns and datatypes are the same as a dataframe returned from AWQMSdata::AWQMS_data(). Defualt is NULL.
#' @param ... Other arguments passed on to methods.
#'
#' @keywords access, mdb, sqlite, database
#' @export
#' @return mdb_to_sqlite() has no returned value.
#'

mdb_to_sqlite <- function(mdb, sqlite) {

  # Requires use of 32 bit R and if using RStudo the 32 bit compatible RStudio Desktop (version 1.1.463)

  library(DBI)
  library(odbc)
  library(RSQLite)

  db_connect_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};","DBQ=", mdb)

  channel <- dbConnect(odbc::odbc(),.connection_string = db_connect_string)

  # create SQlite db
  owri.db <- dbConnect(RSQLite::SQLite(), sqlite)

  owri.tbls <- dbListTables(channel)

  # remove ms system tables

  owri.tbls <- owri.tbls[!grepl("MSys", owri.tbls)]

  for(t in owri.tbls){

    print(paste0("Writing ",t))

    dbWriteTable(owri.db, t, dbReadTable(channel, t))

  }

  dbDisconnect(owri.db)

}
