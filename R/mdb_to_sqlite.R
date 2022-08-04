#' Move Access Tables into a SQLite Database.
#'
#' Move OWRI tables in a Microsoft Access database into a SQlite database.
#' All tables are moved exactly as named using their existing column datatypes.
#' If a table already exists in the SQlite database an error will be returned.
#' This function is to be used to create new tables in a SQlite database rather
#' than append or overwrite them.
#'
#' This function must be utilized with the 32 bit version of R. If using RStudio
#' and converting a *.mdb Microsoft Access database only the 32 bit compatible
#' version of RStudio Desktop can be used (<= version 1.1.463).
#' Alternatively if the *.mdb file can be opened with Access and saved as a *.accdb
#' file, newer versions of RStudio Desktop can be used instead.
#'
#' Microsoft Access 1997 or newer supported with Microsoft Access Driver (*.mdb, *.accdb) installed.
#'
#' @param mdb The full path and file name to the Access database.
#' @param sqlite The path and file name to the new SQLite database to be created.
#' @keywords access, mdb, sqlite, database
#' @export
#' @return mdb_to_sqlite() has no returned value.
#'

mdb_to_sqlite <- function(mdb, sqlite) {

  db_connect_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};","DBQ=", mdb)

  channel <- DBI::dbConnect(odbc::odbc(),.connection_string = db_connect_string)

  # create SQlite db
  owri.db <- DBI::dbConnect(RSQLite::SQLite(), sqlite)

  owri.tbls <- DBI::dbListTables(channel)

  # remove ms system tables

  owri.tbls <- owri.tbls[!grepl("MSys", owri.tbls)]

  for (t in owri.tbls) {

    print(paste0("Writing ",t))

    DBI::dbWriteTable(owri.db, t, DBI::dbReadTable(channel, t))

  }

  DBI::dbDisconnect(owri.db)

}
