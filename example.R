
setwd("E:/GitHub/owri")

owri.mdb <-  "F:/WorkSpace/Quantifying_Conservation/SouthernWillamette/OWEB/OWRI_ExportToAccess_122618/OwriDbExport_122618.mdb"

owri.db <- "OwriDbExport_122618.db"
owri.sqlite <- "OwriDbExport_122618.db"

mdb_to_sqlite(owri.mdb = owri.mdb, owri.sqlite = owri.sqlite, overwrite=TRUE)

unlink(owri.sqlite)

huc8 <- c(17090001, 17090002, 17090003, 17090004, 17090005, 17090006, 17090007, 17090008, 17090010, 17090011, 17090012)
complete.years <- c(2006:2018)

df.treats <- treatments(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)

