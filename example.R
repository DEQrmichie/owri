
setwd("E:/GitHub/owri")

owri.mdb <-  "F:/WorkSpace/Quantifying_Conservation/SouthernWillamette/OWEB/OWRI_ExportToAccess_122618/OwriDbExport_122618.mdb"


owri.sqlite <- "OwriDbExport_122618.db"

mdb_to_sqlite(owri.mdb = owri.mdb, owri.sqlite = owri.sqlite, overwrite=TRUE)

unlink(owri.sqlite)
