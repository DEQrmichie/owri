
setwd("E:/GitHub/owri")


# Write from mdb to SQlite db
owri.mdb <-  "F:/WorkSpace/Quantifying_Conservation/SouthernWillamette/OWEB/OWRI_ExportToAccess_122618/OwriDbExport_122618.mdb"
owri.sqlite <- "OwriDbExport_122618.db"

mdb_to_sqlite(owri.mdb = owri.mdb, owri.sqlite = owri.sqlite, overwrite=TRUE)


# summarize treatments

owri.db <- "OwriDbExport_122618.db"

huc8 <- c(17090001, 17090002, 17090003, 17090004, 17090005, 17090006, 17090007, 17090008, 17090010, 17090011, 17090012)
#huc8.names <- c("Middle Fork Willamette", "Coast Fork Willamette","Upper Willamette", "McKenzie", "North Santiam", "South Santiam",
#                "Middle Willamette", "Yamhill", "Clackamas", "Mollalla-Pudding", "Tualatin", "Lower Willamette")

complete.years <- c(1999:2019)

df.treatments <- treatments(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)

df.summary <- owri_summary(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)

library(writexl)
write_xlsx(treat.list,path="owri_Willamette_2000_2019.xlsx")

