
library(owri)
library(writexl)

# path to most recent database
owri.db <- owri_db()

huc8 <- c(17090001, 17090002, 17090003, 17090004, 17090005, 17090006, 17090007, 17090008, 17090010, 17090011, 17090012)
#huc8.names <- c("Middle Fork Willamette", "Coast Fork Willamette","Upper Willamette", "McKenzie", "North Santiam", "South Santiam",
#                "Middle Willamette", "Yamhill", "Clackamas", "Mollalla-Pudding", "Tualatin", "Lower Willamette")

complete.years <- c(2000:2019)

df.treatments <- treatments(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)
df.summary <- owri_summary(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)
df.summary20 <- owri_summary20(owri.db = owri.db, complete.years = complete.years, huc8 = huc8)


write_xlsx(df.treatments, path = "owri_Willamette_1999_2019.xlsx")

