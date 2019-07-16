

treatments <- function(owri.db, complete.years, huc8) {

  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)

  channel <- dbConnect(RSQLite::SQLite(), owri.db)

  # I'm keeping the dataframes the same name that OWEB uses
  ActivityCost <- dbReadTable(channel, "ActivityCost")
  ActivityLU <- dbReadTable(channel, "ActivityLU")
  ActivityTypeLU <- dbReadTable(channel, "ActivityTypeLU")
  ProjectInfo <- dbReadTable(channel, "ProjectInfo")
  Treatment <- dbReadTable(channel, "Treatment")
  TreatmentLU <- dbReadTable(channel, "TreatmentLU")
  TreatmentMetric <- dbReadTable(channel, "TreatmentMetric")
  TreatmentMetricLU <- dbReadTable(channel, "TreatmentMetricLU")
  UnitLU <- dbReadTable(channel, "UnitLU")
  RiparianVtrRcr <- dbReadTable(channel, "RiparianVtrRcr")
  dbDisconnect(channel)

  # Rename some of the Treatment Metrics
  TreatmentMetricLU2 <- data.frame(TreatmentMetricLUID=c(1:8),
                                   TreatmentMetric=c("Area treated",
                                                     "Number of treatments",
                                                     "Stream sides treated",
                                                     "Volume",
                                                     "Length of treatment",
                                                     "Percent urban area affected",
                                                     "Percent watershed area affected",
                                                     "Volumetric flow rate"))

  # Select only some of the riparian treatment metrics
  RiparianVtrRcr2 <- RiparianVtrRcr %>%
    dplyr::select(TreatmentID, mile=LengthMiles, acre=BestAcres, feet=WidthFeet) %>%
    tidyr::gather(-TreatmentID, key="Unit",value="Quantity") %>%
    mutate(UnitLUID=case_when(Unit == "mile" ~ 10,
                              Unit == "acre" ~ 1,
                              Unit == "feet" ~ 8))

  df.treatments <- Treatment %>%
    dplyr::select(PROJNUM ,ActivityTypeLUID, ActivityLUID, TreatmentLUID, TreatmentID) %>%
    dplyr::left_join(ActivityTypeLU[,c("ActivityTypeLUID", "ActivityType")], by="ActivityTypeLUID") %>%
    dplyr::left_join(ActivityLU[,c("ActivityLUID", "Activity")], by="ActivityLUID") %>%
    dplyr::left_join(TreatmentLU[,c("TreatmentLUID", "Treatment")], by="TreatmentLUID") %>%
    dplyr::left_join(TreatmentMetric, by="TreatmentID") %>%
    dplyr::left_join(TreatmentMetricLU2, by="TreatmentMetricLUID") %>%
    dplyr::left_join(UnitLU, by="UnitLUID") %>%
    dplyr::left_join(ProjectInfo, by="PROJNUM") %>%
    dplyr::filter(CompleteYear %in% complete.years & drvdHUC4thField %in% huc8)

  return(df.treatments)

}
