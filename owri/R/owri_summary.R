#' Summarize OWRI activity and treatments.
#'
#' @param owri.db The path and file name of the owri SQLite database.
#' @param complete.years Vector of numeric years used to fetch projects. Only projects completed in these years will be summarized.
#' @param huc8 Vector of numeric HUC8 codes used to fetch projects. Only projects within the HUC8 will be summarized.
#'
#' @keywords owri, complete years, huc8,
#' @export
#' @return Dataframe with the quantity of activity and treatments implemented.
#'

owri_summary <- function(owri.db, complete.years, huc8) {

  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)
  library(stringr)

  options(stringsAsFactors = FALSE)

  channel <- dbConnect(RSQLite::SQLite(), owri.db)

  # I'm keeping the dataframes the same name that OWEB uses
  ActivityCost <- dbReadTable(channel, "ActivityCost")
  ActivityLU <- dbReadTable(channel, "ActivityLU")
  ActivityTypeLU <- dbReadTable(channel, "ActivityTypeLU")
  ATATLU <- dbReadTable(channel, "ActivityTypeLUXActivityLUXTreatmentLU")
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

  #TreatmentMetric2 <- TreatmentMetric %>%
  #  dplyr::select(TreatmentMetricLUID, UnitLUID) %>%
  #  dplyr::distinct()

  #Activity_Unit_LU <- ATATLU %>%
  #  dplyr::left_join(TreatmentMetric2, by="TreatmentID") %>%
  #  dplyr::left_join(TreatmentMetricLU2, by="TreatmentMetricLUID") %>%
  #  dplyr::left_join(UnitLU, by="UnitLUID")

  year_group1 <- paste0(complete.years[1],"-",complete.years[4])
  year_group2 <- paste0(complete.years[5],"-",complete.years[8])
  year_group3 <- paste0(complete.years[9],"-",complete.years[12])
  year_group4 <- paste0(complete.years[13],"-",complete.years[16])
  year_group5 <- paste0(complete.years[17],"-",complete.years[20])

  # generate year groups
  df.treatments <- df.treatments %>%
    dplyr::mutate(year_group=dplyr::case_when(CompleteYear %in% c(complete.years[1:4]) ~ year_group1,
                                              CompleteYear %in% c(complete.years[5:8]) ~ year_group2,
                                              CompleteYear %in% c(complete.years[9:12]) ~ year_group3,
                                              CompleteYear %in% c(complete.years[13:16]) ~ year_group4,
                                              CompleteYear %in% c(complete.years[17:20]) ~ year_group5,
                                              TRUE ~ as.character(NA)),
                  year_group=factor(year_group, levels = c(year_group1,year_group2,year_group3,year_group4,year_group5)))

  #-- Esturary ----

  tbl.estuary <- df.treatments %>%
    dplyr::filter(ActivityType =="Estuary" & Unit == "acre") %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",str_to_title(Unit),"s)")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE))

  tbl.estuary <- ifelse(NROW(tbl.estuary) > 0, TRUE, FALSE)


  bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
              dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
              dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.estuary <- ifelse(NROW(tbl.estuary) > 0, TRUE, FALSE)

  #-- Fish Pasage ----

  tbl.fish1 <- df.treatments %>%
    dplyr::filter(ActivityType =="Fish Passage" & TreatmentMetricLUID==2) %>%
    dplyr::mutate(Activity_Unit=paste0(ActivityType," ",Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity_Unit) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.fish2 <- df.treatments %>%
    dplyr::filter(ActivityType =="Fish Screening" & TreatmentMetricLUID==2) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity_Unit) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)


  tbl.fish.final <- rbind(tbl.fish1, tbl.fish2) %>%
    arrange(SubbasinActual, Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)


  eval.fish <- ifelse(NROW(tbl.fish.final) > 0, TRUE, FALSE)

  #-- Instream ----

  tbl.instream1 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Unit =="mile"
                  & Activity %in% c("Bank stabilization")) %>%
    dplyr::mutate(Activity_Unit="Stream bank stabilized (Miles)") %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream2 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Activity %in% c("Animal species removal",
                                    "Beaver introduction/encouragement")) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)


  tbl.instream3 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & !(Unit %in% c("main channel", "structure"))
                  & Activity %in% c("Channel alteration")) %>%
    dplyr::mutate(TreatmentMetric=if_else(TreatmentMetric=="Length of treatment", "Feet", TreatmentMetric)) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream4 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Activity %in% c("Engineered structures")) %>%
    dplyr::mutate(Activity_Unit="Engineered structures installed (Number of treatments)") %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream5 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & ActivityLUID %in% c(8:13)) %>%
    dplyr::mutate(Activity=gsub(" *\\(.*?\\) *", "", Activity)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream6 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Activity %in% c("Off-channel habitat")
                  & TreatmentMetricLUID %in% c(2)) %>%
    dplyr::mutate(Activity_Unit=paste0("Off-channel habitat created, protected, or reconnected (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream7 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Activity %in% c("Other instream activity")
                  & TreatmentLUID %in% c(76)) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream8 <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream"
                  & Activity %in% c("Salmon carcass placement")
                  & Unit %in% c("pound", "mile")) %>%
    dplyr::mutate(Unit=if_else(Unit=="mile", "Linear stream miles of treatment", "Pounds")) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",Unit,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.instream.final <- rbind(tbl.instream1, tbl.instream3, tbl.instream2,
                              tbl.instream7, tbl.instream8,
                              tbl.instream4, tbl.instream5, tbl.instream6) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)


  tbl.instream.final1 <- rbind(tbl.instream1, tbl.instream3) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  tbl.instream.final2 <- rbind(tbl.instream2, tbl.instream7, tbl.instream8) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  tbl.instream.final3 <- rbind(tbl.instream4, tbl.instream5, tbl.instream6) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.instream <- ifelse(NROW(tbl.instream.final) > 0, TRUE, FALSE)

  #eval.instream1 <- ifelse(NROW(tbl.instream.final1) > 0, TRUE, FALSE)
  #eval.instream2 <- ifelse(NROW(tbl.instream.final2) > 0, TRUE, FALSE)
  #eval.instream3 <- ifelse(NROW(tbl.instream.final3) > 0, TRUE, FALSE)

  #-- Instream Flow ----

  tbl.flow <- df.treatments %>%
    dplyr::filter(ActivityType =="Instream Flow"
                  & !(is.na(Unit))) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment, " (",Unit,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.flow <- ifelse(NROW(tbl.flow) > 0, TRUE, FALSE)

  #-- Riparian Planting and Tree Retention -----

  # Voluntary Riparian Tree Retention
  tbl.rip1 <- df.treatments %>%
    dplyr::select(-Unit,-UnitLUID,-Quantity) %>%
    dplyr::left_join(RiparianVtrRcr2, by="TreatmentID") %>%
    dplyr::filter(ActivityType =="Riparian" &
                    Activity %in% c("Voluntary riparian tree retention") &
                    Unit %in% c("mile", "acre")) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",str_to_title(Unit),"s)")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)


  # Riparian Planting
  tbl.rip2 <- df.treatments %>%
    dplyr::filter(ActivityType =="Riparian" &
                    Activity %in% c("Riparian tree planting", "Riparian fencing") &
                    Unit %in% c("mile", "acre")) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",str_to_title(Unit),"s)")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)


  tbl.rip.final <- rbind(tbl.rip1, tbl.rip2) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.rip <- ifelse(NROW(tbl.rip.final) > 0, TRUE, FALSE)

  # -- Road ----
  # check with OWEB on 'station' units?? Not being used here.

  tbl.road <- df.treatments %>%
    dplyr::filter(ActivityType =="Road"
                  & TreatmentMetricLUID %in% c(2)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.road <- ifelse(NROW(tbl.road) > 0, TRUE, FALSE)

  # -- Upland ----

  tbl.upland1 <- df.treatments %>%
    dplyr::filter(ActivityType =="Upland"
                  & Activity %in% c("Agriculture management",
                                    "Conservation buffers",
                                    "Conservation tillage",
                                    "Irrigation system improvement")
                  & TreatmentMetricLUID %in% c(1, 2)) %>%
    dplyr::mutate(TreatmentMetric=if_else(Unit=="acre", "acres treated", TreatmentMetric)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.upland2 <- df.treatments %>%
    dplyr::filter(ActivityType =="Upland"
                  & Activity %in% c("Grazing management",
                                    "Nutrient/manure management",
                                    "Off-channel livestock or wildlife watering",
                                    "Upland fencing")
                  & TreatmentMetricLUID %in% c(1, 2)) %>%
    dplyr::mutate(TreatmentMetric=if_else(Unit=="acre", "acres treated", TreatmentMetric)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.upland3 <- df.treatments %>%
    dplyr::filter(ActivityType =="Upland"
                  & Activity %in% c("Other upland activity",
                                    "Terracing",
                                    "Upland erosion control",
                                    "Water/sediment control basins")
                  & TreatmentMetricLUID %in% c(1, 2)) %>%
    dplyr::mutate(TreatmentMetric=if_else(Unit=="acre", "acres treated", TreatmentMetric)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.upland4 <- df.treatments %>%
    dplyr::filter(ActivityType =="Upland"
                  & Activity %in% c("Upland invasive plant control",
                                    "Upland tree planting",
                                    "Upland vegetation management",
                                    "Upland vegetation planting",
                                    "Voluntary upland tree retention")
                  & TreatmentMetricLUID %in% c(1, 2)) %>%
    dplyr::mutate(TreatmentMetric=if_else(Unit=="acre", "acres treated", TreatmentMetric)) %>%
    dplyr::mutate(Activity_Unit=paste0(Activity," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity)

  tbl.upland.final <- rbind(tbl.upland1, tbl.upland2, tbl.upland3, tbl.upland4) %>%
    dplyr::arrange(SubbasinActual,Activity_Unit) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  eval.upland <- ifelse(NROW(tbl.upland1) > 0, TRUE, FALSE)

  #eval.upland1 <- ifelse(NROW(tbl.upland1) > 0, TRUE, FALSE)
  #eval.upland2 <- ifelse(NROW(tbl.upland2) > 0, TRUE, FALSE)
  #eval.upland3 <- ifelse(NROW(tbl.upland3) > 0, TRUE, FALSE)
  #eval.upland4 <- ifelse(NROW(tbl.upland4) > 0, TRUE, FALSE)

  # -- Urban ----

  tbl.urban <- df.treatments %>%
    dplyr::filter(ActivityType =="Urban"
                  & TreatmentMetricLUID %in% c(1,2)) %>%
    dplyr::mutate(Activity_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(ActivityType, SubbasinActual, year_group, Activity_Unit) %>%
    dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
    bind_rows(dplyr::group_by(.,ActivityType, SubbasinActual, Activity_Unit) %>%
                dplyr::summarise(Quantity=sum(Quantity, na.rm = TRUE)) %>%
                dplyr::mutate(year_group = 'Total')) %>%
    tidyr::spread(year_group, Quantity) %>%
    dplyr::rename(Subbasin=SubbasinActual,
                  'Actvity - Treatment'=Activity_Unit) %>%
    replace(.,is.na(.), 0)

  final.tbl <- rbind(tbl.estuary,
                     tbl.fish.final,
                     tbl.instream.final,
                     tbl.flow,
                     tbl.rip.final,
                     tbl.road,
                     tbl.upland,
                     tbl.urban)

  return(final.tbl)

}
