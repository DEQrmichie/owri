#' Summarize OWRI treatments.
#'
#' @param owri.db The path and file name of the owri SQLite database.
#' @param complete.years Vector of numeric years used to fetch projects. The year range must span 20 years. Only projects completed in these years will be summarized.
#' @param huc8 Vector of numeric HUC8 codes used to fetch projects. Only projects within the HUC8 will be summarized.
#'
#' @keywords owri, complete years, huc8,
#' @export
#' @return Dataframe with the sum of treatments implemented grouped into five yearly periods by huc8 and activity type.
#'

owri_summary5 <- function(owri.db, complete.years, huc8) {

  options(stringsAsFactors = FALSE)

  channel <- DBI::dbConnect(RSQLite::SQLite(), owri.db)

  # I'm keeping the dataframes the same name that OWEB uses
  ActivityCost <- DBI::dbReadTable(channel, "ActivityCost")
  ActivityLU <- DBI::dbReadTable(channel, "ActivityLU")
  ActivityTypeLU <- DBI::dbReadTable(channel, "ActivityTypeLU")
  ATATLU <- DBI::dbReadTable(channel, "ActivityTypeLUXActivityLUXTreatmentLU")
  ProjectInfo <- DBI::dbReadTable(channel, "ProjectInfo")
  Treatment <- DBI::dbReadTable(channel, "Treatment")
  TreatmentLU <- DBI::dbReadTable(channel, "TreatmentLU")
  TreatmentMetric <- DBI::dbReadTable(channel, "TreatmentMetric")
  TreatmentMetricLU <- DBI::dbReadTable(channel, "TreatmentMetricLU")
  UnitLU <- DBI::dbReadTable(channel, "UnitLU")
  RiparianVtrRcr <- DBI::dbReadTable(channel, "RiparianVtrRcr")
  DBI::dbDisconnect(channel)

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
    dplyr::mutate(UnitLUID=dplyr::case_when(Unit == "mile" ~ 10,
                                     Unit == "acre" ~ 1,
                                     Unit == "feet" ~ 8),
                  TreatmentMetricLUID=dplyr::case_when(Unit == "mile" ~ 5,
                                                       Unit == "acre" ~ 1,
                                                       Unit == "feet" ~ 5))

  df.treatments <- Treatment %>%
    dplyr::select(PROJNUM ,ActivityTypeLUID, ActivityLUID, TreatmentLUID, TreatmentID) %>%
    dplyr::left_join(ActivityTypeLU[,c("ActivityTypeLUID", "ActivityType")], by="ActivityTypeLUID") %>%
    dplyr::left_join(ActivityLU[,c("ActivityLUID", "Activity")], by="ActivityLUID") %>%
    dplyr::left_join(TreatmentLU[,c("TreatmentLUID", "Treatment")], by="TreatmentLUID") %>%
    dplyr::left_join(TreatmentMetric, by="TreatmentID") %>%
    dplyr::left_join(UnitLU, by="UnitLUID") %>%
    dplyr::left_join(ProjectInfo, by="PROJNUM") %>%
    dplyr::filter(CompleteYear %in% complete.years & drvdHUC4thField %in% huc8)

  #-- Treatment Unit LU table --------------

  Treatment_Unit_LU <- Treatment %>%
    dplyr::select(PROJNUM ,ActivityTypeLUID, ActivityLUID, TreatmentLUID, TreatmentID) %>%
    dplyr::left_join(ActivityTypeLU[,c("ActivityTypeLUID", "ActivityType")], by="ActivityTypeLUID") %>%
    dplyr::left_join(ActivityLU[,c("ActivityLUID", "Activity")], by="ActivityLUID") %>%
    dplyr::left_join(TreatmentLU[,c("TreatmentLUID", "Treatment")], by="TreatmentLUID") %>%
    dplyr::left_join(TreatmentMetric, by="TreatmentID") %>%
    dplyr::left_join(UnitLU, by="UnitLUID") %>%
    dplyr::left_join(ProjectInfo, by="PROJNUM") %>%
    dplyr::select(ActivityTypeLUID, ActivityType, ActivityLUID, Activity, TreatmentLUID, Treatment,
                  TreatmentMetricLUID, UnitLUID, Unit) %>%
    dplyr::distinct() %>%
    dplyr::left_join(ATATLU, by=c("ActivityTypeLUID","ActivityType","ActivityLUID","Activity","TreatmentLUID","Treatment")) %>%
    dplyr::filter(active == "Y") %>%
    dplyr::arrange(DisplayOrder) %>%
    dplyr::filter((ActivityType =="Estuary" & Unit == "acre") |
                    (ActivityType =="Fish Passage" & TreatmentMetricLUID==2) & !(Treatment == "Other treatment") |
                    (ActivityType =="Fish Screening" & TreatmentMetricLUID==2) |
                    (ActivityType =="Instream" & Activity %in% c("Bank stabilization") & Unit =="mile") |
                    (ActivityType =="Instream" & Activity %in% c("Animal species removal","Beaver introduction/encouragement")) |
                    (ActivityType =="Instream" & Activity %in% c("Channel alteration") & !(Unit %in% c("main channel", "structure"))) |
                    (ActivityType =="Instream" & Activity %in% c("Engineered structures")) |
                    (ActivityType =="Instream" & ActivityLUID %in% c(8:13)) |
                    (ActivityType =="Instream" & Activity %in% c("Off-channel habitat") & TreatmentMetricLUID %in% c(2)) |
                    (ActivityType =="Instream" & Activity %in% c("Other instream activity") & TreatmentLUID %in% c(76)) |
                    (ActivityType =="Instream" & Activity %in% c("Salmon carcass placement") & Unit %in% c("pound", "mile")) |
                    (ActivityType =="Instream Flow" & !(is.na(Unit))) |
                    (ActivityType =="Riparian" & Activity %in% c("Voluntary riparian tree retention") & Unit %in% c("mile", "acre")) |
                    (ActivityType =="Riparian" & Activity %in% c("Riparian tree planting", "Riparian fencing") & Unit %in% c("mile", "acre")) |
                    (ActivityType =="Road" & Activity %in% c("Peak flow passage improvement",
                                                             "Surface drainage improvement",
                                                             "Road stabilization",
                                                             "Road decommission" ) & TreatmentMetricLUID %in% c(2,5)) |
                    (ActivityType =="Upland" & Activity %in% c("Grazing management",
                                                               "Nutrient/manure management",
                                                               "Off-channel livestock or wildlife watering",
                                                               "Upland fencing",
                                                               "Grazing management",
                                                               "Nutrient/manure management",
                                                               "Off-channel livestock or wildlife watering",
                                                               "Upland fencing",
                                                               "Terracing",
                                                               "Upland erosion control",
                                                               "Water/sediment control basins",
                                                               "Upland invasive plant control",
                                                               "Upland tree planting",
                                                               "Upland vegetation management",
                                                               "Upland vegetation planting",
                                                               "Voluntary upland tree retention") & TreatmentMetricLUID %in% c(1, 2)) |
                    (ActivityType =="Urban" & TreatmentMetricLUID %in% c(1, 2))
    ) %>%
    dplyr::mutate(UnitLUID=ifelse(Unit=="station", as.integer(10), UnitLUID), # change station to miles (1 station = 100 feet = 0.0189394 miles)
                  Unit=ifelse(Unit=="station", "mile", Unit), # change feet to miles
                  UnitLUID=ifelse(Unit=="feet", as.integer(10), UnitLUID),
                  Unit=ifelse(Unit=="feet", "mile", Unit)) %>%
    dplyr::mutate(TreatmentMetric=dplyr::case_when(TreatmentMetricLUID==1 ~ paste0(Unit,"s treated"),
                                                   TreatmentMetricLUID==2 & Unit=="pound" ~ paste0(Unit,"s"),
                                                   TreatmentMetricLUID==2 & Unit=="each"~ paste0("Number of treatments"),
                                                   TreatmentMetricLUID==2 ~ paste0("Number of ",Unit,"s"),
                                                   TreatmentMetricLUID==4 ~ Unit,
                                                   TreatmentMetricLUID==5 & Unit=="structure"~ paste0("Number of ",Unit,"s"),
                                                   TreatmentMetricLUID==5 & Unit=="mile"~ paste0(Unit,"s of treatment"),
                                                   TreatmentMetricLUID==8 ~ Unit)) %>%
    dplyr::mutate(Treatment_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::select(ActivityType, Activity, Treatment, TreatmentMetric, Treatment_Unit, DisplayOrder) %>%
    dplyr::distinct() %>%
    dplyr::arrange(DisplayOrder) %>%
    dplyr::mutate(Treatment_UnitLUID=dplyr::row_number())

  HUC_nesting <- df.treatments %>%
    dplyr::select(drvdHUC4thField, SubbasinActual) %>%
    dplyr::mutate(HUC8_Name = paste0(df.treatments$drvdHUC4thField,"_",df.treatments$SubbasinActual)) %>%
    dplyr::distinct(HUC8_Name) %>%
    tidyr::separate(col = HUC8_Name, into = c("drvdHUC4thField", "SubbasinActual"), sep = "_") %>%
    dplyr::mutate(drvdHUC4thField =  as.integer(drvdHUC4thField))

  # table to join to include all combinations
  Treatment_Unit_join <- Treatment_Unit_LU %>%
    tidyr::expand(tidyr::nesting(ActivityType, Treatment_Unit),
                  tidyr::nesting(drvdHUC4thField = HUC_nesting$drvdHUC4thField,
                                 SubbasinActual = HUC_nesting$SubbasinActual))

  #-- Year Groups --------------

  year_group1 <- paste0(complete.years[1],"-",complete.years[5])
  year_group2 <- paste0(complete.years[6],"-",complete.years[10])
  year_group3 <- paste0(complete.years[11],"-",complete.years[15])
  year_group4 <- paste0(complete.years[16],"-",complete.years[20])

  # generate year groups
  df.treatments <- df.treatments %>%
    dplyr::mutate(year_group=dplyr::case_when(CompleteYear %in% c(complete.years[1:5]) ~ year_group1,
                                              CompleteYear %in% c(complete.years[6:10]) ~ year_group2,
                                              CompleteYear %in% c(complete.years[11:15]) ~ year_group3,
                                              CompleteYear %in% c(complete.years[16:20]) ~ year_group4,
                                              TRUE ~ as.character(NA)),
                  year_group=factor(year_group, levels = c(year_group1,year_group2,year_group3,year_group4)))

  #-- Treatment Summary -----

  tbl.rip1 <- df.treatments %>%
    dplyr::left_join(ATATLU, by=c("ActivityTypeLUID","ActivityType","ActivityLUID","Activity","TreatmentLUID","Treatment")) %>%
    dplyr::select(-Unit,-UnitLUID,-Quantity, -TreatmentMetricLUID) %>%
    dplyr::left_join(RiparianVtrRcr2, by="TreatmentID") %>%
    dplyr::filter(ActivityType =="Riparian" &
                    Activity %in% c("Voluntary riparian tree retention") &
                    Unit %in% c("mile", "acre"))

  tbl.final <- df.treatments %>%
    dplyr::left_join(ATATLU, by=c("ActivityTypeLUID","ActivityType","ActivityLUID","Activity","TreatmentLUID","Treatment")) %>%
    dplyr::filter(active == "Y") %>%
    dplyr::filter((ActivityType =="Estuary" & Unit == "acre") |
                    (ActivityType =="Fish Passage" & TreatmentMetricLUID==2) & !(Treatment == "Other treatment") |
                    (ActivityType =="Fish Screening" & TreatmentMetricLUID==2) |
                    (ActivityType =="Instream" & Activity %in% c("Bank stabilization") & Unit =="mile") |
                    (ActivityType =="Instream" & Activity %in% c("Animal species removal","Beaver introduction/encouragement")) |
                    (ActivityType =="Instream" & Activity %in% c("Channel alteration") & !(Unit %in% c("main channel", "structure"))) |
                    (ActivityType =="Instream" & Activity %in% c("Engineered structures")) |
                    (ActivityType =="Instream" & ActivityLUID %in% c(8:13)) |
                    (ActivityType =="Instream" & Activity %in% c("Off-channel habitat") & TreatmentMetricLUID %in% c(2)) |
                    (ActivityType =="Instream" & Activity %in% c("Other instream activity") & TreatmentLUID %in% c(76)) |
                    (ActivityType =="Instream" & Activity %in% c("Salmon carcass placement") & Unit %in% c("pound", "mile")) |
                    (ActivityType =="Instream Flow" & !(is.na(Unit))) |
                    #(ActivityType =="Riparian" & Activity %in% c("Voluntary riparian tree retention") & Unit %in% c("mile", "acre")) |
                    (ActivityType =="Riparian" & Activity %in% c("Riparian tree planting", "Riparian fencing") & Unit %in% c("mile", "acre")) |
                    (ActivityType =="Road" & Activity %in% c("Peak flow passage improvement",
                                                             "Surface drainage improvement",
                                                             "Road stabilization",
                                                             "Road decommission" ) & TreatmentMetricLUID %in% c(2,5)) |
                    (ActivityType =="Upland" & Activity %in% c("Grazing management",
                                                               "Nutrient/manure management",
                                                               "Off-channel livestock or wildlife watering",
                                                               "Upland fencing",
                                                               "Grazing management",
                                                               "Nutrient/manure management",
                                                               "Off-channel livestock or wildlife watering",
                                                               "Upland fencing",
                                                               "Terracing",
                                                               "Upland erosion control",
                                                               "Water/sediment control basins",
                                                               "Upland invasive plant control",
                                                               "Upland tree planting",
                                                               "Upland vegetation management",
                                                               "Upland vegetation planting",
                                                               "Voluntary upland tree retention") & TreatmentMetricLUID %in% c(1, 2)) |
                    (ActivityType =="Urban" & TreatmentMetricLUID %in% c(1, 2))
    ) %>%
    rbind(tbl.rip1) %>%
    dplyr::mutate(Quantity=ifelse(Unit=="station", Quantity*0.0189394, Quantity), # convert station to miles (1 station = 100 feet = 0.0189394 miles)
                  UnitLUID=ifelse(Unit=="station", as.integer(10), UnitLUID),
                  Unit=ifelse(Unit=="station", "mile", Unit),
                  Quantity=ifelse(Unit=="feet", Quantity*0.000189394, Quantity), # convert feet to miles
                  UnitLUID=ifelse(Unit=="feet", as.integer(10), UnitLUID),
                  Unit=ifelse(Unit=="feet", "mile", Unit)) %>%
    dplyr::mutate(TreatmentMetric=dplyr::case_when(TreatmentMetricLUID==1 ~ paste0(Unit,"s treated"),
                                                   TreatmentMetricLUID==2 & Unit=="pound" ~ paste0(Unit,"s"),
                                                   TreatmentMetricLUID==2 & Unit=="each"~ paste0("Number of treatments"),
                                                   TreatmentMetricLUID==2 ~ paste0("Number of ",Unit,"s"),
                                                   TreatmentMetricLUID==4 ~ Unit,
                                                   TreatmentMetricLUID==5 & Unit=="structure"~ paste0("Number of ",Unit,"s"),
                                                   TreatmentMetricLUID==5 & Unit=="mile"~ paste0(Unit,"s of treatment"),
                                                   TreatmentMetricLUID==8 ~ Unit)) %>%
    dplyr::mutate(Treatment_Unit=paste0(Treatment," (",TreatmentMetric,")")) %>%
    dplyr::group_by(drvdHUC4thField, SubbasinActual, ActivityType, year_group, Treatment_Unit, DisplayOrder) %>%
    dplyr::summarise(Quantity=round(sum(Quantity, na.rm = TRUE),2)) %>%
    tidyr::spread(year_group, Quantity) %>%
    dplyr::right_join(Treatment_Unit_join, by = c("ActivityType", "Treatment_Unit", "drvdHUC4thField", "SubbasinActual")) %>%
    replace(.,is.na(.), 0) %>%
    dplyr::rename(HUC8=drvdHUC4thField,
                  HUC8_Name=SubbasinActual) %>%
    dplyr::arrange(HUC8, DisplayOrder) %>%
    dplyr::select(-DisplayOrder)

  tbl.final$Total <- rowSums(tbl.final[,c(5:8)], na.rm=TRUE)

  return(tbl.final)
}
