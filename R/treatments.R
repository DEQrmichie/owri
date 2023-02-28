#' Return treatments completed for each project.
#'
#' @param owri.db The path and file name of the owri SQLite database.
#' @param complete.years Vector of numeric years used to fetch projects. Only projects completed in these years will be summarized. Default is NA resulting in summary of all years in the database.
#' @param huc8 Vector of numeric HUC8 codes used to fetch projects. Only projects within the HUC8 will be summarized. Default is NA resulting in summary of all HUC8 codes in the database.
#'
#' @keywords owri, complete years, huc8
#' @export
#' @return Dataframe with the quantity treatments implemented for each project.
#'

treatments <- function(owri.db, complete.years=NA, huc8=NA) {

  options(stringsAsFactors = FALSE)

  channel <- DBI::dbConnect(RSQLite::SQLite(), owri.db)

  # I'm keeping the dataframes the same name that OWEB uses
  ActivityLU <- DBI::dbReadTable(channel, "ActivityLU")
  ActivityTypeLU <- DBI::dbReadTable(channel, "ActivityTypeLU")
  ProjectInfo <- DBI::dbReadTable(channel, "ProjectInfo")
  Treatment <- DBI::dbReadTable(channel, "Treatment")
  TreatmentLU <- DBI::dbReadTable(channel, "TreatmentLU")
  TreatmentMetric <- DBI::dbReadTable(channel, "TreatmentMetric")
  TreatmentMetricLU <- DBI::dbReadTable(channel, "TreatmentMetricLU")
  UnitLU <- DBI::dbReadTable(channel, "UnitLU")
  RiparianVtrRcr <- DBI::dbReadTable(channel, "RiparianVtrRcr")
  DBI::dbDisconnect(channel)

  # Rename some of the Treatment Metrics
  # 9 is added by me since there isn't a buffer width treatment metric
  # to join with the RiparianVtrRcr table
  TreatmentMetricLU2 <- data.frame(TreatmentMetricLUID = c(1:9),
                                   TreatmentMetric = c("Area treated",
                                                       "Number of treatments",
                                                       "Stream sides treated",
                                                       "Volume",
                                                       "Length of treatment",
                                                       "Percent urban area affected",
                                                       "Percent watershed area affected",
                                                       "Volumetric flow rate",
                                                       "Width of riparian treament"))

  df.treatments <- Treatment %>%
    dplyr::select(PROJNUM ,ActivityTypeLUID, ActivityLUID, TreatmentLUID, TreatmentID) %>%
    dplyr::left_join(ActivityTypeLU[,c("ActivityTypeLUID", "ActivityType")], by="ActivityTypeLUID") %>%
    dplyr::left_join(ActivityLU[,c("ActivityLUID", "Activity")], by="ActivityLUID") %>%
    dplyr::left_join(TreatmentLU[,c("TreatmentLUID", "Treatment")], by="TreatmentLUID") %>%
    dplyr::left_join(TreatmentMetric, by="TreatmentID") %>%
    dplyr::left_join(TreatmentMetricLU2, by="TreatmentMetricLUID") %>%
    dplyr::left_join(UnitLU, by="UnitLUID") %>%
    dplyr::left_join(ProjectInfo, by="PROJNUM")

  # Pull in the voluntary riparian treatment metrics
  # using treatment metric 9 for riparian width, which was added to the treatment metric table
  RiparianVtrRcr2 <- RiparianVtrRcr %>%
    dplyr::select(TreatmentID, mile = LengthMiles, acre = BestAcres, feet = WidthFeet) %>%
    tidyr::gather(-TreatmentID, key = "Unit", value = "Quantity") %>%
    dplyr::mutate(UnitLUID = dplyr::case_when(Unit == "mile" ~ 10,
                                              Unit == "acre" ~ 1,
                                              Unit == "feet" ~ 8),
                  TreatmentMetricLUID = dplyr::case_when(Unit == "mile" ~ 5,
                                                         Unit == "acre" ~ 1,
                                                         Unit == "feet" ~ 9)) %>%
    dplyr::left_join(TreatmentMetricLU2, by = "TreatmentMetricLUID")

  # Add in all the project info, Not keeping buffer width since it cannot be summed and
  # is not reported in many cases ( = 0)
  RiparianVtrRcr3 <- df.treatments %>%
    dplyr::select(-Unit, -UnitLUID, -Quantity, -TreatmentMetricLUID, -TreatmentMetric) %>%
    dplyr::inner_join(RiparianVtrRcr2, by = "TreatmentID") %>%
    dplyr::filter(Unit %in% c("mile", "acre"))

  df.treatments2 <- df.treatments %>%
    filter(!TreatmentID %in% unique(RiparianVtrRcr3$TreatmentID)) %>%
    rbind(RiparianVtrRcr3)

  if (!all(is.na(complete.years))) {
    df.treatments2 <- df.treatments2 %>%
      dplyr::filter(CompleteYear %in% complete.years)
  }


  if (!all(is.na(huc8))) {
    df.treatments2 <- df.treatments2 %>%
      dplyr::filter(drvdHUC4thField %in% huc8)
  }

  return(df.treatments2)

}



