#' Return treatments completed for each project.
#'
#' @param owri.db The path and file name of the owri SQLite database.
#' @param complete.years Vector of numeric years used to fetch projects. Only projects completed in these years will be summarized.
#' @param huc8 Vector of numeric HUC8 codes used to fetch projects. Only projects within the HUC8 will be summarized.
#'
#' @keywords owri, complete years, huc8
#' @export
#' @return Dataframe with the quantity treatments implemented for each project.
#'

treatments <- function(owri.db, complete.years, huc8) {

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
  TreatmentMetricLU2 <- data.frame(TreatmentMetricLUID=c(1:8),
                                   TreatmentMetric=c("Area treated",
                                                     "Number of treatments",
                                                     "Stream sides treated",
                                                     "Volume",
                                                     "Length of treatment",
                                                     "Percent urban area affected",
                                                     "Percent watershed area affected",
                                                     "Volumetric flow rate"))

  df.treatments <- Treatment %>%
    dplyr::select(PROJNUM ,ActivityTypeLUID, ActivityLUID, TreatmentLUID, TreatmentID) %>%
    dplyr::left_join(ActivityTypeLU[,c("ActivityTypeLUID", "ActivityType")], by="ActivityTypeLUID") %>%
    dplyr::left_join(ActivityLU[,c("ActivityLUID", "Activity")], by="ActivityLUID") %>%
    dplyr::left_join(TreatmentLU[,c("TreatmentLUID", "Treatment")], by="TreatmentLUID") %>%
    dplyr::left_join(TreatmentMetric, by="TreatmentID") %>%
    dplyr::left_join(TreatmentMetricLU2, by="TreatmentMetricLUID") %>%
    dplyr::left_join(UnitLU, by="UnitLUID") %>%
    dplyr::left_join(ProjectInfo, by="PROJNUM") %>%
    dplyr::filter(CompleteYear %in% complete.years & drvdHUC4thField %in% huc8) #%>%
    # dplyr::mutate(TreatmentMetric_Unit=dplyr::case_when(TreatmentMetricLUID == 1 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 2 ~ paste0("Number of ",Unit,"s"),
                                                        # TreatmentMetricLUID == 3 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 4 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 5 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 6 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 7 ~ paste0(Unit,"s treated"),
                                                        # TreatmentMetricLUID == 8 ~ paste0(Unit,"s treated"),
                                                        # ))

  return(df.treatments)

}



