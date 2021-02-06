#' Extract subject data from json file
#'
#' Internal function for extracting block data from json files exported from
#' PAL task.
#'
#' @param json A json file imported via jsonlite::read_json()
#' @return A dataframe with subject data extracted from json

CreateSubjectDataframe_PALTask <- function(json) {

    djson <- parse_json(json, simplifyVector = T)
    df <- do.call(rbind, djson$TrialData)$Variables

    return(df)
}
