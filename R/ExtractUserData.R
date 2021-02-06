#' Extract user data from json file
#'
#' Internal function for extracting user data from json files exported from
#' MRL1/MRL2/VMR/MRV tasks.
#'
#' @param json A json file imported via jsonlite::read_json()
#' @return A dataframe with subject information extracted from json

ExtractUserData <- function(json) {

    ljson <- parse_json(json, simplifyVector = F)
    system.info <- unlist(ljson[[2]])
    subject.frame <- as.data.frame(t(system.info))
    return(subject.frame)

}
