#' Extract subject data from json file
#'
#' Internal function for extracting block data from json files exported from
#' MRL1/MRL2/VMR/MRV tasks.
#'
#' @param json A json file imported via jsonlite::read_json()
#' @return A dataframe with subject data extracted from json

CreateSubjectDataframe_MotorTasks <- function(json) {

    djson <- parse_json(json, simplifyVector = T)

    # Trial information
    trial.info <- djson$TrialBlocks
    trial.info <- trial.info[[which(!sapply(trial.info, is.null))]]
    trial.info <- flatten(trial.info, recursive = TRUE)
    trial.info <- subset(trial.info, !TrialTypeName %in% c('Instructions', 'FeedbackPath'))

    df <- do.call(rbind.fill, lapply(1:nrow(trial.info), function(i) {
        d <- flatten(trial.info$Trials[[i]], recursive = T)
        df <- CreateBlockDataframe(d)
        df <- cbind(data.frame(Block = rep(i, nrow(df))), df)
        return(df)
    }))

    return(df)
}

