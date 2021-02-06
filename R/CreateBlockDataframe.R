#' Extract block data from json file
#'
#' Internal function for extracting block data from json files exported from
#' MRL1/MRL2/VMR/MRV tasks.
#'
#' @param json A json file imported via jsonlite::read_json()
#' @return A dataframe with task data extracted from json

CreateBlockDataframe_MotorTasks <- function(d) {

    # Relabel scalar variables
    labs <- names(d)
    df <- d[,-which(labs %in% c('TrialNumber', 'Events', 'Streams'))]
    oldlabs <- names(df)
    names(df) <- sapply(strsplit(oldlabs, split = '.', fixed = T),
                        function(i) i[[2]])

    # Extract event information
    df.event <- do.call(rbind.fill, lapply(d$Events, function(e) {
        x <- e$Time
        lab <- paste0('Event.', e$Event)
        d <- as.data.frame(t(as.data.frame(x)))
        names(d) <- lab
        return(d)
    }))
    df <- cbind(df, df.event)
    df$Data <- d$Streams

    df <- cbind(d[,c('TrialNumber')], df)
    names(df)[1] <- c('TrialNumber')
    return(df)
}
