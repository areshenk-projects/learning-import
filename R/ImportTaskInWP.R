#' Extract weather prediction data from Inquisit file
#'
#' Function for extracting data from inquisit files exported from the
#' weather prediction task.
#'
#' @param json A path to a inquisit file
#' @return A dataframe
#'
#' @importFrom plyr revalue
#'
#' @export

ImportTaskInWP <- function(file) {

    raw <- read.table(file, header = T, sep = '\t')
    df  <- raw[,c('subject', 'countTestBlock', 'trialnum', 'trialcode', 'pattern',
                  'cue1', 'cue2', 'cue3', 'cue4',
                  'currentCue1', 'currentCue2', 'currentCue3', 'currentCue4',
                  'P_outcome1', 'P_outcome2', 'outcome', 'response', 'latency',
                  'correctOutcomePrediction')]
    names(df) <- c('Subject', 'Block', 'Trial', 'TrialCode', 'Pattern',
                   'Cue1', 'Cue2', 'Cue3', 'Cue4',
                   'CurrentCue1', 'CurrentCue2', 'CurrentCue3', 'CurrentCue4',
                   'ProbOutcome1', 'ProbOutcome2', 'Outcome', 'Response',
                   'RT', 'Accuracy')
    df$Trial  <- 1:nrow(df)
    df$Response <- as.numeric(revalue(df$Response, c('outcome1' = '1', 'outcome2' = '2')))
    df$Response[df$Response == 0] <- NA
    df$Accuracy <- ifelse(df$Accuracy == 1, 'Correct', 'Incorrect')
    df$Accuracy[is.na(df$Response)] <- 'Spoil'

    return(df)
}
