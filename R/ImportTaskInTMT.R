#' Extract Trail Making data from Inquisit file
#'
#' Function for extracting data from inquisit files exported from the Train Making task.
#'
#' @param json A path to a inquisit file
#' @return A dataframe
#'
#' @export

ImportTaskInTMT <- function(file) {

    raw <- read.table(file, header = T, sep = '\t')
    raw <- subset(raw, blockcode %in% c('trail1', 'trail2'))
    df  <- raw[,c('subject',  'blockcode', 'trialcode',
                  'startDotNumber', 'targetDotNumber')]
    names(df) <- c('Subject', 'Block', 'TrialCode', 'Start', 'Target')

    # Recode errors
    trail1errors <- c(0, diff(raw$trail1Errors))
    trail2errors <- c(0, diff(raw$trail2Errors))

    df$Accuracy <- NA
    df$Accuracy[df$Block == 'trail1'] <- 1 - trail1errors[df$Block == 'trail1']
    df$Accuracy[df$Block == 'trail2'] <- 1 - trail2errors[df$Block == 'trail2']
    df$Accuracy <- ifelse(df$Accuracy == 1, 'Correct', 'Incorrect')

    # Recode RTs
    trail1rt <- diff(raw$trail1Time[raw$blockcode == 'trail1'])
    trail2rt <- diff(raw$trail2Time[raw$blockcode == 'trail2'])

    df$RT <- NA
    df <- subset(df, !TrialCode %in% c('trail1TaskFinished', 'trail2TaskFinished'))
    df$RT <- c(trail1rt, trail2rt)/1000

    df$Trial  <- c(1:length(trail1rt), 1:length(trail2rt))
    df <- df[,c('Subject', 'Block', 'Trial', 'Start', 'Target', 'RT', 'Accuracy')]

    return(df)
}
