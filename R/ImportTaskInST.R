#' Extract Simon data from Inquisit file
#'
#' Function for extracting data from inquisit files exported from the Simon task.
#'
#' @param json A path to a inquisit file
#' @return A dataframe
#'
#' @importFrom plyr revalue
#'
#' @export

ImportTaskInST <- function(file) {

    raw <- read.table(file, header = T, sep = '\t')
    df  <- raw[,c('subject', 'trialnum', 'congruence', 'stimhpos',
                  'stimtype', 'response.1', 'correct', 'latency')]
    names(df) <- c('Subject', 'Trial', 'Congruence', 'StimPosition',
                   'StimColor', 'Response', 'Accuracy', 'RT')
    df$Trial  <- 1:nrow(df)
    df$Congruence <- revalue(df$Congruence, c('congruent' = 'Congruent',
                                              'incongruent' = 'Incongruent'))
    df$StimPosition <- revalue(df$StimPosition, c('right' = 'Right', 'left' = 'Left'))
    df$StimColor <- revalue(df$StimColor, c('red' = 'Red', 'blue' = 'Blue'))
    df$Response  <- revalue(df$Response, c('right' = 'Right', 'left' = 'Left'))
    df$Response[df$Response == ''] <- NA
    df$Accuracy <- ifelse(df$Accuracy == 1, 'Correct', 'Incorrect')
    df$Accuracy[is.na(df$Response)] <- 'Spoil'

    return(df)
}
