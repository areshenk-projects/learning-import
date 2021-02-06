#' Extract IGT data from Inquisit file
#'
#' Function for extracting data from inquisit files exported from the IGT.
#'
#' @param json A path to a inquisit file
#' @return A dataframe
#'
#' @export

ImportTaskInIGT <- function(file) {

    raw <- read.table(file, header = T, sep = '\t')
    df  <- raw[,c('subject', 'trialnum', 'response', 'latency',
                  'gain', 'loss')]
    names(df) <- c('Subject', 'Trial', 'Choice', 'RT', 'Win', 'Loss')
    df$Trial  <- 1:nrow(df)
    df$Choice <- as.numeric(substr(df$Choice, start = 5, stop = 5))
    df$Total  <- df$Win - df$Loss
    df$Pot    <- raw$currenttotal

    return(df)
}
