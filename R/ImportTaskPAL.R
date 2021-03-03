#' Extract PAL data from json file
#'
#' Function for extracting data from json files exported from the PAL task.
#' Parses the json file and returns a list containing global task variables
#' and behavior.
#'
#' @param json A path to a .json file
#' @return A list of four data frames
#'
#' @export

ImportTaskPAL <- function(json) {

    raw <- read_json(json)

    # Extract information
    global.info    <- raw[[2]]$TrialParams[[1]]$Globals
    task.info      <- raw[[4]]$TrialData
    subject.info   <- raw[[4]]$SubjectInfo
    system.info    <- raw[[2]]

    # Assemble global task parameters
    df.global <- data.frame(Box.Size      = global.info$BoxSize,
                            WindowDimX    = system.info$WindowDim[[1]],
                            WindowDimY    = system.info$WindowDim[[2]],
                            ScreenCenterX = system.info$WindowDim[[1]]/2,
                            ScreenCenterY = system.info$WindowDim[[2]]/2)

    # Task info
    pattern <- sapply(task.info, function(i) unlist(i$Variables$Test$Patterns))
    display <- sapply(task.info, function(i) unlist(i$Variables$Test$DisplayOrder))
    select  <- sapply(task.info, function(i) unlist(i$Variables$Test$SelectOrder))
    n.target <- nrow(pattern)
    n.trial  <- ncol(pattern)

    df.task <- data.frame(Trial = rep(1:n.trial, each = n.target),
                          Order = rep(1:n.target, n.trial),
                          Pattern = as.numeric(pattern),
                          Display = as.numeric(display),
                          Select  = as.numeric(select))

    ret <- list(Global = df.global, Data = df.task)
    return(ret)
}
