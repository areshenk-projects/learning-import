#' Extract VMR data from json file
#'
#' Function for extracting data from json files exported from the VMR task.
#' Parses the json file and returns a list containing subject data, system data,
#' global task variables, and behavior.
#'
#' @param json A path to a .json file
#' @return A list of four data frames
#'
#' @importFrom data.table rbindlist
#' @importFrom plyr rbind.fill
#' @importFrom jsonlite read_json
#' @export

ImportTaskVMR <- function(json) {

    raw <- read_json(json)

    # Extract information
    global.info    <- raw[[2]]$TrialParams[[1]]
    task.info      <- raw[[6]]$TrialBlocks
    subject.info   <- raw[[5]]
    system.info    <- raw[[2]]

    # Assemble global task parameters
    df.global <- data.frame(TargetDistance = global.info$Globals$TargetDistance,
                            WindowDimX     = system.info$WindowDim[[1]],
                            WindowDimY     = system.info$WindowDim[[2]],
                            ScreenCenterX  = system.info$WindowDim[[1]]/2,
                            ScreenCenterY  = system.info$WindowDim[[2]]/2)

    # Assemble block information
    task.block.idx <- which(sapply(global.info$Blocks,
                            function(b) b$TrialType) == 'REACH')
    block.info <- global.info$Blocks[task.block.idx]
    df.block   <- rbindlist(lapply(block.info, function(b) {
        b <- unlist(b[-which(names(b) == 'TargetAngles')])
        return(as.data.frame(t(b)))
    }))

    # Assemble subject information
    df.subject <- data.frame(WorkerID   = system.info$TurkWorkerId,
                             Assignment = system.info$TurkAssignmentId,
                             Age    = subject.info$responses$Age,
                             Sex    = subject.info$responses$Sex,
                             Device = subject.info$responses$Device,
                             Hand   = subject.info$responses$Hand,
                             ControlFile     = system.info$ControlFile,
                             OperatingSystem = system.info$OS,
                             Browser         = system.info$Browser$browser,
                             BrowserVersion = system.info$Browser$version)

    # Assemble subject behavior
    response.blocks <- task.info[task.block.idx]
    names(response.blocks) <- df.block$Name

    df.task <- lapply(df.block$Name, function(lab) {
        trials <- response.blocks[[lab]]$Trials

        # Extract trial variables
        df <- rbindlist(lapply(trials, function(i) i$Variables), fill = T)

        # Extract reaction times
        df.times <- rbindlist(lapply(trials, function(i) {
            rt <- sapply(i$Events, function(e) e$Time)
            names(rt) <- paste0('Time', sapply(i$Events, function(e) e$Event))
            return(as.data.frame(t(rt)))
        }), fill = T)
        df <- cbind(df, df.times)

        df <- cbind(lab, 1:nrow(df), df)
        names(df)[1:2] <- c('Block', 'Trial')

        df$Data <- lapply(trials, function(i) rbindlist(i$Streams, fill = T))
        return(df)
    })
    df.task <- do.call(rbind.fill, df.task)

    ret <- list(Subject = df.subject, Global = df.global,
                Blocks = df.block, Data = df.task)

    return(ret)

}
