#' Organize MRL2 data after import
#'
#' Function accepts a list returned by ImportTaskMRL2 and returns a
#' dataframe of subject task data
#'
#' @param json A list returned by ImportTaskMRL2
#' @return A dataframe
#'
#' @export

OrganizeTaskMRL2 <- function(data) {

    subject.data <- data$Subject
    global.data  <- data$Global
    block.data   <- data$Blocks
    task.data    <- data$Data

    # Assemble initial behavioral data frame
    df.behav <- task.data[,c('Block', 'Trial', 'Data', 'Score')]

    # Subject variables
    df.behav <- cbind(subject.data$WorkerID, df.behav)
    names(df.behav)[1:1] <- c('Subject')

    ### Recode cursor into invariant coordinates
    # There are a few transformations here. By default, the cursor y
    # position runs from 0 at the top of the screen, so we flip the y-axis
    # to get a standard orientation. We then place the origin at the center
    # of the screen and scale the coordinates so that the target array lies
    # on the unit circle.
    TransformCoords <- function(x, center.coords, target.distance) {
        x <- (x - center.coords) / target.distance
        x <- x * c(1, -1)
        return(x)
    }

    center.coords <- as.numeric(global.data[,c('ScreenCenterX', 'ScreenCenterY')])
    target.distance <- global.data$TargetDistance

    df.behav$Data <- lapply(df.behav$Data, function(d) {
        d.new <- data.frame(Time = d$Time, x = NA, y = NA,
                            State = d$TrialState)
        coords <- as.matrix(d[,c('CursorX', 'CursorY')])
        cmplx  <- t(apply(coords, 1, TransformCoords,
                          center.coords, target.distance))
        d.new$x <- cmplx[,1]
        d.new$y <- cmplx[,2]
        return(d.new)
    })

    # Reaction times
    df.behav$ReactionTime <- task.data$TimeLeaveStart - task.data$TimeShowStart
    df.behav$MovementTime <- task.data$TimeReachComplete - task.data$TimeLeaveStart
    df.behav$TotalTime    <- df.behav$ReactionTime + df.behav$MovementTime
    df.behav$ValidTrial   <- task.data$FeedbackState == 'Good'

    return(df.behav)
}
