#' Organize VMR data after import
#'
#' Function accepts a list returned by ImportTaskVMR and returns a
#' dataframe of subject task data
#'
#' @param json A list returned by ImportTaskVMR
#' @return A dataframe
#'
#' @export

OrganizeTaskVMR <- function(data) {

    subject.data <- data$Subject
    global.data  <- data$Global
    block.data   <- data$Blocks
    task.data    <- data$Data

    # Assemble initial behavioral data frame
    df.behav <- task.data[,c('Block', 'Trial', 'Data', 'TargetAngle')]
    df.behav$TargetAngle <- 2*pi*task.data$TargetAngle/360

    # Subject variables
    df.behav <- cbind(subject.data$WorkerID, df.behav)
    names(df.behav)[1] <- c('Subject')

    ### Recode cursor into invariant coordinates
    # There are a few transformations here. By default, the cursor y
    # position runs from 0 at the top of the screen, so we flip the y-axis
    # to get a standard orientation. We then place the origin at the center
    # of the screen and scale the coordinates so that the target array lies
    # on the unit circle.
    Coord2Complex <- function(x, center.coords, ring.distance) {
        x <- (x - center.coords) / ring.distance
        x <- x * c(1, -1)
        return(complex(real = x[1], imaginary = x[2]))
    }
    TransformCoords <- function(x, center.coords, ring.distance) {
        x <- (x - center.coords) / ring.distance
        x <- x * c(1, -1)
        return(x)
    }

    center.coords <- as.numeric(global.data[,c('ScreenCenterX', 'ScreenCenterY')])
    ring.distance <- global.data$TargetDistance

    df.behav$Data <- lapply(df.behav$Data, function(d) {
        d.new <- data.frame(Time = d$Time, x = NA, y = NA,
                            State = d$TrialState)
        coords <- as.matrix(d[,c('CursorX', 'CursorY')])
        cmplx  <- t(apply(coords, 1, TransformCoords,
                          center.coords, ring.distance))
        d.new$x <- cmplx[,1]
        d.new$y <- cmplx[,2]
        return(d.new)
    })

    # Compute response angles, and recode target angle to radians with the
    # standard chart -- i.e. angles in [0,2pi) relative to the x-axis.
    response.cmplx <- apply(task.data[,c('CursorDropX', 'CursorDropY')],
                            1, Coord2Complex, center.coords, ring.distance)
    df.behav$ResponseAngle <- Arg(response.cmplx)
    df.behav$TargetAngle   <- -df.behav$TargetAngle + pi/2
    df.behav$Error         <- df.behav$TargetAngle - df.behav$ResponseAngle
    df.behav$Error         <- Arg(complex(modulus = 1, argument = df.behav$Error))

    # Reaction times
    df.behav$ReactionTime <- task.data$TimeLeaveStart - task.data$TimeFillTarget
    df.behav$MovementTime <- task.data$TimeReachComplete - task.data$TimeLeaveStart
    df.behav$TotalTime    <- df.behav$ReactionTime + df.behav$MovementTime
    df.behav$ValidTrial   <- task.data$RT1Valid & task.data$RT2Valid
    df.behav$ValidTrial[is.na(df.behav$ValidTrial)] <- FALSE

    return(df.behav)
}
