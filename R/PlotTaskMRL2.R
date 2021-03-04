#' Plot subject MRL2 data
#'
#' Function accepts a a dataframe output by OrganizeDataMRL1 and plots score
#' and reaction time.
#'
#' @param data A dataframe returned by OrganizeTaskMRL1
#' @param plot Logical. If true, return a ggplot object. If false, returns a dataframe.
#' @return Either a ggplot object or a dataframe
#'
#' @importFrom ggplot2 ggplot aes facet_grid theme_classic scale_color_gradient2
#' @importFrom ggplot2 scale_linetype_manual geom_point geom_path theme
#' @importFrom ggplot2 ggplotGrob element_blank element_rect
#' @importFrom gridExtra grid.arrange
#' @importFrom grid grid.newpage grid.draw unit.pmax
#' @export

PlotTaskMRL2 <- function(data, plot = T) {

    # Assemble trial information
    df <- data[,c('Block', 'Trial', 'Data', 'Score', 'ValidTrial')]

    # Assemble raw trajectories
    movement.phase.index <- 5
    df$Data <- lapply(df$Data, function(i) {
        subset(i, State == movement.phase.index)
    })
    df$Block <- factor(df$Block, levels = unique(df$Block))

    # Assemble plot frame
    pf <- do.call(rbind, lapply(1:nrow(df), function(i) {
        data.frame(Block = df$Block[i], Trial = df$Trial[i],
                   Score = df$Score[i],
                   Valid = ifelse(df$ValidTrial[i], 'Valid', 'Invalid'),
                   x = df$Data[[i]]$x, y = df$Data[[i]]$y)
    }))
    pf$Block <- factor(pf$Block, levels = unique(pf$Block))

    p.scores <-
        ggplot(df, aes(x = .data$Trial, y = .data$Score,
                       color = .data$ValidTrial)) +
        facet_grid(. ~ Block) + theme_classic() + geom_path() +
        scale_color_manual(values = c('red', 'black')) +
        geom_point() + xlab('') +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              legend.position = 'right',
              panel.grid = element_blank())

    p.trajectories <-
        ggplot(pf, aes(x = .data$x, y = .data$y, color = .data$Score,
                       linetype = .data$Valid, group = .data$Trial)) +
        facet_grid(. ~ Block) + theme_classic() + geom_path() +
        scale_color_gradient2(low = '#2166ac', mid = 'white',
                              high = '#b2182b', midpoint = 50) +
        scale_linetype_manual(values = c('dotted', 'solid'), guide = FALSE) +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              legend.position = 'right',
              panel.grid = element_blank())

    p.learning <-
        ggplot(pf, aes(x = .data$x, y = .data$y, color = .data$Trial,
                       linetype = .data$Valid, group = .data$Trial)) +
        facet_grid(. ~ Block) + theme_classic() + geom_path() +
        scale_color_gradient2(low = '#c51b7d', mid = 'white',
                              high = '#4d9221', midpoint = max(df$Trial)/2) +
        scale_linetype_manual(values = c('dotted', 'solid'), guide = FALSE) +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              legend.position = 'right',
              panel.grid = element_blank())

    g.trajectories <- ggplotGrob(p.trajectories)
    g.learning     <- ggplotGrob(p.learning)
    g.scores       <- ggplotGrob(p.scores)
    g.figure <- rbind(g.scores, g.trajectories, g.learning, size = "first")
    g.figure$widths <- unit.pmax(g.trajectories$widths,
                                 g.scores$widths,
                                 g.learning$widths)

    if (plot) {
        grid.newpage()
        grid.draw(g.figure)
    } else {
        return(g.figure)
    }

}
