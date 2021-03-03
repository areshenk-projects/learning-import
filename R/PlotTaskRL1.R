#' Plot subject RL1 data
#'
#' Function accepts a a dataframe output by OrganizeDataRL1 and plots score
#' and reaction time.
#'
#' @param data A dataframe returned by OrganizeTaskRL1
#' @param plot Logical. If true, return a ggplot object. If false, returns a dataframe.
#' @return Either a ggplot object or a dataframe
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot facet_grid theme_bw geom_hline ylab geom_line theme
#' @export

PlotTaskRL1 <- function(data, plot = F) {

    pf <- data[,c('Block', 'Trial', 'Score', 'ReactionTime', 'MovementTime')]
    pf <- gather(pf, key = 'Variable', value = 'Value', Score:MovementTime)

    p <- ggplot(pf, aes(x = .data$Trial, y = .data$Value)) +
        facet_grid(Variable ~ Block, scales = 'free', space = 'free_x') +
        theme_bw(base_size = 12) + ylab('') +
        geom_hline(yintercept = 0, linetype = 'longdash', color = grey(.5)) +
        geom_line() +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              panel.grid = element_blank())

    if (plot) {
        return(p)
    } else {
        return(pf)
    }

}
