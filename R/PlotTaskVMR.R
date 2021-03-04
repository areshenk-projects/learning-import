#' Plot subject VMR data
#'
#' Function accepts a a dataframe output by OrganizeDataVMR and plots error
#' and reaction time.
#'
#' @param data A dataframe returned by OrganizeTaskVMR
#' @param plot Logical. If true, return a ggplot object. If false, returns a dataframe.
#' @return Either a ggplot object or a dataframe
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes facet_grid theme_bw geom_hline ylab geom_line theme
#' @export

PlotTaskVMR <- function(data, plot = F) {

    pf <- data[,c('Block', 'Trial', 'Error', 'ReactionTime', 'MovementTime')]
    pf <- gather(pf, key = 'Variable', value = 'Value', Error:MovementTime)
    pf <- subset(pf, Block != 'Practice')

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
