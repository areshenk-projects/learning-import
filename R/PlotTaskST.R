#' Plot subject ST data
#'
#' Function accepts a a dataframe output by ImportTaskInST and plots choice
#' and reaction time.
#'
#' @param data A dataframe returned by ImportTaskInST
#' @param plot Logical. If true, return a ggplot object. If false, returns a dataframe.
#' @return Either a ggplot object or a dataframe
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes facet_grid theme_classic geom_jitter xlab stat_summary
#' @export

PlotTaskST <- function(data, plot = F) {

    pf <- data

    p <- ggplot(subset(pf, Accuracy == 'Correct'),
           aes(x = .data$Congruence, y = .data$RT)) +
        facet_grid(. ~ Response) + xlab('') +
        theme_classic(base_size = 12) +
        geom_jitter(color = grey(.75), width = .02) +
        stat_summary(fun = mean, group = -1,geom = 'line') +
        stat_summary(fun.data = mean_se) +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              panel.grid = element_blank())

    if (plot) {
        return(p)
    } else {
        return(pf)
    }

}
