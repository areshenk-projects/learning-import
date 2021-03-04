#' Plot subject IGT data
#'
#' Function accepts a a dataframe output by ImportTaskInIGT and plots choice
#' and reaction time.
#'
#' @param data A dataframe returned by ImportTaskInIGT
#' @param plot Logical. If true, return a ggplot object. If false, returns a dataframe.
#' @return Either a ggplot object or a dataframe
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes facet_grid theme_bw geom_hline ylab geom_step theme scale_color_manual
#' @export

PlotTaskIGT <- function(data, plot = F) {

    pf <- data.frame(Trial = data$Trial,
                     A = cumsum(data$Choice == 1),
                     B = cumsum(data$Choice == 2),
                     C = cumsum(data$Choice == 3),
                     D = cumsum(data$Choice == 4))

    pf <- gather(pf, key = 'Deck', value = 'Selections', A:D)

    p <- ggplot(pf, aes(x = .data$Trial, y = .data$Selections,
                        color = .data$Deck, group = .data$Deck)) +
        geom_step(size = 1.5) + theme_classic(base_size = 12) +
        scale_color_manual(values = c('#7b3294','#c2a5cf', '#a6dba0', '#008837')) +
        theme(strip.background = element_rect(color = 'white', fill = 'white'),
              panel.grid = element_blank())

    if (plot) {
        return(p)
    } else {
        return(pf)
    }

}
