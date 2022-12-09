#' Plot a weekly timetable (created by Peng Zhao)
#'
#' @param timetable a data frame or the path for a .csv file
#' @param caption title of the timetable
#'
#' @return a ggplot image
#' @export
#'
#' @examples
#' library(ggplot2)
#' dtf <- structure(list(weekday = c(2L, 2L),
#'                hour_from = 10:11,
#'                hour_to = c(10.25,12),
#'                event = c("Coffee break", "Staff Seminar"),
#'                location = c("ES316","ES354"),
#'                color = c("brown", "red")),
#'           row.names = 3:4,
#'           class = "data.frame")
#' plot_timetable(dtf)
#' ggsave('image/weekly.pdf', device = cairo_pdf, width = 297, height = 210, units = "mm")
#'
#' plot_timetable(timetable = 'overview/timetable.csv') +
#'   theme_void()
plot_timetable <- function(timetable, caption = "Peng Zhao's timetable drawn with the R langauge."){
  if (is.character(timetable)){
    dtf <- read.csv(timetable)
  } else {
    dtf <- timetable
  }
  if (is.character(dtf$weekday)){
    dtf0 <- dtf[nchar(dtf$weekday) == 1, ]
    dtf1 <- dtf[nchar(dtf$weekday) != 1, ]
    row_rep <- lapply(dtf1$weekday, function(x) unlist(strsplit(x, '[^0-7]')))
    for (i in 1:length(row_rep)){
      dtf2 <- dtf1[rep(i, length(row_rep[[i]])),]
      dtf2$weekday <- row_rep[[i]]
      dtf0 <- rbind(dtf0, dtf2)
    }
    dtf0$weekday <- as.integer(dtf0$weekday)
    dtf <- dtf0
  }

  dtf$color[dtf$color == ''] <- 'grey'
  dtf$time1 <- (dtf$hour_from + dtf$hour_to)/2
  dtf$time2 <- dtf$time1 + 0.3
  # windowsFonts(Times=windowsFont("TT Times New Roman"))
  ggplot() +
    geom_rect(aes(xmin = weekday - 0.5, xmax = weekday + 0.5,
                  ymin = hour_from, ymax = hour_to),
              fill = dtf$color,
              data = dtf, alpha = 0.1) +
    geom_text(aes(x = weekday, y = time1, label = event),
              data = dtf,
              # family = "Times",
              fontface = "bold") +
    geom_text(aes(x = weekday, y = time2, label = location),
              data = dtf,
              size = 2.5) +
    scale_x_continuous(
      breaks = 1:5,
      limits = c(0.5, 5.5),
      labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
      minor_breaks = 1:6 - 0.5) +
    labs(x = caption, y = '') +
    scale_y_reverse(
      breaks = 9:17,
      labels = paste0(9:17, ':00'),
      limits = c(17, 9)) +
    theme(panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(margin = margin(t = 30)),
          # axis.text.x = element_text(hjust = 0),
          # legend.position = 'none',
          plot.margin=unit(c(4,4,4,4), 'cm'))
}
