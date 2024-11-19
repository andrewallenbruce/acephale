# creating some data


set.seed(1)

library(data.table)

dtData = data.table(
  DateCol = seq(
    as.Date("1/01/2014", "%d/%m/%Y"),
    as.Date("31/12/2015", "%d/%m/%Y"),
    "days"
  ),
  ValueCol = runif(730)
)
dtData[, ValueCol := ValueCol + (strftime(DateCol,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(DateCol,"%m")) - 6.5)) * runif(1) * 0.75, .I]

dtData

ggplot_horizon(dtData, 'DateCol', 'ValueCol') +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'green', high = 'red') +
  coord_fixed(0.5 * diff(range(dtData$DateCol)) / diff(range(dtData$ValueCol))) +
  facet_wrap(~DateCol, nrow = 2)



# base plot
ggplot_calendar_heatmap(dtData, 'DateCol', 'ValueCol',
                        monthBorderLineEnd = "square",
                        dayBorderColour = "grey80",
                        monthBorderSize = 3,
                        monthBorderColour = "grey80"
                        ) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'green', high = 'red') +
  facet_wrap(~Year, ncol = 1) +
  theme(
    axis.text = element_text(size = 12, face = "bold"),
    axis.ticks = element_blank(),
    legend.position = 'none',
    strip.background = element_rect(fill = "grey80"),
    strip.text = element_text(size = 15, face = "bold"),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "grey80"),
    panel.grid = element_blank())
