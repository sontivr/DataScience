library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)


#' Create some data to play with. Two time series with the same timestamp.
# df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2), series1 = rnorm(8761), series2 = rnorm(8761, 100))

#' Create the two plots.
plot1 <- 
  ggplot(stormdata.top6.health.melt) +
  geom_bar(aes(x = EVTYPE , y = DAMAGEVAL, fill = DAMAGE), stat="identity", position="dodge") +
  labs(title = "Damage caused by top 6 events") + 
  ylab("Number of Fatalities/Injured") +
  # theme_minimal() +
  theme(panel.margin.y = unit(10, "lines")) +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
  theme(axis.text.x=element_text(size=6, color="red")) +
  theme(axis.title.x = element_blank())

plot2 <- 
  ggplot(stormdata.top6.econ.melt) +
  geom_bar(aes(x = EVTYPE, y = DAMAGEVAL, fill = DAMAGE), stat="identity") +
  ylab("Damage to Economy \n (in billions of dollors)") +
  # theme_minimal() +
  theme(panel.margin.y = unit(10, "lines")) +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

