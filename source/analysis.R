raw_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(tidyr)
library("tidyverse")
raw_data <- raw_data %>% drop_na()


selected_variables <- raw_data %>% group_by(year, state) %>%
  mutate(aapi_jail_percent = sum(aapi_jail_pop) / sum(aapi_pop_15to64),
  black_jail_percent = sum(black_jail_pop) / sum(black_pop_15to64),
  latinx_jail_percent = sum(latinx_jail_pop) / sum(latinx_pop_15to64),
  native_jail_percent = sum(native_jail_pop) / sum(native_pop_15to64),
  white_jail_percent = sum(white_jail_pop) / sum(white_pop_15to64)) %>% 
  select(year, state, aapi_jail_percent, black_jail_percent, 
         latinx_jail_percent, native_jail_percent, white_jail_percent) %>%
  unique()

summary(selected_variables)

attach(selected_variables)
boxplot <- boxplot(aapi_jail_percent, black_jail_percent, latinx_jail_percent, 
        native_jail_percent, white_jail_percent, xlab = c("aapi  black  latinx  native  white", "race"),
        ylab = "jail percentage", main = "boxplot of different race in jail percentage")


in_2013 <- selected_variables %>% filter(year== max(year))
summary(in_2013)

in_WA <- selected_variables %>% filter(state=='WA')
detach(selected_variables)
attach(in_WA)
plot(year, aapi_jail_percent,ylim=c(0, 0.016), col =1, ylab="jail percent", main = "time trend of different race in jail percentage in WA")
points(year, black_jail_percent, col = 2)
points(year, latinx_jail_percent, col=3)
points(year, native_jail_percent,col=4)
points(year, white_jail_percent, col=5)

legend(x = 'topright', legend = c("aapi", "black", "latinx", "native", "white"),
       col = 1:5, lwd = 5)

detach(in_WA)
attach(selected_variables)
plot(black_jail_percent, white_jail_percent,xlim=c(0,0.04), ylim=c(0,0.04), main = "black in jail percentage versus white qq-plot")
abline(0,1)

library("ggplot2")
theme_set(theme_bw())
library("usmap")

plot_usmap(regions = "states", data = in_2013, values="black_jail_percent") + 
  labs(title = "black jail percentage in US States in 2013")+ scale_fill_continuous(low = "white", high = "red") + 
  theme(legend.position = "right")

