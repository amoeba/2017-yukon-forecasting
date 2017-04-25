#' april_forecast.R
#'
#' 15/25/50 %tiles of timing using just 2016 AMATC

library(dplyr)
library(ggplot2)

# Data loading/cleanup
######################

yuk <- read.csv("data/yukon.csv",
                stringsAsFactors = FALSE)


# Plotting
##########

ggplot(yuk, aes(amatc, mdj)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = yuk[which(yuk$year == 2017),"amatc"]) +
  labs(x = expression("AMATC,"*~degree*"C"), y = "Median Run Timing (June)")

ggsave("april_forecast/mdj_against_amatc.png", width = 7, height = 6)

# Modeling
##########

# Fit -> 2015
model <- lm(mdj ~ amatc, data = subset(yuk, year < 2017))
summary(model)

predict(model, newdata =  yuk[yuk$year == 2017,])