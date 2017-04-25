#' may_forecast.R
#'
#' 15/25/50 %tiles of timing using

library(dplyr)
library(ggplot2)

# Data loading/cleanup
######################

yuk <- read.csv("data/yukon.csv",
                stringsAsFactors = FALSE)


# Plots
ggplot(yuk, aes(msstc, mdj)) +
  geom_point() +
  geom_vline(xintercept = yuk[which(yuk$year == 2016),"msstc"]) +
  labs(x = expression("MSSTC,"*~degree*"C"), y = "Median Run Timing (June)")

ggsave("may_forecast/mdj_against_msstc.png", width = 7, height = 6)

ggplot(yuk, aes(pice, mdj)) +
  geom_point() +
  geom_vline(xintercept = yuk[which(yuk$year == 2016),"pice"]) +
  labs(x = "Prop. Ice Cover", y = "Median Run Timing (June)")

ggsave("may_forecast/mdj_against_pice.png", width = 7, height = 6)

# Models

# 15%
model_fifdj <- lm(fifdj ~ amatc + msstc + pice, data = subset(yuk, year < 2016))
summary(model_fifdj)
prediction_fifdj <- floor(predict(model_fifdj, newdata = yuk[yuk$year == 2016,]))
prediction_fifdj

# 25%
model_qdj <- lm(qdj ~ amatc + msstc + pice, data = subset(yuk, year < 2016))
summary(model_qdj)
prediction_qdj <- floor(predict(model_qdj, newdata = yuk[yuk$year == 2016,]))
prediction_qdj

# 50%
model_mdj <- lm(mdj ~ amatc + msstc + pice, data = subset(yuk, year < 2016))
summary(model_mdj)
prediction_mdj <- floor(predict(model_mdj, newdata = yuk[yuk$year == 2016,]))
prediction_mdj

predictions <- data.frame(percentile = c("fifdj", "qdj", "mdj"),
                          prediction = as.integer(c(prediction_fifdj,
                                         prediction_qdj,
                                         prediction_mdj)))
write.csv(predictions, file = "may_forecast/predictions.csv")
