#' logistic_curve.R
#'
#' Generate the logistic curve for the website using the predicted percentiles
#' from may_forecast.R

library(ggplot2)

predictions <- read.csv("may_forecast/predictions.csv", stringsAsFactors = FALSE)

# Define the logistic function and its RSS
mu_i <- 20
s_i <- 5

logi_fun <- function(x, mu, s) { 1 / (1 + exp(-((x - mu)/s))) }
logi_rss <- function(pars) {
  # Recalculate
  dat <- data.frame(day = predictions$prediction,
                    cumulative_cpue = logi_fun(predictions$prediction, pars[1], pars[2]))

  # Calculate and return RSS
  sum(c((dat[dat$day == predictions[predictions$percentile == "fifdj","prediction"], "cumulative_cpue"] - 0.15)^2,
        (dat[dat$day == predictions[predictions$percentile == "qdj","prediction"], "cumulative_cpue"] - 0.25)^2,
        (dat[dat$day == predictions[predictions$percentile == "mdj","prediction"], "cumulative_cpue"] - 0.50)^2))
}

optim_result <- optim(par = c(mu_i, s_i), fn = logi_rss)
optim_result
save("optim_result", file = "may_forecast/optim_result.RData")

xrange <- -10:50
cpue <- data.frame(day = xrange,
                   date = as.Date(xrange, format = "%j", origin = as.Date("2016-05-31")),
                   pccpue = 100 * logi_fun(xrange, optim_result$par[1], optim_result$par[2]))

# Write out
write.csv(cpue, file = "may_forecast/logistic_curve.csv", row.names = FALSE)

predictions$percent <- c(15, 25, 50)
predictions$label <- paste0(c(15, 25, 50), "%")
predictions$date <- as.Date(predictions$prediction, format = "%j", origin = as.Date("2016-05-31"))

ggplot() +
  geom_bar(data = predictions, aes(date, percent), stat = "identity", fill = NA, color = "black") +
  geom_text(data = predictions, aes(date, percent, label = label), vjust = -1, size = 3) +
  geom_line(data = cpue, aes(date, pccpue)) +
  labs(x = "Date", y = "Cumulative % CPUE") +
  theme_bw()

ggsave("may_forecast/logisitc_curve.png", width = 6, height = 3)
