#' daily_forecast.R
#'
#' Code for generating the daily forecast.

library(ggplot2)

# Load data
inseason <- read.csv("data/inseason.csv", stringsAsFactors = FALSE)
inseason$ccpue <- cumsum(inseason$cpue)
logistic_curve <- read.csv("may_forecast/logistic_curve.csv",
                           stringsAsFactors = FALSE)
predictions <- read.csv("may_forecast/predictions.csv",
                        stringsAsFactors = FALSE)

# Calculate estimated pccpue
today <- tail(inseason, n = 1)$day
ccpue <- tail(inseason, n = 1)$ccpue
final_ccpue <- ccpue / (logistic_curve[logistic_curve$day == today,"pccpue"] / 100)

estimated <- inseason
estimated$pccpue <- estimated$ccpue / final_ccpue

# Export estimated pccpues for Chart1
write.table(subset(estimated, day >= 1)$pccpue * 100, row.names = FALSE, col.names = FALSE, file = "daily_forecast/estimated.csv")

# Write cumulative CPUEs for Chart 2
write.table(inseason[inseason$day >= 1,"ccpue"], row.names = FALSE, col.names = FALSE, file = "daily_forecast/cumulative_cpue.csv")

# Subset the columns for later use
estimated <- estimated[,c("day", "pccpue")]

# Plot
logistic_curve$date <- as.Date(logistic_curve$date)

predictions$percent <- c(15, 25, 50)
predictions$label <- paste0(c(15, 25, 50), "%")
predictions$date <- as.Date(predictions$prediction, format = "%j", origin = as.Date("2016-05-31"))

estimated$date <- as.Date(estimated$day, format = "%j", origin = as.Date("2016-05-31"))
estimated$pccpue <- estimated$pccpue * 100

logistic_curve$curve <- "Modeled"
estimated$curve <- "Estimated"
combined <- rbind(logistic_curve,
                  estimated)
combined$curve <- ordered(combined$curve, level = c("Modeled", "Estimated"))
#
# date_annotation <- tail(estimated, 1)
# date_annotation$label <- format(date_annotation[1,"date"], "%b %d")

ggplot() +
  geom_bar(data = predictions, aes(date, percent), stat = "identity", fill = NA, color = "black") +
  geom_text(data = predictions, aes(date, percent, label = label), vjust = -1, size = 3) +
  geom_line(data = combined, aes(date, pccpue, color = curve)) +
  # geom_text(data = date_annotation, aes(date, pccpue + 1, label = label), angle = 90, hjust = 0, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Date", y = "Cumulative % CPUE") +
  theme_bw() +
  theme(legend.position = c(.15,.85),
        legend.title = element_blank())

ggsave("daily_forecast/daily_forecast.png", width = 6, height = 3)

# Chart 2: Finaly CPUE timer series
final_cpue <- data.frame(day = inseason$day,
                         estimate = inseason$ccpue / (logistic_curve[logistic_curve$day %in% inseason$day,"pccpue"] / 100))
final_cpue$date <- as.Date(final_cpue$day, format = "%j", origin = as.Date("2016-05-31"))

ggplot(final_cpue, aes(date, estimate)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Final CPUE Estimate") +
  theme_bw()

ggsave("daily_forecast/final_cpue.png", width = 6, height = 3)

