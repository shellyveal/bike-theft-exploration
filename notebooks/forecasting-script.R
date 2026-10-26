rm(list=ls())
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)}

lapply(c("tidyverse", "ggplot2", "knitr",
         "patchwork", "showtext", "ggtext",
         "purrr", "systemfonts", "xtable",
         "ggfortify", "forecast"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
base_path <- dirname(rstudioapi::getActiveDocumentContext()$path)

all_fonts <- system_fonts()
font_table <- all_fonts %>%
  dplyr::group_by(family) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()
purrr::walk2(font_table$family, font_table$path, function(fam, path) {
  try(font_add(fam, regular = path), silent = TRUE)})
showtext_auto()

SVU <- theme_minimal(base_family = "Tahoma", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(margin = margin(t = 10, b = 10), 
                                  face = "bold", size = rel(1.7)),
        plot.subtitle = element_text(face = "plain", size = rel(1.3),
                                     color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", 
                                  size = rel(1.1), hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, 
                                                    b = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10,
                                                    l = 8), hjust = 0.5),
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA),
        palette.colour.discrete = function(n)
          scales::pal_viridis(option = "D")(n),
        palette.fill.discrete = function(n) 
          scales::pal_viridis(option = "D")(n),
        palette.colour.continuous = scales::pal_viridis(option = "D"),
        palette.fill.continuous = scales::pal_viridis(option = "D"))

options(scipen = 999)
set.seed(123)
theme_set(SVU)

# Reading in / Preprocessing the Data
birmingham_data <- read_csv("birmingham_2010_2026.csv")

bdf <- birmingham_data %>% 
  select(c(`Crime type`, "Month")) %>%
  filter(`Crime type` == "Bicycle theft") %>%
  mutate(Month = ymd(paste0(Month, "-01"))) %>%
  group_by(Month) %>%
  summarize(observation_count = n())
# Creating a Time Series Object
bts <- ts(bdf$observation_count, start = c(2013, 5), 
          end = c(2026, 1), 
          frequency = 12)
# Splitting into Train and Test Sets
bts_train <- bts %>% window(start = c(2013, 5), 
                            end = c(2023, 6), 
                            frequency = 12)
bts_test <- bts %>% window(start = c(2023, 7), 
                           end = c(2026, 1), 
                           frequency = 12)
#############
# Analysis
#############

# Initial look at the data:
autoplot(bts_train) + ylim(0, 450) + 
  autolayer(bts_test, color = "#FF008A", alpha = 0.5)

# Once Differentiated:
once_diff <- bts_train %>% diff(lag = 12)
autoplot(once_diff) + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.5)

# Twice Differentiated:
twice_diff <- bts_train %>% diff(lag = 12) %>% diff(lag = 1)
autoplot(twice_diff) + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.5)

# Decomposing the data to have a look at components:
stl_b <- stl(bts_train, s.window = 12)
stl_once_diff <- stl(once_diff, s.window = 12)
stl_twice_diff <- stl(twice_diff, s.window = 12)

a <- autoplot(stl_b) + 
  labs(title = "Basic Data") + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.5)
b <- autoplot(stl_once_diff) + 
  labs(title = "Once Differentiated Data") + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.5)
c <- autoplot(stl_twice_diff) + 
  labs(title = "Twice Differentiated Data") + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.5)

# Plain data vs Once-Differentiated
a+b

# Once-Differentiated vs Twice-Differentiated
b+c

# Fitting an SES model
b_ses <- ses(bts_train, alpha = 0.5, h = 31) 

# Visualizing
fit_ses <- autoplot(bts_train) +
  autolayer(fitted(b_ses), color = "blue", alpha = 0.5) + 
  theme(legend.position = "none")

predict_ses <- autoplot(bts) +
  labs(title = "Bicycle Theft in the West Midlands (SES Model)",
       subtitle = "MAE: 45.2, MAPE: 22.6",
       x = "",
       y = "Bike Thefts (per month)") +
  coord_cartesian(ylim = c(0, 500)) +
  autolayer(fitted(b_ses), color = "blue", alpha = 0.5) + 
  autolayer(b_ses, color = "blue", alpha = 0.5) +
  theme(legend.position = "none")

train_MAE_SES <- 45.1706
train_MAPE_SES <- 22.56231
test_MAE_SES <- mean(abs(bts_test - tail(b_ses$mean, n = 31)))
test_MAPE_SES <- mean(abs((bts_test - tail(b_ses$mean, n = 31))/bts_test)) *100

# Checking specs, visualizing error, autocorrelation:
summary(b_ses)
checkresiduals(b_ses)

# Trying Arima with lags 1 and 2
lag_1 <- arima(bts_train, order = c(1, 0, 0))
lag_2 <- arima(bts_train, order = c(2, 0, 0))

forecast(lag_1, h = 31)

plot_lag_1 <- autoplot(bts) +
  autolayer(fitted(lag_1), color = "seagreen", alpha = 0.5) + 
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 500)) +
  autolayer(forecast(lag_1, h = 31), color = "darkred", alpha = 0.5)
plot_lag_2 <- autoplot(bts) +
  autolayer(fitted(lag_2), color = "seagreen", alpha = 0.5) + 
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 500)) +
  autolayer(forecast(lag_2, h = 31), color = "darkred", alpha = 0.5)

plot_lag_1 + predict_ses
plot_lag_2 + predict_ses

# Checking the Residuals of ARIMA(1,0,0)
checkresiduals(lag_1)
# Checking the Resiudals of ARIMA(2,0,0)
checkresiduals(lag_2)

# lag 2 better, there's definitely a seasonal issue.

# Fitting and evaluating ARIMA
manual_plain <- arima(bts_train, order = c(2, 0, 1),
                      include.mean = FALSE)
summary(manual_plain)
ACF_manual <- ggAcf(manual_plain$residuals)
PACF_manual <- ggPacf(manual_plain$residuals)
ACF_manual + PACF_manual

# Extracting ARIMA MAE for train and test data
train_MAE_plain <- 37.15893
train_MAPE_plain <- 18.14114
man_plain_fc <- forecast(manual_plain, h = 31)
test_MAE_plain <- mean(abs(bts_test - man_plain_fc$mean))
test_MAPE_plain <- mean(abs((bts_test - man_plain_fc$mean)/bts_test))*100

# Fitting and evaluating SARIMA
manual_seasonal <- arima(bts_train, order = c(1, 0, 1),
                         seasonal = list(order = c(1, 1, 0), period = 12),
                         include.mean = FALSE)
summary(manual_seasonal)
ACF_manual_s <- ggAcf(manual_seasonal$residuals)
PACF_manual_s <- ggPacf(manual_seasonal$residuals)
ACF_manual_s + PACF_manual_s

# Extracting SARIMA MAE for train and test data
train_MAE_seasonal <- 30.71295
train_MAPE_seasonal <- 14.41497
man_seas_fc <- forecast(manual_seasonal, h = 31)
test_MAE_seasonal <- mean(abs(bts_test - man_seas_fc$mean))
test_MAPE_seasonal <- mean(abs((bts_test - man_seas_fc$mean)/bts_test))*100

# Final Visualizations:
MANUAL_PLAIN_PLOT <- autoplot(bts) +
  autolayer(fitted(manual_plain), 
            color = "#1134A6", alpha = 0.5) + 
  autolayer(forecast(manual_plain, h = 31), 
            color = "#0080FE", alpha = 0.5) +
  labs(title = "Bicycle Theft in the West Midlands (Non-seasonal Model)",
       subtitle = "MAE: 37.2, MAPE: 18.1",
       x = "",
       y = "Bike Thefts (per month)") +
  coord_cartesian(ylim = c(0, 500)) 
  
MANUAL_PLOT_SEASONAL <- autoplot(bts) +
  autolayer(fitted(manual_seasonal), 
            color = "#FF008A", alpha = 0.5) + 
  autolayer(forecast(manual_seasonal, h = 31), 
            color = "#FF408A", alpha = 0.5) +
  labs(title = "Bicycle Theft in the West Midlands (Seasonal Model)",
       subtitle = "MAE: 30.7, MAPE: 14.4",
       x = "",
       y = "Bike Thefts (per month)") +
  coord_cartesian(ylim = c(0, 500)) 
  
figure <- predict_ses /MANUAL_PLAIN_PLOT / MANUAL_PLOT_SEASONAL

# ggsave("figure.pdf", figure, width = 11,
#       height = 8,
#       units = "in",
#       dpi = 300)

# Comparing results with auto.arima
auto <- auto.arima(bts_train)

ACF_auto <- ggAcf(auto$residuals)
PACF_auto <- ggPacf(auto$residuals)

ACF_auto + PACF_auto
accuracy(auto)

AUTO_PLOT <- autoplot(bts) +
  autolayer(fitted(auto), color = "#1134A6", alpha = 0.5) + 
  autolayer(forecast(auto, h = 31), color = "#0080FE", alpha = 0.5) +
  labs(title = "Bicycle Theft in the West Midlands",
       y = "Bike Theft (per month)")

# Compiling MAE, MAPE scores into table for LateX
results <- data.frame(
  Model = c("Plain", "Seasonal", "SES"),
  Train_MAE  = c(train_MAE_plain, train_MAE_seasonal, train_MAE_SES),
  Train_MAPE = c(train_MAPE_plain, train_MAPE_seasonal, train_MAPE_SES),
  Test_MAE   = c(test_MAE_plain, test_MAE_seasonal, test_MAE_SES),
  Test_MAPE  = c(test_MAPE_plain, test_MAPE_seasonal, test_MAPE_SES))
xt <- xtable(results,
             caption = "Forecast Accuracy Comparison (Train vs Test)",
             label = "tab:forecast_comparison",
             digits = c(0, 0, 2, 2, 2, 2))
print(xt,
      include.rownames = FALSE,
      add.to.row = list(
        pos = list(-1),
        command = paste(
          "\\hline\n",
          "& \\multicolumn{2}{c}{Training} & \\multicolumn{2}{c}{Test} \\\\\n",
          "\\cline{2-3} \\cline{4-5}\n")))
