Sys.setlocale("LC_CTYPE", "Lithuanian")
library(data.table)
library(lubridate)
library(ggplot2)
library(zoo)
library(forecast)
library(xgboost)
library(Metrics)
library(scales)
library(patchwork)
library(gridExtra)
library(caret)

raw_df <- fread("C:\\Users\\jpjus\\Desktop\\Bakalauras\\DK_bakalauras.txt", sep = "|", header = TRUE)
setnames(raw_df, c("Account", "Currency", "Date", "Amount"))
raw_df[, Date := as.Date(Date)]


raw_df[, Amount := Amount / 20]

#Revenue saskaitos
revenue_df <- raw_df[grepl("^R", Account)]
revenue_df[, Week := floor_date(Date, unit = "week", week_start = 1)]


# Pajamos per savaite
total_weekly_revenue <- revenue_df[, .(Total_Weekly_Amount = sum(Amount)), by = Week]

ggplot(total_weekly_revenue, aes(x = Week, y = Total_Weekly_Amount)) +
  geom_line(size = 1) +
  labs(title = "Pajamu grafikas",
       x = "Laikotarpis", y = "Pajamu suma (EUR)") +
  geom_line(color = "darkgreen", linewidth = 1) +
  scale_y_continuous(labels = label_number(big.mark = " ", scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))




head(total_weekly_revenue[order(Total_Weekly_Amount)], 5)


revenue_df[Week == as.Date("2019-04-29"), .(Suma = sum(Amount)), by = Account][order(Suma)]

top_accs <- revenue_df[Week == as.Date("2019-04-29"), 
                       .(Suma = sum(Amount)), by = Account][order(Suma)][1:5]$Account

ggplot(revenue_df[Account %in% top_accs & Week == as.Date("2019-04-29")],
       aes(x = Account, y = Amount)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "5 saskaitos labiausiai prisidejusios prie kritimo 2019-04-29",
    x = "Saskaita",
    y = "Suma (EUR)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))



r130_df <- revenue_df[Account == "R130"]

#savaites suma
r130_weekly <- r130_df[, .(Amount = sum(Amount)), by = Week]

#savaitines laiko seka
all_weeks <- data.table(Week = seq(min(revenue_df$Week), max(revenue_df$Week), by = "week"))

r130_full_ts <- merge(all_weeks, r130_weekly, by = "Week", all.x = TRUE)
r130_full_ts[is.na(Amount), Amount := 0]


ggplot(r130_full_ts, aes(x = Week, y = Amount)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(
    title = "R130 sumos",
    x = "Laikotarpis",
    y = "Suma (EUR)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))





revenue_no_r130 <- revenue_df[Account != "R130"]
total_weekly_no_r130 <- revenue_no_r130[, .(Total_Weekly_Amount = sum(Amount)), by = Week]

#savaitines pajamos be R130
ggplot(total_weekly_no_r130, aes(x = Week, y = Total_Weekly_Amount)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Pajamu grafikas be R130 saskaitos",
    x = "Laikotarpis",
    y = "Pajamu suma (EUR)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))




#____________________ACF ir PACF grafikai____________________________________________
p1 <- ggAcf(ts_diff_seasonal) +
  labs(title = "Autokoreliacijos grafikas (ACF)",
       x = "Atsilikimas", y = "Autokoreliacija") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

p2 <- ggPacf(ts_diff_seasonal) +
  labs(title = "Daliniu autokoreliaciju grafikas (PACF)",
       x = "Atsilikimas", y = "Dal. autokoreliacija") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))


p1+p2


#_______________________________AUTO.ARIMA__________________________________
ts_df <- total_weekly_no_r130[order(Week)]
ts_obj <- ts(ts_df$Total_Weekly_Amount, start = c(2018, 1), frequency = 52)
plot(ts_obj)
#___diferencijavimas
ts_diff <- diff(ts_obj)
ts_diff_seasonal <- diff(ts_diff, lag = 52)
plot(ts_diff_seasonal)
tseries::adf.test(ts_diff_seasonal)


#_______________________________SARIMA_______________________________________



model_better <- auto.arima(ts_obj, stepwise = FALSE, approximation = FALSE)
summary(model_better)
#____________prognoziu grafikas____________________

forecast_better <- forecast(model_better, h = 12)

autoplot(forecast_better) +
  labs(
    title = "SARIMA modelio 3 menesiu prognoze",
    x = "Laikotarpis",
    y = "Pajamu suma"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

forecast_better$mean

#________________________________autoarima residuals__________________________


# Gauti likucius
resid <- residuals(model_better)

# Laiko grafikas
p1 <- autoplot(resid) +
  labs(title = "SARIMA modelio liekanos", y = "Liekanos", x = "Laikotarpis") +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))


# ACF grafikas
p2 <- ggAcf(resid) +
  labs(title = "Autokoreliacijos funkcija (ACF)", y = "Autokoreliacija", x = "Atsilikimas") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

# Histograma su tankio kreive
p3 <- ggplot(data.frame(resid), aes(x = resid)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Liekanu pasiskirstymas", x = "Liekanos", y = "Tankis") +
  scale_x_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

grid.arrange(p1, p2, p3, nrow = 2)


checkresiduals(model_better)

#________________________________ARIMA200________________________________________
ts_diff_seasonal <- ts(diff(ts_diff, lag = 52), start = c(2019, 2), frequency = 52)

model_arima200 <- Arima(ts_diff_seasonal, order = c(2, 0, 0))
summary(model_arima200)
checkresiduals(model_arima200)

forecast_arima <- forecast(model_arima200, h = 12)

autoplot(forecast_arima) +
  labs(
    title = "ARIMA modelio 3 menesiu prognoze",
    x = "Laikotarpis",
    y = "Pajamu suma"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 8),
    labels = function(x) format(as.Date(as.yearmon(x)), "%Y-%m")
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))





resid <- residuals(model_arima200)

dates <- tail(ts_df$Week, length(resid))

df <- data.frame(
  Time = dates,
  Residuals = as.numeric(resid)
)

p1 <- ggplot(df, aes(x = Time, y = Residuals)) +
  geom_line() +
  labs(title = "ARIMA modelio liekanos", y = "Liekanos", x = "Laikotarpis") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

p2 <- ggAcf(resid) +
  labs(title = "Autokoreliacijos funkcija (ACF)", y = "Autokoreliacija", x = "Atsilikimas") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

p3 <- ggplot(data.frame(resid), aes(x = resid)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Liekanu pasiskirstymas", x = "Liekanos", y = "Tankis") +
  scale_x_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

grid.arrange(p1, p2, p3, nrow = 2)


checkresiduals(model_arima200)


#_______________________XGBoost_________________________

create_selected_lags <- function(series, selected_lags) {
  n <- length(series)
  max_lag <- max(selected_lags)
  X <- sapply(selected_lags, function(lag) series[(max_lag - lag + 1):(n - lag)])
  y <- series[(max_lag + 1):n]
  list(X = X, y = y)
}

#Pradiniai duomenys
ts_data <- as.numeric(ts_obj)
selected_lags <- c(1, 2, 3, 4, 6, 12, 13, 26, 52)
lagged <- create_selected_lags(ts_data, selected_lags)

n_total <- nrow(lagged$X)
n_test <- 12
n_train <- n_total - n_test

X_train <- lagged$X[1:n_train, ]
y_train <- lagged$y[1:n_train]
X_test  <- lagged$X[(n_train + 1):n_total, ]
y_test  <- lagged$y[(n_train + 1):n_total]

train_df <- data.frame(y = y_train, X_train)
colnames(train_df) <- make.names(colnames(train_df))

#Caret
control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)

tune_grid <- expand.grid(
  nrounds = c(50, 100),
  eta = c(0.05, 0.1, 0.3),
  max_depth = c(3, 5, 7),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(123)
xgb_caret_model <- train(
  y ~ .,
  data = train_df,
  method = "xgbTree",
  trControl = control,
  tuneGrid = tune_grid,
  metric = "RMSE"
)

best_model <- xgb_caret_model$finalModel

preds <- predict(xgb_caret_model, newdata = data.frame(X_test))

mae <- mae(y_test, preds)
rmse <- rmse(y_test, preds)
mape <- mape(y_test, preds) * 100

cat("MAE:", round(mae), "\n")
cat("RMSE:", round(rmse), "\n")
cat("MAPE:", round(mape, 2), "%\n")


xgb_caret_model$bestTune


start_date <- as.Date("2018-01-01")
week_dates <- start_date + 7 * ((length(ts_data) - length(y_test)):(length(ts_data) - 1))

df_plot <- data.frame(
  Data = week_dates,
  Tikros = y_test,
  Prognozuotos = preds
)

ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Tikros, color = "Tikros reiksmes"), linewidth = 1) +
  geom_line(aes(y = Prognozuotos, color = "Prognozes"), linewidth = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  scale_color_manual(values = c("Tikros reiksmes" = "black", "Prognozes" = "blue")) +
  labs(
    title = "Tikros ir prognozuotos reiksmes (XGBoost modelis)",
    x = "Laikotarpis",
    y = "Pajamu suma",
    color = ""
  ) +
  theme_minimal() +
  theme(
    
    text = element_text(family = "Times New Roman")
  )