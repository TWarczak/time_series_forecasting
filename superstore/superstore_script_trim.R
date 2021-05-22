# - Libraries ---- 
library(tidyverse)
library(lubridate)
library(tidytext)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
library(ggthemes)
library(timetk)
library(xts)
library(DataExplorer)
library(skimr)
library(doFuture)
registerDoFuture()
n_cores <- parallel::detectCores()
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))
plan(sequential)


# - Import Data ----
df <- read_csv("superstore/data/train.csv")

# - Exploratory Data Analysis ----

# glimpse(df)
# tail(df, n = 20) # looks like `Order Date` format is dmy

# * Data Cleaning ----

# str(df)
# only orders in the United States, don't need individual order_ids or ship_mode

data_clean_tbl <- janitor::clean_names(df) %>% 
                  as_tibble() %>% 
                  mutate(order_date = dmy(order_date),
                         across(customer_id:product_name, 
                                .fns = as.factor)) %>% 
                  select(-c(ship_date, country, order_id, ship_mode))

# glimpse(data_clean_tbl)
# 
# # What period of time does this data cover?
# (t <- summarise_by_time(.data     = data_clean_tbl, 
#                         .date_var = order_date))
# (diff_t <- difftime(last(t$order_date), 
#                     first(t$order_date), 
#                     units = 'weeks'))            
# dweeks(208.1429)
# as.period(diff_t)
# So this data is from a timespan of 4 years.  2015-01-03 to 2018-12-30.  There are gaps.  


## * Orders/Sales by State/Region ----
# q <- data_clean_tbl %>% 
#      group_by(region,state) %>% 
#      summarise(orders = n(), sales = sum(sales)) %>% 
#      ungroup() %>% 
#      mutate(state = reorder_within(state, orders, region)) %>% 
#      arrange(desc(sales))
#    
# # **Plot: Orders by State/Region ----
# ggplot(q, aes(reorder_within(state, orders, region, sep = ""), orders, fill = region)) + 
#    geom_col(show.legend = FALSE, alpha       = 0.9) +
#    facet_grid(region ~., scales = 'free', space = 'free') +
#    coord_flip() +
#    scale_x_reordered() +  # gets rid of the ___region 
#    scale_y_continuous(limits = c(0,2000), expand = c(0,0)) +
#    theme_dark_grey() + 
#    scale_color_todd_dark_subtle() + 
#    scale_fill_todd_dark_subtle() +
#    theme(panel.grid.major.y = element_blank(),
#          panel.grid.major.x = element_blank()) +
#    geom_hline(yintercept = c(500,1000,1500), 
#               color      = "#FFCF6A", 
#               alpha      = 0.1, 
#               size       = 1) +
#    labs(x     = element_blank(), 
#         title = 'Orders by State/Region') +
#    geom_text(aes(label = orders), 
#              color    = "white", 
#              size     = 3.5, 
#              fontface = 'bold', 
#              hjust    = -0.2, 
#              vjust    = 0.4)
#    
# ggsave(here::here("superstore", "charts","orders_by_state_region.jpeg"),
#        width = 18.5, height = 12.5)

# **Plot: Sales by State/Region ----
# ggplot(q, aes(reorder_within(state, sales, region, sep = ""), sales, fill = region)) + 
#    geom_col(show.legend = FALSE, alpha = 0.9) +
#    facet_grid(region ~., scales = 'free', space = 'free') +
#    coord_flip() +
#    scale_x_reordered() +  # gets rid of the ___region 
#    scale_y_continuous(limits = c(0,500000), expand = c(0,0), labels=dollar_format(prefix="$")) +
#    theme_dark_grey() + 
#    scale_color_todd_dark_subtle() + 
#    scale_fill_todd_dark_subtle() +
#    theme(panel.grid.major.y = element_blank(),
#          panel.grid.major.x = element_blank()) +
#    geom_hline(yintercept = c(100000,200000,300000,400000), 
#               color      = "#FFCF6A", 
#               alpha      = 0.1, 
#               size       = 1) +
#    labs(x = element_blank(), title = 'Sales by State/Region') +
#    geom_text(aes(label = dollar(round((sales)))), 
#              color="white", size=3.5, fontface = 'bold', 
#              hjust = -0.1, vjust = 0.4)
# 
# ggsave(here::here("superstore","charts","sales_by_state_region.jpeg"),
#        width = 18.5, height = 12.5)
# 
# 
# str(data_clean_tbl)
# table(data_clean_tbl$category)
# table(data_clean_tbl$segment)
# glimpse(data_clean_tbl)
# 
# 
# ## * Grouping Orders & Total Sales  ----
# # Want to identify any outlier customers, and see if any category/segment of their business dominates
# (x <- data_clean_tbl %>% 
#       group_by(customer_name, category, segment) %>% 
#       summarise(sales  = sum(sales), 
#                 orders = n()) %>% 
#       mutate(log_sales = log(sales)))
# 
# (z <- data_clean_tbl %>% 
#       group_by(category, segment) %>% 
#       summarise(sales  = sum(sales), 
#                 orders = n()) %>% 
#       ungroup() %>% 
#       mutate(orders_pct      = round(prop.table(orders) * 100),
#              sales_pct       = round(prop.table(sales) * 100),
#              sales_per_order = round(sales/orders)) %>% 
#       pivot_longer(orders_pct:sales_pct, 
#                    names_to  = 'metric', 
#                    values_to = 'percent'))
# # Easy to see how orders and sales differ by category.
# 
# # **Plot: Order & Sale Percentage by Category/Segment ----
# ggplot(z, aes(category, percent, fill = segment)) +
#    geom_col(position = position_dodge(width = 0.8)) +
#    theme_dark_grey() +
#    scale_fill_manual(values = c("#fbb4ae", "#b3cde3", "#fed9a6")) +
#    facet_grid(~ metric, labeller = as_labeller(c(orders_pct = "% of Orders", 
#                                                  sales_pct  = "% of Sales"))) +
#    geom_text(aes(category, percent, label = paste0(percent, "%")), 
#              color    = "white", 
#              size     = 4, 
#              fontface = 'bold', 
#              vjust    = -0.5, 
#              position = position_dodge(width = 0.8)) +
#    labs(title = "Order & Sale Percentage by Category/Segment",
#         x     = element_blank()) 
# 
# ggsave(here::here("superstore","charts","orders&sale_pct_by_category_segment.jpeg"),
#        width = 14.5, height = 10.5)
# 
# 
# 
# (outlier_furniture_consumer <- x %>% filter(segment  == 'Consumer', 
#                                             category == "Furniture") %>% 
#                                      arrange(desc(orders)) %>% head()) 
# 
# (outlier_technology_corporate_homeoffice <- x %>% 
#                                            filter(segment  == 'Corporate' | segment == "Home Office",
#                                                   category == "Technology") %>% 
#                                            arrange(desc(log_sales)) %>% head())   
# # Make specific tbl for outlier Seth Vernon so label doesn't appear in other facets
# seth_vernon <- data.frame(category = factor("Furniture", levels = c("Furniture","Office Supplies","Technology")),
#                           segment  = factor("Consumer", levels = c("Consumer","Corporate","Home Office")))
# 
# # **Plot: Orders & Sales in log($) per Customer by Category/Segment ----
# ggplot(x, aes(log(sales), orders, color = category)) +
#    geom_jitter(aes(shape = segment), 
#                alpha       = 0.7, 
#                width       = 0, 
#                height      = 0.3, 
#                show.legend = FALSE, 
#                size        = 2) +
#    theme_dark_grey() +
#    scale_color_manual(values = c("#fbb4ae", "#b3cde3", "#fed9a6")) +   
#    facet_grid(segment~category, scales = 'free_y') +
#    labs(title = "Orders & Sales in log($) per Customer by Category/Segment", 
#         x     = 'log($)') +
#    geom_curve(aes(x = 7, xend = 8.9, y = 18, yend = 16),
#               data      = seth_vernon, 
#               curvature = -0.5, 
#               size      = 1, 
#               arrow     = arrow(length = unit(2, "mm")), 
#               color     = 'white', 
#               alpha     = 0.7) +
#    geom_text(aes(x = 6.5, y = 15), 
#              data        = seth_vernon ,
#              label       = "Seth Vernon\n15 Orders\nTotal = $8,332",
#              size        = 3.5, 
#              show.legend = F, 
#              color       = 'white', 
#              alpha       = 0.7)
# 
# ggsave(here::here("superstore","charts","orders&sales_per_customer_by_category_segment.jpeg"),
#        width = 16, height = 10)

# Outliers aren't that extreme.   


# -TIME-SERIES EXPLORATION ----

# Now lets look at the data as a time-series
# data_clean_tbl

sales_daily_tbl <- data_clean_tbl %>% 
                   summarise_by_time(.date_var = order_date, 
                                     .by       = 'day', 
                                     sales     = sum(sales))

# **Plot: DAILY TOTAL SALES ----
# sales_daily_tbl %>% 
#    plot_time_series(.date_var     = order_date, 
#                     .value        = sales, 
#                     .interactive  = FALSE,
#                     .line_color   = "#decbe4", 
#                     .smooth_color = "#ffffcc",
#                     .title        = "Daily Total Sales",
#                     .y_lab        = "$") +
#    theme_dark_grey() 
# 
# ggsave(here::here("superstore","charts","daily_total_sales.jpeg"),
#        width = 16, height = 6)
# 
# # -MISSING DATA ----
# 
# sales_daily_tbl %>% 
#    DataExplorer::plot_missing()

# There are no days with 0 orders/sales.  But....
# When there were no orders/sales for the day, they just didn't include that day in the data.  
# So we need to add a row for each missing day and pad with 0 sales and 0 orders.  

sales_daily_pad_tbl <- sales_daily_tbl %>% 
                       pad_by_time(order_date, 
                                   .by        = 'day', 
                                   .pad_value = 0) 
# 1230 rows to 1458 rows. Added 228 rows with 0 sales
# 1458/365 
# Now we have a full 4-year daily dataset with no missing dates.


# -TRANSFORMATIONS ----

# Apply log1p (because we have 0s) and stardarization transformations to both sales and orders.  
# Could do this at each recipe stage, but we can do it here once and be done with it.  

#   - First take the Log Plus 1: `log1p()`
#   - Then standardize to mean 0, std-dev 1: `standardize_vec()`

sales_daily_pad_trans_tbl <- sales_daily_pad_tbl %>%
                             mutate(sales = log_interval_vec(sales, 
                                                             limit_lower = 0, 
                                                             offset      = 1)) %>%
                             mutate(sales = standardize_vec(sales)) %>%
                             rename(sales_trans = sales)
# We need to keep track of the Standardization Parameters for when we un-transform the data later on.

# *STANDARDIZATION PARAMETERS ----
# Sales
limit_lower <-  0
limit_upper <-  30918.3876
offset <-  1

std_mean_sales <- -4.61074359571939
std_sd_sales <- 2.86059320652223

# **Plot: DAILY TOTAL SALES, PADDED & TRANSFORMED ----
# sales_daily_pad_trans_tbl %>% 
#    plot_time_series(order_date, 
#                     sales_trans, 
#                     .interactive  = FALSE,
#                     .line_color   = "#decbe4", 
#                     .smooth_color = "#ffffcc",
#                     .title        = "Daily Total Sales, Padded & Transformed",
#                     .y_lab        = "Sales") +
#    theme_dark_grey()  
# 
# ggsave(here::here("superstore","charts","daily_sales_pad_trans.jpeg"),
#        width = 16, height = 6)

# -FOURIER SERIES, LAGS, ROLLING LAGS ----

# **Plot: ACF/PACF DIAGNOSTICS ----
# sales_daily_pad_trans_tbl %>% 
#    plot_acf_diagnostics(.date_var = order_date, .value = sales_trans, .lags = 400, 
#                         .show_white_noise_bars = TRUE, .interactive = FALSE, .line_color = "#decbe4",
#                         .line_size = 0.5, .white_noise_line_color = "red", .point_color = "#fed9a6",
#                         .point_size = 0.8) +
#    theme_dark_grey() + 
#    geom_text(aes(label = label),
#              data  = lag_labels, 
#              x     = c(7, 14, 21.7, 29.5, 358, 7, 15, 22, 30),
#              y     = c(.53, .52, .5, .5, .38,.5, .35, .27, .25), 
#              size  = 4, 
#              color = "white", 
#              angle = 55)
# 
# lag_labels <- data.frame(name  = c("ACF","ACF","ACF","ACF","ACF","PACF","PACF","PACF","PACF"), 
#                          label = c('7','14','21','28','357','7','14','21','28'))
# 
# ggsave(here::here("superstore","charts","acf_pacf2.jpeg"),
#        width = 16, height = 10)

# Clearly there is a strong weekly correlation of sales vs sales 7d, 14d, 21d, ... later.   
# The Partial Auto Correlation Features (PACF) de-weights the correlation values depending on the values
# that come before it.  Our PACF shows lags 7, 14, 21, and 28 as the most important lags.  All other lags can
# be considered white noise or close to white noise.  There does seem to be an up-tick in correlation
# around lag 357, so we should definitely add yearly features.  
# We should include these lags and add Fourier Series for these cycles.  


# -FUTURE FORECAST & SPLITS  ----

# Forecast the next 3 months of sales, or 90 days.
forecast_3_month <- 84
lag_period <- 84 # 84 smallest lag period to get our forecast of 3 months into the future
rolling_periods <- c(30, 60, 90) # incorporates 1 week, 1 month, 2 month, and 3 month moving averages as features to catch the trend

# ** Full Tbl ----
full_data_prepared_tbl <- sales_daily_pad_trans_tbl %>% 
                          future_frame(.date_var   = order_date, 
                                       .length_out = forecast_3_month, 
                                       .bind_data  = TRUE) %>% 
                          tk_augment_fourier(.date_var = order_date,
                                             .periods  = c(7,14,21,28,357),
                                             .K        = 3) %>% 
                          tk_augment_lags(.value = sales_trans,
                                          .lags  = lag_period ) %>% 
                          tk_augment_slidify(.value   = sales_trans_lag84,
                                             .f       = ~ mean(.x, na.rm = TRUE),
                                             .period  = rolling_periods,
                                             .partial = TRUE,
                                             .align   = 'center') %>% 
                          rowid_to_column(var = 'rowid') %>% 
                          rename_with(.cols = contains('lag'), 
                                      .fn   = ~ str_c('lag_', .)) %>% 
                          rename_with(.cols = matches('(_sin)|(_cos)'), 
                                      .fn   = ~ str_c('fourier_', .))

# full_data_prepared_tbl %>% 
#    write_rds("superstore/data/full_data_prepared_tbl.rds")
# 
# full_data_prepared_tbl <- read_rds("superstore/data/full_data_prepared_tbl.rds")
# 
# glimpse(full_data_prepared_tbl)
# 
# full_data_prepared_tbl %>% 
#    pivot_longer(c(sales_trans, 
#                   lag_sales_trans_lag84,
#                   lag_sales_trans_lag84_roll_30,
#                   lag_sales_trans_lag84_roll_60, 
#                   lag_sales_trans_lag84_roll_90)) %>% 
#    plot_time_series(order_date, value, name, .smooth = FALSE)

# Full dataset should have NAs for the future forecast of sales_trans, orders_trans, and the lags, rolling lags     
# skimr::skim(full_data_prepared_tbl)

# ** Forecast Tbl ----
forecast_tbl <- full_data_prepared_tbl %>% 
                filter(is.na(sales_trans))

# skimr::skim(forecast_tbl) # 84 rows, 37 columns.  84 rows with missing data.  Good to go.  

# forecast_tbl <- forecast_tbl %>% 
#                 mutate(across(.cols = lag_sales_trans_lag7:lag_sales_trans_lag21,
#                               .fns  = ~ ifelse(is.na(.x), lag_sales_trans_lag84_roll_14, .x)))

# ** Prepared Tbl ----

data_prepared_tbl <- full_data_prepared_tbl %>% 
                     filter(!is.na(sales_trans)) %>% 
                     drop_na()

# skim(data_prepared_tbl) # 1374 rows, 37 columns, no missing data.  Good to go.  
   

# * SPLITS - TRAINING/TESTING ----

# Lets split training/testing so we're testing on the last quarter. 3 months or 90 days. 
# 356/4
splits <- data_prepared_tbl %>% 
          time_series_split(date_var   = order_date,
                            assess     = 84,
                            cumulative = TRUE)
# splits %>% 
#    tk_time_series_cv_plan() %>% 
#    plot_time_series_cv_plan(.date_var    = order_date, 
#                             .value       = sales_trans, 
#                             .smooth      = FALSE,
#                             .interactive = FALSE,
#                             .title       = "Initial Cross Validation Plan",
#                             .y_lab       = "Sales") +
#    theme_dark_grey() + 
#    scale_color_todd_dark_subtle()
# 
# ggsave(here::here("superstore","charts","cv_splits.jpeg"),
#        width = 16, height = 10)


# -RECIPES ----

recipe_spec <- recipe(sales_trans ~ ., data = training(splits)) %>% 
               step_timeseries_signature(order_date) %>% 
               update_role(rowid, new_role = 'indicator') %>% 
               step_rm(matches("(.iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>% 
               step_normalize(matches('(index.num)|(year)|(yday)|(qday)|(mday)|(date_day)')) %>% 
               step_dummy(all_nominal(), one_hot = TRUE) %>% 
               step_holiday(order_date, holidays = timeDate::listHolidays("US")) 
   
# recipe_spec %>% prep() %>% juice() %>% glimpse()

recipe_spec_no_f <- recipe(sales_trans ~ order_date 
                           + lag_sales_trans_lag84
                           + lag_sales_trans_lag84_roll_30
                           + lag_sales_trans_lag84_roll_60
                           + lag_sales_trans_lag84_roll_90, 
                           data = training(splits)) %>% 
   step_timeseries_signature(order_date) %>% 
   step_rm(matches("(.iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>% 
   step_normalize(matches('(index.num)|(year)|(yday)|(qday)|(mday)|(date_day)')) %>% 
   step_dummy(all_nominal(), one_hot = TRUE) %>% 
   step_holiday(order_date, holidays = timeDate::listHolidays("US")) 


recipe_spec_no_f_no_lag <- recipe(sales_trans ~ order_date, 
                           data = training(splits)) %>% 
   step_timeseries_signature(order_date) %>% 
   step_rm(matches("(.iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>% 
   step_normalize(matches('(index.num)|(year)|(yday)|(qday)|(mday)|(date_day)')) %>% 
   step_dummy(all_nominal(), one_hot = TRUE) %>% 
   step_holiday(order_date, holidays = timeDate::listHolidays("US")) 

#recipe_spec <- read_rds("superstore/data/recipe_spec.rds")

# -Models ----

# 1 sequential models: ARIMA
# 4 non-sequential models: Prophet, XGBoost, Prophet_boost, Random Forest 

# * ARIMA ----

# model_fit_1_arima_basic <- arima_reg() %>% 
#    set_engine('auto_arima') %>% 
#    fit(sales_trans ~ order_date, data = training(splits))
# 
# model_fit_1_arima_basic
# 
# model_fit_2_arima_xregs <- arima_reg() %>% 
#    set_engine('auto_arima') %>% 
#    fit(sales_trans ~ order_date + orders_trans, 
#        data = training(splits))
# 
# model_fit_2_arima_xregs
# ?arima_reg
# 
# 
# model_fit_3_arima_xreg_fourier <-  arima_reg() %>% 
#    set_engine('auto_arima') %>% 
#    fit(sales_trans ~ order_date + orders_trans 
#        + fourier_vec(order_date, period = 7)
#        + fourier_vec(order_date, period = 14)
#        + fourier_vec(order_date, period = 28)
#        + fourier_vec(order_date, period = 84),
#        data = training(splits))
# 
# model_fit_3_arima_xreg_fourier

# model_fit_arima <-  arima_reg() %>% 
#    set_engine('auto_arima') %>% 
#    fit(sales_trans ~ order_date,
#        data = training(splits))
# 
# model_fit_arima


# calibration_tbl <- modeltime_table(model_fit_arima,
#                                    model_fit_arima_fourier_1,
#                                    model_fit_arima_fourier_2,
#                                    model_fit_arima_fourier_3) %>%
#    modeltime_calibrate(testing(splits))
#  
# calibration_tbl %>%
#    modeltime_forecast(new_data = testing(splits),
#                       actual_data = data_prepared_tbl) %>%
#    plot_modeltime_forecast(.conf_interval_fill = 1, .legend_max_width = 5)
# 
# calibration_tbl %>% modeltime_accuracy()

# Let's move ahead with model_fit_4_arima_xreg_fourier as model_fit_arima
#model_fit_arima <- model_fit_arima_fourier



# - WORKFLOWS ----

# * PROPHET ----
# Keep date/dttm feature
# wflw_fit_prophet <- workflow() %>% 
#    add_model(spec = prophet_reg() %>% 
#                set_engine('prophet')) %>% 
#    add_recipe(recipe_spec) %>% 
#    fit(training(splits))
# 
wflw_fit_prophet2 <- workflow() %>%
   add_model(spec = prophet_reg() %>%
                set_engine('prophet')) %>%
   add_recipe(recipe_spec_no_f) %>%
   fit(training(splits))



# * XGBOOST ----

# ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_xgboost <- workflow() %>% 
#    add_model(spec = boost_tree(mode = 'regression') %>% 
#                 set_engine('xgboost')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))

# * ARIMA BOOST ----
# Keep date/dttm feature

# wflw_fit_arimaboost <- workflow() %>% 
#    add_model(spec = arima_boost() %>% 
#                 set_engine('auto_arima_xgboost')) %>% 
#    add_recipe(recipe_spec) %>% 
#    fit(training(splits))
# 
wflw_fit_arimaboost2 <- workflow() %>%
   add_model(spec = arima_boost() %>%
                set_engine('auto_arima_xgboost')) %>%
   add_recipe(recipe_spec_no_f) %>%
   fit(training(splits))
# 
# # * PROPHET BOOST ----
# 
# # Keep date/dttm feature
# wflw_fit_prophet_xgboost <- workflow() %>% 
#    add_model(spec = prophet_boost(seasonality_daily  = FALSE,
#                                   seasonality_weekly = FALSE,
#                                   seasonality_yearly = FALSE) %>% 
#                 set_engine('prophet_xgboost')) %>% 
#    add_recipe(recipe_spec) %>% 
#    fit(training(splits))
# 
wflw_fit_prophet_xgboost2 <- workflow() %>%
   add_model(spec = prophet_boost(seasonality_daily  = FALSE,
                                  seasonality_weekly = FALSE,
                                  seasonality_yearly = FALSE) %>%
                set_engine('prophet_xgboost')) %>%
   add_recipe(recipe_spec_no_f) %>%
   fit(training(splits))

# turned Prophet seasonalities off so prophet is only used for trend
# XGBoost will model seasonality with the Prophet Model's residuals using the 
# calandar features from the recipe spec


# * SVM ----

# ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_svm <- workflow() %>% 
#    add_model(spec = svm_rbf(mode = 'regression') %>% 
#                 set_engine('kernlab')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
wflw_fit_svm2 <- workflow() %>%
   add_model(spec = svm_rbf(mode = 'regression') %>%
                set_engine('kernlab')) %>%
   add_recipe(recipe_spec_no_f %>%
                 update_role(order_date, new_role = 'indicator')) %>%
   fit(training(splits))


# * RANDOM FOREST ----

# ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_rf <- workflow() %>% 
#    add_model(spec = rand_forest(mode = 'regression') %>% 
#                 set_engine('ranger')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
# wflw_fit_rf2 <- workflow() %>% 
#    add_model(spec = rand_forest(mode = 'regression') %>% 
#                 set_engine('ranger')) %>% 
#    add_recipe(recipe_spec_no_f %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))



# * NNET ----

# ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_nnet <- workflow() %>% 
#    add_model(spec = mlp(mode = 'regression') %>% 
#                 set_engine('nnet')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
# wflw_fit_nnet2 <- workflow() %>% 
#    add_model(spec = mlp(mode = 'regression') %>% 
#                 set_engine('nnet')) %>% 
#    add_recipe(recipe_spec_no_f %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))

# * MARS ----

# Multiple Adaptive Regression Splines

# # ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_mars <- workflow() %>% 
#    add_model(spec = mars(mode = 'regression') %>% 
#                 set_engine('earth')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
# wflw_fit_mars2 <- workflow() %>% 
#    add_model(spec = mars(mode = 'regression') %>% 
#                 set_engine('earth')) %>% 
#    add_recipe(recipe_spec_no_f %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))


# # * GLMNET ----
# 
# # ML model.  Change date/dttm feature to 'indicator'
# wflw_fit_glmnet <- workflow() %>% 
#    add_model(spec = linear_reg(mode = 'regression',
#                                penalty = 0.1,
#                                mixture = 0.5) %>% 
#                 set_engine('glmnet')) %>% 
#    add_recipe(recipe_spec %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
# wflw_fit_glmnet2 <- workflow() %>% 
#    add_model(spec = linear_reg(mode = 'regression',
#                                penalty = 0.1,
#                                mixture = 0.5) %>% 
#                 set_engine('glmnet')) %>% 
#    add_recipe(recipe_spec_no_f %>% 
#                  update_role(order_date, new_role = 'indicator')) %>% 
#    fit(training(splits))
# 
# # * NNETAR ----
# 
# # Multiple Adaptive Regression Splines
# 
# wflw_fit_nnetar <- workflow() %>% 
#    add_model(nnetar_reg() %>% 
#                 set_engine('nnetar')) %>% 
#    add_recipe(recipe_spec) %>% 
#    fit(training(splits))
# 
wflw_fit_nnetar2 <- workflow() %>%
   add_model(nnetar_reg() %>%
                set_engine('nnetar')) %>%
   add_recipe(recipe_spec_no_f) %>%
   fit(training(splits))



# submodels_tbl <- modeltime_table(model_fit_arima,
#                                  wflw_fit_prophet,
#                                  wflw_fit_prophet2,
#                                  wflw_fit_arimaboost,
#                                  wflw_fit_arimaboost2,
#                                  wflw_fit_prophet_xgboost,
#                                  wflw_fit_prophet_xgboost2,
#                                  wflw_fit_svm,
#                                  wflw_fit_svm2,
#                                  wflw_fit_rf,
#                                  wflw_fit_rf2,
#                                  wflw_fit_nnet,
#                                  wflw_fit_nnet2,
#                                  wflw_fit_mars,
#                                  wflw_fit_mars2,
#                                  wflw_fit_glmnet,
#                                  wflw_fit_glmnet2, 
#                                  wflw_fit_nnetar,
#                                  wflw_fit_nnetar2) %>% 
#    update_model_description(1, "ARIMA(0,1,1)(0,0,2)[7]") %>% 
#    update_model_description(2, "PROPHET") %>% 
#    update_model_description(3, "PROPHET-no_fourier") %>% 
#    update_model_description(4, "ARIMA_BOOST") %>% 
#    update_model_description(5, "ARIMA_BOOST-no_fourier") %>% 
#    update_model_description(6, "PROPHET_XGBOOST") %>% 
#    update_model_description(7, "PROPHET_XGBOOST-no_fourier") %>% 
#    update_model_description(8, "SVM-Kernlab") %>% 
#    update_model_description(9, "SVM-Kernlab-no_fourier") %>% 
#    update_model_description(10, "RandomForest") %>% 
#    update_model_description(11, "RandomForest-no_fourier") %>% 
#    update_model_description(12, "NNET") %>% 
#    update_model_description(13, "NNET-no_fourier") %>% 
#    update_model_description(14, "MARS") %>% 
#    update_model_description(15, "MARS-no_fourier") %>% 
#    update_model_description(16, "GLMNET") %>% 
#    update_model_description(17, "GLMNET-no_fourier") %>% 
#    update_model_description(18, "NNETAR") %>% 
#    update_model_description(19, "NNETAR-no_fourier") %>% 
#    modeltime_calibrate(testing(splits), quiet = F)
# 
# submodels_tbl %>% modeltime_accuracy() %>% arrange(rmse) 

submodels_tbl_2 <- modeltime_table(wflw_fit_prophet2,
                                   wflw_fit_arimaboost2,
                                   wflw_fit_prophet_xgboost2,
                                   wflw_fit_svm2,
                                   wflw_fit_nnetar2) %>% 
   update_model_description(1, "PROPHET") %>% 
   update_model_description(2, "ARIMA_BOOST") %>% 
   update_model_description(3, "PROPHET_XGBOOST") %>% 
   update_model_description(4, "SVM-Kernlab") %>% 
   update_model_description(5, "NNETAR") %>% 
   modeltime_calibrate(testing(splits))

submodels_tbl_2 %>% modeltime_accuracy() %>% arrange(rmse) 

# **Plot: Models fit on testing(splits) ----

# Remove the 2 Prophet models and plot the other 7.  
submodels_tbl_2 %>%
   modeltime_forecast(new_data    = testing(splits),
                      actual_data = filter_by_time(data_prepared_tbl, .start_date = "2018-09")) %>%
   plot_modeltime_forecast(.conf_interval_alpha = 0.03,
                           .conf_interval_fill  = 'skyblue',
                           .interactive         = FALSE,
                           .title               = "Models fit on testing(splits)",
                           .y_lab               = "sales_trans",
                           .line_size           = 1) +
   theme_dark_grey() +
   scale_color_todd_dark_bright()

# ggsave(here::here("superstore","charts","submodels_tbl_best_untuned.jpeg"),
#        width = 16, height = 10)

# Move forward the the 5 best models for hyperparameter tuning. 

# -HYPERPARAMETER TUNING
# * RESAMPLES - K-FOLD ----- 

set.seed(321)
resamples_kfold <- training(splits) %>% vfold_cv(v = 6)

# resamples_kfold %>% 
#    tk_time_series_cv_plan() %>% 
#    plot_time_series_cv_plan(order_date, 
#                             sales_trans, 
#                             .facet_ncol = 2, 
#                             .interactive = FALSE, 
#                             .title = "K-fold CV Plan", 
#                             .y_lab = "sales_trans") +
#    theme_dark_grey() +
#    scale_color_manual(values = c("#f2f2f2", "#db0000"))
# 
# ggsave(here::here("superstore","charts","cv_plan_k_fold.jpeg"),
#        width = 16, height = 7)


# * RESAMPLES - TSCV ----- 

# Sequential models nnetar and arima_boost need time-series cross-validation.  Wont work with k-fold.

set.seed(321)
resamples_tscv <- time_series_cv(data        = training(splits) %>% drop_na(),
                                 cumulative  = TRUE,
                                 assess      = "12 weeks",
                                 skip        = '6 weeks',
                                 slice_limit = 6)

# resamples_tscv %>% 
#    tk_time_series_cv_plan() %>% 
#    plot_time_series_cv_plan(order_date, 
#                             sales_trans, 
#                             .facet_ncol = 2, 
#                             .interactive = FALSE, 
#                             .title = "Time-Series CV Plan", 
#                             .y_lab = "sales_trans") +
#    theme_dark_grey() +
#    scale_color_manual(values = c("#f2f2f2", "#db0000"))
# 
# ggsave(here::here("superstore","charts","cv_plan_tscv.jpeg"),
#        width = 16, height = 8)


# # * XGBOOST TUNE ----
# 
# # ** Tunable Specification
# 
# model_spec_xgboost_tune <- boost_tree(mode           = 'regression',
#                                       mtry           = tune(),
#                                       trees          = tune(),
#                                       min_n          = tune(),
#                                       tree_depth     = tune(),
#                                       learn_rate     = tune(),
#                                       loss_reduction = tune() ) %>% set_engine('xgboost')
# 
# wflw_spec_xgboost_tune <- workflow() %>% 
#                           add_model(model_spec_xgboost_tune) %>% 
#                           add_recipe(recipe_spec %>% update_role(order_date, 
#                                                                  new_role = 'indicator'))
# 
# # ** Tuning
# 
# set.seed(321)
# tune_results_xgboost <- wflw_spec_xgboost_tune %>% 
#                         tune_grid(resamples  = resamples_kfold,
#                                   param_info = parameters(wflw_spec_xgboost_tune) %>% 
#                                      update(learn_rate = learn_rate(range = c(0.001, 0.400), trans = NULL)), 
#                                   grid       = 10, 
#                                   control    = control_grid(verbose = TRUE))
# 
# 
# # ** Results
# 
# tune_results_xgboost %>% show_best('rmse', n = Inf)
# 
# 
# # ** Finalize
# 
# wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>% 
#                           finalize_workflow(select_best(tune_results_xgboost, 'rmse')) %>% 
#                           fit(training(splits))

# * PROPHET-XGBOOST TUNE ----

# ** Tunable Specification

model_spec_prophet_boost_tune <- prophet_boost(changepoint_num    = tune(),
                                               changepoint_range  = 0.8,
                                               seasonality_yearly = FALSE,
                                               seasonality_weekly = FALSE,
                                               seasonality_daily  = FALSE,
                                               mtry           = 0.8,
                                               trees          = tune(),
                                               min_n          = tune(),
                                               tree_depth     = tune(),
                                               learn_rate     = tune(),
                                               loss_reduction = tune() ) %>% 
                                 set_engine('prophet_xgboost')
# turned Prophet seasonalities off so prophet is only used for trend
# XGBoost will model seasonality with the Prophet Model's residuals using the 
# calandar features from the recipe spec

wflw_spec_prophet_boost_tune <- workflow() %>% 
                                add_model(model_spec_prophet_boost_tune) %>% 
                                add_recipe(recipe_spec_no_f)

# ** Tuning

grid_spec_prophet_boost_1 <- grid_latin_hypercube(parameters(model_spec_prophet_boost_tune),
                                                  size = 20)

plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))

# Round 1
set.seed(321)
tune_results_prophet_boost_1 <- wflw_spec_prophet_boost_tune %>% 
                              tune_grid(resamples  = resamples_kfold,
                                        param_info = parameters(wflw_spec_prophet_boost_tune) %>% 
                                           update(learn_rate = learn_rate(range = c(0.001, 0.400), trans = NULL)), 
                                        grid       = grid_spec_prophet_boost_1, 
                                        control    = control_grid(verbose = TRUE))

# ** Results

tune_results_prophet_boost_1 %>% show_best('rmse', n = Inf)
g <- tune_results_prophet_boost_1 %>% autoplot +
   geom_smooth(se = FALSE) 

plotly::ggplotly(g)

# Round 2 
set.seed(321)
grid_spec_prophet_boost_2 <- grid_latin_hypercube(changepoint_num(range = c(28,32)),
                                                  trees(range = c(1500,1525)),
                                                  min_n(range = c(13,16)),
                                                  tree_depth(range = c(4,4)),
                                                  loss_reduction(range = c(-6.5,-5.5), 
                                                                 trans = scales::log10_trans()),
                                                  learn_rate(range = c(-2.72, -2.68), 
                                                             trans = scales::log10_trans()),
                                                  size = 20)

set.seed(321)
tune_results_prophet_boost_2 <- wflw_spec_prophet_boost_tune %>% 
   tune_grid(resamples  = resamples_kfold,
             param_info = parameters(wflw_spec_prophet_boost_tune), 
             grid       = grid_spec_prophet_boost_2, 
             control    = control_grid(verbose = TRUE))

tune_results_prophet_boost_2 %>% show_best('rmse', n = Inf)
g <- tune_results_prophet_boost_2 %>% autoplot +
   geom_smooth(se = FALSE) 

plotly::ggplotly(g)

plan(sequential)
# ** Finalize

wflw_fit_prophet_boost_tuned2 <- wflw_spec_prophet_boost_tune %>% 
                                finalize_workflow(select_best(tune_results_prophet_boost_2, 'rmse')) %>% 
                                fit(training(splits))

write_rds(wflw_fit_prophet_boost_tuned, "superstore/models/wflw_fit_prophet_boost_tuned.rds")
wflw_fit_prophet_boost_tuned <- read_rds("superstore/models/wflw_fit_prophet_boost_tuned.rds")
# * SVM ----

# ** Tunable Specification

model_spec_svm_tune <- svm_rbf(mode      = 'regression',
                               cost      = tune(),
                               rbf_sigma = tune(),
                               margin    = 0.17 ) %>%
                       set_engine('kernlab')

# ML model.  Change date/dttm feature to 'indicator'
wflw_spec_svm_tune <- workflow() %>%
                      add_model(model_spec_svm_tune) %>%
                      add_recipe(recipe_spec_no_f %>% update_role(order_date,
                                                                  new_role = 'indicator'))

# ** Tuning

grid_spec_svm <- grid_latin_hypercube(parameters(model_spec_svm_tune),
                                                 size = 20)

set.seed(321)
tune_results_svm <- wflw_spec_svm_tune %>%
                    tune_grid(resamples  = resamples_kfold,
                              grid       = grid_spec_svm,
                              control    = control_grid(verbose = TRUE))

# ** Results

tune_results_svm_4 %>% show_best('rmse', n = Inf)

g <- tune_results_svm_3 %>% autoplot +
   geom_smooth(se = FALSE)

plotly::ggplotly(g)
?svm_rbf
grid_spec_svm_4 <- grid_latin_hypercube(cost(range = c(3.2,3.5),
                                             trans = scales::log2_trans()),
                                        rbf_sigma(range = c(-2.6,-2.3),
                                                  trans = scales::log10_trans()),
                                        size = 20)
                   # THIS DOESNT WANT TO WORK.  MARGIN IS BROKEN
set.seed(321)
tune_results_svm_4 <- wflw_spec_svm_tune %>%
   tune_grid(resamples  = resamples_kfold,
             grid       = grid_spec_svm_4,
             control    = control_grid(verbose = TRUE))

# ** Finalize

wflw_fit_svm_tuned <- wflw_spec_svm_tune %>%
   finalize_workflow(select_best(tune_results_svm_4, 'rmse')) %>%
   fit(training(splits))


# * RANDOM FOREST TUNE ----

# ** Tunable Specification

# model_spec_rf_tune <- rand_forest(mode  = 'regression',
#                                   mtry  = tune(),
#                                   trees = tune(),
#                                   min_n = tune()) %>% 
#                       set_engine('ranger')
# 
# wflw_spec_rf_tune <- workflow() %>% 
#                      add_model(model_spec_rf_tune) %>% 
#                      add_recipe(recipe_spec %>% update_role(order_date, 
#                                                             new_role = 'indicator'))
# 
# # ** Tuning
# 
# set.seed(321)
# tune_results_rf <- wflw_spec_rf_tune %>% 
#                    tune_grid(resamples = resamples_kfold,
#                              grid      = 10,
#                              control   = control_grid(verbose = TRUE))
# 
# # ** Results
# 
# tune_results_rf %>% show_best('rmse', n = Inf)
# 
# # ** Finalize
# 
# wflw_fit_rf_tuned <- wflw_spec_rf_tune %>% 
#                      finalize_workflow(select_best(tune_results_rf, 'rmse')) %>% 
#                      fit(training(splits))
# 
# 
# # * NNET ----
# 
# # ** Tunable Specification
# 
# model_spec_nnet_tune <- mlp(mode  = 'regression', 
#                             hidden_units = tune(),
#                             penalty = tune(),                            
#                             epochs = 50) %>% 
#                         set_engine('nnet')
# 
# wflw_spec_nnet_tune <- workflow() %>% 
#                        add_model(model_spec_nnet_tune) %>% 
#                        add_recipe(recipe_spec %>% update_role(order_date, 
#                                                               new_role = 'indicator'))
# 
# # ** Tuning
# 
# set.seed(321)
# tune_results_nnet <- wflw_spec_nnet_tune %>% 
#                      tune_grid(resamples = resamples_kfold,
#                                grid      = 15,
#                                control   = control_grid(verbose = TRUE))
# 
# # ** Results
# 
# tune_results_nnet %>% show_best('rmse', n = Inf)
# 
# # ** Finalize
# 
# wflw_fit_nnet_tuned <- wflw_spec_nnet_tune %>% 
#                        finalize_workflow(select_best(tune_results_nnet, 'rmse')) %>% 
#                        fit(training(splits))




# * NNETAR ----

# ** Tunable Specification
recipe_spec_no_f_no_lag <- wflw_fit_nnetar2 %>% 
   pull_workflow_preprocessor() %>% 
   step_naomit(starts_with('lag'))

recipe_spec_no_f_no_lag %>% prep() %>% juice() %>% glimpse()
recipe_spec_no_fourier %>% prep() %>% juice() %>% glimpse()


model_spec_nnetar_tune <- nnetar_reg(mode            = 'regression', 
                                     seasonal_period = 7,
                                     non_seasonal_ar = tune(),
                                     seasonal_ar     = tune(), 
                                     num_networks    = 10,
                                     hidden_units    = tune(),
                                     penalty         = tune(),                            
                                     epochs          = 50) %>% 
   set_engine('nnetar')

grid_spec_nnetar_1 <- grid_latin_hypercube(parameters(model_spec_nnetar_tune),
                                           size = 20)

wflw_spec_nnetar_tune <- workflow() %>% 
   add_model(model_spec_nnetar_tune) %>% 
   add_recipe(recipe_spec_no_f)

# ** Tuning
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))

set.seed(321)
tune_results_nnetar <- wflw_spec_nnetar_tune %>% 
                       tune_grid(resamples = resamples_tscv,
                                 grid      = grid_spec_nnetar_1,
                                 metrics   = default_forecast_accuracy_metric_set(),
                                 control   = control_grid(verbose = TRUE))
?tune_grid
# ** Results

tune_results_nnetar_2 %>% show_best('rmse', n = Inf)
tune_results_nnetar %>% show_best('rsq', n = Inf)
tune_results_nnetar %>% show_best('mase', n = Inf)


plan(sequential)

g <- tune_results_nnetar_2 %>% autoplot +
   geom_smooth(se = FALSE) 
   
plotly::ggplotly(g)

set.seed(321)
grid_spec_nnetar_2 <- grid_latin_hypercube(non_seasonal_ar(range = c(1,1)),
                                           seasonal_ar(range = c(0,0)),
                                           hidden_units(range = c(5,5)),
                                           penalty(range = c(-6,-3), 
                                                   trans = scales::log10_trans()),
                                           size = 10)

set.seed(321)
tune_results_nnetar_2 <- wflw_spec_nnetar_tune %>% 
   tune_grid(resamples = resamples_tscv,
             grid      = grid_spec_nnetar_2,
             metrics   = default_forecast_accuracy_metric_set(),
             control   = control_grid(verbose = TRUE))

# ** Finalize

wflw_fit_nnetar_tuned3 <- wflw_spec_nnetar_tune %>% 
   finalize_workflow(select_best(tune_results_nnetar_2, 'rmse')) %>% 
   fit(training(splits))

plan(sequential)

write_rds(wflw_fit_nnetar_tuned, "superstore/models/wflw_fit_nnetar_tuned.rds")
wflw_fit_nnetar_tuned <- read_rds("superstore/models/wflw_fit_nnetar_tuned.rds")


# # * MARS TUNE ----
# 
# # ** Tunable Specification
# 
# model_spec_mars_tune <- mars(mode        = 'regression',
#                              num_terms   = tune(),
#                              prod_degree = tune()) %>% 
#                         set_engine('earth')
# 
# wflw_spec_mars_tune <- workflow() %>% 
#                        add_model(model_spec_mars_tune) %>% 
#                        add_recipe(recipe_spec %>% update_role(order_date, 
#                                                               new_role = 'indicator'))
# 
# # ** Tuning
# 
# set.seed(321)
# tune_results_mars <- wflw_spec_mars_tune %>% 
#                      tune_grid(resamples = resamples_kfold,
#                                grid      = 10,
#                                control   = control_grid(verbose = TRUE))
# 
# # ** Results
# 
# tune_results_mars %>% show_best('rmse')
# 
# # ** Finalize
# 
# wflw_fit_mars_tuned <- wflw_spec_mars_tune %>% 
#                        finalize_workflow(select_best(tune_results_mars, 'rmse')) %>% 
#                        fit(training(splits))


# 6.0 EVALUATE PANEL FORECEASTS  -----

# * Model Table ----

submodels_tuned_tbl <- modeltime_table(wflw_fit_nnetar_tuned3,
                                       wflw_fit_prophet_boost_tuned,
                                       wflw_fit_svm_tuned) %>% 
                       update_model_description(1, "NNETAR-tuned") %>% 
                       update_model_description(2, "PROPHET_XGBOOST-tuned") %>%   
                       update_model_description(3, "SVM-Kernlab-tuned")  

   
# * Calibration ----

calibration_tbl <- submodels_tuned_tbl %>% modeltime_calibrate(testing(splits))

# * Accuracy ----

calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse) 

# * Forecast Test ----

# Let's drop the xgboost and mars
calibration_tbl %>% 
   modeltime_forecast(new_data    = testing(splits),
                      actual_data = filter_by_time(data_prepared_tbl, .start_date = "2018-09"),
                      keep_data   = TRUE) %>% 
   plot_modeltime_forecast(.conf_interval_alpha = 0.04, 
                           .conf_interval_fill  = 'skyblue', 
                           .interactive         = FALSE, 
                           .title               = "Tuned Models fit on testing(splits)",
                           .y_lab               = "sales_trans", 
                           .line_size           = 1.2) +
   theme_dark_grey() +
   scale_color_todd_dark_bright()

ggsave(here::here("superstore","charts","tuned_models_fit_testing.jpeg"),
       width = 16, height = 10)


# 8.0 FUTURE FORECAST ----

refit_tbl <- calibration_tbl %>% 
             modeltime_refit(data = data_prepared_tbl)

refit_tbl %>% 
   modeltime_forecast(new_data    = forecast_tbl,
                      actual_data = data_prepared_tbl,
                      keep_data   = FALSE) %>%  
   mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(x    = .,
                                                               mean = std_mean_sales,
                                                               sd   = std_sd_sales))) %>%
   mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(x           = ., 
                                                                limit_lower = limit_lower,
                                                                limit_upper = limit_upper,
                                                                offset      = offset))) %>% 
   filter_by_time(.start_date = '2018-01' ) %>% 
   plot_modeltime_forecast(.conf_interval_alpha = 0.05, 
                           .conf_interval_fill  = 'skyblue', 
                           .interactive         = FALSE, 
                           .title               = "Tuned Models Forecast",
                           .y_lab               = "Daily Sales ($)",
                           .line_size           = 1) +
   theme_dark_grey() +
   scale_color_todd_dark_bright()

   ggsave("superstore/charts/tuned_models_forecast_inv_trans_15months.jpeg",
          width = 16, height = 8)
   
# -WEIGHTED ENSEMBLE ----   
   
   # * Calibration ----
   calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse) 
   
   weight_ensemble_tbl <- calibration_tbl[-c(1,6),] %>%  
                          ensemble_weighted(loadings = c(2,5,2,2)) %>% 
                          modeltime_table()
   
   weight_ensemble_tbl %>% modeltime_accuracy(testing(splits))

   refit_weight_tbl <- weight_ensemble_tbl %>% 
                       modeltime_refit(data = data_prepared_tbl)
   
   refit_weight_tbl %>% 
      modeltime_forecast(new_data    = forecast_tbl,
                         actual_data = data_prepared_tbl)  %>%  
      mutate(across(.value, .fns = ~ standardize_inv_vec(x    = .,
                                                         mean = std_mean_sales,
                                                         sd   = std_sd_sales))) %>%
      mutate(across(.value, .fns = ~ log_interval_inv_vec(x           = ., 
                                                          limit_lower = limit_lower,
                                                          limit_upper = limit_upper,
                                                          offset      = offset))) %>% 
      filter_by_time(.start_date = '2018' ) %>% 
      plot_modeltime_forecast(.conf_interval_alpha = 0.05, 
                              .conf_interval_fill  = 'skyblue', 
                              .interactive         = FALSE, 
                              .title               = "Weighted Ensemble Forecast", 
                              .y_lab               = "Daily Sales ($)",
                              .line_size           = 1) +
      theme_dark_grey() +
      scale_color_manual(values = c("#fbb4ae", "#75e6da"))

   ggsave("superstore/charts/weight_ensemble_forecast_inv_trans_15months.jpeg",
          width = 16, height = 10)
   




























