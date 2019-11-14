# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# DEMAND FORECAST ANALYSIS ----

# 1.0 LIBRARIES -----

# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)
library(directlabels)
#library(ggrepel)

# Modeling Libraries
library(parsnip)
library(timetk)

# Data
library(readr)



# 2.0 PROCESSED DATA ----
setwd("C:/Users/charl/Desktop/BusinessScienceR/DS4B_102_Challenge_Project_App/")

all_us_data_tbl <- read_csv("data/cleaned_data.csv")

processed_data_tbl <- all_us_data_tbl %>% 
    filter(measure %in% c("Personal Vehicle Passengers" , "Bus Passengers", "Pedestrians", "Train Passengers")) %>%
    #group_by(entry_date, measure) %>% 
    #mutate(people_cnt_method_sum = sum(value)) %>% 
    #ungroup() %>%
    rename(entry_method = measure, people_cnt = value)


# 3.0 TIME SERIES AGGREGATION ----

# 3.1 DATA MANIPULATION ----
# Group by border, state, entry_method, entry_method
time_unit <- "quarter"

time_plot_tbl <- processed_data_tbl %>%
    
    mutate(entry_date = floor_date(entry_date, unit = time_unit)) %>%
    
    group_by(state, border, entry_date, entry_method) %>%
    summarize(people_cnt = sum(people_cnt)) %>% 
    select(border, state, entry_method, entry_method, entry_date, people_cnt) %>% 
    ungroup() %>%
    
    mutate(label_text = str_glue("Entry Date: {entry_date}
                                 Entry Method: {entry_method}
                                 People: {round(people_cnt/1e6, 2)} millions"))

time_plot_tbl

# 3.2 FUNCTION ----

# TODO - aggregate_time_series() 

aggregate_time_series <- function(data, time_unit = "month") {
    # Remove partial year date for 2019 since data recorded only to 03/2019
    if(time_unit == "year") {
        output_tbl <- data %>%
            filter(year(entry_date) != 2019) %>% 
            mutate(entry_date = floor_date(entry_date, unit = time_unit)) %>%
            group_by(entry_date, entry_method) %>%
            summarize(people_cnt = sum(people_cnt)) %>% 
            select(entry_method, entry_date, people_cnt) %>% 
            ungroup() %>%
            mutate(label_text = str_glue("Entry Date: {entry_date}
                            People Count: {round(people_cnt/1e6, 2)} millions"))
    } else {
        output_tbl <- data %>%
            mutate(entry_date = floor_date(entry_date, unit = time_unit)) %>%
            group_by(entry_date, entry_method) %>%
            summarize(people_cnt = sum(people_cnt)) %>% 
            select(entry_method, entry_date, people_cnt) %>% 
            ungroup() %>% 
            mutate(label_text = str_glue("Entry Date: {entry_date}
                            People Count: {round(people_cnt/1e6, 2)} millions"))
    }
    
    return(output_tbl)
}

processed_data_tbl %>% 
    aggregate_time_series("month")


# 3.3 TIME SERIES PLOT ----

data <- processed_data_tbl %>%
    aggregate_time_series("year")
    

unique(data$state)
unique(data$entry_method)

create_data_dummy <- function(data) {
    data %>% 
        mutate(
            border_canada = border %>% str_to_lower() %>% str_detect("canada") %>% as.numeric(),
            
            state_ak = state %>% str_to_lower() %>% str_detect("alaska") %>% as.numeric(), 
            state_az = state %>% str_to_lower() %>% str_detect("arizona") %>% as.numeric(), 
            state_ca = state %>% str_to_lower() %>% str_detect("california") %>% as.numeric(), 
            state_id = state %>% str_to_lower() %>% str_detect("idaho") %>% as.numeric(), 
            state_ma = state %>% str_to_lower() %>% str_detect("maine") %>% as.numeric(), 
            state_mi = state %>% str_to_lower() %>% str_detect("michigan") %>% as.numeric(), 
            state_mn = state %>% str_to_lower() %>% str_detect("minnesota") %>% as.numeric(), 
            state_mo = state %>% str_to_lower() %>% str_detect("montana") %>% as.numeric(), 
            state_nm = state %>% str_to_lower() %>% str_detect("new mexico") %>% as.numeric(), 
            state_ny = state %>% str_to_lower() %>% str_detect("new_york") %>% as.numeric(), 
            state_nd = state %>% str_to_lower() %>% str_detect("north dakota") %>% as.numeric(), 
            state_tx = state %>% str_to_lower() %>% str_detect("texas") %>% as.numeric(), 
            state_va = state %>% str_to_lower() %>% str_detect("vermont") %>% as.numeric(),
            
            entry_method_bus = entry_method %>% str_to_lower() %>% str_detect("bus") %>% as.numeric(),
            entry_method_ped = entry_method %>% str_to_lower() %>% str_detect("ped") %>% as.numeric(),
            entry_method_veh = entry_method %>% str_to_lower() %>% str_detect("vehicle") %>% as.numeric()
        )
}

create_data_entry_dummy <- function(data) {
    data %>% 
        mutate(
            entry_method_bus = entry_method %>% str_to_lower() %>% str_detect("bus") %>% as.numeric(),
            entry_method_ped = entry_method %>% str_to_lower() %>% str_detect("ped") %>% as.numeric(),
            entry_method_veh = entry_method %>% str_to_lower() %>% str_detect("vehicle") %>% as.numeric()
        )
}

processed_data_tbl %>%
    aggregate_time_series("year") %>% 
    create_data_dummy()

# Predict above the reaggregate for plot
# Load model based on time unit???
g <- data %>%
    ggplot(aes(entry_date, people_cnt, group=entry_method, color=entry_method)) +
    
    geom_line() +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.05) + 
    
    theme_tq() +
    #expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "", y = "", col="Entry Method")

ggplotly(g, tooltip = "text")


# 3.4 FUNCTION ----

# TODO - MAKE FUNCTION 
plot_time_series <- function(data) {
    g <- data %>%
        ggplot(aes(entry_date, people_cnt, group=entry_method, color=entry_method)) +
        
        geom_line() +
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.05) + 
        
        theme_tq() +
        #expand_limits(y = 0) +
        scale_y_continuous(labels = scales::comma_format()) +
        labs(x = "", y = "", col="Entry Method") + scale_color_brewer(palette = "Accent")
    
    ggplotly(g, tooltip = "text")  %>% layout(margin = list(b=100))
}

processed_data_tbl %>% 
    aggregate_time_series(time_unit = "quarter") %>% 
    plot_time_series()

# 4.0 FORECAST -----

# 4.1 SETUP TRAINING DATA AND FUTURE DATA ----

# TODO - timetk

entry_methods  <-  unique(processed_data_tbl$entry_method)

data_list = vector(mode = "list", length = length(entry_methods))
for(method in entry_methods) {
    print(method)
    data <- processed_data_tbl %>% filter(entry_method == method) %>% 
        aggregate_time_series(time_unit = "month")
    data_list[[match(method, entry_methods)]] <-  data
}

data_list[2]
match("Pedestrians", entry_methods)
data <- processed_data_tbl %>% filter(entry_method == "Pedestrians") %>% 
    aggregate_time_series(time_unit = "month")

data %>% tk_index() %>% tk_get_timeseries_signature()
data %>% tk_index() %>% tk_get_timeseries_summary()

# helper functions
tk_get_timeseries_unit_frequency()
data %>% tk_get_timeseries_variables()

data %>% tk_augment_timeseries_signature()

train_tbl <- data %>% 
    tk_augment_timeseries_signature()

future_data_tbl <- data %>%
    tk_index() %>% 
    tk_make_future_timeseries(n_future = 24, inspect_weekdays = FALSE, inspect_months = FALSE) %>% 
    tk_get_timeseries_signature()

# 4.2 MACHINE LEARNING ----

# TODO - XGBoost

seed <- 123
set.seed(seed)
model_xgboost <- boost_tree(
        mode = "regression", 
        mtry = 20, 
        trees = 500, 
        min_n = 3, 
        tree_depth = 8, 
        learn_rate = 0.01, 
        loss_reduction = 0.01) %>% 
    set_engine(engine = "xgboost") %>% 
    fit.model_spec(people_cnt ~ ., data = train_tbl %>% select(-entry_date, -entry_method, -label_text, -diff))


# 4.3 MAKE PREDICTION & FORMAT OUTPUT ----

# TODO - predict
future_data_tbl

prediction_tbl <- predict(model_xgboost, new_data = future_data_tbl) %>% 
    bind_cols(future_data_tbl) %>% 
    select(.pred, index) %>% 
    rename(people_cnt = .pred,
           entry_date = index) %>% 
    mutate(label_text = str_glue("Entry Date: {entry_date}
                            People Count: {round(people_cnt/1e6, 2)} millions")) %>% 
    add_column(key = "Prediction", 
               entry_method = "Pedestrians")

output_tbl <- data %>% 
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl)

output_tbl

# 4.4 FUNCTION ----

# TODO - generate_forecast()
n_future <- 12
seed <- 123

generate_forecast <- function(data, n_future = 12, seed = NULL) {
    
    entry_methods  <-  unique(data$entry_method)
    time_scale <- data %>% 
        tk_index() %>%
        tk_get_timeseries_summary() %>% 
        pull(scale)
    
    for(method in entry_methods) {
        method_data <- data %>% filter(entry_method == method) %>% 
            aggregate_time_series(time_unit = "month")

        train_tbl <- method_data %>% 
            tk_augment_timeseries_signature()
        
        future_data_tbl <- method_data %>% 
            tk_index() %>% 
            tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
            tk_get_timeseries_signature()
        
        if (time_scale == "year") {
            model <- linear_reg(mode = "regression") %>% 
                set_engine(engine = "lm") %>% 
                fit.model_spec(people_cnt ~ ., data = train_tbl %>% select(-entry_date, -entry_method, -label_text, -diff))
        }
        else {
            seed <- seed
            set.seed(seed)
            model <- boost_tree(
                mode = "regression", 
                mtry = 20, 
                trees = 500, 
                min_n = 3, 
                tree_depth = 8, 
                learn_rate = 0.01, 
                loss_reduction = 0.01) %>% 
                set_engine(engine = "xgboost") %>% 
                fit.model_spec(people_cnt ~ ., data = train_tbl %>% select(-entry_date, -entry_method, -label_text, -diff))
        }
    
        prediction_tbl <- predict(model, new_data = future_data_tbl) %>% 
            bind_cols(future_data_tbl) %>% 
            select(.pred, index) %>% 
            rename(people_cnt = .pred,
                   entry_date = index) %>% 
            mutate(label_text = str_glue("Entry Date: {entry_date}
                            People Count: {round(people_cnt/1e6, 2)} millions")) %>% 
            add_column(key = "Prediction", 
                       entry_method = method, newKey = str_glue("{method} - Prediction"))
        
            if(match(method, entry_methods) == 1) {
                output_tbl <- prediction_tbl
            } else {
                output_tbl <- output_tbl %>% 
                bind_rows(prediction_tbl)
            }
    }
    
    output_tbl <- data %>% 
        mutate(newKey = str_glue("{entry_method} - Actual")) %>% 
        add_column(key = "Actual") %>%
        bind_rows(output_tbl)
        
    return(output_tbl)
}

data <- processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123)

match("Personal Vehicle Passengers", entry_methods) == 1



processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123)

# 5.0 PLOT FORECAST ----

# 5.1 PLOT ----

# TODO - plot
data <- processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123)

g <- data %>%
    ggplot(aes(entry_date, people_cnt, group=entry_method, color=newKey)) +
    
    geom_line() + 
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.05, linetype="dotted") + 
    
    theme_tq() +
    #expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "", y = "", col="Entry Method") + scale_color_brewer(palette = "Accent")

ggplotly(g, tooltip = "text")

data %>% filter(key == "Prediction") %>% distinct(entry_date) %>% nrow()

# 5.2 FUNCTION ----

# TODO - plot_forecast()
data <- processed_data_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(n_future = 1, seed = 123)

plot_forecast <- function(data){
    
    time_scale <- data %>% 
        tk_index() %>% 
        tk_get_timeseries_summary() %>% 
        pull(scale)
    
    # Only 1 Prediction - convert to points
    n_predictions <- data %>% 
        filter(key == "Prediction") %>% 
        distinct(entry_date) %>% 
        nrow()
    
    g <- data %>%
        ggplot(aes(entry_date, people_cnt, group=entry_method, color=newKey)) +
        
        geom_line() + 
        
        theme_tq() +
        #expand_limits(y = 0) +
        scale_y_continuous(labels = scales::comma_format()) +
        labs(x = "", y = "", col="Entry Method") + scale_color_brewer(palette = "Accent")
    
    # Yearly - LM Smoother
    if(time_scale == "year") {
        g <- g +  geom_smooth(method = "lm")
    } else {
        g <- g + geom_smooth(aes(color= "#d3d3d3"), method = "loess", span = 0.05, linetype="dotted")
    }
    
    # Only one prediction
    if(n_predictions == 1) {
        g <- g + geom_point(aes(text = label_text), size = 1)
    } else {
        g <- g + geom_point(aes(text = label_text), size = 0.01) 
    }
    ggplotly(g, tooltip = "text")  %>% layout(margin = list(b=100))
}

processed_data_tbl %>% filter(entry_method %in% list("Personal Vehicle Passengers", "Bus Passengers")) %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(n_future = 5, seed = 123) %>% plot_forecast()

processed_data_tbl %>%  
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123) %>% plot_forecast()

temp_tbl <- processed_data_tbl %>%  
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123)
# 6.0 SAVE FUNCTIONS ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "entry_forecast.R")
