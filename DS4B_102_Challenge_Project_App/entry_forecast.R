aggregate_time_series <-
function(data, time_unit = "month") {
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
plot_time_series <-
function(data) {
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
generate_forecast <-
function(data, n_future = 12, seed = NULL) {
    
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
plot_forecast <-
function(data){
    
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
        g <- g + geom_smooth(color= "#C0C0C0", method = "loess", span = 0.05, linetype="dotted")
    }
    
    # Only one prediction
    if(n_predictions == 1) {
        g <- g + geom_point(aes(text = label_text), size = 1)
    } else {
        g <- g + geom_point(aes(text = label_text), size = 0.01) 
    }
    ggplotly(g, tooltip = "text")  %>% layout(margin = list(b=100))
}
