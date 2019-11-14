# 1.0 Libraries ----

# App
# library(flexdashboard)
# library(shiny)
# library(shinyWidgets)
# library(shinyjs)

# Core
library(tidyverse)
library(tidyquant)
library(DT)

# Clean Up
library(janitor)

# Visualizations
library(plotly)

# Modeling
library(parsnip)
library(xgboost)

# Database
library(readr)

# 2.0 Explore Data ----
all_us_data_tbl <- read_csv("data/Border_Crossing_Entry_Data.csv") %>% glimpse()

all_us_data_tbl %>% str()

all_us_data_tbl %>% summary()

data_tbl %>%  summarize_all(~ sum(is.na(.))) %>% glimpse()

# Count missing values in each column
all_us_data_tbl %>% summarize_all(~ sum(is.na(.))) %>% glimpse()

# Count unique
all_us_data_tbl %>% summarize_all(n_distinct)

data_tbl <- all_us_data_tbl %>% janitor::clean_names()

# convert date to date
data_tbl <- data_tbl %>% 
    separate(
        date,
        into = "entry_date",
        sep = " ",
        remove = TRUE
    ) %>% 
    mutate(entry_date = as.Date(entry_date, format = "%m/%d/%Y")) %>% 
    extract(location, into = c("long", "lat"), regex = "(-?[\\d.]+) (-?[\\d.]+)", remove=TRUE, convert = TRUE)
    
data_tbl %>% summarize_all(n_distinct)

# 116 port name but 117 port codes????

port_tbl <- data_tbl %>% distinct(port_name, port_code) %>% arrange(port_code)

port_tbl[duplicated(port_tbl$port_name),]

# Eastport duplicated twice, there is Eastport in Maine and Idaho

# Port codes can have multiple locations
data_tbl %>% distinct(lat, long)
temp_tbl <- data_tbl %>% distinct(port_code, lat, long) %>% count(port_code)

label_tbl <- temp_tbl %>% group_by(n) %>% summarize(count = n()) %>% ungroup()

# plot count for ports with n number of locations
temp_tbl %>% ggplot(aes(x=n)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1)

data_tbl %>% distinct(measure)

# value counts for people will be for Personal Vehicle Passengers, Bus Passengers, Pedestrians, Train Passengers 
process_data <- data_tbl %>% 
    filter(measure %in% c("Personal Vehicle Passengers" , "Bus Passengers", "Pedestrians", "Train Passengers")) %>%
    mutate(entry_date = floor_date(entry_date, unit = "year")) %>% 
    group_by(entry_date, measure) %>% 
    summarize(people_cnt = sum(value)) %>% 
    ungroup() %>% 
    mutate(label = str_glue("Date: {entry_date}
                            Method: {measure}
                            People: {people_cnt}"))

# Bar chart of passengers by years and measure given above
process_data_tbl <- data_tbl %>% 
    filter(measure %in% c("Personal Vehicle Passengers" , "Bus Passengers", "Pedestrians", "Train Passengers"), year(entry_date) != 2019) %>%
    mutate(entry_date = floor_date(entry_date, unit = "year")) %>% 
    group_by(entry_date, measure) %>% 
    summarize(people_cnt = sum(value)) %>% 
    ungroup() %>% 
    mutate(label = str_glue("Date: {entry_date}
                            Method: {measure}
                            People: {people_cnt}"))

#labels <- process_data_tbl %>% distinct(entry_date)
#labels$entry_date <- year(labels$entry_date)
#c(labels$entry_date)

process_data_tbl %>% ggplot(aes(entry_date, people_cnt, fill = measure)) + geom_col() + 
    scale_y_continuous(labels = scales::scientific_format()) + theme_tq() + 
    labs(title="Border Crossing Entry by Method", x= "", y="Number of People", fill="Crossing Means") + 
    facet_wrap(~ measure, ncol = 2, scales = "free_y")

# Bar chart of passengers by which border crossing Mexico VS Canada
process_data_tbl_2 <- data_tbl %>% 
    filter(measure %in% c("Personal Vehicle Passengers" , "Bus Passengers", "Pedestrians", "Train Passengers"), year(entry_date) != 2019) %>%
    mutate(entry_date = floor_date(entry_date, unit = "year")) %>% 
    group_by(entry_date, border) %>% 
    summarize(people_cnt = sum(value)) %>% 
    ungroup() %>% 
    mutate(label = str_glue("Date: {entry_date}
                            Border: {border}
                            People: {people_cnt}"))


process_data_tbl_2 %>% ggplot(aes(entry_date, people_cnt, fill = border)) + geom_col() + 
    scale_y_continuous(labels = scales::scientific_format()) + theme_tq() + 
    labs(title="Border Crossing Entry by Border", x= "", y="Number of People", fill="Border") +
    #geom_text(aes(label = people_cnt), position = position_stack(vjust = 0.5))
    facet_wrap(~ border, ncol = 2)

# Scatter Geo size by count of people
# Since multiple ports, let's choose a single lat/long for a port when suming people
port_tbl <- data_tbl %>% distinct(port_code, lat, long)
port_tbl <- port_tbl[!duplicated(port_tbl$port_code), ]

data_tbl %>% summarize_all(n_distinct)
data_tbl_2 <- data_tbl

for (i in 1:nrow(data_tbl)) {
    index <- match(data_tbl$port_code[i], port_tbl$port_code)
    data_tbl$lat[i] = port_tbl$lat[index]
    data_tbl$long[i] = port_tbl$long[index]
}

data_tbl %>% write_csv("./data/cleaned_data.csv")
data_tbl %>% distinct(port_code, long)


process_data_tbl_3 <- data_tbl %>% 
    filter(measure %in% c("Personal Vehicle Passengers" , "Bus Passengers", "Pedestrians", "Train Passengers")) %>%
    group_by(port_code) %>% 
    mutate(people_cnt = sum(value), lat = mean(lat), long = mean(long)) %>% 
    select(port_name, lat, long, people_cnt) %>%
    ungroup() %>% 
    mutate(label_text = str_glue("Port: {port_name}
                            Border Entry Count: {people_cnt/1e6} millions"))


process_data_tbl_3 %>%
    plot_geo(locationmode = "USA-states", sizes = c(10, 250)) %>%
    add_markers(
        x = ~long, 
        y = ~lat, 
        size = ~people_cnt/100000, 
        color = ~people_cnt, 
        hoverinfo = "text",
        text = ~label_text
    ) %>% 
    layout(
        title = "Border Entry", 
        geo = list(
            scope = "usa",
            projection = list(type = "albers usa"),
            showlake  = TRUE,
            lakecolor  = toRGB("gray85"),
            subunitwidth = 1,
            countrywidth = 1
        )
    )

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)
df %>% glimpse()
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
)

plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
    add_markers(
        x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
        text = ~paste(df$name, "<br />", df$pop/1e6, " million")
    ) %>%
    layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

nrow(data_tbl)
index <- match(data_tbl$port_code[1000], port_tbl$port_code)
port_tbl$lat[index]

#data_tbl$port_code[2]
#port_tbl[which(data_tbl$port_code[2] == 108),]


data_tbl %>% colnames()


















# Filter only data for states
state_data_tbl <- all_us_data_tbl %>% filter(STATE %in% str_to_upper(state.name))

state_data_tbl %>% glimpse()

# View Summary
state_data_tbl %>% summary()

# Count missing values in each column
state_data_tbl %>% summarize_all(~ sum(is.na(.))) %>% glimpse()

# 2016, 2017 has a lot of missing values drop rows
state_data_tbl <- state_data_tbl %>% filter(YEAR != 2017, YEAR != 2016)

# Drop Primary key and enroll
state_data_tbl <- state_data_tbl %>% select(-PRIMARY_KEY, -ENROLL)

# Fix Expenditure Rows, replace NA with 0
state_data_tbl <- state_data_tbl %>% replace_na(list(OTHER_EXPENDITURE=0))

# 10 na for GRADES_PK_G and GRADES_ALL_G...
# Replace GRADES_PK_G with 0 and recalculate GRADES_ALL_G based on remainder grade values
temp_tbl <- state_data_tbl %>% filter(is.na(GRADES_PK_G))

state_data_tbl <- state_data_tbl %>% replace_na(list(GRADES_PK_G=0))
state_data_tbl <- state_data_tbl %>% replace_na(list(GRADES_ALL_G = sum(select(.,GRADES_PK_G:GRADES_9_12_G))))


# Find years the math/reading score is given
state_data_tbl %>% filter(!is.na(AVG_MATH_4_SCORE)) %>% glimpse() %>% pull(YEAR) %>% unique()

# Test years are 1992 1996 2000 2003 2005 2007 2009 2011 2013 2015

temp_tbl <- state_data_tbl %>% filter(YEAR %in% list(1992, 1996, 2000, 2003, 2005, 2007, 2009, 2011, 2013, 2015), (is.na(AVG_MATH_4_SCORE)|is.na(AVG_MATH_8_SCORE)|is.na(AVG_READING_4_SCORE)|is.na(AVG_READING_8_SCORE)))

state_data_tbl %>% mutate(YEAR = format(as.Date(as.character(YEAR), "%Y"), "%Y"))

state_data_tbl %>% write_csv("./data/state_cleaned.csv")                                      

state_data_tbl %>% 
    ggplot(aes(x=YEAR, y=TOTAL_REVENUE, fill=STATE)) + geom_col()

state_data_tbl %>% 
    ggplot(aes(x=YEAR, y=TOTAL_EXPENDITURE, fill=STATE)) + geom_col()

state_data_tbl %>% filter(AVG_READING_8_SCORE != 0) %>% 
    ggplot(aes(x=AVG_READING_8_SCORE, y=TOTAL_REVENUE, color=STATE)) + geom_point()

           
           