library(tidyverse)
# library(blscrapeR) no longer on CRAN
library(xts)
library(rvest)
library(tigris)
library(curl)
library(polite)
library(blsAPI)
library(devtools)
library(jsonlite)
options(tigris_use_cache = TRUE)

# code for dateCast from blscrapeR
# from https://github.com/keberwein/blscrapeR/blob/master/R/dateCast.R
dateCast <- function (api_df=NULL, dt_format=NULL){
  period <- api_df$period
  year <- api_df$year
  
  if ("year" %in% colnames(api_df) & "period" %in% colnames(api_df)){
    api_df$date <- as.Date(paste(api_df$year, ifelse(api_df$period == "M13", 12, substr(api_df$period, 2, 3)), "01", sep="-"))
  }else{
    message("Please be sure to have columns named 'year' and 'period' in your dataframe as they are returned from the bls_api() function.")
  }
  if (!is.null(dt_format)) {
    as.character(dt_format)
    api_df$date <- format(api_df$date, format=dt_format)
  }
  return(api_df)
}

# the BLS shut down the ability to scrape their time.series pages
# AllIowaURL <- "https://download.bls.gov/pub/time.series/la/la.data.22.Iowa"

thisyear <- year(today())
# desired county FIPS 
counties <- map(c("011", "031", "095", "103", "105", "113", "183"), function(x) paste0("LAUCN19", x))
some <- map(c("04", "05", "06"), function(x) paste0("00000000", x))
all <- map(counties, function(x) paste0(x, some)) %>% list_c()

previous_data <- tibble(filenames = list.files(path = "data/"), dates = c())
most_recent <- previous_data %>% 
  mutate(dates = as.Date(str_extract(filenames, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))) %>% 
  arrange(desc(dates)) %>% 
  slice_head()

last_pulled_plus_28 <- most_recent %>% 
  pull(dates)+20
last_pulled_file <- most_recent %>% 
  pull(filenames)

if (today() > last_pulled_plus_28) {

response <- blsAPI(all[1])
all_responses <- blsAPI(
  list(
    'seriesid'=all,
    'startyear'=(thisyear-9),
    'endyear'=thisyear
    )
  )
json <- fromJSON(response)
all_json <- fromJSON(all_responses)

df <- c()
for (i in 1:length(all)){
data <- all_json[4]$Results$series$data[[i]]
series_num <- all_json[4]$Results$series$seriesID[i]
data2 <- data %>%
  mutate(series_id = series_num)
df <- rbind(df, data2)
}
saveRDS(df, paste0("data/LAUS_", today(), ".RDS"))

} else {
  
  df <- readRDS(paste0("data/", last_pulled_file))
  
}
  
ICRIowaData <- df %>% 
  filter(grepl("CN19011|CN19031|CN19095|CN19103|CN19105|CN19113|CN19183", series_id) &
           period != "M13") %>%
  mutate(county = case_when(
    grepl("19011", series_id) ~ "Benton",
    grepl("19031", series_id) ~ "Cedar",
    grepl("19095", series_id) ~ "Iowa",
    grepl("19103", series_id) ~ "Johnson",
    grepl("19105", series_id) ~ "Jones",
    grepl("19113", series_id) ~ "Linn",
    grepl("19183", series_id) ~ "Washington"
  )) %>% 
  mutate(FIPS = str_sub(series_id, start = 6, end = 10)
  ) %>% 
  mutate(metric_name = case_when(
    grepl("6$", series_id) ~ "Labor Force",
    grepl("5$", series_id) ~ "Employment",
    grepl("4$", series_id) ~ "Unemployment"
  )) %>% 
  dateCast() %>% 
  mutate(date = ymd(date),
         value = as.integer(value))

LF_ICRIowa <- ICRIowaData %>% 
  filter(grepl("6$", series_id)) %>%
  select(-footnotes, -period, -year)

Emp_ICRIowa <- ICRIowaData %>% 
  filter(grepl("5$", series_id)) %>%
  select(-footnotes, -period, -year)

Unemp_ICRIowa <- ICRIowaData %>% 
  filter(grepl("4$", series_id)) %>%
  select(-footnotes, -period, -year)

mostRecent <- ICRIowaData %>% select(date) %>% pull() %>% max()
leastRecent <- ICRIowaData %>% select(date) %>% pull() %>% min()


ICRIowaXTS <- xts(ICRIowaData,
                  order.by = as.Date(ICRIowaData$date))

# https://community.rstudio.com/t/r-shiny-make-a-reactive-map-of-state-by-county/63224
IAcounties <- tigris::counties(state = "IA", 
                               keep_zipped_shapefile = TRUE,
                               refresh = FALSE)
ICRcounties <- IAcounties %>% 
  filter(NAME %in% c("Benton", "Linn", "Jones", "Washington", "Johnson", "Iowa", "Cedar"))

# LAUSSchedule <- read_html("https://www.bls.gov/lau/")
# reprogram with table from https://www.bls.gov/schedule/news_release/metro.htm
# longerLAUSSchedule <- read_html("https://www.bls.gov/schedule/news_release/metro.htm")
# https://atfutures.github.io/calendar/

nextReleaseSchedule <- read_html("LAUSschedule.html") %>% 
  # html_element(css = "#bodytext > div.highlight-box-green > ul") %>% 
  html_text() %>% 
  str_replace_all("[\n]|[\r]", " ")

# I can't scrape the page 
# fullReleaseSchedule <- longerLAUSSchedule %>% 
#   html_element(css = "#bodytext > table") %>%
fullReleaseSchedule <- read_html("longerLAUSschedule.html") %>% 
  html_table() %>%
  as.data.frame() %>% 
  mutate(
    `Reference Month` = lubridate::my(`Reference.Month`),
    `Release Date` = lubridate::mdy(`Release.Date`)
    ) %>% 
  filter(`Release Date` >= lubridate::today() - 30 & `Release Date` < lubridate::today() + 60) %>% 
  mutate(
    `Reference Month` = strftime(`Reference Month`, format = "%b %Y"),
    `Release Date` = strftime(`Release Date`, format = "%m/%d/%Y")
  )

