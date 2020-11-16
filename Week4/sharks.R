# loading the needed libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

#initializing variables
key <- "SUBSTITUTE API KEY"
url1 <- paste("http://api.digitalnz.org/v3/records.json?api_key=", key, sep = "")
query1 <- "&text=shark+attack&and[category][]=Newspapers&fields=id&per_page=1&page=1"
query2 <- "&text=shark+attack&and[category][]=Newspapers&fields=id,publisher,published_date&per_page=100&page="

# finding out, how many API calls we need
firstCall <- GET(paste(url1, query1, sep = "")) %>% content()
number_calls <- ceiling(firstCall$search$result_count / 100)   # 49 API calls
api_calls <- paste(url1, query2, 1:number_calls, sep="")

# testing
data <- fromJSON(api_calls[1])
results <- data$search$results
ID <- results$id
# This statistics doesn't need the title, leaving it out from the API
# call spares time and bandwidth
# Title <- results$title
 
#The publisher field comes in List form, unlisting it
Publisher <- unlist(results$publisher)
# also cleaning off the extra information in 'Date', which normally looks like this
# "1937-11-02T00:00:00.000Z". The result will be 1937-11-02, plus converting it from
# type "character" to type "double" via the ymd function"
Date <- ymd(unlist(results$published_date))
# then creating a new field of data, that contains only the months
Month <- sub("[0-9]{4}-", "", sub("-[0-9]{2}$", "", Date))
TestData <- tibble(ID, Title, Publisher, Date, Month)

TestData$Date <- sub("T.*", "", TestData$Date[])

# adding it to TestData
TestData$Month <- Month


#Plotting at the end

TibbleFromCall <- function(x) {
  data <- fromJSON(x)
  results <- data$search$results
  ID <- results$id
  Publisher <- unlist(results$publisher)
  Date <- ymd(sub("T.*", "", unlist(results$published_date)))
  Month <- sub("[0-9]{4}-", "", sub("-[0-9]{2}$", "", Date))
  TestData <- tibble(ID,Publisher,Date,Month)
  return(TestData)
}
# putting it all together
all.data <- tibble(ID=integer(),
                   Publisher=character(),
                   Date=double(),
                   Month=character()
)
for (i in api_calls) {
  all.data <- rbind(all.data, TibbleFromCall(i))
}

# plotting it by months
ggplot(all.data) +
  aes(x = Month, fill = Publisher) +
  geom_bar() +
  scale_fill_hue() +
  theme_gray()

#plotting it by years
ggplot(all.data) +
  aes(x = Date, fill = Publisher) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal()
