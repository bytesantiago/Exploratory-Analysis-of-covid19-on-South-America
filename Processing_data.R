my_data <- as.data.frame(owid.covid.data)

data[is.na(data)] <- 0

my_data$date <- as.Date(my_data$date, format= "%Y-%m-%d")

new_data <- subset(my_data, date > "2020-06-18")

