% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_handle.R
\name{weekly_from_daily}
\alias{weekly_from_daily}
\title{Convert Daily Data to Weekly}
\usage{
weekly_from_daily(
  my_daily_data,
  starting_date = "2011-01-01",
  ending_date = "2022-12-31",
  year_week_format = "\%Y-\%W",
  week_ending_format = "\%Y-\%W-\%u",
  week_ending_day = "-7",
  year_week = "year_week",
  week_ending_date = "week_ending_date"
)
}
\arguments{
\item{my_daily_data}{A data frame containing first column as dates and others are columns contains daily data}

\item{starting_date}{From which date data is present}

\item{ending_date}{Upto which date data is present}

\item{year_week_format}{specify the year week format}

\item{week_ending_format}{specify week ending format}

\item{week_ending_day}{corresponding days of a week 7 or 6 days}

\item{year_week}{this is a variable, leave this as it is}

\item{week_ending_date}{name of the first column of the output data frame}
}
\value{
\itemize{
  \item my_weekly_data: Data frame containing converted data into weekly one
  }
}
\description{
Converts daily data to weekly data. One needs to specify the week format.
}
\examples{
# creating example ####

# 1st element ####
# Create a sequence of dates from "2011-01-01" to "2015-12-31"
dates <- seq(as.Date("2011-01-01"), as.Date("2011-03-31"), by="day")

# Generate random prices for each date
price_1 <- runif(length(dates), min=0, max=100)

# Combine the dates and prices into a data frame
df <- data.frame(Dates = dates, Price_a = price_1)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates2 <- seq(as.Date("2011-05-01"), as.Date("2011-12-31"), by="day")

# Generate random prices for each date
price_2 <- runif(length(dates2), min=0, max=100)

# Combine the dates and prices into a data frame
df2 <- data.frame(Dates = dates2, Price_a = price_2)

# Merge the two data frames row-wise
df <- rbind(df, df2)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates3 <- seq(as.Date("2012-02-01"), as.Date("2012-12-31"), by="day")

# Generate random prices for each date
price_3 <- runif(length(dates3), min=0, max=100)

# Combine the dates and prices into a data frame
df3 <- data.frame(Dates = dates3, Price_a = price_3)

# Merge the two data frames row-wise
df <- rbind(df, df3)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates4 <- seq(as.Date("2013-04-01"), as.Date("2022-12-31"), by="day")

# Generate random prices for each date
price_4 <- runif(length(dates4), min=0, max=100)

# Combine the dates and prices into a data frame
df4 <- data.frame(Dates = dates4, Price_a = price_4)

# Merge the two data frames row-wise
df <- rbind(df, df4)

# Specify column data types
df <- data.frame(Dates = as.Date(df$Dates),
                price_a = round(as.numeric(df$Price_a)))
# 2nd element ####
# Create a sequence of dates from "2011-01-01" to "2015-12-31"
dates <- seq(as.Date("2011-01-01"), as.Date("2011-05-31"), by="day")

# Generate random prices for each date
price_1 <- runif(length(dates), min=0, max=100)

# Combine the dates and prices into a data frame
df_second <- data.frame(Dates = dates, Price_b = price_1)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates2 <- seq(as.Date("2011-06-01"), as.Date("2011-10-31"), by="day")

# Generate random prices for each date
price_2 <- runif(length(dates2), min=0, max=100)

# Combine the dates and prices into a data frame
df_second2 <- data.frame(Dates = dates2, Price_b = price_2)

# Merge the two data frames row-wise
df_second <- rbind(df_second, df_second2)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates3 <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="day")

# Generate random prices for each date
price_3 <- runif(length(dates3), min=0, max=100)

# Combine the dates and prices into a data frame
df_second3 <- data.frame(Dates = dates3, Price_b = price_3)

# Merge the two data frames row-wise
df_second <- rbind(df_second, df_second3)

# Create a sequence of dates from "2016-02-01" to "2022-12-31"
dates4 <- seq(as.Date("2013-03-01"), as.Date("2022-12-31"), by="day")

# Generate random prices for each date
price_4 <- runif(length(dates4), min=0, max=100)

# Combine the dates and prices into a data frame
df_second4 <- data.frame(Dates = dates4, Price_b = price_4)

# Merge the two data frames row-wise
df_second <- rbind(df_second, df_second4)

# Specify column data types
df_second <- data.frame(Dates = as.Date(df_second$Dates),
                       price_b = round(as.numeric(df_second$Price_b)))
# my_list ####
# Create a list
my_list <- list()

# Add the data frame to the list
my_list$df <- df

my_list$df_second <- df_second

# getting output ####
my_combined_data <- clean_and_combine(my_list = my_list)
print(head(my_combined_data))
my_imputed_data <- impute_combined(my_combined_data)
print(head(my_imputed_data))
my_weekly_data <- weekly_from_daily(my_imputed_data)
print(head(my_weekly_data))
}
\references{
\itemize{
  \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
  \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
  \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
  \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
  }
}
