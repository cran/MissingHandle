#' @title Fill Missing Dates and Combine Data into a Data Frame
#' @description Many times, you will not find data for all dates. After first January, 2011 you may have next data on 20th January, 2011 and so on. Also available dates may have zero values. Try to gather all such kinds of data in different excel sheets of a single excel file. Every sheet will contain two columns (1st one is dates and second one is the data). Load every sheet to separate elements of a list. Using this you can fill the gaps for all the sheets and mark all the corresponding values as zeros. Here I am talking about daily data. Finally, it will combine all the filled results into one data frame (first column is date and other columns will be corresponding values of your sheets) and give one csv file. Number of columns in the data frame will be number of sheets plus one.
#' @param my_list List of elements containing two columns each. First column is data which may have missing dates and second column is corresponding time series values.
#' @param starting_date From which date data is needed
#' @param ending_date Upto which date data is needed
#' @param date_format Specify the date format of your data
#' @import zoo
#' @return
#' \itemize{
#'   \item clean_and_combined_df: Data frame of combined data containing multiple columns. First column is complete dates and others are corresponding values of second column of every element of input list. Missing values are denoted as zeros.
#'   }
#' @export
#' @examples
#'# creating example ####
#'
#'# 1st element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-03-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df <- data.frame(Dates = dates, Price_a = price_1)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-05-01"), as.Date("2011-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df2 <- data.frame(Dates = dates2, Price_a = price_2)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df2)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-02-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df3 <- data.frame(Dates = dates3, Price_a = price_3)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df3)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-04-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df4 <- data.frame(Dates = dates4, Price_a = price_4)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df4)
#'
#'# Specify column data types
#'df <- data.frame(Dates = as.Date(df$Dates),
#'                 price_a = round(as.numeric(df$Price_a)))
#'# 2nd element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-05-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second <- data.frame(Dates = dates, Price_b = price_1)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-06-01"), as.Date("2011-10-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second2 <- data.frame(Dates = dates2, Price_b = price_2)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second2)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second3 <- data.frame(Dates = dates3, Price_b = price_3)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second3)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-03-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second4 <- data.frame(Dates = dates4, Price_b = price_4)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second4)
#'
#'# Specify column data types
#'df_second <- data.frame(Dates = as.Date(df_second$Dates),
#'                        price_b = round(as.numeric(df_second$Price_b)))
#'
#'# my_list ####
#'# Create a list
#'my_list <- list()
#'
#'# Add the data frame to the list
#'my_list$df <- df
#'
#'my_list$df_second <- df_second
#'
#'# getting output ####
#'my_combined_data <- clean_and_combine(my_list = my_list)
#'print(head(my_combined_data))
#' @references
#' \itemize{
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   }
clean_and_combine <- function(my_list,
                              starting_date = as.Date("2011-01-01"),
                              ending_date = as.Date("2022-12-31"),
                              date_format = "%d-%m-%y") {

  # create an empty list to store the cleaned data frames
  data_complete_list <- list()

  # loop through each sheet and apply the cleaning code
  for (e in seq_along(my_list)) {

    # Read in the sheet as a data frame
    data <- my_list[[e]]

    # Convert the date column to a Date object
    data_date <- data[,1]

    data2 <- data.frame(data_date, data[,2])
    #####
    # Create a time series object with a complete sequence of dates
    complete_dates <- seq(from = starting_date, to = ending_date, by = "day")

    data_date <- as.Date(data2[,1], format = date_format)
    # Find the indices of matching dates in data$Date
    matching_indices <- match(complete_dates, data_date)

    # Create a time series object with the matched data
    ts_data <- zoo(data2[,2][matching_indices], order.by = complete_dates)

    # Convert the time series object to a data frame
    data_complete <- data.frame(index(ts_data), coredata(ts_data))

    # Replace any remaining missing values with 0
    data_complete[is.na(data_complete)] <- 0

    # set column names
    colnames(data_complete) <- colnames(data)

    # add the cleaned data frame to the list
    data_complete_list[[e]] <- data_complete
  }

  # extract second column of every element in the list
  extracted_cols <- lapply(data_complete_list, "[[", 2)

  # extract the first column of the first element of the list
  date_col <- as.character(data_complete_list[[1]][[1]])

  # combine extracted columns into a single data frame
  my_df <- do.call(cbind, extracted_cols)

  # extract the column names of every second column of each data frame
  second_colnames_list <- list()

  for (i in seq_along(data_complete_list)) {
    # extract the column names of every second column
    second_colnames <- colnames(data_complete_list[[i]])[seq(2, ncol(data_complete_list[[i]]), 2)]

    # add the column names to the second_colnames_list
    second_colnames_list[[i]] <- second_colnames
  }

  My_df <- data.frame(cbind(Dates = date_col, my_df))

  # loop over the names in second_colnames_list and assign them to my_df
  for (i in seq_along(second_colnames_list)) {
    names(My_df)[i+1] <- second_colnames_list[[i]]
  }

  clean_and_combined_df <- data.frame(My_df)
  # return the combined data frame
  return(clean_and_combined_df)
}
#' @title Fill Zeros as NA and Impute
#' @description Imputation will be done. It will assign dates from start date to end date in the specified format. Finally, imputed data will be provided.
#' @param My_df Data frame with 1st column as dates and others containing missing values denoted as zeros
#' @param method_impute Select imputation method from ImputeTS package
#' @import imputeTS
#' @return
#' \itemize{
#'   \item imputed_df: Data frame of combined imputed data
#'   }
#' @export
#' @examples
#'# creating example ####
#'
#'# 1st element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-03-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df <- data.frame(Dates = dates, Price_a = price_1)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-05-01"), as.Date("2011-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df2 <- data.frame(Dates = dates2, Price_a = price_2)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df2)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-02-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df3 <- data.frame(Dates = dates3, Price_a = price_3)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df3)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-04-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df4 <- data.frame(Dates = dates4, Price_a = price_4)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df4)
#'
#'# Specify column data types
#'df <- data.frame(Dates = as.Date(df$Dates),
#'                 price_a = round(as.numeric(df$Price_a)))
#'# 2nd element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-05-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second <- data.frame(Dates = dates, Price_b = price_1)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-06-01"), as.Date("2011-10-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second2 <- data.frame(Dates = dates2, Price_b = price_2)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second2)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second3 <- data.frame(Dates = dates3, Price_b = price_3)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second3)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-03-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second4 <- data.frame(Dates = dates4, Price_b = price_4)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second4)
#'
#'# Specify column data types
#'df_second <- data.frame(Dates = as.Date(df_second$Dates),
#'                        price_b = round(as.numeric(df_second$Price_b)))
#'
#'# my_list ####
#'# Create a list
#'my_list <- list()
#'
#'# Add the data frame to the list
#'my_list$df <- df
#'
#'my_list$df_second <- df_second
#'
#'# getting output ####
#'my_combined_data <- clean_and_combine(my_list = my_list)
#'print(head(my_combined_data))
#'my_imputed_data <- impute_combined(my_combined_data)
#'print(head(my_imputed_data))
#' @references
#' \itemize{
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   }
impute_combined <- function(My_df,
                            method_impute = na_kalman) {

  # Replace zeros with NA
  My_df[My_df == 0] <- NA
  suppressWarnings({
    for (i in 2:ncol(My_df)) {
      x <- as.numeric(My_df[,i])
      x.withoutNA <- round(method_impute(x), 2)
      My_df[,i] <- x.withoutNA
    }
  })
  imputed_df <- data.frame(My_df)
  return(imputed_df)
}
#' @title Convert Daily Data to Weekly
#' @description Converts daily data to weekly data. One needs to specify the week format.
#' @param my_daily_data A data frame containing first column as dates and others are columns contains daily data
#' @param starting_date From which date data is present
#' @param ending_date Upto which date data is present
#' @param year_week_format specify the year week format
#' @param week_ending_format specify week ending format
#' @param week_ending_day corresponding days of a week 7 or 6 days
#' @param year_week this is a variable, leave this as it is
#' @param week_ending_date name of the first column of the output data frame
#' @import dplyr
#' @return
#' \itemize{
#'   \item my_weekly_data: Data frame containing converted data into weekly one
#'   }
#' @export
#' @examples
#' # creating example ####
#'
#'# 1st element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-03-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df <- data.frame(Dates = dates, Price_a = price_1)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-05-01"), as.Date("2011-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df2 <- data.frame(Dates = dates2, Price_a = price_2)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df2)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-02-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df3 <- data.frame(Dates = dates3, Price_a = price_3)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df3)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-04-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df4 <- data.frame(Dates = dates4, Price_a = price_4)
#'
#'# Merge the two data frames row-wise
#'df <- rbind(df, df4)
#'
#'# Specify column data types
#'df <- data.frame(Dates = as.Date(df$Dates),
#'                 price_a = round(as.numeric(df$Price_a)))
#'# 2nd element ####
#'# Create a sequence of dates from "2011-01-01" to "2015-12-31"
#'dates <- seq(as.Date("2011-01-01"), as.Date("2011-05-31"), by="day")
#'
#'# Generate random prices for each date
#'price_1 <- runif(length(dates), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second <- data.frame(Dates = dates, Price_b = price_1)
#'
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates2 <- seq(as.Date("2011-06-01"), as.Date("2011-10-31"), by="day")
#'
#'# Generate random prices for each date
#'price_2 <- runif(length(dates2), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second2 <- data.frame(Dates = dates2, Price_b = price_2)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second2)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates3 <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_3 <- runif(length(dates3), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second3 <- data.frame(Dates = dates3, Price_b = price_3)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second3)
#'
#'# Create a sequence of dates from "2016-02-01" to "2022-12-31"
#'dates4 <- seq(as.Date("2013-03-01"), as.Date("2022-12-31"), by="day")
#'
#'# Generate random prices for each date
#'price_4 <- runif(length(dates4), min=0, max=100)
#'
#'# Combine the dates and prices into a data frame
#'df_second4 <- data.frame(Dates = dates4, Price_b = price_4)
#'
#'# Merge the two data frames row-wise
#'df_second <- rbind(df_second, df_second4)
#'
#'# Specify column data types
#'df_second <- data.frame(Dates = as.Date(df_second$Dates),
#'                        price_b = round(as.numeric(df_second$Price_b)))
#'
#'# my_list ####
#'# Create a list
#'my_list <- list()
#'
#'# Add the data frame to the list
#'my_list$df <- df
#'
#'my_list$df_second <- df_second
#'
#'# getting output ####
#'my_combined_data <- clean_and_combine(my_list = my_list)
#'print(head(my_combined_data))
#'my_imputed_data <- impute_combined(my_combined_data)
#'print(head(my_imputed_data))
#'my_weekly_data <- weekly_from_daily(my_imputed_data)
#'print(head(my_weekly_data))
#' @references
#' \itemize{
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   }
weekly_from_daily <- function(my_daily_data,
                              starting_date = "2011-01-01",
                              ending_date = "2022-12-31",
                              year_week_format = "%Y-%W",
                              week_ending_format = "%Y-%W-%u",
                              week_ending_day = "-7",
                              year_week = "year_week",
                              week_ending_date = "week_ending_date") {
  df <- data.frame(my_daily_data[,-1])
  # create sequence of dates
  dates <- seq(as.Date(starting_date), as.Date(ending_date), by = "day")
  # add dates to data frame
  daily_data <- data.frame(dates, df)
  daily_data$date <- dates

  # get column names
  col_names <- colnames(df)

  # select first and last columns
  first_col <- col_names[1]
  last_col <- col_names[length(col_names)]

  # create weekly data by taking mean of each price series
  weekly_data <- daily_data %>%
    group_by(year_week = format(date, year_week_format)) %>%
    summarize(across({{first_col}}:{{last_col}}, mean))

  # add week ending date to weekly data
  weekly_data$week_ending_date <- as.Date(paste0(weekly_data$year_week, week_ending_day), format = week_ending_format)

  # summarize weekly data
  weekly_data_summarized <- weekly_data %>%
    select(-year_week) %>%
    group_by(week_ending_date) %>%
    summarize(across({{first_col}}:{{last_col}}, sum))

  # subset weekly data frame by price series columns
  weekly_data <- weekly_data_summarized[, c(week_ending_date, col_names)]
  weekly_data <- weekly_data[-1,]

  my_weekly_data <- as.data.frame(weekly_data)
  # return weekly prices data frame
  return(my_weekly_data)
}
