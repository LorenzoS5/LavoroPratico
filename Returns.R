# Load required libraries
library(readxl)

# Read the Excel file
msci_data <- read_excel("MSCI-Indexes.xlsx")

# Functions to calculate monthly returns and Data Frame with the results
monthly_returns <- function(index) {
  returns <- numeric()
  for (i in seq_along(index)) {
    if (i == 1) {
      next
    }
    returns <- c(returns, 100 * (index[i] - index[i - 1]) / index[i - 1])
  }
  returns
}

calculate_monthly_returns <- function(data) {
  dates <- data$"Date"

  returns_df <- data.frame(Date = dates[-1])

  for (col in names(data)[-1]) {
    returns_df[[col]] <- monthly_returns(data[[col]])
  }

  returns_df
}


# Functions to calculate annual returns and Data Frame with the results
annual_returns <- function(index) {
  returns <- numeric()
  for (i in seq_along(index)) {
    if (i <= 12) {
      next  # Skip the first 12 months since there's no previous year to compare
    }
    returns <- c(returns, 100 * (index[i] - index[i - 12]) / index[i - 12])
  }
  returns
}

calculate_annual_returns <- function(data) {
  dates <- data$"Date"

  # Exclude the first 12 months from the Date column
  returns_df <- data.frame(Date = dates[-(1:12)])

  for (col in names(data)[-1]) {
    returns_df[[col]] <- annual_returns(data[[col]])
  }

  returns_df
}


monthly_returns_df <- calculate_monthly_returns(msci_data)
annual_returns_df <- calculate_annual_returns(msci_data)

# Plot monthly returns
jpeg("img/monthly_returns_plot.jpg", width = 1920, height = 1080)
plot(monthly_returns_df$Date, monthly_returns_df[[2]], type = "l", col = "blue",
     xlab = "Date", ylab = "Monthly Returns (%)", main = "Monthly Returns")
for (i in 3:ncol(monthly_returns_df)) {
  lines(monthly_returns_df$Date, monthly_returns_df[[i]], col = i)
}
legend("topright", legend = names(monthly_returns_df)[-1], col = 2:ncol(monthly_returns_df), lty = 1)
dev.off()

# Plot annual returns
jpeg("img/annual_returns_plot.jpg", width = 1920, height = 1080)
plot(annual_returns_df$Date, annual_returns_df[[2]], type = "l", col = "blue",
     xlab = "Date", ylab = "Annual Returns (%)", main = "Annual Returns")
for (i in 3:ncol(annual_returns_df)) {
  lines(annual_returns_df$Date, annual_returns_df[[i]], col = i)
}
legend("topright", legend = names(annual_returns_df)[-1], col = 2:ncol(annual_returns_df), lty = 1)
dev.off()

