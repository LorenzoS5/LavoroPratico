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

# Color list
my_colors <- c(
  "#1F77B4", "#D62728", "#2CA02C",
  "#FF7F0E", "#9467BD", "#8C564B"
)

# Plot monthly returns
jpeg("img/monthly_returns_plot.jpg", width = 1000, height = 1000)
plot(monthly_returns_df$Date, monthly_returns_df[[2]],
  main = "Monthly Returns",
  xlab = "Date", ylab = "Monthly Returns (%)",
  xlim = c(min(monthly_returns_df$Date), max(monthly_returns_df$Date)),
  ylim = c(min(monthly_returns_df[, -1], na.rm = TRUE),
    max(monthly_returns_df[, -1], na.rm = TRUE)
  ),
  type = "l", col = my_colors[2], lwd = 2

)
for (i in 3:ncol(monthly_returns_df)) {
  lines(monthly_returns_df$Date, monthly_returns_df[[i]],
    col = my_colors[i], lwd = 2
  )
}
legend("topright",
  legend = names(monthly_returns_df)[-1],
  col = 2:ncol(monthly_returns_df), lty = 1
)
dev.off()

# Plot annual returns
jpeg("img/annual_returns_plot.jpg", width = 1000, height = 1000)
plot(annual_returns_df$Date, annual_returns_df[[2]],
  main = "Annual Returns",
  xlab = "Date", ylab = "Annual Returns (%)",
  xlim = c(min(annual_returns_df$Date), max(annual_returns_df$Date)),
  ylim = c(min(annual_returns_df[, -1], na.rm = TRUE),
    max(annual_returns_df[, -1], na.rm = TRUE)
  ),
  type = "l", col = my_colors[2], lwd = 2
)
for (i in 3:ncol(annual_returns_df)) {
  lines(annual_returns_df$Date, annual_returns_df[[i]],
    col = my_colors[i], lwd = 2
  )
}
legend("topright",
  legend = names(annual_returns_df)[-1],
  col = 2:ncol(annual_returns_df), lty = 1
)
dev.off()


# Correlation scatter plots

# ACWI correlations plot
my_pch <- 19
jpeg("img/ACWI_Correlations.jpg", width = 2000, height = 2000)
plot(monthly_returns_df$"ACWI", monthly_returns_df$"ACWI ex USA",
  main = "ACWI correlations",
  cex.main = 3, cex.lab = 2, cex.axis = 2,
  xlab = "ACWI % variation", ylab = "",
  col = my_colors[1], pch = my_pch, cex = 3
)
points(monthly_returns_df$"ACWI", monthly_returns_df$"EM",
  col = my_colors[2], pch = my_pch, cex = 3
)
points(monthly_returns_df$"ACWI", monthly_returns_df$"EM ex CHINA",
  col = my_colors[3], pch = my_pch, cex = 3
)
points(monthly_returns_df$"ACWI", monthly_returns_df$"WORLD",
  col = my_colors[4], pch = my_pch, cex = 3
)
legend("topright",
  legend = c("ACWI ex USA", "EM", "EM ex CHINA", "WORLD"),
  col = my_colors[1:4], pch = my_pch,
  cex = 3, pt.cex = 3
)
dev.off()


# World vs EM correlations plot
jpeg("img/WorldVSEM.jpg", width = 2000, height = 2000)
plot(monthly_returns_df$"WORLD", monthly_returns_df$"EM",
  main = "WORLD vs EM correlations",
  cex.main = 3, cex.lab = 1.5, cex.axis = 2,
  xlab = "WORLD % variation",
  ylab = "EM % variation",
  col = my_colors[5], pch = my_pch, cex = 3
)
dev.off()
