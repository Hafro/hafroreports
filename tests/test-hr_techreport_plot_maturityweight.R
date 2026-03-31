library(hafroreports)

# Useless as example data goes, but demonstrates plots work
input_data <- expand.grid(year = 2000:2020, age = 3:10)
input_data$stock_weight <- rnorm(nrow(input_data), mean = 2500, sd = 100)
input_data$catch_weight <- rnorm(nrow(input_data), mean = 3000, sd = 100)
input_data$maturity <- runif(nrow(input_data))

hr_techreport_plot_maturityweight(input_data, "pred")
hr_techreport_plot_maturityweight(input_data, "period")
