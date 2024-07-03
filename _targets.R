library(targets)
library(tarchetypes)

source("functions.R")

tar_option_set(packages = c("readr", "ggplot2", "dplyr", "mgcv"))

list(
  tar_target(
    raw_data,
    get_data("bltper.csv")
  ),
  tar_target(missing_values, check_missing_values(raw_data)),
  tar_target(data_types, check_data_types(raw_data)),
  tar_target(
    life_expectancy_plot,
    plot_life_expectancy(raw_data)
  ),
  tar_target(
    mortality_plot,
    plot_mortality_rate(raw_data)
  ),
  tar_target(
    gam_summary,
    fit_gam_summary(raw_data)
  ),
  tar_target(
    gam_plot_path,
    save_gam_plot(raw_data, "gam_model_plot.png"),
    format = "file"
  ),
  tar_quarto(report, 
             "assignment4_29796431.qmd")
)