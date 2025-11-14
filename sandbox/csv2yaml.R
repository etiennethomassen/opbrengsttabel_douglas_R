library(readr)
library(dplyr)
library(yaml)

# Load your CSV
params_df <- read_csv("inst/constants.csv")

# Create a named list from param names and a scenario column
param_list <- params_df %>%
  select(name, fb) %>%
  deframe()  # OR use setNames() below if deframe fails

param_list <- setNames(params_df$none, params_df$name)

write_yaml(as.list(param_list), "sandbox/parameters.yaml")

