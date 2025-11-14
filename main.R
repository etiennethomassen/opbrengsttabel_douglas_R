message("Running yield model at ", Sys.time())
devtools::load_all()
cat("🚀 Running model...\n")
options(scipen = 999) # do not use scientific notation (2.23e-13)
source("inst/model_runner.R")
message("Done.")

