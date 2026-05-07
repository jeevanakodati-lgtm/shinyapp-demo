library(haven)

# Get all XPT files
xpt_files <- list.files(
  "./data/adam",
  pattern = "\\.xpt$",
  full.names = TRUE
)

# Read all datasets into a named list
adam_data <- lapply(xpt_files, haven::read_xpt)

# Use file names as list names
names(adam_data) <- tools::file_path_sans_ext(
  basename(xpt_files)
)

# Save all datasets into one RDS
saveRDS(
  adam_data,
  "./inst/extdata/adam.rds"
)


#####
adam = readRDS("./inst/extdata/adam.rds")
adsl = adam$adsl
adtte = adam$adtte
advs = adam$advs
