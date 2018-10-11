
rivers_model <- readr::read_csv("data-raw/rivers.csv")

devtools::use_data(rivers_model, overwrite = TRUE)

