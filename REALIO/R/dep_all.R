#' @import tidyverse
#' @param years is vector of years
#' @param cntrys vector of 3-letter country codes as used in relevant dataset

data_dir <- "C:/R_OT/Projects/IO_to_DLS/Data/ICIO"

years <- seq(1995, 2020, 5)

countries <- read_csv(paste0(data_dir, "/ICIO_BEC_conc.csv")) %>%
  select(c(Code, GS_GN)) %>%
  filter(GS_GN == "GS")

dep_all <- get_GN_dep_ratio_mult(years, countries$Code) %>%
  filter(!is.nan(GN_dep_ratio))


dep_sec_all <- get_GN_sec_dep_ratio_mult(years, countries$Code, "Energy") %>%
  filter(!is.nan(GN_dep_ratio))
