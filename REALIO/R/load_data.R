#' @import tidyverse
#' @import readr

load_data <- function(data_dir, year) {

  file1 <- paste0(data_dir,"/",as.character(year),"_SML.csv")
  file2 <- paste0(data_dir,"/","ICIO_BEC_conc.csv")

  icio <- read_csv(file1, show_col_types =  FALSE)
  conc <- read_csv(file2, show_col_types =  FALSE) %>%
    select(1:4) %>%
    na.omit

  cntry_list <- read_csv(file2, show_col_types =  FALSE) %>% select(7:9)

  return(list(
    icio = icio,
    conc = conc,
    cntry_list = cntry_list,
    year = year
  ))

}
