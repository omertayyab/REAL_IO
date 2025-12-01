#' @import tidyverse
#' @param years is vector of years
#' @param data_dir directory where datasets are located
#' @param cntrys vector of 3-letter country codes as used in relevant dataset
#' @param region_1 is the region of interest from which imports originate

get_region_imp_ratio <- function(data_dir, years, region_1, cntrys) {

  main_func <- function(datalist, cntry_1)
  {

    n_ind <- 45

    imp_all <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1, negate = TRUE)) %>%
      slice(1:(n()-3)) %>% #removing totals at the bottom of the table
      mutate(imp_all = rowSums(across(2:last_col())), .keep = "none") %>%
      sum

    imp_region <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1, negate = TRUE)) %>%
      slice(1:(n()-3)) %>% #removing totals at the bottom of the table
      filter(str_detect(V1, paste0(region_1, collapse = "|"))) %>%
      mutate(imp_all = rowSums(across(2:last_col())), .keep = "none") %>%
      sum

    return(tibble(country = cntry_1, reg_imp_ratio = imp_region/imp_all))
  }

  dr_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
  cntrys <- cntrys[!(cntrys %in% region_1)] # filtering out countries if they present in both sets

  for(i in 1:length(years)){
    x <- list(0)
    datalist <- load_data(data_dir, years[i])
    for(j in 1:length(cntrys)) {
      x[[j]] <- main_func(datalist, cntrys[j])
    }
    x <- x %>% bind_rows
    dr_by_yr[[i]] <-  x %>%
      add_column(year = rep(years[i], dim(x)[1]))
  }

  return(bind_rows(dr_by_yr) %>%
           relocate(country, year) %>%
           arrange(country, year)
  )

}
