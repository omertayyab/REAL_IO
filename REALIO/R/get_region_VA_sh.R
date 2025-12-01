#' @import tidyverse
#' @param data_dir is data directory with ICIO files with default names
#' @param years yers for which the analysis is required
#' @param region_1 vector of 3-letter iso codes for region comprising importing countries
#' @param sector is optional to use function for sector-specific imports

get_region_VA_sh <- function(data_dir, years, region_1, sector = NULL)
{
  main_func <- function(datalist, region_1) {

    VA_region <- datalist$icio %>%
      select(contains(region_1)) %>%
      slice((n()-1)) %>%
      sum()

    VA_total <- datalist$icio %>%
      select(-V1) %>%
      slice((n()-1)) %>%
      sum()

    X_region <- datalist$icio %>%
      select(contains(region_1)) %>%
      slice(n()) %>%
      sum()

    X_total <- datalist$icio %>%
      select(-V1) %>%
      slice(n()) %>%
      sum()

    VA_X_tib <- tibble(VA_region = VA_region,
                       X_region = X_region,
                       VA_all = VA_total,
                       X_all = X_total
                       ) %>%
      mutate(VA_sh = VA_region/VA_all,
             X_sh = X_region/X_all) %>%
      mutate(VA_X_diff = VA_sh - X_sh)

    ##### If sector is specified

    if(!is.null(sector))
    {
    }

    else  return(VA_X_tib )

  }

  dr_by_yr <- list(0) #initializing list for storing tibbles with imp ratios

  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    dr_by_yr[[i]] <- main_func(datalist, region_1) %>%
      add_column(year = rep(years[i], length(source))) %>%
      relocate(year)
  }

  return(bind_rows(dr_by_yr))


}
