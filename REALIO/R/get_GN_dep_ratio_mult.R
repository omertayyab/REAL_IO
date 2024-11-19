#' @import tidyverse
#' @param years is vector of years
#' @param cntrys vector of 3-letter country codes as used in relevant dataset

get_GN_dep_ratio_mult <- function(years, cntrys) {

  dr_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
  dr_by_cntry <- as.vector(0) #initializing vector for storing dep ratios


  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    for(j in 1:length(cntrys)) {
      t2 <- icio(datalist, cntrys[j])$t2
      dr_by_cntry[j] <- get_GN_dep_ratio(t2)
    }
    dr_by_yr[[i]] <- tibble(country = cntrys,
                          year = rep(years[i], length(cntrys)),
                          GN_dep_ratio = dr_by_cntry )
  }

  return(bind_rows(dr_by_yr))
}
