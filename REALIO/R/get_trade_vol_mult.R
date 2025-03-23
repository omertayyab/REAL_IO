#' @import tidyverse
#' @param years is vector of years
#' @param cntrys vector of 3-letter country codes as used in relevant dataset

get_trade_vol_mult <- function(years, cntrys){


  get_trade_vol <- function(datalist, cntry_1) {

    imp <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1, negate=TRUE)) %>%
      slice(1:(n()-3)) %>%
      select(-1) %>%
      sum

    exp <- datalist$icio %>%
      filter(str_detect(V1, cntry_1)) %>%
      select(!starts_with(c(cntry_1,"OUT"))) %>%
      select(-1) %>%
      sum

    r1 <- exp + imp

    return(r1)

  }

  vol_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
  vol_by_cntry <- as.vector(0) #initializing vector for storing dep ratios


  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    for(j in 1:length(cntrys)) {
      vol_by_cntry[j] <- get_trade_vol(datalist, cntrys[j])
    }
    vol_by_yr[[i]] <- tibble(country = cntrys,
                             year = rep(years[i], length(cntrys)),
                             trade_vol = vol_by_cntry )
  }

  return(bind_rows(vol_by_yr))


}
