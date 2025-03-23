#' @import tidyverse
#' @param years is vector of years
#' @param cntrys vector of 3-letter country codes as used in relevant dataset

get_trade_bal_mult <- function(years, cntrys){

  bal_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
  bal_by_cntry <- as.vector(0) #initializing vector for storing dep ratios


  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    for(j in 1:length(cntrys)) {
      bal_by_cntry[j] <- get_trade_bal(datalist, cntrys[j])
    }
    bal_by_yr[[i]] <- tibble(country = cntrys,
                            year = rep(years[i], length(cntrys)),
                            trade_bal = bal_by_cntry )
  }

  return(bind_rows(bal_by_yr))


}
