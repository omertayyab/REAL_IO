#' @import tidyverse
#' @param datalist is list loaded from load_data
#' @param cntry_1 is iso3 code of country of interest



get_trade_bal <- function(datalist, cntry_1) {

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

  r1 <- exp - imp

  return(r1)

}
