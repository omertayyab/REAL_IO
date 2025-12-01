#' @import tidyverse
#' @import readr
#' @import here
#' @param data_dir is directory of ICIO files
#' @param years is the vector of years for which reading has to be done


calc_L_mat <- function(data_dir, years){

  L_mat_main <- function(datalist) {
    A_global <- datalist$icio %>%
      mutate(across(where(is.numeric), ~./.[n()])) %>%
      mutate(across(everything(),~replace(.,is.nan(.),0))) %>%   #removing divide by zero errors
      slice(1:3465) %>%
      select(2:3466) %>%
      #    filter(!str_detect(V1,"_T")) %>%
      #    select(-V1, -contains("_T")) %>%
      as.matrix

    L_global <- (diag(dim(A_global)[1]) - A_global) %>%
      solve()

    return(L_global)
  }

L_mat_list <- list(0)

for (i in 1:length(years)) {
  L_mat_list[[i]] <- L_mat_main(load_data(data_dir, years[i]))
  }

return(L_mat_list)

}
