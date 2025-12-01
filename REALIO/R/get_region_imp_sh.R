#' @import tidyverse
#' @param data_dir is data directory with ICIO files with default names
#' @param years yers for which the analysis is required
#' @param region_1 vector of 3-letter iso codes for region comprising importing countries
#' @param region_2 vector of 3-letter iso codes for region comprising exporting countries
#' @param ctrys_sp vector of 3-letter iso codes for countries in region 1 whose shares need to be separately reported
#' @param sector is optional to use function for sector-specific imports

get_region_imp_sh <- function(data_dir, years, region_1,region_2, ctrys_sp, sector = NULL)
{
  main_func <- function(datalist, region_1, region_2, ctrys_sp) {

    n_ind <- 45

    region_1_data <- lapply(region_1, function(x)
    {
      datalist$icio %>%
        slice(1:(n()-2)) %>%
        select(1, starts_with(x)) %>%
        filter(!str_detect(V1, x)) %>%
        rename_with(~gsub("^.{0,4}","",.), contains("_"))
    }) %>%
      bind_rows()


    imp_all <- region_1_data %>%
      #group_by(V1) %>%
      #summarize(across(everything(), sum)) %>%
      #ungroup %>%
      select(2:last_col()) %>%
      sum()

    imp_region_2 <- region_1_data %>%
      filter(str_detect(V1, paste(region_2, collapse = "|"))) %>%
      select(2:last_col()) %>%
      sum()

    imp_within <- sapply(ctrys_sp, function(x){
      region_1_data %>%
      filter(str_detect(V1, x)) %>%
      select(2:last_col()) %>%
      sum()
    }
    ) %>%
      unname %>%
      tibble(source = ctrys_sp, imp_ratio = .) %>%
      add_row(source = "region_2" , imp_ratio = imp_region_2)

    within_rest = imp_all - sum(imp_within$imp_ratio)

     imp_return <- imp_within %>%
       add_row(source = "within_rest", imp_ratio = within_rest ) %>%
       mutate(imp_ratio = imp_ratio/imp_all)




    ##### If sector is specified

    if(!is.null(sector))
    {
    }

    else  return(imp_return )

  }

  dr_by_yr <- list(0) #initializing list for storing tibbles with imp ratios

  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    dr_by_yr[[i]] <- main_func(datalist, region_1, region_2, ctrys_sp) %>%
      add_column(year = rep(years[i], length(source))) %>%
      relocate(year)
  }

  return(bind_rows(dr_by_yr))


}
