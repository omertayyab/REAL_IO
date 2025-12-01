#' @import tidyverse
#' @import readr
#' @import here
#' @param data_dir is the directory where L_mats are stored as RDS and concordance file as csv
#' @param datalist is the output of load_data function
#' @param cntrys is the vector of 3-letter ISO code of the countries of interest i.e. importers
#' @param region_1 is the vector of 3 letter ISO codes for countries which are exporter



get_region_CFR <- function(data_dir, years, cntrys, region_1){

  main_func <- function(cntry_1) {

    n_ind <- 45

    ctry_FD <- icio %>%
      select(V1,contains(cntry_1)) %>%
      select(V1, !(2: (n_ind + 1) )) %>%
      filter(V1 != "TLS", V1 != "OUT", V1 != "VA") %>%
      mutate(FD = rowSums(across(where(is.numeric))), .keep = "unused") %>%
      select(FD) %>%
      as.matrix

    all_GO <- L_mat %*% ctry_FD %>%
      as_tibble %>%
      add_column(V1 = row.names(L_mat)) %>%
      relocate(V1) %>%
      rename(GO = FD)

    ctry_GO <- all_GO %>%
      filter(str_detect(V1,cntry_1)) %>%
      mutate(V1 = conc$DLS) %>%
      group_by(V1) %>%
      summarize(c_GO = sum(GO))

    other_GO <- all_GO %>%
      filter(str_detect(V1,cntry_1, negate = TRUE)) %>%
      mutate(V1 = str_replace(V1, "^.{4}","")) %>%
      group_by(V1) %>%
      summarize(GO = sum(GO)) %>%
      ungroup %>%
      mutate(V1 = conc$DLS) %>%
      group_by(V1) %>%
      summarize(all_else_GO = sum(GO))

    region_GO <- all_GO %>%
      filter(str_detect(V1, paste0(region_1, collapse = "|"))) %>%
      mutate(V1 = str_replace(V1, "^.{4}","")) %>%
      group_by(V1) %>%
      summarize(GO = sum(GO)) %>%
      ungroup %>%
      mutate(V1 = conc$DLS) %>%
      group_by(V1) %>%
      summarize(r_GO = sum(GO))


    CFR_ret <- ctry_GO %>%
      left_join(., other_GO, by = "V1") %>%
      left_join(., region_GO, by = "V1") %>%
      rename(sector = V1) %>%
      mutate(CFR_all = all_else_GO/c_GO, CFR_region =  r_GO/c_GO) %>%
      add_column(country = cntry_1) %>%
      relocate(country)
#      arrange(desc(FIR_all))

    return(CFR_ret)

  }

  CFR_by_yr <- list(0)
  CFR_by_cntry <- list(0)
  conc <- read_csv(paste0(data_dir,"/","ICIO_BEC_conc.csv"), show_col_types =  FALSE) %>%
    select(1:4) %>%
    na.omit

  for(i in 1:length(years)){
    L_mat <- readRDS(paste0(data_dir,"/L_mat_", years[i],".rds"))
    icio <- read_csv(paste0(data_dir,"/",years[i],"_SML.csv"), show_col_types =  FALSE)

    for(j in 1:length(cntrys)) {
      CFR_by_cntry[[j]] <- main_func(cntrys[j])
    }
    CFR_by_yr[[i]] <- bind_rows(CFR_by_cntry) %>%
      add_column(year = rep(years[i], length(cntrys) * length(unique(conc$DLS)))) %>%
      relocate(year)
  }

  return(bind_rows(CFR_by_yr))

}
