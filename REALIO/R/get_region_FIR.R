#' @import tidyverse
#' @import readr
#' @import here
#' @param data_dir is the directory where L_mats are stored as RDS and concordance file as csv
#' @param datalist is the output of load_data function
#' @param cntrys is the vector of 3-letter ISO code of the countries of interest i.e. importers
#' @param region_1 is the vector of 3 letter ISO codes for countries which are exporter



get_region_FIR <- function(data_dir, years, cntrys, region_1){

      main_func <- function(cntry_1) {

          ctry_data <- L_mat %>%
          select(cs, contains(cntry_1)) %>%
          filter(!str_detect(cs,cntry_1))

        ctry_VA <- icio %>%
          filter(V1 == "VA")  %>%
          select(contains(cntry_1)) %>%
          pivot_longer(cols = everything(), names_to = "sector", values_to = "VA")

        ctry_FD <- icio %>%
          select(V1,contains(cntry_1)) %>%
          select(V1, contains(c("HFCE", "NPISH","GGFC", "GFCF", "INVNT", "DPABR"))) %>%
          filter(V1 != "TLS", V1 != "OUT", V1 != "VA") %>%
          mutate(V1 = str_remove(V1, "^.{4}")) %>%
          group_by(V1) %>%
          summarize(across(everything(), sum)) %>%
          mutate(V1 = str_replace(V1,".*", paste0(cntry_1,"_",V1))) %>%
          mutate(FD = rowSums(across(where(is.numeric))), .keep = "unused") %>%
          rename(sector = V1)

        ctry_GO <- icio %>%
          select(V1,contains("OUT")) %>%
          filter(V1 != "TLS", V1 != "OUT", V1 != "VA") %>%
          filter(str_detect(V1, cntry_1)) %>%
          rename(sector = V1, GO = OUT)


        ctry_FIR_sector <- ctry_data %>%
          summarize(across(2:last_col(),sum)) %>%
          pivot_longer(cols = everything(), names_to = "sector", values_to = "FIR_all") %>%
          left_join(., ctry_VA, join_by("sector" == "sector")) %>%
          left_join(., ctry_FD, join_by("sector" == "sector")) %>%
          left_join(.,ctry_GO, join_by("sector" == "sector"))

        ctry_FIR_region_sector <- ctry_data %>%
          filter(str_detect(cs, paste0(region_1, collapse = "|"))) %>%
          summarize(across(2:last_col(),sum)) %>%
          pivot_longer(cols = everything(), names_to = "sector", values_to = "FIR_region") %>%
          left_join(., ctry_VA, join_by("sector" == "sector")) %>%
          left_join(., ctry_FD, join_by("sector" == "sector")) %>%
          left_join(.,ctry_GO, join_by("sector" == "sector"))

        FIR_ret <- left_join(ctry_FIR_sector,
                             ctry_FIR_region_sector,
                             by = (c("sector", "VA", "FD", "GO"))) %>%
          mutate(sector = conc$DLS) %>%
          group_by(sector) %>%
          summarize(FIR_all = weighted.mean(FIR_all, GO),
                    FIR_region = weighted.mean(FIR_region,GO),
                    VA = sum(VA),
                    FD = sum(FD),
                    GO = sum(GO)
                    ) %>%
          add_column(country = cntry_1) %>%
          relocate(country) %>%
          arrange(desc(FIR_all))

        return(FIR_ret)

      }

      FIR_by_yr <- list(0)
      FIR_by_cntry <- list(0)
      conc <- read_csv(paste0(data_dir,"/","ICIO_BEC_conc.csv"), show_col_types =  FALSE) %>%
        select(1:4) %>%
        na.omit

      for(i in 1:length(years)){
        L_mat <- readRDS(paste0(data_dir,"/L_mat_", years[i],".rds"))
        icio <- read_csv(paste0(data_dir,"/",years[i],"_SML.csv"), show_col_types =  FALSE)

        sel_names <- row.names(L_mat)

        L_mat <- L_mat %>%
          as_tibble()

        colnames(L_mat) <- sel_names

        L_mat <- L_mat %>%
          add_column(cs = sel_names) %>%
          relocate(cs)

        for(j in 1:length(cntrys)) {
          FIR_by_cntry[[j]] <- main_func(cntrys[j])
        }
        FIR_by_yr[[i]] <- bind_rows(FIR_by_cntry) %>%
          add_column(year = rep(years[i], length(cntrys) * length(unique(conc$DLS)))) %>%
          relocate(year)
      }

      return(bind_rows(FIR_by_yr))



}
