#' @import tidyverse
#' @param years is vector of years
#' @param data_dir directory where datasets are located
#' @param cntrys vector of 3-letter country codes as used in relevant dataset
#' @param region_1 is the region of interest from which imports originate

get_region_imp_int_ratio <- function(data_dir, years, region_1, cntrys) {

  main_func <- function(datalist, cntry_1)
  {

    n_ind <- 45

    imp_CN <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1, negate = TRUE)) %>%
      slice(1:(n()-3)) %>% #removing totals at the bottom of the table
      filter(str_detect(V1, paste0(region_1, collapse = "|")))

    imp_CN$V1 <- rep(1:n_ind, length(imp_CN$V1)/n_ind) %>% as.factor()

    imp_CN <- imp_CN %>%
      summarize(across(1:last_col(), sum), .by = V1)


    dom <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1))

    dom$V1 <- rep(1:n_ind, length(imp_CN$V1)/n_ind) %>% as.factor()

    # Exports

    exp <- datalist$icio %>%
      filter(str_detect(V1, cntry_1)) %>%
      select(!starts_with(c(cntry_1,"OUT"))) %>%
      mutate(exp = rowSums(across(2:last_col())), .keep = "unused")

    #Am matrix calculation

    Xd <- mutate(dom,
                 int = rowSums(across(2:46)),
                 final = rowSums(across(47:52)),
                 total = rowSums(across(2:52)),
                 .keep="unused")

    Xm <- mutate(imp_CN,
                 int = rowSums(across(2:46)),
                 final = rowSums(across(47:52)),
                 total = rowSums(across(2:52)),
                 .keep="unused")

    Am_mat_CN <- select(imp_CN, 2:(n_ind+1)) %>%
      rbind(., (Xd$total + exp$exp)) %>%
      lapply(., function(x)x/x[length(x)]) %>%
      as_tibble %>%
      mutate(across(everything(),~replace(.,is.nan(.),0))) %>%
      slice(-n()) %>%
      as.matrix

    AmLF <- Am_mat_CN %*% (Xd$total)


    #ratios calculation
    dep_rat <- (sum(imp_CN[2:52]) - sum(AmLF))/(sum(dom[47:52]) + sum(imp_CN[47:52]))


    dep_rat_prod <- sum(imp_CN[2:46])/(sum(dom[47:52]) + sum(exp$exp))

    # Collecting sector-wise results

      A_mat <- select(dom, c(2:(n_ind+1))) %>%
        rbind(., (Xd$total + exp$exp)) %>%
        lapply(., function(x)x/x[length(x)]) %>%
        as_tibble %>%
        mutate(across(everything(),~replace(.,is.nan(.),0))) %>%
        slice(-n()) %>%
        as.matrix

      L_mat <- (diag(dim(A_mat)[1]) - A_mat ) %>% solve()

      emb_imp_CN <- (Am_mat_CN %*% L_mat)
      emb_imp_CN <- zapsmall(emb_imp_CN, digits = 5)


      final_2 <-
        tibble(
          DLS= datalist$conc$DLS,
          emb_imp_CN = colSums(emb_imp_CN),
          dom = colSums(L_mat),
          imp_final = Xm$final,
          dom_final = Xd$final,
          exports = exp$exp
        ) %>%
        mutate(emb_imp_CN_final = emb_imp_CN * (dom_final+exports)) %>%
        mutate(emb_imp_CN_dom = emb_imp_CN * (dom_final))

      result_2  <- final_2 %>%
        group_by(DLS) %>%
        filter(DLS == "Nutrition" |DLS =="Energy" | DLS =="Manufacturing") %>%
        summarise_all(sum) %>%
        mutate(ov_imp = imp_final + emb_imp_CN_final ) %>%
        mutate(local_imp = imp_final + emb_imp_CN_dom ) %>%
        #select(-c("imports", "domestic")) %>%
        arrange(desc(ov_imp))

      return(
        result_2 %>%
          mutate(imp_int_c = local_imp/(dom_final + imp_final),
                 imp_int_p = emb_imp_CN_final/(dom_final + exports)
          ) %>%
          select(DLS, imp_int_c,imp_int_p) %>%
          rename(sector = DLS) %>%
          add_row(sector = "All",
                  imp_int_c = dep_rat,
                  imp_int_p = dep_rat_prod) %>%
          add_column(country = cntry_1)
      )
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

