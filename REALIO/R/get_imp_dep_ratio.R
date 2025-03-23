#' @import tidyverse
#' @param years is vector of years
#' @param cntrys vector of 3-letter country codes as used in relevant dataset

get_imp_dep_ratio <- function(data_dir, years, cntrys, sector = NULL) {

  main_func <- function(datalist, cntry_1)
  {

    n_ind <- 45

    imp_CN <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1, negate = TRUE)) %>%
      slice(1:(n()-3)) #removing totals at the bottom of the table

    imp_CN$V1 <- rep(1:n_ind, length(imp_CN$V1)/n_ind) %>% as.factor()

    imp_CN <- imp_CN %>%
      summarize(across(1:last_col(), sum), .by = V1)


    dom <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_1))

    dom$V1 <- rep(1:n_ind, length(imp_CN$V1)/n_ind) %>% as.factor()

    dep_rat <- sum(imp_CN[2:52])/(sum(dom[47:52]) + sum(imp_CN[47:52]))

    # Exports

    exp <- datalist$icio %>%
      filter(str_detect(V1, cntry_1)) %>%
      select(!starts_with(c(cntry_1,"OUT"))) %>%
      mutate(exp = rowSums(across(2:last_col())), .keep = "unused")

    dep_rat_prod <- sum(imp_CN[2:46])/(sum(dom[47:52]) + sum(exp$exp))

    # If sector is specified

    if(!is.null(sector))
      {
      #  imp_CN$f <- rowSums(imp_CN[47:52])

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


        A_mat <- select(dom, c(2:45)) %>%
          slice(-n())
        A_mat <- rbind(A_mat, (Xd$total[-45] + exp$exp[-45])) %>%
          lapply(., function(x)x/x[length(x)]) %>%
          as_tibble %>%
          slice(-n()) %>%
          as.matrix

        L_mat <- (diag(dim(A_mat)[1]) - A_mat ) %>% solve()


        Am_mat_CN <- select(imp_CN, 2:45) %>%
          slice(-n())
        Am_mat_CN <- rbind(Am_mat_CN, (Xd$total[-45] + exp$exp[-45])) %>%
          lapply(., function(x)x/x[length(x)]) %>%
          as_tibble %>%
          slice(-n()) %>%
          as.matrix


        emb_imp_CN <- (Am_mat_CN %*% L_mat)
        emb_imp_CN <- zapsmall(emb_imp_CN, digits = 5)


        final_2 <-
          tibble(
            DLS= conc$DLS[-45],
            emb_imp_CN = colSums(emb_imp_CN),
            dom = colSums(L_mat),
            imp_final = Xm$final[-45],
            dom_final = Xd$final[-45],
            exports = exp$exp[-45]
          ) %>%
          mutate(emb_imp_CN_final = emb_imp_CN * (dom_final+exports)) %>%
          mutate(emb_imp_CN_dom = emb_imp_CN * (dom_final))

        result_2  <- final_2 %>%
          group_by(DLS) %>%
          summarise_all(sum) %>%
          mutate(ov_imp = imp_final + emb_imp_CN_final ) %>%
          mutate(local_imp = imp_final + emb_imp_CN_dom ) %>%
          #select(-c("imports", "domestic")) %>%
          arrange(desc(ov_imp))

        return(
          result_2 %>%
            filter(DLS == sector) %>%
            mutate(imp_dep_ratio_c = local_imp/(dom_final + imp_final),
                   imp_dep_ratio_p = emb_imp_CN_final/(dom_final + exports)
                   ) %>%
            select(imp_dep_ratio_c,imp_dep_ratio_p)
        )
      }

      else  return( list(dep_rat, dep_rat_prod) )
  }

  dr_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
  drc_by_cntry <- as.vector(0) #initializing vector for storing dep ratios
  drp_by_cntry <- as.vector(0)


  for(i in 1:length(years)){
    datalist <- load_data(data_dir, years[i])
    for(j in 1:length(cntrys)) {
      x <- main_func(datalist, cntrys[j])
      drc_by_cntry[j] <- x[[1]]
      drp_by_cntry[j] <- x[[2]]
    }
    dr_by_yr[[i]] <- tibble(country = cntrys,
                            year = rep(years[i], length(cntrys)),
                            imp_dep_ratio_cons = drc_by_cntry,
                            imp_dep_ratio_prod = drp_by_cntry,
                            sector = ifelse(is.null(sector), "All", sector)
                            )
  }

  return(bind_rows(dr_by_yr))

}

