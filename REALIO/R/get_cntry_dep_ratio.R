#' @import tidyverse
#' @param datalist is output from load_data function
#' @param cntrys is 3-letter iso code for country of study. Can be multiple
#' @param cntry_2 is 3-letter iso code for country that's source of imports
#' @param sector is optional to use function for sector-specific imports



get_cntry_dep_ratio <- function(data_dir, years, cntrys, cntry_2, sector = NULL)
  {
    main_func <- function(datalist, cntry_1, cntry_2) {

    n_ind <- 45

    imp_CN <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_2))

    imp_CN$V1 <- rep(1:n_ind, length(imp_CN$V1)/n_ind) %>% as.factor()

    imp_CN <- imp_CN %>%
      summarize(across(1:last_col(), sum), .by = V1)


    imp_else <- datalist$icio %>%
      select(starts_with(c("V1",cntry_1))) %>%
      filter(str_detect(V1, cntry_2, negate = TRUE)) %>%
      filter(str_detect(V1, cntry_1, negate = TRUE)) %>%
      slice(1:(n()-3))


    imp_else$V1 <- rep(1:n_ind, length(imp_else$V1)/n_ind) %>% as.factor()

    imp_else <- imp_else %>%
      summarize(across(1:last_col(), sum), .by = V1)

    CN_rat <- sum(imp_CN[2:52])/(sum(imp_CN[2:52]) + sum(imp_else[2:52]))

    # Exports

    exp_CN <- datalist$icio %>%
      filter(str_detect(V1, cntry_1)) %>%
      select(!starts_with(c(cntry_1,"OUT"))) %>%
      select(c(1,starts_with(cntry_2))) %>%
      mutate(exp = rowSums(across(2:last_col())), .keep = "unused")


    exp_else <- datalist$icio %>%
      filter(str_detect(V1, cntry_1)) %>%
      select(!starts_with(c(cntry_1,"OUT"))) %>%
      select(c(1,!starts_with(cntry_2))) %>%
      mutate(exp = rowSums(across(2:last_col())), .keep = "unused")


    # If sector is specified

    if(!is.null(sector))
      {
        imp_CN$f <- rowSums(imp_CN[47:52])
        imp_else$f <- rowSums(imp_else[47:52])

        dom <- datalist$icio %>%
          select(starts_with(c("V1",cntry_1))) %>%
          filter(str_detect(V1, cntry_1))
        dom$V1 <- gsub("^.{0,4}","", dom$V1)


        exp <- datalist$icio %>%
          filter(str_detect(V1, cntry_1)) %>%
          select(!starts_with(c(cntry_1,"OUT"))) %>%
          mutate(exp = rowSums(across(2:last_col())), .keep = "unused")

        imp <- datalist$icio %>%
          select(starts_with(c("V1",cntry_1))) %>%
          filter(str_detect(V1, cntry_1, negate=TRUE)) %>%
          slice(1:(n()-3))
        imp$V1 <- gsub("^.{0,4}","", imp$V1)

        imp <- imp %>%
          summarize(across(1:last_col(), sum), .by = V1)

        Xd <- mutate(dom,
                     int = rowSums(across(2:46)),
                     final = rowSums(across(47:52)),
                     total = rowSums(across(2:52)),
                     .keep="unused")

        Xm <- mutate(imp,
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
        Am_mat_CN <-rbind(Am_mat_CN, (Xd$total[-45] + exp$exp[-45])) %>%
          lapply(., function(x)x/x[length(x)]) %>%
          as_tibble %>%
          slice(-n()) %>%
          as.matrix

        Am_mat_else <- select(imp_else, 2:45) %>%
          slice(-n())
        Am_mat_else <-rbind(Am_mat_else, (Xd$total[-45] + exp$exp[-45])) %>%
          lapply(., function(x)x/x[length(x)]) %>%
          as_tibble %>%
          slice(-n()) %>%
          as.matrix

        emb_imp_CN <- (Am_mat_CN %*% L_mat)
        emb_imp_CN <- zapsmall(emb_imp_CN, digits = 5)

        emb_imp_else <- (Am_mat_else %*% L_mat)
        emb_imp_else <- zapsmall(emb_imp_else, digits = 5)



        final_2 <-
          tibble(
            DLS= conc$DLS[-45],
            emb_imp_CN = colSums(emb_imp_CN),
            emb_imp_else = colSums(emb_imp_else),
            dom = colSums(L_mat),
            imp_final = Xm$final[-45],
            imp_CN_final = imp_CN$f[-45],
            imp_else_final = imp_else$f[-45],
            dom_final = Xd$final[-45],
            exports = exp$exp[-45]
          ) %>%
          mutate(emb_imp_CN_final = emb_imp_CN * (dom_final+exports)) %>%
          mutate(emb_imp_else_final = emb_imp_else * (dom_final+exports))

        result_2  <- final_2 %>%
          group_by(DLS) %>%
          summarise_all(sum) %>%
          mutate(ov_imp = imp_final + emb_imp_CN_final + emb_imp_else_final) %>%
          #select(-c("imports", "domestic")) %>%
          arrange(desc(ov_imp))

        return(
          result_2 %>%
            filter(DLS == sector) %>%
            mutate(imp_dep_ratio = (imp_CN_final + emb_imp_CN_final)/ov_imp) %>%
            add_column(country = cntry_1) %>%
            add_column(source = cntry_2) %>%
            select(country, source, imp_dep_ratio, DLS) %>%
            rename(sector = DLS)
      )
    }

    else  return(
      tibble(country = cntry_1,
             source = cntry_2,
             imp_dep_ratio = CN_rat,
             sector = "All"
             )
      )

  }

    dr_by_yr <- list(0) #initializing list for storing tibbles with cntry dep ratios
    dr_by_cntry <- list(0) #initializing vector for storing dep ratios


    cntrys <- cntrys[ cntrys != cntry_2]


    for(i in 1:length(years)){
      datalist <- load_data(data_dir, years[i])
      for(j in 1:length(cntrys)) {
        dr_by_cntry[[j]] <- main_func(datalist, cntrys[j], cntry_2)
          }
      dr_by_yr[[i]] <- bind_rows(dr_by_cntry) %>%
        add_column(year = rep(years[i], length(cntrys)))
    }

    return(bind_rows(dr_by_yr))


}
