#' @import tidyverse
#' @param t2 is tibble with GN GS separation of imports

get_GN_dep_ratio <- function(t2) {

  r1 <- t2 %>%
    select(imp_GS_final, emb_imp_GS_final, imp_GN_final, emb_imp_GN_final) %>%
    summarize(across(everything(), sum)) %>%
    mutate(GN_rat_dom =
             (imp_GN_final + emb_imp_GN_final) / (imp_GS_final +
                                                    emb_imp_GS_final +
                                                    imp_GN_final +
                                                    emb_imp_GN_final) ,
           .keep = "none") %>%
    unlist

  return(r1)

}
