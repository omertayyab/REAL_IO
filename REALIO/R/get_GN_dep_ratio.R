#' @import tidyverse
#' @param t2 is tibble with GN GS separation of imports

get_GN_dep_ratio <- function(t2) {

  r1 <- t2 %>%
    select(dom_final, imp_GN_final, emb_imp_GN_final) %>%
    summarize(across(1:3, sum)) %>%
    mutate(GN_rat_dom = (imp_GN_final + emb_imp_GN_final) / dom_final, .keep = "none") %>%
    unlist

  return(r1)

}
