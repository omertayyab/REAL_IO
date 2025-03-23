library(tidyverse)
library(ggpubr)
library(ggplot2)



### removing NA countries from dep ratio table  for ACF and PACF ####
c_NA <- dep_all %>%
  filter(is.nan(GN_dep_ratio) | is.na(GN_dep_ratio)) %>%
  .$country %>%
  unique

dep_all_NA_removed <- dep_all %>%
  filter(!country %in% c_NA) %>%
  arrange(country)

try1_NA_rem <- try1 %>%
  filter(!country %in% c_NA) %>%
  arrange(country)
### Acf and Pacf for chgecking autocorrelation ###

acf_plots <- lapply(seq(1,length(try1_NA_rem$GN_dep_ratio), 26),
                    function(x) forecast::ggAcf(try1_NA_rem$GN_dep_ratio[x: (x + 26 - 1)],
                                      main = try1_NA_rem$country[x])
                    )
pacf_plots <- lapply(seq(1,length(try1_NA_rem$GN_dep_ratio), 26),
                     function(x) forecast::ggPacf(try1_NA_rem$GN_dep_ratio[x: (x + 26 - 1)],
                                        main = try1_NA_rem$country[x])
                     )

ggarrange(plotlist = acf_plots,
                  ncol = 7, nrow = 6,
                  common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(.,
                  top = paste0("ACF", year_1," to ", year_2)
  )


ggarrange(plotlist = pacf_plots,
          ncol = 7, nrow = 6,
          common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(.,
                  top = paste0("PACF", year_1," to ", year_2)
  )

#### Chinese immports in GS countries

imp_dep_CHN <- get_cntry_dep_ratio(data_dir, years, c_GS$Code, "CHN") %>%
  arrange(country, year)

imp_dep_CHN$GN_dep_ratio <- dep_all %>%
  arrange(country) %>%
  filter(country != "CHN", country != "ROW") %>%
  .$GN_dep_ratio

dep_all_CHN <- imp_dep_CHN %>%
  select(country, year, imp_dep_ratio) %>%
  inner_join(dep_all1, ., join_by("country" == "country", "year" == "year")) %>%
  select(country, year, GN_dep_ratio, tot_cap, eci, imp_dep_ratio, pop) %>%
  filter(!country %in% c_NA)


ggplot(data = na.omit(imp_dep_CHN), aes(x = year, y = imp_dep_ratio)) +
  geom_line() +
  geom_line(aes(y = GN_dep_ratio), color = "blue") +
  facet_wrap(~ country, scales = "free")


#### Indian imports in gS countries for counter check ####

imp_dep_IND <- get_cntry_dep_ratio(data_dir, years, c_GS$Code, "IND") %>%
  arrange(country, year)

imp_dep_IND$GN_dep_ratio <- dep_all %>%
  filter(country != "ROW",country != "IND" ) %>%
  arrange(country) %>%
  .$GN_dep_ratio

dep_all_IND <- imp_dep_IND%>%
  select(country, year, imp_dep_ratio) %>%
  inner_join(dep_all1, ., join_by("country" == "country", "year" == "year")) %>%
  select(country, year, GN_dep_ratio, tot_cap, eci, imp_dep_ratio, pop) %>%
  filter(!country %in% c_NA)


ggplot(data = na.omit(imp_dep_CHN), aes(x = year, y = imp_dep_ratio)) +
  geom_line() +
  geom_line(aes(y = GN_dep_ratio), color = "blue") +
  facet_wrap(~ country, scales = "free")



### Pak imports in GS countries for counter check ###


####checking differenced values####



try <- pivot_wider(dep_all_CHN,
                   names_from = country,
                   values_from = c(eci, tot_cap, GN_dep_ratio, imp_dep_ratio, pop),
                   id_cols = year)

#try <- pivot_wider(eci,
#                   names_from = country,
#                   values_from = c(hs_eci, trade_bal, hs_eci_rank),
#                   id_cols = year)


try1 <- try %>%
  #  mutate(across(contains("tot_cap"), ~log(.))) %>%
  mutate(across(!year, ~ (. - dplyr::lag(.)) )) %>%
  slice(-1) %>%
  pivot_longer(cols = 2:last_col(), names_to = "name", values_to = "value")

try1$country <- str_sub(try1$name, -3, -1)
try1$name <- gsub('.{4}$', '', try1$name)

try1 <- pivot_wider(try1, names_from = name, values_from = value)

#try1$eci <- dep_all1 %>%
#  filter(year != 1995) %>%
#  arrange(year)

try2 <- dep_all1 %>%
  inner_join(.,try1[c(1,2,3)], join_by("year" == "year", "country" == "country"))


#### PLM testing
try_plm <- plm(GN_dep_ratio ~  eci + tot_cap + imp_dep_ratio ,
               data=na.omit(try1_NA_rem),
               index=c("country", "year"),
               model="within")

pgrangertest(GN_dep_ratio ~ imp_dep_ratio ,
             data = na.omit(try1), order =1 )

plm_plot <- try_plm$fitted.values %>%
  as_tibble %>% add_column(year = seq(1997, 2019)) %>%
  relocate(year) %>%
  pivot_longer(cols = 2:last_col(), names_to = "country") %>%
  inner_join( ., try1_NA_rem , join_by("year" == "year", "country" == "country"))


ggplot(na.omit(plm_plot), aes(y = value, x = GN_dep_ratio)) +
  geom_point() +
  geom_abline(slope = 1, color = "red") +
  facet_wrap(~ country, scales = "fixed")

ggplot(na.omit(dep_all_CHN), aes(y = try_pmg$fitted.values, x = GN_dep_ratio)) +
  geom_point() +
  geom_abline(slope = 1, color = "red")

#try_plm <- plm(trade_bal ~ hs_eci ,
#               data=na.omit(try1),
#               index=c("country"),
#               model="within")

ggplot(na.omit(try1), aes(y = GN_dep_ratio, x = year)) +
  geom_point() +
  facet_wrap(~ country, scales = "fixed")
