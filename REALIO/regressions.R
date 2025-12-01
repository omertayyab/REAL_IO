library(tidyverse)
library(ggpubr)
library(ggplot2)
library(plm)

##### union rates ####
uni_d <- read_csv("C:/R_OT/Projects/IO_to_DLS/Data/AIAS-ICTWSS-Union/oecd-union_v1.1.csv")

pop_GN <- pop_d %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}")) %>%
  select(7:34) %>%
  relocate(country) %>%
  pivot_longer(3:last_col(), names_to = "year") %>%
  filter(country %in% c_GN$country) %>%
  filter(year %in% years) %>%
  select(!contains("SCALE")) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(country, year)

uni <- uni_d %>%
  mutate(UD_s = ifelse(UD_s == -88, NA,UD_s),
         UD_hist = ifelse(UD_hist == -88, NA,UD_hist)
         )%>%
  filter(year > 1994 & year < 2020) %>%
  filter(iso3c %in% c_GN$country) %>%
  select(year, iso3c, UD_s, UD_hist) %>%
  rename(country = iso3c) %>%
  left_join(.,pop_GN, join_by("year" == "year", "country" == "country"))

uni_sel_ctrys <- uni %>%
  select(year, country, UD_hist) %>%
  group_by(country) %>%
  na.omit %>%
  summarize(ct = n()) %>%
  filter(ct == max(ct)) %>%
  .$country

uni_avgs <- uni %>%
  filter(country %in% uni_sel_ctrys) %>%
  group_by(year) %>%
  summarize(
    wt_avg_UD_s = sum(UD_s * value, na.rm = TRUE)/sum(value, na.rm = TRUE),
    sim_avg_UD_s = mean(UD_s, na.rm = TRUE),
    wt_avg_UD_h = sum(UD_hist * value, na.rm = TRUE)/sum(value, na.rm = TRUE),
    sim_avg_UD_h = mean(UD_hist, na.rm = TRUE)
    )

#############VGN's A minus Output share #####
VA_X_sh_diff <- get_region_VA_sh(data_dir, years, c_GN$country)

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

#### Chinese imports in GS countries

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


#### Indian imports in GS countries for counter check ####

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

ggplot(na.omit(dep_all_CHN), aes(y = try_plm$fitted.values, x = GN_dep_ratio)) +
  geom_point() +
  geom_abline(slope = 1, color = "red")

#try_plm <- plm(trade_bal ~ hs_eci ,
#               data=na.omit(try1),
#               index=c("country"),
#               model="within")

ggplot(na.omit(try1), aes(y = GN_dep_ratio, x = year)) +
  geom_point() +
  facet_wrap(~ country, scales = "fixed")

##### share of imports graph ####
dep_all_shares <- dep_all %>%
  filter(country != "ROW") %>%
  na.omit %>%
  left_join(., imp_dep_CHN[c(1:5)],
            join_by("country" == "country","year" == "year")
            ) %>%
  replace_na(., list(source = "CHN", imp_dep_ratio = 0, sector = "All")) %>%
  rename(CHN_dep_ratio = imp_dep_ratio, src1 = source) %>%
  left_join(.,imp_dep_IND[c(1:5)],
            join_by("country" == "country", "year" == "year")
            ) %>%
  replace_na(., list(source = "IND", imp_dep_ratio = 0, sector = "All")) %>%
  rename(IND_dep_ratio = imp_dep_ratio, src2 = source) %>%
  select(!contains(c("sector", "src"))) %>%
  mutate(rest_GS = 1 - GN_dep_ratio - CHN_dep_ratio - IND_dep_ratio) %>%
  pivot_longer(cols = !c("country", "year"), names_to = "rtype", values_to = "value")


ggplot(na.omit(dep_all_shares), aes(x = year )) +
  geom_area(aes(y = value, fill = rtype), alpha = 0.8) +
  facet_wrap(~country, scales = "fixed") +
  scale_fill_brewer(palette = "RdYlBu")+
  theme_pubclean()


dep_all_shares_P_SP  <- dep_all %>%
  filter(country != "ROW") %>%
  na.omit %>%
  left_join(., imp_dep_CHN[c(1:5)],
            join_by("country" == "country","year" == "year")
  ) %>%
  replace_na(., list(source = "CHN", imp_dep_ratio = 0, sector = "All")) %>%
  rename(CHN_dep_ratio = imp_dep_ratio, src1 = source) %>%
  left_join(.,imp_dep_IND[c(1:5)],
            join_by("country" == "country", "year" == "year")
  ) %>%
  replace_na(., list(source = "IND", imp_dep_ratio = 0, sector = "All")) %>%
  rename(IND_dep_ratio = imp_dep_ratio, src2 = source) %>%
  select(!contains(c("sector", "src"))) %>%
  mutate(rest_GS = 1 - GN_dep_ratio - CHN_dep_ratio - IND_dep_ratio) %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  group_by(year, C_P_SP) %>%
  summarize(CHN_imp = mean(CHN_dep_ratio, na.rm = TRUE),
            IND_imp = mean(IND_dep_ratio, na.rm = TRUE),
            rest_GS = mean(rest_GS, na.rm = TRUE),
            GN_imp = mean(GN_dep_ratio, na.rm = TRUE)
            ) %>%
  pivot_longer(cols = !c("C_P_SP", "year"), names_to = "rtype", values_to = "value")



na.omit(dep_all_shares_P_SP) %>%
  arrange() %>%
  ggplot(., aes(x = year )) +
    geom_area(aes(y = value, fill = rtype), alpha = 0.8) +
    facet_wrap(~C_P_SP, scales = "fixed") +
    scale_fill_brewer(palette = "RdYlBu") +
    theme_pubclean()

all_imp_shares <- get_region_imp_sh(data_dir,
                                    years,
                                    c_GS$Code, c_GN$country, c("CHN", "IND")
                                    )

ggplot(na.omit(all_imp_shares), aes(x = year )) +
  geom_area(aes(y = imp_ratio, fill = source)) +
#  facet_wrap(~C_P_SP, scales = "fixed") +
  scale_fill_brewer(palette = "RdYlBu") +
  ggpubr::theme_pubclean()


#### graphs of correlation to replace regression #####

dep_stack <- dep_all1 %>%
  select(!contains(c("_st", "gov", "prv", "ppp", "rank"))) %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country" == "Code")) %>%
  left_join(.,dep_all_CHN[c("country", "year", "imp_dep_ratio")], , by = c("country", "year")) %>%
  rename(CHN_dep_ratio = imp_dep_ratio) %>%
  left_join(.,dep_all_IND[c("country", "year", "imp_dep_ratio")], , by = c("country", "year")) %>%
  rename(IND_dep_ratio = imp_dep_ratio) %>%
  left_join(.,dep_all_PAK[c("country", "year", "imp_dep_ratio")], , by = c("country", "year")) %>%
  rename(PAK_dep_ratio = imp_dep_ratio) %>%
  group_by(year, C_P_SP) %>%
  summarize(eci = mean(eci, na.rm = TRUE),
            GN_dep_ratio = mean(GN_dep_ratio, na.rm = TRUE),
            tot_cap = mean(tot_cap, na.rm = TRUE),
            CHN_dep_ratio = mean(CHN_dep_ratio, na.rm = TRUE),
            IND_dep_ratio = mean(IND_dep_ratio, na.rm = TRUE),
            PAK_dep_ratio = mean(PAK_dep_ratio, na.rm = TRUE)
            ) %>%
  ungroup #%>%
#  pivot_longer(cols = c(3:5), names_to = "grp", values_to = "value")

dep_stack <- eci_GN %>%
  group_by(year) %>%
  summarize(eci_GN_rank = mean(hs_eci_rank)) %>%
  right_join(.,dep_stack, join_by("year" == "year"))

dep_stack <- uni_avgs %>%
  select(year, wt_avg_UD_h) %>%
  mutate(year = as.numeric(year)) %>%
  right_join(.,dep_stack, join_by("year" == "year"))

ggplot(dep_stack, aes(y = eci, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~C_P_SP, scales = "free")

ggplot(dep_stack, aes(y = CHN_dep_ratio, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~C_P_SP, scales = "free")

ggplot(dep_stack, aes(y = tot_cap, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~country, scales = "free")

ggplot(dep_stack, aes(y = eci_GN_rank, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~C_P_SP, scales = "free")

ggplot(dep_stack, aes(y = wt_avg_UD_h, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~C_P_SP, scales = "free")

VA_X_sh_diff[c("year", "VA_X_diff")] %>%
  right_join(.,dep_stack, join_by("year" == "year")) %>%
  ggplot(., aes(y = VA_X_diff, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~country, scales = "free")



####correlation plots with differences#####

dep_stack %>%
  group_by(C_P_SP) %>%
  mutate(across(!year, ~ . - dplyr::lag(.)) ) %>%
  ungroup %>%
  ggplot(., aes(y = wt_avg_UD_h, x = GN_dep_ratio, group = C_P_SP, color = C_P_SP)) +
  geom_point() +
  #geom_smooth(method='loess', fullrange=TRUE, color = "black") +
  facet_wrap(~C_P_SP, scales = "free")



###scatter plots between regressors and GN_dep_Ratio individual countries####

dep_all_CHN %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country" == "Code")) %>%
  ggplot(., aes(y = log(tot_cap), x = GN_dep_ratio, colour  = C_P_SP)) +
    geom_point() +
    geom_smooth(method='loess', fullrange=TRUE, color = "black", se = FALSE, size = 0.1) +
    facet_wrap(~country, scales = "free")

dep_all_CHN %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country" == "Code")) %>%
  ggplot(., aes(y = eci, x = GN_dep_ratio, colour  = C_P_SP)) +
    geom_point() +
    geom_smooth(method='loess', fullrange=TRUE, color = "black", se = FALSE, size = 0.1) +
    facet_wrap(~country, scales = "free")

dep_all_CHN %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country" == "Code")) %>%
  ggplot(., aes(y = imp_dep_ratio, x = GN_dep_ratio, colour  = C_P_SP)) +
    geom_point() +
    geom_smooth(method='loess', fullrange=TRUE, color = "black", se = FALSE, size = 0.1) +
    facet_wrap(~country, scales = "free")

dep_all_CHN %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country" == "Code")) %>%
  left_join(.,uni_avgs[c("year", "wt_avg_UD_h")], join_by("year"=="year")) %>%
  ggplot(., aes(y = wt_avg_UD_h, x = GN_dep_ratio, colour  = C_P_SP)) +
  geom_point() +
  geom_smooth(method='loess', fullrange=TRUE, color = "black", se = FALSE, size = 0.1) +
  facet_wrap(~country, scales = "free")










