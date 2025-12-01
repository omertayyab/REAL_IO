library(tidyverse)
library(ggplot2)
library(networkD3)

data_dir <- "C:/R_OT/Projects/IO_to_DLS/Data/ICIO"

years <- seq(1995, 2020)

c_GS <- read_csv(paste0(data_dir, "/ICIO_BEC_conc.csv")) %>%
  select(c(Code, GS_GN)) %>%
  filter(Code != "ROW", GS_GN == "GS")

c_GN <- read_csv(paste0(data_dir, "/ICIO_BEC_conc.csv")) %>%
  select(c(Code, GS_GN)) %>%
  filter(Code != "ROW",GS_GN == "GN") %>%
  select(Code) %>%
  rename(country = Code)

c_GN$GN_dep_ratio <- 1

ctrys <- read_csv(paste0(data_dir, "/ICIO_BEC_conc.csv")) %>%
  select(c(Code, countries, GS_GN, C_P_SP))


ctrys$countries[14] <- "Ivory Cost"  #removing special characters for kable
ctrys$countries[71] <- "Turkiye"



eci_d <- haven::read_dta(paste0(data_dir, "/rankings.dta")) %>%
  filter(year %in% unique(years))

unsd <- read_delim(paste0(data_dir, "/UNSD.csv"), delim = ";") %>%
  mutate(M49 = as.numeric(`M49 Code`))

eci_d$country <- unsd$`ISO-alpha3 Code`[match(eci_d$country_id, unsd$M49)]

eci_GN <- eci_d %>%
  select("country","year", "hs_eci", "hs_eci_rank") %>%
  filter(country %in% unique(c_GN$country)) %>%
  arrange(country, year)

eci <- eci_d %>%
  select("country","year", "hs_eci", "hs_eci_rank") %>%
  filter(country %in% unique(c_GS$Code)) %>%
  arrange(country, year)

eci_reg <- filter(eci, year != 2020)

dep_all <- get_GN_dep_ratio_mult(years, c_GS$Code)

dep_energy_all <- get_GN_sec_dep_ratio_mult(years, c_GS$Code, "Energy") #%>%
  #filter(!is.nan(GN_dep_ratio))

dep_food_all <- get_GN_sec_dep_ratio_mult(years, c_GS$Code, "Nutrition") #%>%
#filter(!is.nan(GN_dep_ratio))

dep_man_all <- get_GN_sec_dep_ratio_mult(years, c_GS$Code, "Manufacturing") #%>%


icsd <- read_csv(r"(C:\R_OT\Projects\IO_to_DLS\Data\IMF\ICSD\ICSD2021.csv)")
icsd_reg <- icsd %>%
  filter(year %in% years ) %>%
  filter(isocode %in% c_GS$Code) %>%
  filter(isocode %in% unique(eci$country)) %>%
  arrange(isocode, year)


pop_d <- read_csv(r"(C:\R_OT\Projects\IO_to_DLS\Data\IMF\WEO_pop\pop.csv)")

pop <- pop_d %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}")) %>%
  select(7:34) %>%
  relocate(country) %>%
  pivot_longer(3:last_col(), names_to = "year") %>%
  filter(country %in% c_GS$Code) %>%
  filter(country %in% unique(eci$country)) %>%
  filter(year %in% years) %>%
#  filter(year != 2020) %>%
  arrange(country, year)


dep_all1 <- dep_all %>%
  filter(year != 2020) %>%
  filter(country != "ROW") %>%
  filter(country %in% unique(eci$country)) %>%
  arrange(country,year) %>%
  add_column(gov_cap_st = icsd_reg$kgov_rppp) %>%
  add_column(prv_cap_st = icsd_reg$kpriv_rppp) %>%
  add_column(ppp_cap_st = icsd_reg$kppp_rppp) %>%
  add_column(pop = pop$value) %>%
  replace_na(list(gov_cap_st = 0, prv_cap_st = 0, ppp_cap_st = 0)) %>%
  mutate(gov_cap = gov_cap_st/pop,
         prv_cap = prv_cap_st/pop,
         ppp_cap = ppp_cap_st/pop,
         tot_cap = (gov_cap_st + prv_cap_st + ppp_cap_st)/pop,
         eci = eci_reg$hs_eci,
         eci_rank = eci_reg$hs_eci_rank
         )


ggplot(na.omit(dep_all1), aes(y = GN_dep_ratio, x = year, group= country)) +
  geom_line() +
#  geom_smooth(method='lm', fullrange=TRUE, color = "red") +
#  geom_point(aes(y = (1/eci_rank)*tot_cap/5), color = "blue") +
#  geom_line(aes(y = gov_cap/10), color = "red") +
#  geom_line(aes(y = prv_cap/100), color = "steelblue") +
#  geom_line(aes(y = tot_cap/50), color = "green") +
# scale_y_continuous(
#    name = "Primary Y-axis (GN_dep)",
#    sec.axis = sec_axis(~ . * 5, name = "Secondary Y-axis (ECI)")  # Reverse transformation for secondary axis
#    ) +
  facet_wrap(~ country, scales = "fixed")


# Direct Import Dependency vs GN import dep graphs

imp_int_all <- get_imp_dep_ratio(data_dir, years, c_GS$Code )
imp_int_GN <- get_region_imp_dep_ratio(data_dir, years, c_GN$country, c_GS$Code) %>%
  rename(GN_int_c = imp_int_c, GN_int_p = imp_int_p)

imp_int_all %>%
  na.omit %>%
  ggplot(., aes( x = year, colour = sector)) +
  geom_line(aes(y = imp_int_p)) +
  geom_line(aes(y = imp_int_c)) +
  ylab("Import Intensity ratio") +
  facet_wrap(~ country, scales = "fixed")

pop_wt <- pop_d %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}")) %>%
  select(7:34) %>%
  relocate(country) %>%
  pivot_longer(3:last_col(), names_to = "year") %>%
  filter(country %in% unique(imp_int_all$country)) %>%
  filter(year %in% years) %>%
  arrange(country, year) %>%
  select(-SCALE.Name) %>%
  mutate(year = as.numeric(year)) %>%
  rename(pop = value)

imp_int_all_P <- imp_int_all %>%
  left_join(.,ctrys[c(1,4)], join_by("country" == "Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
            ) %>%
  na.omit() %>%
  group_by(C_P_SP, year, sector) %>%
  summarize(avg_simple_c = mean(imp_int_c),
            avg_simple_p = mean(imp_int_p),
            avg_wt_c = weighted.mean(imp_int_c, pop),
            avg_wt_p = weighted.mean(imp_int_p, pop),
            avg_simple_c_GN = mean(GN_int_c),
            avg_simple_p_GN = mean(GN_int_p),
            avg_wt_c_GN = weighted.mean(GN_int_c, pop),
            avg_wt_p_GN = weighted.mean(GN_int_p, pop),
            pop = sum(pop, na.rm = TRUE)
            )

prod_plot <- imp_int_all_P %>%
  filter(sector != "All") %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = avg_wt_p_GN, ymax = avg_wt_p), fill = "red",alpha = 0.3) +
  geom_line(aes(y = avg_wt_p_GN, colour = "Core countries"), size = 1) +
  geom_line(aes(y = avg_wt_p, colour = "All countries"), size = 1) +
  ylim(0, 0.6) +
  ylab("IIP (population-weighted average)") +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_grid( C_P_SP ~ sector, scales = "fixed")

cons_plot <- imp_int_all_P %>%
  filter(sector != "All") %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = avg_wt_c_GN, ymax = avg_wt_c), fill = "blue", alpha = 0.1) +
  geom_line(aes(y = avg_wt_c_GN, colour = "Core countries"), size = 1) +
  geom_line(aes(y = avg_wt_c, colour = "All countries"), size = 1) +
  ylim(0, 0.6) +
  ylab("IIC (population-weighted average)") +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_grid( C_P_SP ~ sector, scales = "fixed")


IIP_IIC_graph <- ggpubr::ggarrange(prod_plot, cons_plot,
                  ncol = 1, nrow = 2,
                  common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                  top = paste0("IIP and IIC spreads from", year_1," to ", year_2)
  )



imp_int_all %>%
  left_join(.,ctrys[c(1,4)], join_by("country" == "Code")) %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
  ) %>%
  filter(sector != "All") %>%
  na.omit() %>%
  ggplot(., aes( x = year, group = sector, fill = sector)) +
  geom_ribbon(aes(ymin = GN_int_p, ymax = imp_int_p),alpha = 0.5) +
  facet_wrap(~country, scales = "fixed") +
  ggtitle("Import intensity of Production")


# GN Dependency of imports ratio tables

val <- avg_dep_ratios(dep_all, pop_d)


c_Semi_P <- ctrys %>%
  select(c(Code, C_P_SP)) %>%
  filter(C_P_SP == "SP", Code != "ROW") %>%
  select(Code) %>%
  rename(country = Code)

c_P <- ctrys %>%
  select(c(Code, C_P_SP)) %>%
  filter(C_P_SP == "P", Code != "ROW") %>%
  select(Code) %>%
  rename(country = Code)

C_C <- ctrys %>%
  select(c(Code, C_P_SP)) %>%
  filter(C_P_SP == "C", Code != "ROW") %>%
  select(Code) %>%
  rename(country = Code)

val_SP <- dep_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(P_SP = "SP")

val_P <- dep_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d)%>%
  add_column(P_SP = "P")

val_P_SP <- rbind(val_P, val_SP)

ggplot(val_P_SP, aes(x = year, group = P_SP, color  = P_SP)) +
  geom_line(aes(y = wt_av))

### Sector specific dependency ratio tables

val_energy <- avg_dep_ratios(dep_energy_all, pop_d)

val_energy_SP <- dep_energy_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Energy", P_SP = "SP")

val_energy_P <- dep_energy_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Energy", P_SP = "P")

val_food <- avg_dep_ratios(dep_food_all, pop_d)

val_food_SP <- dep_food_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Nutrition", P_SP = "SP")

val_food_P <- dep_food_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Nutrition", P_SP = "P")

val_man <- avg_dep_ratios(dep_man_all, pop_d)

val_man_SP <- dep_man_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Manufacturing", P_SP = "SP")

val_man_P <- dep_man_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d) %>%
  add_column(sector = "Manufacturing", P_SP = "P")

val_all_sec <- rbind(val_man_P, val_energy_P, val_food_P, val_man_SP, val_energy_SP, val_food_SP)

ggplot(val_all_sec, aes(x = year, group = P_SP, colour = P_SP)) +
  geom_line(aes(y = wt_av)) +
  facet_wrap(~sector, scale = "fixed")
## World Maps of Dependency

year_1 <- 1995
year_2 <- 2020

dep_map1 <- world_dep_map(year_1,dep_all,c_GN)
dep_map2 <- world_dep_map(year_2,dep_all,c_GN)

dep_map_food1 <- world_dep_map(year_1,dep_food_all,c_GN)
dep_map_food2 <- world_dep_map(year_2,dep_food_all,c_GN)

dep_map_energy1 <- world_dep_map(year_1,dep_energy_all,c_GN)
dep_map_energy2 <- world_dep_map(year_2,dep_energy_all,c_GN)


### ECI difference SP and P

dep_all_eci <- dep_all %>%
  filter(country != "ROW") %>%
  filter(country %in% unique(eci$country)) %>%
  arrange(country,year) %>%
  add_column(eci = eci$hs_eci)

dep_all_eci$C_P_SP <- ctrys$C_P_SP[match(dep_all_eci$country, ctrys$Code)]

eci_P_SP <- dep_all_eci %>%
  select(c(country, year, eci, C_P_SP)) %>%
  group_by(year, C_P_SP) %>%
  summarize(across(where(is.numeric), mean)) %>%
  arrange(C_P_SP) %>%
  add_column(GN_dep_ratio_wt_av = c(val_P$wt_av, val_SP$wt_av)) %>%
  add_column(GN_dep_ratio_simple_av = c(val_P$av, val_SP$av))


ggplot(na.omit(eci_P_SP ), aes( x = year)) +
  geom_line(aes( y = GN_dep_ratio_wt_av, color = "dep ratio") ) +
  geom_line(aes( y = eci + 0.75, color = "eci (right axis)")) +
  scale_y_continuous(
        name = "GN dep ratio (wt avg)",
        sec.axis = sec_axis(~ . -0.75, name = "ECI (simple avg)")  # Reverse transformation for secondary axis
        ) +
  facet_wrap(~ C_P_SP, scales = "free") +
  ggtitle("ECI and Dependence ratio on global north imports (Periphery and Semi periphery)") +
  labs(caption = "GN dep ratio  = imports from GN / total imports\n
       GN dep ratio is taken as average weighted by population in each group of countries\n
       ECI is econmomic complexity index",
       color = "Series",
       labels = c("GN_dep_ratio","ECI") )


####### analyzing trade balances######

trade_GS <- get_trade_bal_mult(years, cntrys = c_GS$Code)

trade_GS$GN_dep_ratio <- dep_all$GN_dep_ratio

trade_vol_GS <- get_trade_vol_mult(years, cntrys = c_GS$Code)
trade_GS$trade_vol <- trade_vol_GS$trade_vol


ggplot(na.omit(trade_GS), aes( x = year, group = country)) +
  geom_line(aes(y = trade_vol/10, color = "trade vol (right axis)")) +
  geom_line(aes(y = -trade_bal, color = "trade deficit")) +
  scale_y_continuous(
     name = "",
     sec.axis = sec_axis(transform = ~ .*10 ,
                         name = "trade deficit")  # Reverse transformation for secondary axis
     ) +
  theme(text = element_text(size = 10),
       axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ country, scales = "free") +
  ggtitle(paste0("Trade volume and Trade deficit from", year_1," to ", year_2)) +
  labs(caption = "We're trying to assess absolute depndence of imports, regardless of the source of imports")



rrr <- na.omit(trade_GS) %>%
  group_by(country) %>%
  summarize(min_t = min(trade_bal),
            max_t = max(trade_bal),
            min_d = min(GN_dep_ratio),
            max_d = max(GN_dep_ratio),
            st_t = sqrt(var(trade_bal)),
            st_d = sqrt(var(GN_dep_ratio))
            ) %>%
  mutate(rat = (max_t - min_t)/(max_d - min_d), rat2 = st_t/st_d)

sp_plotter <- function(ccc, rat){

  plot <-
    ggplot(na.omit(trade_GS) %>% filter(country == ccc), aes( x = year)) +
      geom_line(aes(y = (GN_dep_ratio) * rat, color = "dep ratio (right axis)")) +
      geom_line(aes(y = -trade_bal, color = "trade deficit")) +
      scale_y_continuous(
        name = "trade deficit",
        sec.axis = sec_axis(transform = ~ ./rat ,
                            name = "dep ratio")
        ) +
    ggtitle(ccc, ) +
    theme(legend.position = "none",
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 7)
          )

  return(plot)
}

dep_def_plots <- map2(rrr$country, rrr$rat2, sp_plotter)

ggpubr::ggarrange(plotlist = dep_def_plots,
                  ncol = 7, nrow = 6,
                  common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(.,
                  top = paste0("GN dependence and Trade deficit from", year_1," to ", year_2),
                  bottom = text_grob("We're trying to assess depndence of GN imports here")
  )


#### trade deficit and ECI######

eci$trade_bal <- trade_GS %>%
  filter(country %in% unique(eci$country)) %>%
  arrange(country, year) %>%
  select(trade_bal) %>%
  unlist %>%
  unname

rrr <- na.omit(eci) %>%
  group_by(country) %>%
  summarize(min_t = min(trade_bal),
            max_t = max(trade_bal),
            min_d = min(hs_eci),
            max_d = max(hs_eci),
            st_t = sqrt(var(trade_bal)),
            st_d = sqrt(var(hs_eci))
  ) %>%
  mutate(rat = (max_t - min_t)/(max_d - min_d), rat2 = st_t/st_d)

sp_plotter <- function(ccc, rat){

  plot <-
    ggplot(na.omit(eci) %>% filter(country == ccc), aes( x = year)) +
    geom_line(aes(y = (hs_eci) * rat, color = "ECI (right axis)")) +
    geom_line(aes(y = -trade_bal, color = "trade deficit")) +
    scale_y_continuous(
      name = "trade deficit",
      sec.axis = sec_axis(transform = ~ ./rat ,
                          name = "ECI")
    ) +
    ggtitle(ccc, ) +
    theme(legend.position = "none",
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 7)
    )

  return(plot)
}

dep_def_plots <- map2(rrr$country, rrr$rat2, sp_plotter)

ggpubr::ggarrange(plotlist = dep_def_plots,
                  ncol = 7, nrow = 6,
                  common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(.,
                  top = paste0("ECI and Trade deficit from", year_1," to ", year_2),
                  bottom = text_grob("We're trying to assess ECI and deficit correlation")
  )



###attaching trade balance to population dataset for weighting ####

pop$trade <- trade_GS %>%
  filter(year != 2020) %>%
  filter( country %in% unique(pop$country)) %>%
  arrange(country, year) %>%
  select(trade_vol)



################GN ECI plot####

ggplot(na.omit(eci_GN), aes(y = hs_eci, x = year)) +
  geom_line() +
  facet_wrap(~ country, scales = "free")

###### k-means cluster to check C-P-SP structure#####
km_data <- read_csv(r"(C:\R_OT\Projects\IO_to_DLS\Data\UNESCO\unesco_sociecon_95.csv)")
 km_data_1 <-km_data %>%
   pivot_wider(id_cols = c(LOCATION, Country),
               names_from = DEMO_IND,
               values_from = Value) %>%
   rename(pop = `200101`, Code = LOCATION) %>%
   select(-c("SP_POP_GROW")) %>%
   mutate(gov_exp_cap = XTGOV_IMF/pop, .keep = "unused") %>%
   mutate(across(where(is.numeric), ~scale(.))) %>%
   filter(Code %in% ctrys$Code)

##### checking ECI weighted capital stock#####

 ggplot(na.omit(try), aes(y = log(eci_cap), x = year, color = isocode)) +
   geom_line() +
   facet_wrap(~ isocode, scales = "free")

######FIR analysis #######
FIR_all <- get_region_FIR(data_dir, years, c_GS$Code, c_GN$country)

FIR_all %>%
  filter(year ==1995 | year ==2020) %>%
  group_by(year, country) %>%
  summarize(FIR_all = weighted.mean(FIR_all,GO),
            FIR_region = weighted.mean(FIR_region, GO)
            ) %>%
  pivot_wider(names_from= year, values_from = c(FIR_all, FIR_region)) %>%
  mutate(FIR_r_diff = FIR_region_1995 - FIR_region_2020,
         FIR_a_diff = FIR_all_1995 - FIR_all_2020) %>%
  filter(FIR_r_diff > 0 & FIR_a_diff > 0) %>%
  arrange(desc(FIR_r_diff))


 ##FIR plots#####
FIR_plot <- FIR_all %>%
  group_by(country, year, sector) %>%
  summarize(FIR_all= weighted.mean(FIR_all,GO),
            FIR_region = weighted.mean(FIR_region,GO)
            ) %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  na.omit %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  #mutate(GN_FIRR = FIR_region/FIR_all) %>%
  group_by(C_P_SP, year, sector) %>%
  summarize(across(c(FIR_all, FIR_region), ~weighted.mean(.,pop))) %>%
  ggplot(aes(x = year) ) +
  ggpubr::theme_pubclean() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_line(aes(y = FIR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = FIR_all, colour = "All countries"), size = 1) +
  geom_ribbon(aes(ymin = FIR_region, ymax = FIR_all),fill = "orange", alpha = 0.5 ) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 1.4) +
  ylab("FIR (population-weighted average)") +
  facet_grid(C_P_SP ~ sector, scales = "fixed")


#####CFR analysis #####
CFR_all <- get_region_CFR(data_dir, years, c_GS$Code, c_GN$country)

#### CFR plots #####
CFR_plot <- CFR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  na.omit %>%
  group_by(C_P_SP, year, sector) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  summarize(across(c(CFR_all, CFR_region), ~weighted.mean(.,pop))) %>%
  ggplot(aes(x = year) ) +
  ggpubr::theme_pubclean() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_ribbon(aes(ymin = CFR_region, ymax = CFR_all), fill = "green", alpha = 0.2 ) +
  geom_line(aes(y = CFR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = CFR_all, colour = "All countries"), size = 1) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 1.4) +
  ylab("CFR (population-weighted average)") +
  facet_grid(C_P_SP ~ sector, scales = "fixed", axis.labels = "all")


FIR_CFR_graph <- ggpubr::ggarrange(FIR_plot, CFR_plot,
                                   ncol = 1, nrow = 2,
                                   common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                          top = paste0("FIR and CFR spread between Total and Core Output")
  )


CFR_FIR_IIP_IIC <- left_join(CFR_all, FIR_all, join_by("year"== "year", "country"=="country", "sector" == "sector")) %>%
  select(year, country, sector, CFR_all, CFR_region, FIR_all, FIR_region) %>%
  filter(sector %in% c("Nutrition","Energy", "Manufacturing")) %>%
  left_join(.,imp_int_all, join_by("year"== "year", "country"=="country", "sector" == "sector")) %>%
  left_join(.,imp_int_GN, join_by("year"== "year", "country"=="country", "sector" == "sector")) %>%
  left_join(., ctrys[c(1,4)], join_by("country" == "Code")) %>%
  mutate(CFR_r = CFR_region/CFR_all,
         FIR_r = FIR_region/FIR_all,
         IIC_r = GN_int_c/imp_int_c,
         IIP_r = GN_int_p/imp_int_p,
         .keep = "unused"
         )

CFR_FIR_IIP_IIC %>%
  filter(sector == "Manufacturing") %>%
  ggplot(.,aes(x = year )) +
  geom_line(aes(y = CFR_r, colour = "CFR"), alpha = 0.3, size = 1) +
  geom_line(aes(y = FIR_r, colour = "FIR"), alpha = 0.3, size = 1) +
  geom_line(aes(y = IIC_r, colour = "IIC"), alpha = 0.3, size = 1) +
  geom_line(aes(y = IIP_r, colour = "IIP"), alpha = 0.3, size = 1) +
  scale_color_manual(name = "", values = c("CFR" = "green", "FIR" = "orange",
                                           "IIC" = "blue", "IIP" = "red")
                     ) +
  ylab("") +
  theme(legend.position = "bottom") +
  facet_wrap(~country, scales = "fixed")

#### China plots #####
CHN_CFR_plot <- CFR_all %>%
  ungroup %>%
  filter(country == "CHN") %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  na.omit %>%
  ggplot(aes(x = year) ) +
  ggpubr::theme_pubclean() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_ribbon(aes(ymin = CFR_region, ymax = CFR_all), fill = "green", alpha = 0.2 ) +
  geom_line(aes(y = CFR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = CFR_all, colour = "All countries"), size = 1) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 0.6) +
  ylab("CFR") +
  theme(strip.text = element_blank()) +
  facet_wrap(~sector, scales = "fixed")

CHN_FIR_plot <- FIR_all %>%
  group_by(country, year, sector) %>%
  filter(country == "CHN") %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  summarize(FIR_all= weighted.mean(FIR_all,GO),
            FIR_region = weighted.mean(FIR_region,GO)
            ) %>%
  ungroup %>%
  na.omit %>%
  ggplot(aes(x = year) ) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_line(aes(y = FIR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = FIR_all, colour = "All countries"), size = 1) +
  geom_ribbon(aes(ymin = FIR_region, ymax = FIR_all),fill = "orange", alpha = 0.5 ) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 0.6) +
  ylab("FIR") +
  xlab("") +
  theme(strip.text = element_blank()) +
  facet_wrap( ~ sector, scales = "fixed")

CHN_IIC_plot <- imp_int_all %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
            ) %>%
  filter(country == "CHN") %>%
  filter(sector != "All") %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = GN_int_c, ymax = imp_int_c), fill = "blue", alpha = 0.1) +
  geom_line(aes(y = GN_int_c, colour = "Core countries"), size = 1) +
  geom_line(aes(y = imp_int_c, colour = "All countries"), size = 1) +
  ylim(0, 0.6) +
  ylab("IIC") +
  xlab("") +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  theme(strip.text = element_blank()) +
  facet_wrap( ~ sector, scales = "fixed")

CHN_IIP_plot <- imp_int_all %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
  ) %>%
  filter(country == "CHN") %>%
  filter(sector != "All") %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = GN_int_p, ymax = imp_int_p), fill = "red",alpha = 0.3) +
  geom_line(aes(y = GN_int_p, colour = "Core countries"), size = 1) +
  geom_line(aes(y = imp_int_p, colour = "All countries"), size = 1) +
  ylim(0, 0.6) +
  ylab("IIP") +
  xlab("") +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_wrap( ~ sector, scales = "fixed")


ggpubr::ggarrange(CHN_IIP_plot, CHN_IIC_plot, CHN_FIR_plot, CHN_CFR_plot,
                  ncol = 1, nrow = 4,
                  common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                          top = paste0("Spread between Total and Core Output across Metrics for China")
  )

imp_int_CHN <- get_region_imp_dep_ratio(data_dir, years, "CHN", c_GS$Code) %>%
  rename(CHN_int_c = imp_int_c, CHN_int_p = imp_int_p)

FIR_CHN <- get_region_FIR(data_dir, years, c_GS$Code[c_GS$Code != "CHN"], "CHN") %>%
  rename(FIR_CHN = FIR_region)

FIR_CHN_all <-  FIR_CHN %>%
  group_by(country, year) %>%
  summarize(FIR_all = weighted.mean(FIR_all,GO),
            FIR_CHN = weighted.mean(FIR_CHN,GO)
            )

CFR_CHN <- get_region_CFR(data_dir, years, c_GS$Code[c_GS$Code != "CHN"], "CHN") %>%
  rename(CFR_CHN = CFR_region)

CFR_CHN_all <- CFR_CHN %>%
  group_by(country, year) %>%
  summarize(CFR_all = weighted.mean(CFR_all,c_GO),
            CFR_CHN = weighted.mean(CFR_CHN,c_GO)
            )

FIR_GN <- FIR_all %>%
  group_by(country, year) %>%
  summarize(FIR_all = weighted.mean(FIR_all, GO),
            FIR_GN = weighted.mean(FIR_region, GO)
            ) %>%
  mutate(FIR_GN_r = FIR_GN/FIR_all, .keep = "unused")

CFR_GN <- CFR_all %>%
  group_by(country, year) %>%
  summarize(CFR_all = weighted.mean(CFR_all, c_GO),
            CFR_GN = weighted.mean(CFR_region,c_GO)
            ) %>%
  mutate(CFR_GN_r = CFR_GN/CFR_all, .keep = "unused")

metrics_CHN_1 <- left_join(FIR_CHN[c(1:5)], CFR_CHN,
                         join_by("year"=="year", "country" == "country", "sector" == "sector")
                         ) %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  left_join(.,imp_int_CHN, join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  left_join(imp_int_all, join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  mutate(FIR_CHN_r = FIR_CHN/FIR_all,
         CFR_CHN_r = CFR_CHN/ CFR_all,
         IIP_CHN_r = CHN_int_p/imp_int_p,
         IIC_CHN_r = CHN_int_c/imp_int_c,
         .keep = "unused"
         ) %>%
  left_join(.,FIR_all[c(1:5)], join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  left_join(.,CFR_all, join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  left_join(.,imp_int_GN, join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  left_join(imp_int_all, join_by("year"=="year", "country" == "country", "sector" == "sector")) %>%
  mutate(FIR_GN_r = FIR_region/FIR_all,
         CFR_GN_r = CFR_region/ CFR_all,
         IIP_GN_r = GN_int_p/imp_int_p,
         IIC_GN_r = GN_int_c/imp_int_c,
         .keep = "unused"
         )

metrics_CHN_2 <- left_join(filter(imp_int_CHN, sector == "All", country != "CHN"),
                         FIR_CHN_all,
                         join_by("year"=="year", "country" == "country")) %>%
  left_join(.,CFR_CHN_all, join_by("year"=="year", "country" == "country")) %>%
  mutate(FIR_CHN_r = FIR_CHN/FIR_all,
         CFR_CHN_r = CFR_CHN/CFR_all,
         .keep = "unused" ) %>%
  left_join(., filter(FIR_GN, country != "CHN")) %>%
  left_join(., filter(CFR_GN, country != "CHN")) %>%
  left_join(.,filter(imp_int_GN, country != "CHN", sector == "All"))


metrics_CHN_2 %>%
#  filter(country %in%
#           (filter(pop_wt, year == 2020) %>%
#              arrange(desc(value)) %>% head(11) %>% .$country)
#  ) %>%
  mutate(CFR_other_GS_r = 1 - CFR_GN_r - CFR_CHN_r) %>%
  select(year, country, sector, CFR_other_GS_r, CFR_GN_r, CFR_CHN_r) %>%
  pivot_longer(cols = c(CFR_other_GS_r, CFR_GN_r, CFR_CHN_r),
               names_to = "sfc", values_to = "CFR" ) %>%
  ggplot(.,aes(x = year)) +
  geom_area(aes(y = CFR, fill = sfc), alpha = 0.8) +
  facet_wrap(~country, scales = "fixed") +
  scale_fill_brewer(palette = "RdYlBu",
                    labels = c("China", "Global North", "Rest of Global South")
  ) +
  labs(fill="Share of Foreign Output")


metrics_CHN_2 %>%
  filter(country %in%
           (filter(pop_wt, year == 2020) %>%
              arrange(desc(value)) %>% head(11) %>% .$country)
         ) %>%
  mutate(FIR_other_GS_r = 1 - FIR_GN_r - FIR_CHN_r) %>%
  select(year, country, sector, FIR_other_GS_r, FIR_GN_r, FIR_CHN_r) %>%
  pivot_longer(cols = c(FIR_other_GS_r, FIR_GN_r, FIR_CHN_r),
               names_to = "sfo", values_to = "FIR"
               ) %>%
  ggplot(.,aes(x = year)) +
  geom_area(aes(y = FIR, fill = sfo), alpha = 0.8) +
  facet_wrap(~country, scales = "fixed", nrow = 3) +
  scale_fill_brewer(palette = "RdYlBu",
                    labels = c("China", "Global North", "Rest of Global South")
                    ) +
  labs(fill="Share of Foreign Output")
