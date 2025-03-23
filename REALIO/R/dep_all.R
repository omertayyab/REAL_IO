library(tidyverse)
library(ggplot2)
library(networkD3)
library(ggplot2)

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
  filter(year != 2020) %>%
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

# Overall Import Dependency -imports over final domestic consumption- tables

imp_to_final_ratio <- get_imp_dep_ratio(data_dir, years, c_GS$Code ) %>%
  arrange(country, year)

imp_to_final_energy <- get_imp_dep_ratio(data_dir, years, c_GS$Code, sector = "Energy" ) %>%
  arrange(country, year)
imp_to_final_food <- get_imp_dep_ratio(data_dir, years, c_GS$Code, sector = "Nutrition" ) %>%
  arrange(country, year)

ggplot(na.omit(imp_to_final_ratio), aes( x = year)) +
  geom_line(aes(y = imp_dep_ratio_prod, color = "prod")) +
  geom_line(aes(y = imp_dep_ratio_cons, color = "cons")) +
  ylab("Import Intensity ratio") +
  facet_wrap(~ country, scales = "fixed")

pop_wt <- pop_d %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}")) %>%
  select(7:34) %>%
  relocate(country) %>%
  pivot_longer(3:last_col(), names_to = "year") %>%
  filter(country %in% unique(imp_to_final_ratio$country)) %>%
  filter(year %in% years) %>%
  arrange(country, year)

imp_to_final_ratio$pop <- pop_wt$value
imp_to_final_energy$pop <- pop_wt$value
imp_to_final_food$pop <- pop_wt$value

imp_to_final_P <- imp_to_final_ratio %>%
  inner_join(.,ctrys[c(1,4)], join_by(country == Code)) %>%
  na.omit %>%
  group_by(C_P_SP, year) %>%
  summarize(avg_simple_c = mean(imp_dep_ratio_cons),
            avg_simple_p = mean(imp_dep_ratio_prod),
            avg_wt_c = sum(pop * imp_dep_ratio_cons)/sum(pop),
            avg_wt_p = sum(pop * imp_dep_ratio_prod)/sum(pop),
            pop = sum(pop)
            )

ggplot(na.omit(imp_to_final_P), aes( x = year)) +
  geom_line(aes(y = avg_simple_p, color = "simple avg import intenstiy of production"),alpha = 0.5) +
  geom_line(aes(y = avg_simple_c, color = "simple avg import intenstiy of consumption"), alpha = 0.5) +
  geom_line(aes(y = avg_wt_p, color = "wt avg import intenstiy of production")) +
  geom_line(aes(y = avg_wt_c, color = "wt avg import intenstiy of consumption"))  +
  scale_y_continuous(
    name = "") +
  facet_wrap(~ C_P_SP, scales = "fixed") +
  ggtitle("Import intensity of production and consumption")


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
  avg_dep_ratios(.,pop_d)

val_P <- dep_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d)


### Sector specific dependency ratio tables

val_energy <- avg_dep_ratios(dep_energy_all, pop_d)

val_energy_SP <- dep_energy_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d)

val_energy_P <- dep_energy_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d)

val_food <- avg_dep_ratios(dep_food_all, pop_d)

val_food_SP <- dep_food_all %>%
  filter(country %in% c_Semi_P$country) %>%
  avg_dep_ratios(.,pop_d)

val_food_P <- dep_food_all %>%
  filter(country %in% c_P$country) %>%
  avg_dep_ratios(.,pop_d)


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
                  top = paste0("GN dependece and Trade deficit from", year_1," to ", year_2),
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


 #### Jason plots #####
 ## dep ratio series for P and SP.  dep series by income per capita ##



