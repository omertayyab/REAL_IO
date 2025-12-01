library(tidyverse)
library(ggplot2)
library(REALIO)
#library(networkD3)

data_dir <- "C:/R_OT/Projects/IO_to_DLS/Data/ICIO"

year_1 <- 1995
year_2 <- 2020

years <- seq(year_1, year_2)

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


GN_imp_ratio <- get_region_imp_ratio(data_dir, years, c_GN$country , c_GS$Code) %>%
  rename(GN_dep_ratio = reg_imp_ratio)

pop_d <- read_csv(paste0(data_dir, "/pop.csv"))

pop_wt <- pop_d %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}")) %>%
  select(7:34) %>%
  relocate(country) %>%
  pivot_longer(3:last_col(), names_to = "year") %>%
  filter(country %in% unique(GN_imp_ratio$country)) %>%
  filter(year %in% years) %>%
  arrange(country, year) %>%
  select(-SCALE.Name) %>%
  mutate(year = as.numeric(year)) %>%
  rename(pop = value)

GN_imp_ratio_P_SP <- GN_imp_ratio %>%
  left_join(.,pop_wt) %>%
  left_join(.,ctrys[c(1,4)], join_by("country" == "Code")) %>%
  na.omit

# Total Import Dependency vs GN import dependency graphs

imp_int_all <- get_imp_int_ratio(data_dir, years, c_GS$Code )
imp_int_GN <- get_region_imp_int_ratio(data_dir, years, c_GN$country, c_GS$Code) %>%
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

theme_set(ggpubr::theme_pubclean(base_size = 18))

prod_plot <- imp_int_all_P %>%
  filter(sector != "All") %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = avg_wt_p_GN, ymax = avg_wt_p), fill = "red",alpha = 0.3) +
  geom_line(aes(y = avg_wt_p_GN, colour = "Core countries"), linewidth = 1) +
  geom_line(aes(y = avg_wt_p, colour = "All countries"), linewidth = 1) +
  ylim(0, 0.6) +
  ylab("IIP (population-weighted average)") +
  xlab("Year") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
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
  xlab("Year") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_grid( C_P_SP ~ sector, scales = "fixed")


IIP_IIC_graph <- ggpubr::ggarrange(prod_plot, cons_plot,
                                   ncol = 1, nrow = 2,
                                   common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                          top = paste0("IIP and IIC spreads from", year_1," to ", year_2)
  )

## test plot for all sectors in country specific graphs##

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

## World Maps of Dependency


dep_map1 <- world_dep_map(year_1,GN_imp_ratio,c_GN)
dep_map2 <- world_dep_map(year_2,GN_imp_ratio,c_GN)

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

### Ratios of metrics plots###
theme_set(ggpubr::theme_pubclean(base_size = 14))

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
  xlab("Year") +
  theme(legend.position = "bottom", panel.border = element_rect(linetype = "solid", fill = NA)) +
  facet_wrap(~country, scales = "fixed")



#### China plots #####

theme_set(ggpubr::theme_pubclean(base_size = 18) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)))

CHN_CFR_plot <- CFR_all %>%
  ungroup %>%
  filter(country == "CHN") %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  na.omit %>%
  ggplot(aes(x = year) ) +
  geom_ribbon(aes(ymin = CFR_region, ymax = CFR_all), fill = "green", alpha = 0.2 ) +
  geom_line(aes(y = CFR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = CFR_all, colour = "All countries"), size = 1) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 0.6) +
  ylab("CFR") +
  xlab("Year") +
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


#### China share of metric plot###########

imp_int_CHN <- get_region_imp_int_ratio(data_dir, years, "CHN", c_GS$Code) %>%
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


theme_set(ggpubr::theme_pubclean(base_size = 14) +
            theme(panel.border = element_rect(linetype = "solid", fill = NA)))

metrics_CHN_2 %>%
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
  xlab("Year") +
  labs(fill="Share of Foreign Output") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#metrics_CHN_2 %>%
#  filter(country %in%
#           (filter(pop_wt, year == 2020) %>%
#              arrange(desc(value)) %>% head(11) %>% .$country)
#  ) %>%
#  mutate(FIR_other_GS_r = 1 - FIR_GN_r - FIR_CHN_r) %>%
#  select(year, country, sector, FIR_other_GS_r, FIR_GN_r, FIR_CHN_r) %>%
#  pivot_longer(cols = c(FIR_other_GS_r, FIR_GN_r, FIR_CHN_r),
#               names_to = "sfo", values_to = "FIR"
#  ) %>%
#  ggplot(.,aes(x = year)) +
#  geom_area(aes(y = FIR, fill = sfo), alpha = 0.8) +
#  facet_wrap(~country, scales = "fixed", nrow = 3) +
#  scale_fill_brewer(palette = "RdYlBu",
#                    labels = c("China", "Global North", "Rest of Global South")
#  ) +
#  labs(fill="Share of Foreign Output")


### Results Section Figures Calculation

# Section 4.1#
GN_imp_ratio_P_SP %>%
  group_by(year) %>%
  summarize(GN_dep_ratio_wt = weighted.mean(GN_dep_ratio, pop)) %>%
  filter(year == 1995 | year == 2020)
print("Population-weighted average of ratio of Northern imports to total imports of GS countries")

GN_imp_ratio_P_SP %>%
  group_by(year) %>%
  summarize(GN_dep_ratio = mean(GN_dep_ratio)) %>%
  filter(year == 1995 | year == 2020)
print("Simple average of ratio of Northern imports to total imports of GS countries")


# Section 4.2 #
imp_int_all_P %>%
  filter(year == 1995| year == 2020, sector != "All") %>%
  select(1:3,contains("wt")) %>%
  rename(IIC_total = avg_wt_c,IIP_total = avg_wt_p, IIC_GN = avg_wt_c_GN, IIP_GN = avg_wt_p_GN )


# Section 4.3 #

CFR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  na.omit %>%
  group_by(C_P_SP, year, sector) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  summarize(across(c(CFR_all, CFR_region), ~weighted.mean(.,pop))) %>%
  filter(year == 1995 | year == 2020)

FIR_all %>%
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
  group_by(C_P_SP, year, sector) %>%
  summarize(across(c(FIR_all, FIR_region), ~weighted.mean(.,pop))) %>%
  filter(year == 1995 | year == 2020)


#Section 4.4 ##

imp_int_all %>%
  left_join(.,ctrys[c(1,4)], join_by("country" == "Code")) %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
  ) %>%
  filter(sector == "Manufacturing", year == 2020) %>%
  mutate(IIC_ratio = GN_int_c/imp_int_c) %>%
  filter(IIC_ratio > .5) %>%
  .$country %>%
  length


# Section 4.5 ##

CFR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy", country == "CHN") %>%
  ungroup %>%
  filter(year == 2020)

CFR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy", country != "CHN") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  na.omit %>%
  group_by(C_P_SP, year, sector) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  summarize(across(c(CFR_all, CFR_region), ~weighted.mean(.,pop))) %>%
  filter(year == 2020)

FIR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy", country == "CHN") %>%
  ungroup %>%
  filter(year == 2020)

FIR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy", country != "CHN") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  na.omit %>%
  group_by(C_P_SP, year, sector) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  summarize(across(c(FIR_all, FIR_region), ~weighted.mean(.,pop))) %>%
  filter(year == 2020)


imp_int_all %>%
  left_join(.,ctrys[c(1,4)], join_by("country" == "Code")) %>%
  left_join(.,pop_wt, join_by("country" == "country", "year" == "year")) %>%
  left_join(.,imp_int_GN,
            join_by("country" == "country","sector" =="sector", "year" == "year")
  ) %>%
  na.omit() %>%
  filter(country != "CHN", year == 2020, sector != "All") %>%
  group_by(C_P_SP, year, sector) %>%
  summarize(avg_wt_c = weighted.mean(imp_int_c, pop),
            avg_wt_p = weighted.mean(imp_int_p, pop),
            avg_wt_c_GN = weighted.mean(GN_int_c, pop),
            avg_wt_p_GN = weighted.mean(GN_int_p, pop),
            pop = sum(pop, na.rm = TRUE)
  )

imp_int_all %>%
  filter(country == "CHN", year == 2020, sector != "All")

###Results Section Figures Concluded###


### Appendix plots #########

prod_plot_app <- imp_int_all_P %>%
  filter(sector != "All") %>%
  select(1:3, contains("simple")) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = avg_simple_p_GN, ymax = avg_simple_p), fill = "red",alpha = 0.3) +
  geom_line(aes(y = avg_simple_p_GN, colour = "Core countries"), linewidth = 1) +
  geom_line(aes(y = avg_simple_p, colour = "All countries"), linewidth = 1) +
  ylim(0, 0.7) +
  ylab("IIP (simple average)") +
  xlab("Year") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_grid( C_P_SP ~ sector, scales = "fixed")

cons_plot_app <- imp_int_all_P %>%
  filter(sector != "All") %>%
  select(1:3, contains("simple")) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  ggplot(., aes( x = year, group = sector)) +
  geom_ribbon(aes(ymin = avg_simple_c_GN, ymax = avg_simple_c), fill = "blue", alpha = 0.1) +
  geom_line(aes(y = avg_simple_c_GN, colour = "Core countries"), size = 1) +
  geom_line(aes(y = avg_simple_c, colour = "All countries"), size = 1) +
  ylim(0, 0.7) +
  ylab("IIC (simple average)") +
  xlab("Year") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black"))+
  facet_grid( C_P_SP ~ sector, scales = "fixed")


IIP_IIC_graph_app <- ggpubr::ggarrange(prod_plot_app, cons_plot_app,
                                   ncol = 1, nrow = 2,
                                   common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                          top = paste0("IIP and IIC spreads from", year_1," to ", year_2)
  )


FIR_plot_app <- FIR_all %>%
  group_by(country, year, sector) %>%
  summarize(FIR_all= weighted.mean(FIR_all,GO),
            FIR_region = weighted.mean(FIR_region,GO)
  ) %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  na.omit %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  #mutate(GN_FIRR = FIR_region/FIR_all) %>%
  group_by(C_P_SP, year, sector) %>%
  summarize(across(c(FIR_all, FIR_region), mean)) %>%
  ggplot(aes(x = year) ) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_line(aes(y = FIR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = FIR_all, colour = "All countries"), size = 1) +
  geom_ribbon(aes(ymin = FIR_region, ymax = FIR_all),fill = "orange", alpha = 0.5 ) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 2.7) +
  ylab("FIR (simple average)") +
  facet_grid(C_P_SP ~ sector, scales = "fixed")


CFR_plot_app <- CFR_all %>%
  filter(sector == "Manufacturing"| sector == "Nutrition"| sector =="Energy") %>%
  ungroup %>%
  left_join(.,ctrys[c("Code", "C_P_SP")], join_by("country"=="Code")) %>%
  na.omit %>%
  group_by(C_P_SP, year, sector) %>%
  mutate(C_P_SP = case_match(C_P_SP, "P" ~ "Periphery", "SP" ~ "Semi-Periphery")) %>%
  summarize(across(c(CFR_all, CFR_region), mean)) %>%
  ggplot(aes(x = year) ) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  geom_ribbon(aes(ymin = CFR_region, ymax = CFR_all), fill = "green", alpha = 0.2 ) +
  geom_line(aes(y = CFR_region, colour = "Core countries"), size = 1) +
  geom_line(aes(y = CFR_all, colour = "All countries"), size = 1) +
  scale_color_manual(name = "", values = c("Core countries" = "grey", "All countries" = "black")) +
  ylim(0, 2.7) +
  ylab("CFR (simple average)") +
  facet_grid(C_P_SP ~ sector, scales = "fixed", axis.labels = "all")


FIR_CFR_graph_app <- ggpubr::ggarrange(FIR_plot_app, CFR_plot_app,
                                   ncol = 1, nrow = 2,
                                   common.legend = TRUE, legend = "bottom") %>%
  ggpubr::annotate_figure(.,
                          top = paste0("FIR and CFR spread between Total and Core Output")
  )



CFR_FIR_IIP_IIC %>%
  filter(sector == "Nutrition") %>%
  ggplot(.,aes(x = year )) +
  geom_line(aes(y = CFR_r, colour = "CFR"), alpha = 0.3, size = 1) +
  geom_line(aes(y = FIR_r, colour = "FIR"), alpha = 0.3, size = 1) +
  geom_line(aes(y = IIC_r, colour = "IIC"), alpha = 0.3, size = 1) +
  geom_line(aes(y = IIP_r, colour = "IIP"), alpha = 0.3, size = 1) +
  scale_color_manual(name = "", values = c("CFR" = "green", "FIR" = "orange",
                                           "IIC" = "blue", "IIP" = "red")
  ) +
  ylab("") +
  xlab("Year") +
  theme(legend.position = "bottom", panel.border = element_rect(linetype = "solid", fill = NA)) +
  facet_wrap(~country, scales = "fixed")
