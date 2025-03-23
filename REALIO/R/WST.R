### UN votes analysis #####
library(unvotes)

icsd <- read_csv(r"(C:\R_OT\Projects\IO_to_DLS\Data\IMF\ICSD\ICSD2021.csv)")  %>%
  mutate(income = recode(income,
                         `Low Income Developing Countries` = "Low (LIDC)",
                         `Emerging Market Economies` = "Mid (EME)",
                         `Advanced Economies` = "High (AE)")
         )


unsd <- read_delim(paste0(data_dir, "/UNSD.csv"), delim = ";") %>%
  mutate(M49 = as.numeric(`M49 Code`))

pop_d <- read_csv(r"(C:\R_OT\Projects\IO_to_DLS\Data\IMF\WEO_pop\pop.csv)")

ctrys <- read_csv(paste0(data_dir, "/ICIO_BEC_conc.csv")) %>%
  select(c(Code, countries, GS_GN, C_P_SP))

ctrys$countries[14] <- "Ivory Cost"  #removing special characters for kable
ctrys$countries[71] <- "Turkiye"


un_data <- un_votes %>%
  mutate(vote = recode(vote, yes = 1, no = -1, abstain = 0)) %>%
  inner_join(., un_roll_calls[c(1,4)], by = "rcid") %>%
  mutate(date = lubridate::year(date)) %>%
  filter(date > 1994 & date < 2021)

voteUS <- un_data %>% filter(country_code == "US")

un_data <- inner_join(un_data, voteUS[c(1,4)], by = "rcid")

corUS <- un_data %>%
  group_by(country_code, country) %>%
  summarize(corUS = cor(vote.x, vote.y)) %>%
  inner_join(., unsd[c(11,12)], join_by(country_code == `ISO-alpha2 Code`)) %>%
  rename( Code = `ISO-alpha3 Code`) %>%
  arrange(desc(corUS))

corUS95 <- un_data %>%
  filter(date == 1995) %>%
  group_by(country_code, country) %>%
  summarize(corUS = cor(vote.x, vote.y)) %>%
  inner_join(., unsd[c(11,12)], join_by(country_code == `ISO-alpha2 Code`)) %>%
  rename( Code = `ISO-alpha3 Code`) %>%
  arrange(desc(corUS))

corUS19 <- un_data %>%
  filter(date == 2019) %>%
  group_by(country_code, country) %>%
  summarize(corUS = cor(vote.x, vote.y)) %>%
  inner_join(., unsd[c(11,12)], join_by(country_code == `ISO-alpha2 Code`)) %>%
  rename( Code = `ISO-alpha3 Code`) %>%
  arrange(desc(corUS))


icsd95 <- filter(icsd, year == 1995) %>%
  select(isocode, GDP_rppp, income, kgov_rppp, kpriv_rppp, kppp_rppp) %>%
  replace_na(list(kgov_rppp = 0,
                  kpriv_rppp = 0,
                  kppp_rppp = 0)
             ) %>%
  mutate(capstock = kgov_rppp + kpriv_rppp + kppp_rppp, .keep = "unused")


icsd19 <- filter(icsd, year == 2019) %>%
  select(isocode, GDP_rppp, income, kgov_rppp, kpriv_rppp, kppp_rppp) %>%
  replace_na(list(kgov_rppp = 0,
                  kpriv_rppp = 0,
                  kppp_rppp = 0)
  ) %>%
  mutate(capstock = kgov_rppp + kpriv_rppp + kppp_rppp, .keep = "unused")

pop95 <-  pop_d %>%
  select(SERIES_CODE, `1995`) %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}"), .keep = "unused") %>%
  relocate(country) %>%
  rename(pop = `1995`)

pop19 <-  pop_d %>%
  select(SERIES_CODE, `2019`) %>%
  mutate(country = str_extract(SERIES_CODE, "^.{3}"), .keep = "unused") %>%
  relocate(country) %>%
  rename(pop = `2019`)

corUS_95 <- corUS95 %>%
  inner_join(., icsd95, join_by(Code == isocode)) %>%
  inner_join(.,pop95, join_by(Code == country)) %>%
  na.omit %>%
  arrange(desc(corUS)) %>%
  mutate(gdp_cap = GDP_rppp/pop) %>%
  mutate(capst_cap = capstock/pop)

corUS_19 <- corUS19 %>%
  inner_join(., icsd19, join_by(Code == isocode)) %>%
  inner_join(.,pop19, join_by(Code == country)) %>%
  na.omit %>%
  arrange(desc(corUS)) %>%
  mutate(gdp_cap = GDP_rppp/pop) %>%
  mutate(capst_cap = capstock/pop)


corUS_19 %>%
  ungroup %>%
  #slice(2:(n()-1)) %>%
  filter(Code %in% ctrys$Code) %>%
  ggplot(aes(y = gdp_cap, x = (corUS))) +
  geom_point(aes(size = pop, colour = income)) +
  geom_smooth(method='loess', fullrange=TRUE, color = "black")

corUS19 %>%
  filter(Code %in% ctrys$Code) %>%
  ggplot(aes(x=corUS)) +
  geom_histogram(stat="bin",binwidth=.05, color="black",fill="steelblue")+
  geom_density(aes(y=after_stat(density)*(length(corUS$corUS[-1]) * 0.05)),alpha=0.6, fill="gray") +
  ylab("Count")

corUS_19 %>%
  filter(pop > 4.9 ) %>%
  ggplot(aes(x=(GDP_rppp/pop))) +
  geom_histogram(stat="bin",binwidth=2, color="black",fill="steelblue")+
  geom_density(aes(y=after_stat(density)*(length(corUS$corUS) * 2)), bw = 3 ,alpha=0.6, fill="gray") +
  ylab("Count")


corUS_19 %>%
  ungroup %>%
  mutate(gc = (GDP_rppp/pop)) %>%
  arrange(gc) %>%
  filter(pop > 1) %>%
  slice(5:(n()-4)) %>%
  ggplot(aes(x=(gc))) +
  geom_histogram(stat="bin",binwidth=2, color="black",fill="steelblue")+
  geom_density(aes(y=after_stat(density)*((140) * 2)),
               bw = 2 ,alpha=0.6, fill="gray") +
  ylab("Count")


#### clustering algorithms ####

set.seed(123)

trydf <- corUS_19 %>%
  ungroup %>%
  filter(pop > 10) %>%
  #slice(2:(n()-1)) %>%
  filter( Code != "USA") %>%
  select(country, corUS, gdp_cap) %>%
  mutate(across(where(is.numeric), scale)) %>%
  as.data.frame(trydf)

rownames(trydf) <- trydf$country

trydf <- trydf[-1]


factoextra::fviz_nbclust(trydf, kmeans, method = "wss")
factoextra::fviz_nbclust(trydf, kmeans, method = "silhouette")


gap_stat <- cluster:: clusGap(trydf,
                              FUN = kmeans, nstart = 100, K.max = 10, B = 50)

factoextra::fviz_gap_stat(gap_stat)

k2 <- kmeans(trydf, 2, nstart = 50)
k3 <- kmeans(trydf, 3, nstart = 50)

factoextra::fviz_cluster(k2, data = trydf)
factoextra::fviz_cluster(k3, data = trydf)


