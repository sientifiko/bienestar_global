


library(tidyverse)
library(imputeTS)
library(countrycode)
library(patchwork)

theme_set(theme_bw())
options(scipen = 999)

pib <- readxl::read_excel("data/mpd2020.xlsx", sheet = 3) %>% 
  select(1, 3, 4, 5) %>% 
  filter(year >= 1500)

for (pais in unique(pib$countrycode)) {
  pib$gdppc_hat[pib$countrycode == pais] <- na_kalman(pib$gdppc[pib$countrycode==pais],
                                                      model = "StructTS")
}

pib$continente <- countrycode(pib$countrycode,
                              origin = "iso3c",
                              destination = "continent")




pib %>% 
  filter(year >= 1830,
         !is.na(continente)) %>% 
  group_by(year) %>% 
  summarise(p90 = quantile(gdppc_hat, .9,na.rm = T),
            p10 = quantile(gdppc_hat, .1,na.rm = T)) %>% 
  mutate(ratio = p90/p10)-> asd 
  ggplot() +
  aes(year, ratio) +
  geom_line(size = .9) +
  labs(x="",
       y="90/10")


pib %>% 
  filter(year == 1830,
         gdppc_hat <= 901.3000)

pib %>% 
  filter(year == 1830,
         gdppc_hat >= 2771.575)


pib %>% 
  filter(year == 2000,
         gdppc_hat <= 1200.2087) %>% 
  arrange(desc(gdppc_hat))

pib %>% 
  filter(year == 2000,
         gdppc_hat >= 33388.483) %>% 
  arrange(gdppc_hat)


# ======= EV

vida <- read.csv("data/life-expectancy.csv")

colnames(vida)[4] <- "LE"

vida$continente <- countrycode(vida$Code,
                               origin = "iso3c",
                               destination = "continent")



paises <- vida %>% 
  filter(Year < 1950,
         !is.na(continente)) %>% 
  pull(Code) %>% 
  unique()




vida %>% 
  filter(Year >= 1890,
         Code %in% paises,
         continente != "Africa") %>% 
  group_by(Year) %>% 
  summarise(p90 = quantile(LE, .9,na.rm = T),
            p10 = quantile(LE, .1,na.rm = T)) %>% 
  mutate(ratio = p90/p10) %>%  
ggplot() +
  aes(Year, ratio) +
  geom_line(size = .9) +
  geom_ribbon(aes(xmin = 1914, xmax = 1945), alpha = .3, fill = "red") +
  labs(x="",
       y="90/10")


vida %>% 
  filter(Year == 1890,
         LE <= 35.212) %>% 
  arrange(desc(LE))

vida %>% 
  filter(Year == 1890,
         LE >= 48.062) %>% 
  arrange(LE)

vida %>% 
  filter(Year == 1921,
         LE <= 24.656) %>% 
  arrange(desc(LE))

vida %>% 
  filter(Year == 1921,
         LE >= 61.004) %>% 
  arrange(LE)

vida %>% 
  filter(Year == 2020,
         LE <= 66.000) %>% 
  arrange(desc(LE))

vida %>% 
  filter(Year == 2020,
         LE >= 82.400) %>% 
  arrange(LE)



# vida %>% 
#   filter(Year >= 1830,
#          Code %in% paises) %>% 
#   group_by(Year, continente) %>% 
#   count() %>% 
#   ggplot() +
#   aes(Year, n, color = continente) +
#   geom_line()


# ========= ESTATURA 

estath <- read.csv("data/average-height-of-men.csv")
estatm <- read.csv("data/average-height-of-women.csv")

colnames(estath)[4] <- "hmale"
colnames(estatm)[4] <- "hfemale"

estath$continente <- countrycode(estath$Code,
                                 origin = "iso3c",
                                 destination = "continent")

estatm$continente <- countrycode(estatm$Code,
                                 origin = "iso3c",
                                 destination = "continent")

estath %>% 
  filter(Year %in% c(1896, 1996)) %>%
  mutate(Year = paste0("y", Year)) %>% 
  spread(Year, hmale) %>% 
  mutate(ratio = (y1996-y1896)/y1896) %>% 
  mutate(sexo = "Male") -> estath2

estatm %>% 
  filter(Year %in% c(1896, 1996)) %>%
  mutate(Year = paste0("y", Year)) %>% 
  spread(Year, hfemale) %>% 
  mutate(ratio = (y1996-y1896)/y1896) %>% 
  mutate(sexo = "Female") -> estatm2


library(fixest)


rbind(estath2,estatm2) %>% 
  filter(!is.na(continente)) %>% 
  ggplot() +
  aes(y1896, ratio, color = continente) +
  guides(color = "none") +
  geom_jitter() +
  facet_grid(continente~relevel(as.factor(sexo), ref = "Male")) +
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  labs(y = "Δestatura",
       x="Estatura en 1896 (cm.)",
       title = "Variación en la estatura global población adulta",
       subtitle = "Eje Y es tasa de variación en estatura entre 1886 a 1996. Cada punto es un país")


library(ggrepel)


estatura <- readxl::read_excel("Height_Broad.xlsx") %>% 
  gather("year", "estatura", 3:553) %>% 
  na.omit()

estatura$continente <- countrycode(estatura$ccode,
                                   "iso3n",
                                   "continent")

estatura$year <- as.numeric(estatura$year)

estatura %>% 
  filter(year >= 1800 & year <= 1820) %>% 
  group_by(`country name`, continente) %>% 
  summarise(avg1800 = mean(estatura, na.rm = T)) -> en1800


estatura %>% 
  filter(year >= 1980 & year <= 2000,
         `country name` %in% unique(en1800$`country name`)) %>% 
  group_by(`country name`, continente) %>% 
  summarise(avg2000 = mean(estatura, na.rm = T)) -> en2000

en1800 %>% 
  left_join(en2000, by = c("country name", "continente")) %>% 
  mutate(ratio = (avg2000 - avg1800)/avg1800) %>% 
  ggplot() +
  aes(avg1800, ratio, color = continente) +
  guides(color = "none") +
  geom_jitter() +
  geom_text_repel(aes(label = `country name`), size = 3, color = "black") + 
  geom_hline(yintercept = 0) +
  facet_wrap(.~continente) +
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  labs(y = "Δestatura",
       x="Estatura media entre 1800 a 1820 (cm.)",
       title = "Variación en la estatura global población adulta",
       subtitle = "Eje Y es tasa de variación en estatura entre c1800 y c2000. Cada punto es un país")

  





