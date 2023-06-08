
library(tidyverse)
library(imputeTS)
library(countrycode)
library(patchwork)

theme_set(theme_bw())
options(scipen = 999)

pib <- readxl::read_excel("data/mpd2020.xlsx", sheet = 3) %>% 
  select(1, 3, 4, 5) %>% 
  filter(year >= 1500)

# pib2 <- pib %>% 
#   group_by(countrycode) %>% 
#   complete(year = full_seq(min(year):max(year), 1)) 


for (pais in unique(pib$countrycode)) {
  pib$gdppc_hat[pib$countrycode == pais] <- na_kalman(pib$gdppc[pib$countrycode==pais],
                                                    model = "StructTS")
}

for (pais in unique(pib$countrycode)) {
  pib$pop_hat[pib$countrycode == pais] <- na_interpolation(pib$pop[pib$countrycode==pais])
}


pib$continente <- countrycode(pib$countrycode,
                          origin = "iso3c",
                          destination = "continent")
# pib <- pib %>%
#   filter(!is.na(continente))


pib$continente2 <- countrycode(pib$countrycode,
                              origin = "iso3c",
                              destination = "region")

pib$continente3 <- countrycode(pib$countrycode,
                               origin = "iso3c",
                               destination = "un.regionsub.name")


# ================ EL PIB 
pib %>% 
  filter(!is.na(continente)) %>% 
  group_by(year, continente) %>% 
  summarise(median = median(gdppc_hat, na.rm = T),
            p75 = quantile(gdppc_hat, .75, na.rm = T),
            p25 = quantile(gdppc_hat, .25, na.rm = T)) %>% 
  ggplot() +
  aes(year, y = median) +
  geom_point(aes(color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  scale_y_continuous(trans = "log10") +
  theme(strip.text.x = element_text(size = 15)) +
  labs(x="",
       y="",
       title = "A) PIB per cápita en U$2011") -> pibpc





# ================ LA POBLACIÓN 

# pob <- read.csv("data/population.csv") %>% 
#   filter(Year >= 1500)
# 
# colnames(pob)[4] <- "pop"
# 
# pob$continente <- countrycode(pob$Code,
#                               origin = "iso3c",
#                               destination = "continent")
# 
# pob %>% 
#   filter(!is.na(continente)) %>% 
#   group_by(Year, continente) %>% 
#   summarise(median = quantile(pop, .5, na.rm = T),
#             p75 = quantile(pop, .75, na.rm = T),
#             p25 = quantile(pop, .25, na.rm = T)) %>% 
#   ggplot() +
#   aes(Year, y = median) +
#   geom_point(aes(color = continente), size = .5) +
#   geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
#   facet_wrap(.~continente) +
#   guides(fill = "none", color = "none") +
#   scale_y_continuous(trans = "log10") +
#   labs(x="",
#        y="PIB pc")



# ============== ESPERANZA DE VIDA 

vida <- read.csv("data/life-expectancy.csv")

colnames(vida)[4] <- "LE"

vida$continente <- countrycode(vida$Code,
                               origin = "iso3c",
                               destination = "continent")

vida %>% 
  filter(!is.na(continente)) %>%
  group_by(Year, continente) %>% 
  summarise(median = median(LE, na.rm = T),
            p75 = quantile(LE, .75, na.rm = T),
            p25 = quantile(LE, .25, na.rm = T)) %>% 
  ggplot() +
  aes(Year) +
  geom_point(aes(y = median, color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  scale_y_continuous(trans = "log10") +
  theme(strip.text.x = element_text(size = 15)) +
  labs(x="",
       y="",
       title = "B) Esperanza de vida al nacer") -> le


pibpc/le



# ============= DEATH GLOBALLY age

death <- read.csv("data/deaths-globally-by-age.csv")

colnames(death)[4:24] <- c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                           "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49",
                           "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74",
                           "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99",
                           "100 o más")

death <- death %>% 
  gather("rango", "cantidad", 4:24)


death$continente <- countrycode(death$Code,
                                origin = "iso3c",
                                destination = "continent")


death %>% 
  filter(!is.na(continente)) %>% 
  group_by(Year, continente, rango) %>% 
  mutate(totalrango = sum(cantidad)) -> death2

death2$rangoinv_w <- death2$cantidad * (1-(death2$cantidad/death2$totalrango))


death2$rango <- factor(death2$rango,
                       unique(death2$rango),
                       unique(death2$rango))


death2 %>% 
  group_by(Year, continente, rango) %>% 
  summarise(cantidad = sum(rangoinv_w)) %>% 
  group_by(Year, continente) %>% 
  mutate(cantidad_w = cantidad/sum(cantidad)) %>% 
  filter(Year %in% c(1950, 2020)) %>% 
  ggplot() +
  aes(rango, cantidad_w, fill = Year) +
  guides(fill = "none") +
  geom_col() +
  facet_grid(continente~Year) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5,
                                   size = 11),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13)) +
  labs(x="Rango de edad de muerte",
       y="% ponderado")



# ==================== CONVERGENCIAS Y DIVERGENCIAS PIB Y ESPERANZA DE VIDA

library(ineq)

pib %>% 
  filter(!is.na(continente)) %>% 
  group_by(year, continente) %>% 
  summarise(gini = ineq(gdppc_hat)) %>% 
  rbind(
    pib %>% 
      filter(!is.na(continente)) %>% 
      group_by(year) %>%
      summarise(continente = "World",
                gini = ineq(gdppc_hat))
  ) %>% 
  ggplot() +
  aes(year, forecast::ma(gini, 5), color = continente) +
  guides(color = "none") +
  geom_line(size = .9) +
  facet_wrap(.~continente)+
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5,
                                   size = 11),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13)) +
  labs(x="",
       y="",
       title = "A) Gini Pib per cápitas",
       subtitle = "Media movil a 5 años") -> gs1


pib %>% 
  filter(!is.na(continente)) %>%
  group_by(year, continente) %>% 
  summarise(cont_var = var(gdppc_hat, na.rm = T)) %>% 
  left_join(
    pib %>% 
      filter(!is.na(continente)) %>%
      group_by(year) %>% 
      summarise(world_var = var(gdppc_hat, na.rm = T)) ,
    by = "year"
  ) %>% 
  mutate(within = cont_var/(cont_var+world_var)) %>% 
  mutate(between = 1-within) %>% 
  gather("tipo", "varianza", 5:6) -> asd 
  ggplot() +
  aes(year, varianza, fill = tipo) %>% 
  geom_col() +
  facet_wrap(.~continente)+
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5,
                                   size = 11),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.position = c(.8, .2)) +
  labs(x="",
       y="",
       fill = "Fuente varianza",
       title = "B) Correlación intra clase PIB percápita") -> iccs1


vida %>% 
  filter(!is.na(continente)) %>% 
  group_by(Year, continente) %>% 
  summarise(gini = ineq(LE)) %>% 
  rbind(
    vida %>% 
      filter(!is.na(continente)) %>% 
      group_by(Year) %>%
      summarise(continente = "World",
                gini = ineq(LE))
  ) %>% 
  ggplot() +
  aes(Year, forecast::ma(gini, 5), color = continente) +
  guides(color = "none") +
  geom_line(size = .9) +
  facet_wrap(.~continente)+
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5,
                                   size = 11),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13)) +
  labs(x="",
       y="",
       title = "C) Gini Pib esperanza de vida",
       subtitle = "Media movil a 5 años") -> gs2


vida %>% 
  filter(!is.na(continente)) %>%
  group_by(Year, continente) %>% 
  summarise(cont_var = var(LE, na.rm = T)) %>% 
  left_join(
    vida %>% 
      filter(!is.na(continente)) %>%
      group_by(Year) %>% 
      summarise(world_var = var(LE, na.rm = T)) ,
    by = "Year"
  ) %>% 
  mutate(within = cont_var/(cont_var+world_var)) %>% 
  mutate(between = 1-within) %>% 
  gather("tipo", "varianza", 5:6) %>% 
  ggplot() +
  aes(Year, varianza, fill = tipo) %>% 
  geom_col() +
  facet_wrap(.~continente)+
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5,
                                   size = 11),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.position = c(.8, .2)) +
  labs(x="",
       y="",
       fill = "Fuente varianza",
       title = "D) Correlación intra clase esperanza de vida") -> iccs2




(gs1 + iccs1)/(gs2 + iccs2)



asd %>% 
  filter(continente == "Americas",
         tipo == "within") -> temp

asd %>% 
  filter(continente == "Europe",
         tipo == "within") -> temp


asd %>% 
  filter(continente == "Asia",
         tipo == "within") -> temp




# ============== POBREZA

pov <- read.csv("data/pobreza.csv")


colnames(pov)



















# ================== LA DESIGUALDAD
pib %>% 
  group_by(year, continente) %>% 
  summarise(p10 = quantile(gdppc, .1, na.rm = T),
            p90 = quantile(gdppc, .9, na.rm = T)) %>% 
  mutate(ratio = p90/p10) %>% 
  ggplot() +
  aes(year, y = ratio) +
  geom_point(aes(color = continente), size = .5) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") 















## ====================================
#               EXTRAS




dat <- readxl::read_excel("gastosocial.xlsx")


dat %>% 
  ggplot() +
  aes(anno, perc, color = presidencia) +
  guides(color = "none") +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 30)) +
  facet_wrap(.~reorder(presidencia, anno), ncol = 7, scales = "free_x") +
  scale_x_continuous(breaks = seq(1990, 2020, 3)) +
  geom_hline(yintercept = mean(dat$perc)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
  labs(x="",
       y="",
       title = "Gasto social en Chile por presidencia",
       subtitle = "Total de gasto social como porcentaje del PIB",
       caption = "Fuente SOCX, OCDE\n@sientifiko1")



# cálculos y filtros rápidos
pib %>%
  filter(year == 2000,
         continente == "Americas") -> temps


pib %>%
  filter(continente == "Americas",
         year == 2000) -> temp

pib %>%
  filter(continente == "Americas",
         year == 1800) -> temp


# Africa
4456.431

pib %>%
  filter(continente == "Africa",
         year == 2000) -> temp

temp2 <- asd %>%
  filter(continente == "Europe",
         p25 >= 4456.431)


pib %>%
  filter(continente == "Europe",
         year == 1958) -> temp



