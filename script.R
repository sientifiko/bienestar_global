
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



# distribución esperanza de vida
vida %>% 
  filter(Year == 2020) %>% 
  ggplot() +
  aes(LE, y = ..count../sum(..count..)) +
  geom_histogram() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Esperanza de vida",
       y="",
       title = "Distribución de la esperanza de vida global al 2020")






# ============== POBREZA

library(mFilter)

temp <- vida %>% 
  select(Entity, Code) %>% 
  unique()

pov <- read.csv("data/pobreza.csv") %>% 
  filter(ppp_version == 2017,
         welfare_type != "",
         reporting_level == "national")

pov <- pov %>% 
  left_join(temp, by = c("country"="Entity"))

pov$continente <- countrycode(pov$Code,
                             origin = "iso3c",
                             destination = "continent")


povertys <- pov %>% 
  select(1, 2, 109, 110, 4, 7:10)


(povertys %>% 
  filter(welfare_type == "income") %>% 
  group_by(year, continente) %>% 
  summarise(median = median(headcount_ratio_international_povline),
            p25 = quantile(headcount_ratio_international_povline, .25),
            p75 = quantile(headcount_ratio_international_povline, .75)) %>% 
  ggplot() +
  aes(year) +
  geom_point(aes(y = median, color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  theme(strip.text.x = element_text(size = 15)) +
  labs(x="",
       y="",
       title = "A) Serie ingresos")
) +
(povertys %>% 
  filter(welfare_type == "consumption",
         !is.na(continente)) %>% 
  group_by(year, continente) %>% 
  summarise(median = median(headcount_ratio_international_povline),
            p25 = quantile(headcount_ratio_international_povline, .25),
            p75 = quantile(headcount_ratio_international_povline, .75)) %>% 
  ggplot() +
  aes(year) +
  geom_point(aes(y = median, color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  theme(strip.text.x = element_text(size = 15)) +
  labs(x="",
       y="",
       title = "B) Serie consumo")) +
  plot_annotation(title = "Pobreza absoluta",
                  subtitle = "Porcentaje de gente que vive con menos de $2.15 USD al día",
                  theme = theme(plot.title = element_text(size = 20),
                                plot.subtitle = element_text(size = 15))) 

povertys %>% 
  select(2:5, 7) %>% 
  spread(welfare_type, headcount_ratio_international_povline) %>% 
  filter(!is.na(continente)) %>% 
  ggplot() +
  aes(income, consumption) +
  geom_jitter() +
  # facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  theme(strip.text.x = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(x="Pobreza absoluta ingreso",
       y="Pobreza absoluta consumo",
       title = "Relación entre pobreza por consumo e ingreso",
       subtitle = "Cada punto es un país y año. Representa % de personas bajo umbral de $2.15 USD al día")



lapply(unique(povertys$Code), function(x){
  
  temp <- povertys %>% 
    filter(Code == x) %>% 
    group_by(welfare_type) %>% 
    summarise(n = n()) %>%  
    filter(n == max(n)) %>% 
    pull(welfare_type)
  
  data.frame(pais = x, tipo = temp)
  
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> mainsrvy

mainsrvy <- mainsrvy %>% 
  filter(!(pais == "SYC" & tipo == "consumption"))


povertys <- povertys %>% 
  left_join(mainsrvy, by = c("Code" = "pais"))

povertys <- povertys %>% 
  filter(welfare_type == tipo)

lapply(unique(povertys$Code), function(i){
  
  temp <- povertys %>% 
    select(Code, year, 
           headcount_ratio_international_povline,
           headcount_ratio_lower_mid_income_povline,
           headcount_ratio_upper_mid_income_povline) %>% 
    filter(Code == i)
  
  temp$pov1_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_international_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  temp$pov2_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_lower_mid_income_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  temp$pov3_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_upper_mid_income_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  
  temp <- temp %>%
    select(1, 2, 6:8)
  
  temp
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> hd_hats

colnames(hd_hats)[3:5] <- c("pov1", "pov2", "pov3")

povertys2 <- povertys %>% 
  left_join(hd_hats, 
            by = c("Code", "year"))


povertys2 %>% 
  group_by(continente, welfare_type) %>% 
  count() %>% 
  spread(welfare_type, n)

povertys2$headcount_ratio_international_povline



((povertys2 %>%
  filter(continente %in% c("Africa", "Asia", "Oceania"),
         welfare_type == "consumption") %>%
  group_by(year, continente) %>%
  summarise(median = median(headcount_ratio_international_povline, na.rm = T),
            p25 = quantile(headcount_ratio_international_povline, .25, na.rm = T),
            p75 = quantile(headcount_ratio_international_povline, .75, na.rm = T)) %>%
  ggplot() +
  aes(year) +
  geom_point(aes(y = median, color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  theme(strip.text.x = element_text(size = 15)) +
  labs(x="",
       y="",
       title = "A) Línea de $2.15 USD al día"))/
  (
    povertys2 %>%
      filter(continente %in% c("Americas", "Europe"),
             welfare_type == "income") %>%
      group_by(year, continente) %>%
      summarise(median = median(headcount_ratio_international_povline, na.rm = T),
                p25 = quantile(headcount_ratio_international_povline, .25, na.rm = T),
                p75 = quantile(headcount_ratio_international_povline, .75, na.rm = T)) %>%
      ggplot() +
      aes(year) +
      geom_point(aes(y = median, color = continente), size = .5) +
      geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
      facet_wrap(.~continente) +
      guides(fill = "none", color = "none") +
      theme(strip.text.x = element_text(size = 15)) +
      labs(x="",
           y="")
  )) -> g1


((povertys2 %>%
    filter(continente %in% c("Africa", "Asia", "Oceania"),
           welfare_type == "consumption") %>%
    group_by(year, continente) %>%
    summarise(median = median(headcount_ratio_lower_mid_income_povline, na.rm = T),
              p25 = quantile(headcount_ratio_lower_mid_income_povline, .25, na.rm = T),
              p75 = quantile(headcount_ratio_lower_mid_income_povline, .75, na.rm = T)) %>%
    ggplot() +
    aes(year) +
    geom_point(aes(y = median, color = continente), size = .5) +
    geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
    facet_wrap(.~continente) +
    guides(fill = "none", color = "none") +
    theme(strip.text.x = element_text(size = 15)) +
    labs(x="",
         y="",
         title = "B) Línea de $3.65 USD al día"))/
  (
    povertys2 %>%
      filter(continente %in% c("Americas", "Europe"),
             welfare_type == "income") %>%
      group_by(year, continente) %>%
      summarise(median = median(headcount_ratio_lower_mid_income_povline, na.rm = T),
                p25 = quantile(headcount_ratio_lower_mid_income_povline, .25, na.rm = T),
                p75 = quantile(headcount_ratio_lower_mid_income_povline, .75, na.rm = T)) %>%
      ggplot() +
      aes(year) +
      geom_point(aes(y = median, color = continente), size = .5) +
      geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
      facet_wrap(.~continente) +
      guides(fill = "none", color = "none") +
      theme(strip.text.x = element_text(size = 15)) +
      labs(x="",
           y="")
  ) )-> g2


((povertys2 %>%
    filter(continente %in% c("Africa", "Asia", "Oceania"),
           welfare_type == "consumption") %>%
    group_by(year, continente) %>%
    summarise(median = median(headcount_ratio_upper_mid_income_povline, na.rm = T),
              p25 = quantile(headcount_ratio_upper_mid_income_povline, .25, na.rm = T),
              p75 = quantile(headcount_ratio_upper_mid_income_povline, .75, na.rm = T)) %>%
    ggplot() +
    aes(year) +
    geom_point(aes(y = median, color = continente), size = .5) +
    geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
    facet_wrap(.~continente) +
    guides(fill = "none", color = "none") +
    theme(strip.text.x = element_text(size = 15)) +
    labs(x="",
         y="",
         title = "C) Línea de $6.85 USD al día"))/
  (
    povertys2 %>%
      filter(continente %in% c("Americas", "Europe"),
             welfare_type == "income") %>%
      group_by(year, continente) %>%
      summarise(median = median(headcount_ratio_upper_mid_income_povline, na.rm = T),
                p25 = quantile(headcount_ratio_upper_mid_income_povline, .25, na.rm = T),
                p75 = quantile(headcount_ratio_upper_mid_income_povline, .75, na.rm = T)) %>%
      ggplot() +
      aes(year) +
      geom_point(aes(y = median, color = continente), size = .5) +
      geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
      facet_wrap(.~continente) +
      guides(fill = "none", color = "none") +
      theme(strip.text.x = element_text(size = 15)) +
      labs(x="",
           y="")
  ) )-> g3


((g1 + g2)/g3) +
  plot_layout(ncol = 2,
              heights = unit(c(5, 9), c("cm", "cm"))) +
  plot_annotation(title = "Tendencia global de la pobreza a distintos umbrales",
                  # caption = "Fuente: PIP\n@sientifiko1",
                  theme = theme(plot.title = element_text(size = 20)))




povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income") %>%
  group_by(year, continente) %>%
  summarise(median = median(headcount_ratio_international_povline, na.rm = T),
            p25 = quantile(headcount_ratio_international_povline, .25, na.rm = T),
            p75 = quantile(headcount_ratio_international_povline, .75, na.rm = T)) -> low


povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income") %>%
  group_by(year, continente) %>%
  summarise(median = median(headcount_ratio_lower_mid_income_povline, na.rm = T),
            p25 = quantile(headcount_ratio_lower_mid_income_povline, .25, na.rm = T),
            p75 = quantile(headcount_ratio_lower_mid_income_povline, .75, na.rm = T)) -> mid


povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income") %>%
  group_by(year, continente) %>%
  summarise(median = median(headcount_ratio_upper_mid_income_povline, na.rm = T),
            p25 = quantile(headcount_ratio_upper_mid_income_povline, .25, na.rm = T),
            p75 = quantile(headcount_ratio_upper_mid_income_povline, .75, na.rm = T)) -> hi



povertys2 %>%
  filter(continente %in% c("Africa"),
         welfare_type == "consumption",
         pov1!= "") %>%
  group_by(Code) %>% 
  mutate(var = last(pov1) - first(pov1)) %>% 
  ggplot() +
  aes(year, pov1, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "A) Tendencia en pobreza abosluta") -> g1

povertys2 %>%
  filter(continente %in% c("Africa"),
         welfare_type == "consumption",
         pov2!= "") %>%
  group_by(Code) %>% 
  mutate(var = last(pov2) - first(pov2)) %>% 
  ggplot() +
  aes(year, pov2, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "B) Tendencia umbral $3.65") -> g2

povertys2 %>%
  filter(continente %in% c("Africa"),
         welfare_type == "consumption",
         pov3!= "") %>%
  group_by(Code) %>% 
  mutate(var = last(pov3) - first(pov3)) %>% 
  ggplot() +
  aes(year, pov3, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "C) Tendencia umbral $6.85") -> g3

(g1 + g2 + g3) +
  plot_annotation(title = "Tendencias en pobreza en África",
                  subtitle = "Rectas representan tendencia estimada por filtro Hodrick–Prescott parámetro suavización 100")



povertys2 %>% 
  filter(continente %in% c("Africa"),
         welfare_type == "consumption") %>% 
  ggplot() +
  aes(year, headcount_ratio_international_povline, color = Code) +
  geom_line()

unique(povertys2$pov1)

povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income",
         pov1 != "") %>% 
  group_by(Code) %>% 
  mutate(var = last(pov1) - first(pov1)) %>% 
  ggplot() +
  aes(year, pov1, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "A) Tendencia en pobreza abosluta") -> g1

povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income",
         pov2!= "") %>%
  group_by(Code) %>% 
  mutate(var = last(pov2) - first(pov2)) %>% 
  ggplot() +
  aes(year, pov2, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "B) Tendencia umbral $3.65") -> g2

povertys2 %>%
  filter(continente %in% c("Americas"),
         welfare_type == "income",
         pov3!= "") %>%
  group_by(Code) %>% 
  mutate(var = last(pov3) - first(pov3)) %>% 
  ggplot() +
  aes(year, pov3, color = ifelse(var > 0, "asd", "asdasd")) +
  guides(color = "none") +
  scale_color_manual(values = c("darkred", "green")) +
  geom_line(size = .9) +
  facet_wrap(.~reorder(Code, var)) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "C) Tendencia umbral $6.85") -> g3  

  
  
(g1 + g2 + g3) +
  plot_annotation(title = "Tendencias en pobreza en América",
                  subtitle = "Rectas representan tendencia estimada por filtro Hodrick–Prescott parámetro suavización 100")



t1980 <- povertys2 %>% 
  filter(year >= 1980 & year <= 1989) %>% 
  select(country, 7:9) %>% 
  group_by(country) %>% 
  summarise(avgabs = mean(headcount_ratio_international_povline, na.rm = T),
            avgmid = mean(headcount_ratio_lower_mid_income_povline, na.rm = T),
            avgup = mean(headcount_ratio_upper_mid_income_povline, na.rm = T)) %>% 
  as.data.frame()

rownames(t1980) <- t1980$country


t2010 <- povertys2 %>% 
  filter(year >= 2010 & year <= 2019 ,
         country %in% unique(t1980$country)) %>% 
  select(country, 7:9) %>% 
  group_by(country) %>% 
  summarise(avgabs = mean(headcount_ratio_international_povline, na.rm = T),
            avgmid = mean(headcount_ratio_lower_mid_income_povline, na.rm = T),
            avgup = mean(headcount_ratio_upper_mid_income_povline, na.rm = T)) %>% 
  as.data.frame()

rownames(t2010)<- t2010$country


t1980$country <- NULL
t2010$country <- NULL

library(factoextra)
 
clust80 <- eclust(scale(t1980), "hclust", k = 5)
clust10 <- eclust(scale(t2010), "hclust", k = 5)

fviz_dend(clust80, rect = T, cex = .6, lwd = .5, horiz = T) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Cluster pobreza 1980-1989") -> g1 

fviz_dend(clust10, rect = T, cex = .6, lwd = .5, horiz = T) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Cluster pobreza 2010-2019") -> g2


# =========== POBREZA 2

pov2 <- readxl::read_excel("data/poverty CBN.xlsx", skip = 2, sheet = 1) %>% 
  gather("iso", "pobreza", 2:26)


pov2$continente <- countrycode(pov2$iso,
                               origin = "iso3c",
                               destination = "continent")

pov2$Year

pov2 %>% 
  group_by(Year, continente) %>%
  summarise(median = median(pobreza, na.rm = T),
            p25 = quantile(pobreza, .25, na.rm = T),
            p75 = quantile(pobreza, .75, na.rm = T)) %>%
  ggplot() +
  aes(Year) +
  geom_point(aes(y = median, color = continente), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = continente), alpha = .3) +
  facet_wrap(.~continente) +
  guides(fill = "none", color = "none") +
  theme(strip.text.x = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15)) +
  labs(title = "Pobreza en el muy largo plazo",
       subtitle = "Porcentaje de la población asalariada que no alcanza a comprar una canasta básica")



# ======= POBREZA RURAL

pov <- read.csv("data/pobreza.csv") %>% 
  filter(ppp_version == 2017,
         welfare_type != "",
         reporting_level == "rural")

pov <- pov %>% 
  left_join(temp, by = c("country"="Entity"))

pov$continente <- countrycode(pov$Code,
                              origin = "iso3c",
                              destination = "continent")


povertys <- pov %>% 
  select(1, 2, 109, 110, 4, 7:10)


lapply(unique(povertys$Code), function(x){
  
  temp <- povertys %>% 
    filter(Code == x) %>% 
    group_by(welfare_type) %>% 
    summarise(n = n()) %>%  
    filter(n == max(n)) %>% 
    pull(welfare_type)
  
  data.frame(pais = x, tipo = temp)
  
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> mainsrvy

mainsrvy <- mainsrvy %>% 
  filter(!(pais == "SYC" & tipo == "consumption"))


povertys <- povertys %>% 
  left_join(mainsrvy, by = c("Code" = "pais"))

povertys <- povertys %>% 
  filter(welfare_type == tipo)

lapply(unique(povertys$Code), function(i){
  
  temp <- povertys %>% 
    select(Code, year, 
           headcount_ratio_international_povline,
           headcount_ratio_lower_mid_income_povline,
           headcount_ratio_upper_mid_income_povline) %>% 
    filter(Code == i)
  
  temp$pov1_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_international_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  temp$pov2_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_lower_mid_income_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  temp$pov3_hat <- tryCatch({
    
    hpfilter(temp$headcount_ratio_upper_mid_income_povline,
             type = "lambda", 
             freq = 100)$trend
    
  }, error = function(x){
    return(NA)
  } )
  
  
  temp <- temp %>%
    select(1, 2, 6:8)
  
  temp
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> hd_hats

colnames(hd_hats)[3:5] <- c("pov1", "pov2", "pov3")

povertys2 <- povertys %>% 
  left_join(hd_hats, 
            by = c("Code", "year"))


povertys2 %>% 
  group_by(continente, welfare_type) %>% 
  count() %>% 
  spread(welfare_type, n)


povertys2 %>% 
  gather("tipo", "pov",11:13) %>% 
  filter(!is.na(pov)) %>% 
  ggplot() +
  aes(year, pov, color = tipo) +
  # guides(color = "none") +
  geom_line(size = 1) +
  facet_wrap(.~Code) +
  scale_color_manual(values = c("darkred", "darkgoldenrod" ,"darkgreen"),
                     labels = c("Absoluta", "$3.65 al día", "$6.85 al día")) +
  scale_x_continuous(breaks = seq(1980, 2020, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Pobreza en zonas rurales",
       subtitle = "Rectas representan tendencia estimada por filtro Hodrick–Prescott parámetro suavización 100",
       color = "Umbral pobreza")







  
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


asd %>% 
  filter(continente == "Americas",
         tipo == "within") -> temp

asd %>% 
  filter(continente == "Europe",
         tipo == "within") -> temp


asd %>% 
  filter(continente == "Asia",
         tipo == "within") -> temp


t1980$clust <- clust80$cluster


g1 + g2

povertys2 %>% 
  filter(country %in% c("Nigeria", "Lesotho",
                        "Botswana", "Ghana")) %>% 
  ggplot() +
  aes(year, headcount_ratio_international_povline, color = country) +
  guides(color = "none") +
  geom_line() -> g1


povertys2 %>% 
  filter(country %in% c("Nigeria", "Lesotho",
                        "Botswana", "Ghana")) %>% 
  ggplot() +
  aes(year, headcount_ratio_lower_mid_income_povline, color = country) +
  guides(color = "none") +
  geom_line() -> g2


povertys2 %>% 
  filter(country %in% c("Nigeria", "Lesotho",
                        "Botswana", "Ghana")) %>% 
  ggplot() +
  aes(year, headcount_ratio_upper_mid_income_povline, color = country) +
  # guides(color = "none") +
  geom_line() -> g3


g1 + g2 + g3

t2010$clust <- clust10$cluster

asd <- t2010 %>% 
  filter(clust == 1)

asd2 <- t2010 %>% 
  filter(clust == 2)
