

library(tidyverse)
library(imputeTS)
library(countrycode)
library(patchwork)

theme_set(theme_bw())
options(scipen = 999)




informal <- readxl::read_excel("SDG_0831_SEX_ECO_RT_A_EN.xlsx", skip = 5) %>% 
  filter(Sex == "Total") %>% 
  select(1, 4, 5)


informal$iso <- countrycode(informal$`Reference area`,
                            origin = "country.name",
                            destination = "iso3c")

conso <- informal %>% 
  left_join(povertys2, 
            by = c("iso"="Code",
                             "Time"="year"))

conso$headcount_ratio_international_povline
conso$headcount_ratio_lower_mid_income_povline
conso$headcount_ratio_upper_mid_income_povline




conso %>% 
  filter(!is.na(continente)) %>%
  ggplot() +
  aes(Total, headcount_ratio_international_povline) +
  geom_jitter() +
  facet_wrap(.~continente) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(title = "Pobreza absoluta") -> g1

conso %>% 
  filter(!is.na(continente)) %>%
  ggplot() +
  aes(Total, headcount_ratio_lower_mid_income_povline) +
  geom_jitter() +
  facet_wrap(.~continente) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(title = "Pobreza al $3.65") -> g2

conso %>% 
  filter(!is.na(continente)) %>%
  ggplot() +
  aes(Total, headcount_ratio_upper_mid_income_povline) +
  geom_jitter() +
  facet_wrap(.~continente) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(title = "Pobreza al $6.85") -> g3

(g1 + g2 + g3)

colnames(conso)[9:11] <- c("absoluta", "pov3.65", "pov6.85")



conso %>% 
  filter(!is.na(continente)) %>%
  gather("tipopov", "valorpov", 9:11) %>% 
  ggplot() +
  aes(Total, valorpov, color = tipopov) +
  geom_jitter() +
  scale_color_manual(values = c("darkred", "darkgoldenrod" ,"darkgreen"),
                     labels = c("Absoluta", "$3.65 al día", "$6.85 al día")) +
  facet_wrap(.~continente) +
  theme(legend.position = c(.85, .25),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 20),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.caption = element_text(vjust = 15))+
  labs(title = "Pobreza y trabajo informal",
       subtitle = "Cada punto es un país y año",
       color = "Umbral pobreza",
       x="% trabajo informal",
       y= "% pobreza",
       caption = "Fuente: elaboración propia con base a Ilostat y PIP\n@sientifiko1") 

library(fixest)
library(forecast)

colnames(conso)[3] <- "informalidad"

feols(absoluta ~ informalidad + informalidad^2 | continente,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model1

feols(absoluta ~ informalidad + informalidad^2 | continente + Time,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model2

feols(absoluta ~ informalidad + informalidad^2 | continente + Time,
      # panel.id = c("iso", "Time"),
      vcov = "cluster",
      conso) -> model3



feols(pov3.65 ~ informalidad + informalidad^2 | continente,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model4

feols(pov3.65 ~ informalidad + informalidad^2 | continente + Time,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model5

feols(pov3.65 ~ informalidad + informalidad^2 | continente + Time,
      # panel.id = c("iso", "Time"),
      vcov = "cluster",
      conso) -> model6


feols(pov6.85 ~ informalidad + informalidad^2 | continente,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model7

feols(pov6.85 ~ informalidad + informalidad^2 | continente + Time,
      panel.id = c("iso", "Time"),
      vcov = "NW",
      conso) -> model8

feols(pov6.85 ~ informalidad + informalidad^2 | continente + Time,
      # panel.id = c("iso", "Time"),
      vcov = "cluster",
      conso) -> model9


etable(model1,
       model2, 
       model3)

etable(model4,
       model5, 
       model6)

etable(model7,
       model8, 
       model9)




conso %>% 
  filter(!is.na(continente)) %>%
  group_by(Time, continente) %>% 
  summarise(medinfor = median(Total, na.rm = T),
            p75 = quantile(Total, .75, na.rm = T),
            p25 = quantile(Total, .25, na.rm = T)) %>% 
  ggplot() +
  aes(as.integer(Time), medinfor, ymin = p25, ymax=p75) +
  geom_point() +
  geom_ribbon(fill = "red", alpha = .3) +
  facet_wrap(.~continente)
  



