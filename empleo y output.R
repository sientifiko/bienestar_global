
library(forcats)


emp <- read.csv("employment-by-economic-sector.csv") %>% 
  na.omit()

colnames(emp)[4:6] <- c("services", "industry", "agriculture")

emp$total <- rowSums(emp[, 4:6])

emp$p_industry <- emp$industry/emp$total

emp$continente <- countrycode(emp$Code, 
                              origin = "iso3c",
                              destination = "continent")


asd <- emp %>% 
  filter(continente == "Americas")


emp %>% 
  filter(Year >= 1960) %>% 
  group_by(Year, continente) %>% 
  summarise(p_ind = sum(industry, na.rm = T)/sum(total, na.rm = T)) %>% 
  ggplot() +
  aes(Year, forecast::ma(p_ind, 5), color = continente) +
  guides(color = "none") +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  facet_wrap(.~continente) +
  labs(x="",
       y="",
       title = "Empleo en sector manufacturero",
       subtitle = "Porcentaje de empleo total. Media movil a 5 años") -> g1


emp %>% 
  filter(Year >= 1960) %>% 
  group_by(Year, continente) %>% 
  summarise(med = median(p_industry, na.rm = T),
            q75 = quantile(p_industry, .75, na.rm = T),
            q25 = quantile(p_industry, .25, na.rm = T),
            n = length(unique(Code))) -> asd
  ggplot() +
  aes(Year, med) +
  guides(color = "none") +
  geom_line(aes(color = continente),size = 1) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = .3, fill = "red") +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  facet_wrap(.~continente) +
  labs(x="",
       y="",
       title = "Empleo en sector manufacturero",
       subtitle = "Porcentaje de empleo total")





manuf <- readxl::read_excel("API_NV.IND.MANF.KD_DS2_en_excel_v2_5457823.xls",
                            sheet = 1, skip = 3) %>% 
  gather("anno", "valormanuf", 5:ncol(.))


manuf$anno <- as.numeric(manuf$anno) 

manuf$continente <- countrycode(manuf$`Country Code`,
                                origin = "iso3c",
                                destination = "continent")

manuf %>% 
  group_by(anno, continente) %>% 
  summarise(tot_manuf = sum(valormanuf, na.rm = T)) %>% 
  filter(continente %in% c("Americas", "Asia", "Europe"),
         anno < 2022) %>% 
  ggplot() +
  aes(anno, log(tot_manuf), color = continente) +
  guides(color = "none") +
  geom_line(size = 1) +
  facet_wrap(.~continente) +
  theme(strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  labs(x="",
       y="",
       title = "Valor agregado en manufactura",
       subtitle = "Precios constantes 2015 USD, en logarítmo natural",
       caption = "Fuente: OWID y BM\n@sientifiko1") -> g2

(g1/g2) 

manuf$valormanuf

manuf %>% 
  group_by(anno, continente) %>% 
  summarise(med = median(valormanuf, na.rm = T),
            q75 = quantile(valormanuf, .75, na.rm = T),
            q25 = quantile(valormanuf, .25, na.rm = T),
            n = length(unique(`Country Code`))) %>% 
  filter(continente %in% c("Americas", "Asia", "Europe"),
         anno < 2022) %>% 
  ggplot() +
  aes(anno, med) +
  guides(color = "none") +
  geom_line(aes(color = continente),size = 1) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = .3, fill = "red") +
  theme(strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(.~continente) +
  labs(x="",
       y="",
       title = "Valor agregado en manufactura",
       subtitle = "Precios constantes 2015 USD, en logarítmo natural",
       caption = "Fuente: OWID y BM\n@sientifiko1") 






