

wages <- readxl::read_excel("LabourersRealWage_Broad.xlsx", sheet = 2)


wages$iso <- countrycode(wages$ccode,
                         origin = "iso3n",
                         destination = "iso3c")


wages$continente <- countrycode(wages$ccode,
                                origin = "iso3n",
                                destination = "continent")


wages$region <- countrycode(wages$ccode,
                            origin = "iso3n",
                            destination = "region")

wages$region <- factor(wages$region,
                       unique(wages$region)[c(6, 3, 1, 7, 4, 2, 5)])


wages %>% 
  filter(!is.na(region)) %>% 
  group_by(year, region) %>% 
  summarise(median = median(value, na.rm = T),
            p25 = quantile(value, .25, na.rm = T),
            p75 = quantile(value, .75, na.rm = T)) %>% 
  ggplot() +
  aes(year) +
  geom_point(aes(y = median, color = region), size = .5) +
  geom_ribbon(aes(ymax = p75, ymin = p25, fill = region), alpha = .3) +
  facet_wrap(.~region) +
  guides(fill = "none", color = "none") +
  scale_y_continuous(trans = "log10") +
  theme(strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  labs(x="",
       y="",
       title = "Salarios reales",
       subtitle = "Canastas de subsistencia básicas que un jornal diario puede comprar",
       caption = "Fuente: Clio Infra\n@sientifiko1")

asd <- wages %>% 
  filter(region == "East Asia & Pacific")


asd %>% 
  filter(region == "East Asia & Pacific",
         year >= 1950,
         !(iso %in% c("LAO", "SLB", "PNG"))) %>% 
  ggplot() +
  aes(year, value, color = iso, group = iso) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~iso) +
  scale_y_continuous(trans = "log10")
  
  
  
  
  
  
  
  
