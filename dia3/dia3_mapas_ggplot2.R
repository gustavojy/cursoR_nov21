install.packages("maps")

library(ggplot2)
library(maps)

# Bancos de dados
desnutricao <- read_csv("desnutricao.csv")

mapa_mundi <- map_data("world")

desnutri_mod <- desnutricao %>% 
  mutate(Value = if_else(Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2018-2020") %>% 
  rename(region = Area, perc_value = Value) %>% 
  select(region, perc_value)

mapa_desnutri <- left_join(mapa_mundi, desnutri_mod, by = "region")

ggplot(data = mapa_desnutri,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_value), color = "black")+
  scale_fill_gradient(low = "light blue",
                      high = "red")





