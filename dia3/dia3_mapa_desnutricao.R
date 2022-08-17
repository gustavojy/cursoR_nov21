
# Bibliotecas
library(tidyverse)
library(maps)

# Base de dados
pou <- read_csv("dia3/desnutricao.csv")

mapa_mundi <- map_data("world")

##---------------------------------------------------------------
#------------------------2018-2020-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_18_20 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2018-2020") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi1 <- left_join(mapa_mundi, pou_18_20, by = "region")


### Mapa
ggplot(mapa_mundi1,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2018-2020\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2017-2019-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_17_19 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2017-2019") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi2 <- left_join(mapa_mundi, pou_17_19, by = "region")


### Mapa
ggplot(mapa_mundi2,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2017-2019\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2016-2018-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_16_18 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2016-2018") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi3 <- left_join(mapa_mundi, pou_16_18, by = "region")

### Mapa
ggplot(mapa_mundi3,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2016-2018\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2015-2017-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_15_17 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2015-2017") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi4 <- left_join(mapa_mundi, pou_15_17, by = "region")

### Mapa
ggplot(mapa_mundi4,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2015-2017\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2014-2016-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_14_16 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2014-2016") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi5 <- left_join(mapa_mundi, pou_14_16, by = "region")

### Mapa
ggplot(mapa_mundi5,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2014-2016\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2013-2015-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_13_15 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2013-2015") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi6 <- left_join(mapa_mundi, pou_13_15, by = "region")

### Mapa
ggplot(mapa_mundi6,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2013-2015\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2012-2014-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_12_14 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2012-2014") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi7 <- left_join(mapa_mundi, pou_12_14, by = "region")

### Mapa
ggplot(mapa_mundi7,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2012-2014\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2011-2013-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_11_13 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2011-2013") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi8 <- left_join(mapa_mundi, pou_11_13, by = "region")

### Mapa
ggplot(mapa_mundi8,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2011-2013\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2010-2012-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_10_12 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2010-2012") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi9 <- left_join(mapa_mundi, pou_10_12, by = "region")

### Mapa
ggplot(mapa_mundi9,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2010-2012\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2009-2011-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_09_11 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2009-2011") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi10 <- left_join(mapa_mundi, pou_09_11, by = "region")

### Mapa
ggplot(mapa_mundi10,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2009-2011\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2008-2010-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_08_10 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2008-2010") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi11 <- left_join(mapa_mundi, pou_08_10, by = "region")

### Mapa
ggplot(mapa_mundi11,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2008-2010\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2007-2009-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_07_09 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2007-2009") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi12 <- left_join(mapa_mundi, pou_07_09, by = "region")

### Mapa
ggplot(mapa_mundi12,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2007-2009\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2006-2008-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_06_08 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2006-2008") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi13 <- left_join(mapa_mundi, pou_06_08, by = "region")

### Mapa
ggplot(mapa_mundi13,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2006-2008\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2005-2007-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_05_07 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2005-2007") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi14 <- left_join(mapa_mundi, pou_05_07, by = "region")

### Mapa
ggplot(mapa_mundi14,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2005-2007\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2004-2006-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_04_06 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2004-2006") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi15 <- left_join(mapa_mundi, pou_04_06, by = "region")

### Mapa
ggplot(mapa_mundi15,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2004-2006\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2003-2005-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_03_05 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2003-2005") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi16 <- left_join(mapa_mundi, pou_03_05, by = "region")

### Mapa
ggplot(mapa_mundi16,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2003-2005\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2002-2004-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_02_04 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2002-2004") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi17 <- left_join(mapa_mundi, pou_02_04, by = "region")

### Mapa
ggplot(mapa_mundi17,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2002-2004\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

##---------------------------------------------------------------
#------------------------2001-2003-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_01_03 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2001-2003") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi18 <- left_join(mapa_mundi, pou_01_03, by = "region")


### Mapa
ggplot(mapa_mundi18,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2001-2003\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


##---------------------------------------------------------------
#------------------------2000-2002-------------------------------
##---------------------------------------------------------------

### Tidy e transformação
pou_00_02 <- pou %>% 
  mutate(Value = if_else (Value == "<2.5", "2.5", as.character(Value))) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  filter(Year == "2000-2002") %>% 
  rename(region = Area, perc_undernou = Value) %>% 
  select(region, perc_undernou)

### Join "pou" e mapa
mapa_mundi19 <- left_join(mapa_mundi, pou_00_02, by = "region")

### Mapa
ggplot(mapa_mundi19,
       aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = perc_undernou), color = "black")+
  scale_fill_gradient(name = "2000-2002\nPrev. desnutrição (%)",
                      low = "light blue",
                      high = "red",
                      na.value = "light grey")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
