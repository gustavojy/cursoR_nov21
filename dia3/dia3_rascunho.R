##--------------------------DIA 3------------------------------

#------------------------Bibliotecas---------------------------

## install.packages("tidyverse")

library(tidyverse)

tidyverse_packages()

## library(dplyr)
## library(ggplot2)

#-------------------------Banco de dados-----------------------
## IMDB - Internet Movie Database
## link download: https://github.com/gustavojy/cursoR_nov21/blob/main/dia3_cursoR.zip?raw=true

(filmes <- read_csv("dia3/imdb.csv"))

view(filmes)

names(filmes)
unique(filmes)


#------------------------Selecionar colunas--------------------
select(filmes, titulo, ano)

select(filmes, duracao:generos)
select(filmes, 4:6)

select(filmes, starts_with("ator"))
# ends_with()
# contains()

select(filmes, -likes_facebook)
select(filmes, -(titulo:pais))
select(filmes, -starts_with("ator"))


#-----------------------Ordenar--------------------------------
## Por padrão, ordena em ordem crescente
arrange(filmes, orcamento)

## Ordem decrescente
arrange(filmes, desc(orcamento))

## Ordenar por mais de uma variável
arrange(filmes, desc(ano), desc(orcamento))


#-------------------------Pipe---------------------------------
## E se nós quisermos utilizar 2 funções juntas?
## pipe: facilita a programação; conecta funções;
##       a ordem importa.
## símbolo: %>% 
## Atalho: crtl + shift + M
filmes %>% 
  arrange(desc(receita)) %>% 
  select(titulo, ano, receita)

## Exemplo de como faria sem pipe
### Opção 1: função dentro de função confunde...
select(arrange(filmes, desc(receita)), titulo, ano, receita)

####( g( f(x) ) )


### Opção 2: imagina se fosse pra criar 10 funções...
ordenar_receita <- arrange(filmes, desc(receita))
select(ordenar_receita, titulo, ano, receita)



#-----------------Filtrar valores das linhas-------------------
## Filtrar somente filmes com notas maiores que 9
filter(filmes, nota_imdb > 9)

### Filtrar sem o tidyverse
filmes$nota_imdb[filmes$nota_imdb > 9]

## Aplicando junto com a select
filmes %>% 
  filter(nota_imdb > 9) %>% 
  select(titulo, ano, nota_imdb)

## Aplicando mais de um filtro
filmes %>% 
  filter(diretor == "Steven Spielberg", nota_imdb > 8) %>% 
  select(titulo, diretor, ano, nota_imdb)

## Filtros a partir de operações entre colunas
filmes %>% 
  filter(receita - orcamento > 0) %>% 
  select(titulo, ano, orcamento, receita)

## Filtrar multiplos valores
filmes %>% 
  filter(ator_1 %in% c("Angelina Jolie Pitt", "Brad Pitt")) %>% 
  select(titulo, ano, ator_1:ator_3)

## Filtrar a partir de um padrão de texto
unique(filmes$generos)

filmes %>% 
  filter(stringr::str_detect(generos, "Documentary"))

filmes %>% 
  filter(stringr::str_detect(titulo, "Spider"))


#--------------Modificar e criar colunas-------------------
## Transformar coluna `duracao` de minutos para horas
filmes %>% 
  mutate(duracao = duracao/60) %>% 
  select(titulo, duracao)

## Criando nova coluna com o mesmo exemplo
filmes %>% 
  mutate(duracao_horas = duracao/60) %>% 
  select(titulo, duracao, duracao_horas)

## Criando nova coluna com comprimento=1
filmes %>% 
  mutate(lucro = receita - orcamento,
         pais = "EUA") %>% 
  select(titulo, lucro, pais)


#---------------------Resumo de medidas--------------------
## Média
filmes %>% 
  summarise(media_orcamento = mean(orcamento, na.rm = TRUE))

filmes %>% 
  summarise(media_orcamento = mean(orcamento, na.rm = TRUE),
            mediana_orcamento = median(orcamento, na.rm = TRUE))


## Resumo de medidas agrupadas a partir de uma variável categ.
filmes %>% 
  group_by(diretor) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE))

## Agrupar com mais de uma variável categ.
filmes %>% 
  group_by(ano, classificacao) %>% 
  summarise(orcamento_medio = mean(orcamento, na.rm = TRUE))



#-----------------------Estudos de caso-------------------------
## Vamos analisar, graficamente, o lucro dos filmes

filmes %>% 
  select(receita, orcamento) %>% 
  mutate(lucro = receita-orcamento) %>% 
  ggplot()+
  geom_point(aes(x = orcamento, y = receita))

filmes %>% 
  select(receita, orcamento) %>% 
  mutate(lucro = receita-orcamento) %>% 
  ggplot()+
  geom_point(aes(x = orcamento,
                 y = receita,
                 color = lucro))


filmes %>% 
  select(receita, orcamento) %>% 
  mutate(lucro = receita-orcamento) %>% 
  ggplot()+
  geom_point(aes(x = orcamento,
                 y = receita,
                 color = lucro), size = 2)+
  scale_color_gradient(low = "light green",
                       high = "red")+
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank())


filmes %>% 
  select(receita, orcamento) %>% 
  mutate(lucro = receita-orcamento) %>% 
  ggplot(aes(x = orcamento,
             y = receita))+
  geom_point(aes(color = lucro))+
  scale_color_gradient(low = "light green",
                       high = "red")+
  geom_smooth(method = "lm", se = FALSE, color = "black")
# mostrar a ordem das geoms influenciando


filmes %>% 
  mutate(lucro = receita - orcamento,
         lucrou = ifelse(lucro >= 0, "Sim", "Não")) %>%
#  filter(!is.na(lucro)) %>% 
  ggplot()+
  geom_point(aes(x = orcamento,
                 y = receita, 
                 color = lucrou))+
  geom_smooth(aes(x = orcamento,
                  y = receita),
              method = "lm", se = FALSE)


## Vamos analisar as médias das notas ao longo do tempo
filmes %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot()+
  geom_line(aes(x = ano,
                y = nota_media))


filmes %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot()+
  geom_line(aes(x = ano,
                y = nota_media))+
  scale_y_continuous(limits = c(0, 9),
                     breaks = seq(0, 9, by = 1))


### Nota média do ator "Sylvester Stallone"
filmes %>% 
  filter(ator_1 %in% c("Sylvester Stallone")) %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano,
             y = nota_media))+
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, by = 1))+
  scale_x_continuous(limits = c(1978, 2015),
                     breaks = seq(1978, 2015, 1))+
  theme(axis.text.x = element_text(angle = -90))


#### Com legendas
filmes %>% 
  filter(ator_1== "Sylvester Stallone") %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  mutate(nota_media = round(nota_media, 1)) %>% 
  ggplot(aes(x = ano,
             y = nota_media))+
  geom_line()+
  geom_label(aes(label = nota_media))+
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, by = 1))+
  scale_x_continuous(limits = c(1978, 2015),
                     breaks = seq(1978, 2015, 1))+
  theme(axis.text.x = element_text(angle = -90))



## Os diretores que mais fizeram filmes (presentes no banco de dados)
filmes %>% 
  count(diretor) %>% 
  top_n(10, n) %>% 
  ggplot()+
  geom_col(aes(x = diretor, y = n))

filmes %>% 
  count(diretor) %>% 
    filter(!is.na(diretor)) %>% 
  top_n(10, n) %>% 
  ggplot()+
  geom_col(aes(x = diretor,
               y = n,
               fill = diretor))

filmes %>% 
  count(diretor) %>% 
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>% 
  ggplot()+
    geom_col(aes(x = diretor,
                 y = n,
                 fill = diretor),
             show.legend = FALSE)


filmes %>% 
  count(diretor) %>% 
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>% 
    mutate(diretor = forcats::fct_reorder(diretor, n)) %>% 
  ggplot()+
  geom_col(aes(x = diretor,
               y = n,
               fill = diretor),
           show.legend = FALSE)


filmes %>% 
  count(diretor) %>% 
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>% 
  mutate(diretor = forcats::fct_reorder(diretor, n)) %>% 
  ggplot()+
  geom_col(aes(x = diretor,
               y = n,
               fill = diretor),
           show.legend = FALSE)+
  geom_label(aes(label = n, x = diretor, y = n/2))+
  coord_flip()


# Distribuição de lucro dos diretores que produziram >=15 filmes

filmes %>% 
  filter(!is.na(diretor)) %>% 
  group_by(diretor) %>% 
  filter(n() >= 15) %>% 
  mutate(lucro = receita - orcamento) %>% 
  ggplot(aes(x = diretor,
             y = lucro))+
  geom_boxplot()




