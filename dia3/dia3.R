
# Instalar o pacote tidyverse
install.packages("tidyverse") # só precisa instalar 1 única vez

# Rodar o pacote
library(tidyverse)
library(ggplot2)

tidyverse_packages()

# Importar o documento "imdb.csv"
filmes <- read_csv("imdb.csv")

filmes

unique(filmes$pais)

## Manipular
library(dplyr)

### Seleção

select(filmes, titulo, diretor)

select(filmes, titulo:pais)
select(filmes, 1:7)

select(filmes, starts_with("ator"))
# ends_with()
# contains()


select(filmes, -likes_facebook)
select(filmes, -(titulo:pais))
select(filmes, -starts_with("ator"))


### Ordenar
#### Por padrão a função ordena em crescente
arrange(filmes, ano)

#### Ordem decrescente
arrange(filmes, desc(ano))

#### Utilizando mais de duas colunas para ordenar
arrange(filmes, desc(ano), desc(orcamento))

arrange(filmes, desc(orcamento), desc(ano))
# ou
arrange(filmes, desc(orcamento, ano))


### pipe
#### Sem o pipe
arrange(select(filmes, orcamento, receita), desc(receita))
#### g( f(x) )

#### com o pipe (%>%)
# Atalho no teclado ctrl + shift + M
filmes %>% 
  select(orcamento, receita) %>%
  arrange(desc(receita))

### Filtro
filmes %>% filter(classificacao == "Livre")

filmes %>% filter(duracao > 120)

#### Sem a função filter
filmes[filmes$duracao > 120, ]
# ou
subset(filmes, duracao>120)

#### Filtrar 1 diretor
filmes %>% 
  filter(diretor == "Michael Bay") %>% 
  select(titulo, ano, diretor) %>% 
  arrange(desc(ano))

#### Filtrar >1 diretor
filmes %>% 
  filter(diretor == c("Michael Bay", "Steven Spielberg"))

#### Filtro
filmes %>% 
  filter(receita - orcamento > 0)



### Resumo de medidas
filmes %>% summarise(media_orcamento = mean(orcamento, na.rm = TRUE))

filmes %>% summarise(contagem = is.na(orcamento))

#### Resumo de medidas agrupada
filmes %>% 
  group_by(ano) %>% 
  summarise(media_orcamento = mean(orcamento, na.rm = TRUE)) %>% 
  arrange(desc(ano))


### Criar e transformar colunas
#### Transformou uma coluna
filmes %>% mutate(duracao = duracao/60)

#### Criar nova coluna
filmes_transformado <- filmes %>%
  mutate(duracao_hr = duracao/60)


#### Resumindo
library(dplyr)
select()
arrange()
filter()
mutate()
summarise()
group_by()



### Analisar relação orçamento x receita

library(dplyr)
library(ggplot2)

filmes %>% 
  select(orcamento, receita) %>% 
  ggplot(aes(x = orcamento, y = receita)) +
  geom_point()+
  geom_smooth(method = "lm")

filmes %>% 
  select(orcamento, receita) %>% 
  ggplot(aes(x = orcamento, y = receita)) +
  geom_smooth(method = "lm")+
  geom_point()


filmes %>% 
  mutate(lucro = receita - orcamento) %>% 
  select(orcamento, receita, lucro) %>% 
  ggplot(aes(x = orcamento, y = receita, color = lucro))+
  geom_point()+
  scale_color_gradient(low = "red",
                       high = "green")



filmes %>% 
  mutate(lucro = receita - orcamento,
         lucrou = ifelse(lucro >= 0, "Sim", "Não")) %>%
  filter(!is.na(lucrou)) %>% 
  select(orcamento, receita, lucro, lucrou) %>% 
  ggplot(aes(x = orcamento, y = receita, color = lucrou))+
  geom_point()



### Analisar a nota média dos filmes ao longo do ano
filmes %>% 
  group_by(ano) %>% 
  summarise(media_notas = mean(nota_imdb, na.rm = FALSE)) %>% 
  select(ano, media_notas) %>% 
  ggplot(aes(x = ano, y = media_notas))+
  geom_line()+
  ylim(limits = c(0, 9))



### Filmes que não deram lucro
filmes %>% 
  mutate(lucro = receita - orcamento,
         lucrou = ifelse(lucro >= 0, "Sim", "Não")) %>%
  filter(!is.na(lucrou)) %>% 
  select(titulo, orcamento, receita, lucro, lucrou) %>% 
  filter(lucrou == "Sim") %>% 
  arrange(desc(lucro)) %>% 
  view()


