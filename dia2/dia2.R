
# Data frames
# Usando data frames existentes no R
View(iris)

# Criar data frames
dados_pessoas <- data.frame(
  nomes = c("Roseli", "Mauricio", "Jonathan"),
  idade = c(61, 21, 25),
  sexo = c("F", "M", "M")
)

View(dados_pessoas)

# Importar um data frame
install.packages("readr")
library(readr)

dados_alunos <- read_csv("dados_ggplot2/dados_alunos.csv")


# Gráficos em ggplot2
install.packages("ggplot2")
library(ggplot2)

# Gráfico de dispersão: altura x peso
ggplot(data = dados_alunos,
       aes(x = altura,
           y = peso,
           color = sexo,
           shape = sexo,
           size = idade,
           alpha = idade))+
  geom_point()

## Facetas (Facets)
ggplot(data = dados_alunos,
       aes(x = altura,
           y = peso,
           color = sexo))+
  geom_point()+
  facet_wrap(~sexo+futuro, nrow = 2) ## nrow para linha; ncol para coluna


# Gráfico de linhas
prod_milho <- read_csv("dados_ggplot2/produtiv_milho.csv")

ggplot(data = prod_milho)+
  geom_line(aes(x = Ano,
                y = Valor,
                color = Local))+
  geom_point(aes(x = Ano,
                 y = Valor),
             color = "black")


# Linhas de regressão
ggplot(data = prod_milho,
       aes(x = Ano,
           y = Valor,
           color = Local))+
  geom_line()+
  geom_smooth(method = "lm",
              level = 0.70,  # por padrão level = 0.95
              se = FALSE) +
  scale_x_continuous(breaks = seq(from = 1961,
                                  to = 2019,
                                  by = 1))+
  theme(axis.text.x = element_text(angle = -90))


# Gráfico de barras (colunas)
## geom_bar()
View(diamonds)
help("diamonds")

ggplot(data = diamonds,
       aes(x = cut,
           fill = cut))+
  geom_bar(color = "black")+
  coord_flip()+
  scale_fill_manual(values = c("black", "blue", "green",
                               "gold", "grey"))
#  scale_fill_brewer(palette = "Pastel1")

## Temas dos gráficos: theme_()
ggplot(data = diamonds,
       aes(x = cut,
           fill = cut))+
  geom_bar(color = "black")+
#  theme_classic()
#  theme_dark()
  theme_minimal()


## geom_col()
milho_trigo <- read_csv("dados_ggplot2/milho_trigo.csv")
View(milho_trigo) 

ggplot(data = milho_trigo,
       aes(x = Local,
           y = Valor,
           fill = Cultura))+
#  geom_col(position = "dodge")
#  geom_col(position = "fill")
  geom_col(position = "stack")+
  geom_text(aes(label = Valor),
            vjust = 1.5,
            position = position_stack(0.9))

# Gráficos de medidas-resumo
## Boxplot
ggplot(data = dados_alunos,
       aes(y = media_ponderada,
           x = sexo,
           fill = futuro))+
  geom_boxplot()

## Histograma
ggplot(data = dados_alunos,
       aes(x = media_ponderada))+
  geom_histogram(bins = 8,
                 fill = "light green",
                 color = "black")

## Densidade
ggplot(data = dados_alunos,
       aes(x = media_ponderada,
           fill = sexo))+
  geom_density(alpha = 0.5)

