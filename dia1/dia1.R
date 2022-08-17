#Comentário
require(stats) # for lowess, rpois, rnorm
require(graphics) # for plot methods
plot(cars)
#?cars

library(readxl)
dados_teste <- read_excel("dados/dados.xlsx")
dados_teste
dados_teste$altura
summary(dados_teste$altura)
dados_teste[,"altura"]
dados_teste[1,"altura"]
dados_teste[1:3,"altura"]
dados_teste[c(1,3),"altura"]
dados_teste[1:3,]
dados_teste[1:3,c("altura", "fuma")]
dados_teste[1:3,c(1,4)]

class(dados_teste)
str(dados_teste)

(henrique1<- data.frame(x=1:1000, y=1:1000))
(henrique2<- tibble::as_tibble(henrique1))
head(henrique1, 5)
tail(henrique1, 5)

class(dados_teste$fuma)
class(dados_teste$altura)
table(dados_teste$fuma)
tapply(dados_teste$altura, dados_teste$fuma, mean)
subset(dados_teste, fuma=="sim")
dados_teste$fuma=="sim"
table(dados_teste$fuma=="sim")
table(dados_teste$fuma)
subset(dados_teste, fuma=="sim" & altura>190)
summary(subset(dados_teste, fuma=="sim" & altura>190))
subset(dados_teste, altura>170)

jonathan<- subset(dados_teste, fuma=="sim" & altura>170)
tabela<- table(jonathan$semestre_graduacao)
plot(tabela, lwd=20)

demo(graphics)
x <- stats::rnorm(50)
plot(x, ann = FALSE, type = "n")
abline(h = 0, col = gray(.90))
lines(x, col = "green4", lty = "dotted")
points(x, bg = "limegreen", pch = 21)
title(main = "Simple Use of Color In a Plot",
               xlab = "Just a Whisper of a Label",
               col.main = "blue", col.lab = gray(.8),
               cex.main = 1.2, cex.lab = 1.0, 
      font.main = 4, font.lab = 3)


(mauricio<- subset(dados_teste, fuma=="sim" & 
                    altura>170 & semestre_graduacao=="2"))

(teste<- subset(dados_teste, semestre_graduacao != "5"))
table(teste$semestre_graduacao)
(teste2<- subset(dados_teste, semestre_graduacao < 5))
table(teste2$semestre_graduacao)
(teste3<- subset(dados_teste, fuma !=  "sim"))
table(teste3$fuma)
#?subset
#help("subset")
#help("==")

# Trabalhando com dados faltantes
dados_NA <- read_excel("dados/dadosNA.xlsx")
summary(dados_NA$altura)
(gabriel<- min(dados_NA$altura, na.rm=TRUE))
dados_NA$altura[5]<- gabriel
mean(dados_NA$altura)

