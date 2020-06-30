#data
require(xtable)

library(readxl)
Conjunto_de_dados <- read_excel("./Conjunto de dados.xlsx", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text", 
                                              "numeric", "text", "numeric"), skip = 1)
View(Conjunto_de_dados)
Conjunto_de_dados

##Frequencia tabela
tabelaexport <- function(x){
  freq <- table(x)
  freq_abs<-data.frame(freq)
  freq_rel<-data.frame(prop.table(freq))
  xtable(data.frame(VAlores=  freq_abs$x,
                    FrequenciaAbsoluta = freq_abs$Freq,
                    FrequenciaRelativa = freq_rel$Freq),caption = "title")
}

### Trabalhando com a v.a genero
genero <- Conjunto_de_dados$Gênero
length(c(genero))

tabelaexport(genero) #para o relatório
intervaloConfP <- function(x, conf = 0.95) {
  n <- length(x)
  proporcao <- prop.table(table(genero))
  proporcao <- proporcao[1] #Feminino
  
  quantis1 <- -qnorm((1 - conf)/2,mean=0,sd=1)  
  quantis2 <- qnorm((1 - conf)/2,mean=0,sd=1)
  quartis<-c(quantis1, quantis2)
  ic <- proporcao + quantis * sqrt(1/4*n)
  return(ic)
}
za <- -qnorm((1 - 0.95)/2,mean=0,sd=1) 
za

quartis <- c(5,10)
quantis

(1 - 0.95)/2
intervaloConfP(genero)

proporcao <- c(prop.table(table(genero))[1])
proporcao+quantis*sqrt(1/4*50)
sqrt(1/4*50)

### Trabalhando com a v.a idade
idade <- Conjunto_de_dados$Idade

intervaloConfM <- function(x, conf = 0.95) {
  n <- length(x)
  media <- mean(x)
  variancia <- var(x)
  quantis <- qt(c((1 - conf)/2, 1 - (1 - conf)/2), df = n - 1)
  ic <- media + quantis * sqrt(variancia/n)
  return(ic)
}
intervaloConfM(c(idade))
