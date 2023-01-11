###Com o código abaixo, nós baixamos dados referente a preços de carros.

price = as.numeric(read.csv2('https://raw.githubusercontent.com/dataspelunking/MLwR/master/Machine%20Learning%20with%20R%20(2nd%20Ed.)/Chapter%2002/usedcars.csv', header=T,
                             sep=',', dec='.')$price)

###Obtendo o valor mínimo, máximo, média, mediana, primeiro e terceiro quartil

summary(price)

###Obtendo o IQR

IQR(price)

###Obtendo o preço mais frequente dos dados (moda)

func_moda <- function(x) {
  val_unicos <- unique(x)
  val_unicos[which.max(tabulate(match(price, val_unicos)))]
}

func_moda(price)

###Boxplot

boxplot(price)

###Histograma

hist(price)

###Formato

moments::kurtosis(price)

#Se assemelha a uma curva de distribuição normal, 
#no entanto, possui uma assimetria negativa por conta 
#de valores extremos à esquerda da moda. Pode ser #classificada como leptocúrtica por conta 
#do excesso de curtose que possui (0.4795), 
#o que justifica a existência de caudas pesadas.

###Variância e desvio padrão

var(price)

sqrt(var(price))

###Densidade

hist(price, freq = F)
lines(density(price))

###Intervalo provável

#Se utilizarmos como referência o intervalo interquartil, 
#os valores mais prováveis de serem observados se situam
#entre 10995 (primeiro quartil) e 14904 (terceiro quartil).