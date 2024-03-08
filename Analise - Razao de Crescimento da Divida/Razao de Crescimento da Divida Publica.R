### Importaçao das Bibliotecas

library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(latex2exp)
library(magrittr)

### Importaçao dos Dados

data_inflacao <- read_excel('Inflation Exp.xlsx')

data_pib_real <- read_excel('GDP Growth.xlsx')

data_five_y_bonds <- read_excel('Five Year Bonds.xlsx')


### Junçao dos 3 dataframes e manejamento do dataframe resultante

data <- merge(data_five_y_bonds, 
              data_pib_real,
              by = 'Countries',
              all.x = TRUE) %>%
  merge(data_inflacao,
        by = 'Countries',
        all.x = TRUE) %>%
  subset(select = c('Countries', 'Classification', 'Five_Year_Bonds',
                    '2028.x', '2028.y')) %>%
  `colnames<-` (c('Países', 'Classificação', 'Bonds_Cinco_Anos',
                  'PIB_Real', 'Inflação')) %>%
  filter(!is.na(Bonds_Cinco_Anos)) %>%
  mutate(Classificação = as.factor(Classificação)) %>%
  mutate(across(c(PIB_Real, Inflação), ~ as.numeric(.)/100)) %>%
  mutate(Razao_de_Cresc = (1 + Bonds_Cinco_Anos)/((1 + PIB_Real)*(1 + Inflação)))


### Criar subset para o Brasil

paises_subset <- subset(data, 
                        Países != 'Brazil' & Países != 'Türkiye, Republic of' & Países != 'Vietnam')

brasil <- subset(data, Países == 'Brazil')


### Criar grafico com jitter envolvendo Classificaçao como variavel
### categorica no eixo X, razao de crescimento da divida
### no eixo Y e inflaçao sendo expressa atraves do tamanho de cada ponto

ggplot(paises_subset, aes(y = Razao_de_Cresc, x = Classificação))+
  geom_jitter(aes(fill = Classificação, size = Inflação), 
              shape = 21)+
  geom_point(data = brasil,
             aes(y = Razao_de_Cresc, 
                 x = Classificação,
                 size = Inflação),
             shape = 21)+
  theme_classic()+
  geom_text(data = brasil, 
            label = 'Brasil',
            vjust = -1)+
  ggtitle(TeX('Análise gráfica do parâmetro   $$\\frac{(1+i)}{(1+g)(1+\\pi)}$$'))+
  theme(plot.margin = unit(c(1,0,0,0), 'cm'),
        plot.title = element_text(vjust = 7, 
                                  size = 15,
                                  face = 'italic'),
        axis.text = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        plot.caption = element_text(size = 7,
                                    hjust = 0))+
  labs(x = element_blank(), 
       y = element_blank(),
       caption = 
         'Dados sobre projeção de crescimento real e inflação: Fundo Monetário Internacional.
Dados sobre os retornos de títulos públicos de 5 anos: World Government Bonds.')+
  geom_hline(yintercept = 1.0, 
             linetype = 'dashed',
             linewidth = 0.3,
             col = 'red')


ggsave('Grafico_Razao_Cresc.png')