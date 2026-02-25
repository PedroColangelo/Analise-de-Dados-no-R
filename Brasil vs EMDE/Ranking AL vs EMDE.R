library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# ---- Arquivos
arquivo_crescimento  <- "Real GDP Growth IMF.xls"
arquivo_participacao <- "GDP Share World IMF.xls"

anos <- 1980:2024

# ---- Nomes FMI (exatos)
nome_china <- "China, People's Republic of"
nome_emde  <- "Emerging market and developing economies"

# ---- Lista LATAM (AJUSTE para os nomes EXATOS do seu arquivo)
paises_latam <- c(
  "Argentina",
  "Chile",
  "Colombia",
  "Mexico",
  "Peru",
  "Uruguay",
  "Paraguay",
  "Bolivia",
  "Ecuador",
  "Venezuela",
  "Costa Rica",
  "Panama",
  "Guatemala",
  "Honduras",
  "El Salvador",
  "Nicaragua",
  "Dominican Republic"
)

# ---- 1) Crescimento em formato longo
crescimento_longo <- read_excel(arquivo_crescimento, sheet = 1) %>%
  rename(entidade = 1) %>%
  pivot_longer(cols = any_of(as.character(anos)), names_to = "ano", values_to = "crescimento") %>%
  mutate(ano = as.integer(ano), crescimento = as.numeric(crescimento)) %>%
  filter(ano %in% anos)

# ---- 2) Participação no PIB mundial em formato longo
participacao_longo <- read_excel(arquivo_participacao, sheet = 1) %>%
  rename(entidade = 1) %>%
  pivot_longer(cols = any_of(as.character(anos)), names_to = "ano", values_to = "participacao") %>%
  mutate(ano = as.integer(ano), participacao = as.numeric(participacao)) %>%
  filter(ano %in% anos)

# Séries fixas
crescimento_emde <- crescimento_longo %>% filter(entidade == nome_emde)  %>% select(ano, crescimento_emde = crescimento)
crescimento_china <- crescimento_longo %>% filter(entidade == nome_china) %>% select(ano, crescimento_china = crescimento)

participacao_emde <- participacao_longo %>% filter(entidade == nome_emde)  %>% select(ano, participacao_emde = participacao)
participacao_china <- participacao_longo %>% filter(entidade == nome_china) %>% select(ano, participacao_china = participacao)

# ---- Função: calcula % de anos acima da 45° para um país
avaliar_pais <- function(nome_pais) {
  crescimento_pais <- crescimento_longo %>%
    filter(entidade == nome_pais) %>%
    select(ano, crescimento_pais = crescimento)
  
  participacao_pais <- participacao_longo %>%
    filter(entidade == nome_pais) %>%
    select(ano, participacao_pais = participacao)
  
  base <- crescimento_emde %>%
    left_join(crescimento_china, by = "ano") %>%
    left_join(crescimento_pais,  by = "ano") %>%
    left_join(participacao_emde, by = "ano") %>%
    left_join(participacao_china, by = "ano") %>%
    left_join(participacao_pais, by = "ano") %>%
    mutate(
      participacao_emde_sem_china_e_pais = participacao_emde - participacao_china - participacao_pais,
      crescimento_emde_sem_china_e_pais =
        (participacao_emde * crescimento_emde
         - participacao_china * crescimento_china
         - participacao_pais  * crescimento_pais) /
        participacao_emde_sem_china_e_pais,
      acima_45 = crescimento_pais > crescimento_emde_sem_china_e_pais
    ) %>%
    filter(
      is.finite(crescimento_emde_sem_china_e_pais),
      is.finite(crescimento_pais),
      participacao_emde_sem_china_e_pais > 0
    )
  
  tibble(
    pais = nome_pais,
    anos_validos = nrow(base),
    anos_acima_45 = sum(base$acima_45, na.rm = TRUE),
    pct_acima_45 = 100 * mean(base$acima_45, na.rm = TRUE)
  )
}

# ---- Rodar ranking
ranking_latam <- bind_rows(lapply(paises_latam, avaliar_pais)) %>%
  arrange(desc(pct_acima_45), desc(anos_acima_45))

ranking_latam