library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# ---- Arquivos
arquivo_crescimento  <- "Real GDP Growth IMF.xls"
arquivo_participacao <- "GDP Share World IMF.xls"

anos <- 1980:2024

# ---- Nomes exatamente como aparecem no FMI (não altere)
nome_pais  <- "Chile"
nome_china <- "China, People's Republic of"
nome_emde  <- "Emerging market and developing economies"

# ---- 1) Ler e transformar a base de crescimento (formato longo)
crescimento_longo <- read_excel(arquivo_crescimento, sheet = 1) %>%
  rename(entidade = 1) %>%
  pivot_longer(
    cols = any_of(as.character(anos)),
    names_to = "ano",
    values_to = "crescimento"
  ) %>%
  mutate(
    ano = as.integer(ano),
    crescimento = as.numeric(crescimento)
  ) %>%
  filter(ano %in% anos)

crescimento_pais <- crescimento_longo %>%
  filter(entidade == nome_pais) %>%
  select(ano, crescimento_pais = crescimento)

crescimento_china <- crescimento_longo %>%
  filter(entidade == nome_china) %>%
  select(ano, crescimento_china = crescimento)

crescimento_emde <- crescimento_longo %>%
  filter(entidade == nome_emde) %>%
  select(ano, crescimento_emde = crescimento)

# ---- 2) Ler e transformar a base de participação no PIB mundial
participacao_longo <- read_excel(arquivo_participacao, sheet = 1) %>%
  rename(entidade = 1) %>%
  pivot_longer(
    cols = any_of(as.character(anos)),
    names_to = "ano",
    values_to = "participacao"
  ) %>%
  mutate(
    ano = as.integer(ano),
    participacao = as.numeric(participacao)
  ) %>%
  filter(ano %in% anos)

participacao_china <- participacao_longo %>%
  filter(entidade == nome_china) %>%
  select(ano, participacao_china = participacao)

participacao_pais <- participacao_longo %>%
  filter(entidade == nome_pais) %>%
  select(ano, participacao_pais = participacao)

participacao_emde <- participacao_longo %>%
  filter(entidade == nome_emde) %>%
  select(ano, participacao_emde = participacao)

# ---- 3) Construir crescimentos (EMDE sem China; EMDE sem China e sem o país)
base_final <- crescimento_emde %>%
  left_join(crescimento_china, by = "ano") %>%
  left_join(crescimento_pais,  by = "ano") %>%
  left_join(participacao_emde, by = "ano") %>%
  left_join(participacao_china, by = "ano") %>%
  left_join(participacao_pais, by = "ano") %>%
  mutate(
    # 1) EMDE sem China
    participacao_emde_sem_china = participacao_emde - participacao_china,
    crescimento_emde_sem_china =
      (participacao_emde * crescimento_emde - participacao_china * crescimento_china) /
      participacao_emde_sem_china,
    
    # 2) EMDE sem China e sem o país
    participacao_emde_sem_china_e_pais =
      participacao_emde - participacao_china - participacao_pais,
    crescimento_emde_sem_china_e_pais =
      (participacao_emde * crescimento_emde
       - participacao_china * crescimento_china
       - participacao_pais  * crescimento_pais) /
      participacao_emde_sem_china_e_pais
  ) %>%
  filter(
    is.finite(crescimento_emde_sem_china),
    is.finite(crescimento_emde_sem_china_e_pais),
    is.finite(crescimento_pais),
    participacao_emde_sem_china > 0,
    participacao_emde_sem_china_e_pais > 0
  ) %>%
  relocate(crescimento_emde_sem_china,
           crescimento_emde_sem_china_e_pais,
           .after = last_col())

# ---- 5) Métricas (Chile vs EMDE ex-China ex-Chile)

metricas_chile <- base_final %>%
  summarise(
    anos_validos = n(),
    pct_abaixo_45 = 100 * mean(crescimento_pais < crescimento_emde_sem_china_e_pais, na.rm = TRUE),
    
    crescimento_mediano_chile = median(crescimento_pais, na.rm = TRUE),
    crescimento_mediano_emde_benchmark = median(crescimento_emde_sem_china_e_pais, na.rm = TRUE),
    
    pct_crescimento_negativo_chile = 100 * mean(crescimento_pais < 0, na.rm = TRUE),
    pct_crescimento_negativo_emde_benchmark = 100 * mean(crescimento_emde_sem_china_e_pais < 0, na.rm = TRUE)
  )

t(metricas_chile)

# ---- 4) Gráfico de dispersão (X = EMDE ex-China ex-País, Y = País)
# --- Arco indicando 45° perto da origem
arco_45 <- data.frame(
  teta = seq(0, pi/4, length.out = 60)
) %>%
  mutate(
    x = 0.9 * cos(teta),
    y = 0.9 * sin(teta)
  )

grafico <- ggplot(base_final, aes(x = crescimento_emde_sem_china_e_pais,
                                  y = crescimento_pais)) +
  geom_point(shape = 21, size = 3, stroke = 0.7,
             fill = "#0B5FFF", color = "grey15", alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1,
              color = "#E63946", linewidth = 1.1, alpha = 0.7) +
  geom_path(data = arco_45, aes(x = x, y = y),
            inherit.aes = FALSE,
            color = "grey45", linewidth = 0.6, alpha = 0.8) +
  annotate("text", x = 1.05, y = 0.55, label = "45°",
           fontface = "bold", size = 3, color = "grey35") +
  geom_hline(yintercept = 0, linetype = "longdash",
             color = "grey65", linewidth = 0.5, alpha = 0.7) +
  coord_equal() +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 8, 2),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = expansion(mult = 0.05),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Chile vs Emergentes e Em Desenvolvimento (EMDE)",
    subtitle = "Crescimento real do Chile versus grupo dos países emergentes e em desenvolvimento de 1980 a 2024",
    x = "Países Emergentes e Em Desenvolvimento (exceto China e Chile)",
    y = "Chile"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(face = "bold")
  )

print(grafico)


# ---- 5) Gráfico alternativo: regressão (Chile vs EMDE ex-China ex-Chile)

modelo <- lm(crescimento_pais ~ crescimento_emde_sem_china_e_pais, data = base_final)

coef <- coef(modelo)
intercepto <- coef[1]
inclinacao <- coef[2]
r2 <- summary(modelo)$r.squared

texto_eq <- sprintf("y = %.2f + %.2f x\nR² = %.2f", intercepto, inclinacao, r2)

grafico_regressao <- ggplot(base_final,
                            aes(x = crescimento_emde_sem_china_e_pais,
                                y = crescimento_pais)) +
  geom_point(shape = 21, size = 2.6, stroke = 0.7,
             fill = "#0B5FFF", color = "grey15", alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE, color = "#E63946", linewidth = 1.1) +
  annotate("text",
           x = Inf, y = Inf,
           label = texto_eq,
           hjust = 1.05, vjust = 1.2,
           fontface = "bold", size = 3.5, color = "grey15") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Chile vs EMDE (exceto China e Chile): reta de regressão (1980–2024)",
    x = "EMDE (exceto China e Chile) — crescimento real (%)",
    y = "Chile — crescimento real (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(face = "bold")
  )

print(grafico_regressao)
