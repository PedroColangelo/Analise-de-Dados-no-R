library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)

# ---- Arquivos
arquivo_crescimento <- "Real GDP Growth IMF.xls"
arquivo_participacao <- "GDP Share World IMF.xls"

anos <- 1980:2024

# ---- Nomes exatamente como aparecem no FMI (não altere)
nome_brasil <- "Brazil"
nome_china  <- "China, People's Republic of"
nome_emde   <- "Emerging market and developing economies"

# ---- 1) Ler e transformar a base de crescimento (formato longo)
crescimento_bruto <- read_excel(arquivo_crescimento, sheet = 1)

crescimento_longo <- crescimento_bruto %>%
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

crescimento_brasil <- crescimento_longo %>%
  filter(entidade == nome_brasil) %>%
  select(ano, crescimento_brasil = crescimento)

crescimento_china <- crescimento_longo %>%
  filter(entidade == nome_china) %>%
  select(ano, crescimento_china = crescimento)

crescimento_emde <- crescimento_longo %>%
  filter(entidade == nome_emde) %>%
  select(ano, crescimento_emde = crescimento)

# ---- 2) Ler e transformar a base de participação no PIB mundial
participacao_bruto <- read_excel(arquivo_participacao, sheet = 1)

participacao_longo <- participacao_bruto %>%
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

participacao_brasil <- participacao_longo %>%
  filter(entidade == nome_brasil) %>%
  select(ano, participacao_brasil = participacao)

participacao_emde <- participacao_longo %>%
  filter(entidade == nome_emde) %>%
  select(ano, participacao_emde = participacao)

# ---- 3) Construir crescimento EMDE ex-China (média ponderada)
base_final <- crescimento_emde %>%
  left_join(crescimento_china,  by = "ano") %>%
  left_join(crescimento_brasil, by = "ano") %>%
  left_join(participacao_emde,  by = "ano") %>%
  left_join(participacao_china, by = "ano") %>%
  left_join(participacao_brasil, by = "ano") %>%
  mutate(
    # 1) EMDE sem China
    participacao_emde_sem_china = participacao_emde - participacao_china,
    crescimento_emde_sem_china =
      (participacao_emde * crescimento_emde - participacao_china * crescimento_china) /
      participacao_emde_sem_china,
    
    # 2) EMDE sem China e sem Brasil
    participacao_emde_sem_china_e_brasil =
      participacao_emde - participacao_china - participacao_brasil,
    crescimento_emde_sem_china_e_brasil =
      (participacao_emde * crescimento_emde
       - participacao_china  * crescimento_china
       - participacao_brasil * crescimento_brasil) /
      participacao_emde_sem_china_e_brasil
  ) %>%
  filter(
    is.finite(crescimento_emde_sem_china),
    is.finite(crescimento_emde_sem_china_e_brasil),
    is.finite(crescimento_brasil),
    participacao_emde_sem_china > 0,
    participacao_emde_sem_china_e_brasil > 0
  ) %>%
  # deixa as duas séries como as DUAS ÚLTIMAS colunas
  relocate(crescimento_emde_sem_china,
           crescimento_emde_sem_china_e_brasil,
           .after = last_col())

# ---- 5) Métricas (Brasil vs EMDE ex-China ex-Brasil)

metricas_brasil <- base_final %>%
  summarise(
    anos_validos = n(),
    pct_abaixo_45 = 100 * mean(crescimento_brasil < crescimento_emde_sem_china_e_brasil, na.rm = TRUE),
    
    crescimento_mediano_brasil = median(crescimento_brasil, na.rm = TRUE),
    crescimento_mediano_emde_benchmark = median(crescimento_emde_sem_china_e_brasil, na.rm = TRUE),
    
    pct_crescimento_negativo_brasil = 100 * mean(crescimento_brasil < 0, na.rm = TRUE),
    pct_crescimento_negativo_emde_benchmark = 100 * mean(crescimento_emde_sem_china_e_brasil < 0, na.rm = TRUE)
  )

t(metricas_brasil)

# ---- 4) Gráfico de dispersão (X = EMDE ex-China, Y = Brasil)

# --- Arco indicando 45° perto da origem
arco_45 <- data.frame(
  teta = seq(0, pi/4, length.out = 60)
) %>%
  mutate(
    x = 0.9 * cos(teta),
    y = 0.9 * sin(teta)
  )

lim_inf_y <- 2 * floor(min(base_final$crescimento_brasil, na.rm = TRUE) / 2)
lim_sup_y <- 2 * ceiling(max(base_final$crescimento_brasil, na.rm = TRUE) / 2)

grafico <- ggplot(base_final, aes(x = crescimento_emde_sem_china_e_brasil,
                                  y = crescimento_brasil)) +
  geom_point(shape = 21, size = 3, stroke = 0.7,
             fill = "#2F6FED", color = "grey15", alpha = 0.9) +
  geom_text_repel(
    aes(label = ifelse((ano >= 2004 & ano <= 2011) | (ano >= 2021 & ano <= 2024),
                       as.character(ano), NA)),
    size = 2.6, fontface = "bold",
    color = "grey20", max.overlaps = 200
  ) +
  geom_abline(intercept = 0, slope = 1, 
              color = "#E63946", linewidth = 1.1, alpha = 0.7) +
  geom_path(data = arco_45, aes(x = x, y = y),
            inherit.aes = FALSE,
            color = "grey45", linewidth = 0.6, alpha = 0.8) +
  annotate("text", x = 1.05, y = 0.55, label = "45°",
           fontface = "bold", size = 3, color = "grey35") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey65", linewidth = 0.5, alpha = 0.7) +
  coord_equal() +
  coord_cartesian(xlim = c(0, 8)) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 8, 2),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.05),
    breaks = seq(lim_inf_y, lim_sup_y, by = 2),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Brasil vs Emergentes e Em Desenvolvimento (EMDE)",
    subtitle = "Crescimento real do Brasil versus grupo dos países Emergentes e Em Desenvolvimento ex-China de 1980 a 2024",
    x = "Países Emergentes e Em Desenvolvimento",
    y = "Brasil"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.title.x = element_text(face = "bold",
                                margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold",
                                margin = margin(r = 12)),
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(face = "bold")
  )

print(grafico)

ggsave(
  filename = "Brasil_vs_EMDE_45graus.png",
  plot = grafico,
  width = 7.5, height = 3.92, units = "in",  # dimensões
  dpi = 600,                              # qualidade (LinkedIn: 300 é ótimo)
  bg = "white"                            # fundo branco
)


# ---- 5) Gráfico alternativo: regressão (Brasil vs EMDE ex-China ex-Brasil)

# Ajuste da regressão
modelo <- lm(crescimento_brasil ~ crescimento_emde_sem_china_e_brasil, data = base_final)

coef <- coef(modelo)
intercepto <- coef[1]
inclinação <- coef[2]
r2 <- summary(modelo)$r.squared

texto_eq <- sprintf("y = %.2f + %.2f x\nR² = %.2f", intercepto, inclinação, r2)

grafico_regressao <- ggplot(base_final,
                            aes(x = crescimento_emde_sem_china_e_brasil,
                                y = crescimento_brasil)) +
  geom_point(shape = 21, size = 2.6, stroke = 0.7,
             fill = "#0B5FFF", color = "grey15", alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE, color = "#E63946", linewidth = 1.1) +
  annotate("text",
           x = Inf, y = Inf,
           label = texto_eq,
           hjust = 1.05, vjust = 1.2,
           fontface = "bold", size = 3.5, color = "grey15") +
  labs(
    title = "Brasil vs EMDE (exceto China e Brasil): reta de regressão (1980–2024)",
    x = "EMDE (exceto China e Brasil) — crescimento real (%)",
    y = "Brasil — crescimento real (%)"
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


peso_china_emde <- participacao_longo %>%
  filter(entidade %in% c(nome_china, nome_emde)) %>%
  select(ano, entidade, participacao) %>%
  pivot_wider(names_from = entidade, values_from = participacao) %>%
  transmute(
    ano,
    china_no_mundo = `China, People's Republic of`,
    emde_no_mundo  = `Emerging market and developing economies`,
    peso_china_no_emde = china_no_mundo / emde_no_mundo,          # fração (0–1)
    peso_china_no_emde_pct = 100 * peso_china_no_emde             # em %
  )

print(peso_china_emde)
