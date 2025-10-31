### Bibliotecas

library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(latex2exp)
library(magrittr)
library(scales)
library(tidyr)

### Importacao dos Dados

data_inflacao <- read_excel('Inflation Exp.xlsx')

data_pib_real <- read_excel('GDP Growth.xlsx')

data_five_y_bonds <- read_excel('Five Year Bonds.xlsx')

data_divida <- read_excel('DBGG.xlsx')

### Junçao dos 4 dataframes e manejamento do dataframe resultante

data <- data_five_y_bonds %>%
  # mantenha só o que você precisa deste arquivo
  select(Countries, Region, Classification, Five_Year_Bonds) %>%
  # adiciona PIB real (coluna '2030.0' -> 'PIB_Real')
  left_join(
    data_pib_real %>%
      select(Countries, `2030.0`) %>%
      rename(PIB_Real = `2030.0`),
    by = "Countries"
  ) %>%
  # adiciona Inflação (coluna '2030.0' -> 'Inflação')
  left_join(
    data_inflacao %>%
      select(Countries, `2030.0`) %>%
      rename(`Inflação` = `2030.0`),
    by = "Countries"
  ) %>%
  # adiciona Dívida/PIB (coluna '2030.0' -> 'Divida_PIB')
  left_join(
    data_divida %>%
      select(Countries, `2030.0`) %>%
      rename(Divida_PIB = `2030.0`),
    by = "Countries"
  ) %>%
  rename(Países = Countries,
         Classificação = Classification,
         Bonds_Cinco_Anos = Five_Year_Bonds) %>%
  filter(!is.na(Bonds_Cinco_Anos)) %>%
  mutate(Classificação = as.factor(Classificação)) %>%
  # percentuais -> decimais
  mutate(across(c(PIB_Real, `Inflação`), ~ as.numeric(.)/100)) %>%
  mutate(Divida_PIB = as.numeric(Divida_PIB),
         Divida_PIB = ifelse(Divida_PIB > 1, Divida_PIB/100, Divida_PIB)) %>%
  mutate(Razao_de_Cresc = (1 + Bonds_Cinco_Anos) / ((1 + PIB_Real) * (1 + `Inflação`))) %>%
  mutate(Primario_Est = (Razao_de_Cresc - 1) * Divida_PIB) %>%
  mutate(Primario_Est_pctPIB = 100 * Primario_Est)


### Criar subset para o Brasil

paises_subset <- subset(data,
                        Países != 'Brazil' & 
                          Países != "China, People's Republic of" &
                          Países != 'Singapore' &
                          Países != 'Japan'
                        )

brasil <- subset(data, Países == 'Brazil')

paises_subset_sem_focus <- paises_subset %>% 
  dplyr::filter(Países != "Brazil (Focus)")

brasil_focus <- data %>%
  dplyr::filter(Países == "Brazil (Focus)")

# base para regressões (sem o Brazil Focus)
reg_df <- dplyr::bind_rows(paises_subset_sem_focus, brasil)

### Gráfico 1 ###

des_col <- "#F8766D"  # Desenvolvido
sub_col <- "#00BFC4"  # Subdesenvolvido

ggplot() +
  geom_point(
    data = paises_subset_sem_focus,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB,
        fill = Classificação, size = `Inflação`),
    shape = 21, colour = "grey30", stroke = 0.6, alpha = 0.9
  ) +
  geom_point(
    data = brasil,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB,
        size = `Inflação`, fill = Classificação),
    shape = 21, colour = "black", stroke = 1.4, show.legend = FALSE
  ) +
  geom_text(
    data = brasil,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    label = "Brasil", vjust = -1, fontface = "bold",
    nudge_y = 0.2
  ) +
  geom_point(
    data = brasil_focus,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB,
        size = `Inflação`, fill = Classificação),
    shape = 21, colour = "black", stroke = 1.2, alpha = 0.45,
    show.legend = FALSE
  ) +
  geom_text(
    data = brasil_focus,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    label = "Brasil (Focus)", vjust = -1, fontface = "italic",
    nudge_y = 0.4
  ) +
  geom_smooth(
    data = reg_df %>%
      dplyr::filter(Classificação == "Desenvolvido",
                    !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB)),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = scales::alpha(des_col, 0.45), linewidth = 3,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_smooth(
    data = reg_df %>%
      dplyr::filter(Classificação == "Desenvolvido",
                    !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB)),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = des_col, linewidth = 1.1, linetype = "dotted",
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_smooth(
    data = reg_df %>%
      dplyr::filter(Classificação == "Subdesenvolvido",
                    !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB)),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = scales::alpha(sub_col, 0.45), linewidth = 3,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_smooth(
    data = reg_df %>%
      dplyr::filter(Classificação == "Subdesenvolvido",
                    !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB)),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = sub_col, linewidth = 1.1, linetype = "dotted",
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  theme_classic() +
  labs(
    title = "Esforço fiscal vs. dívida/PIB",
    x = "Dívida bruta do governo geral (% do PIB)",
    y = "Superávit primário estabilizador (% do PIB)",
    fill = "Classificação",
    caption = "Dados: FMI (g, π, dívida/PIB) e World Government Bonds (juros de 5 anos)."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, col = "red") +
  scale_fill_manual(values = c("Desenvolvido" = des_col, "Subdesenvolvido" = sub_col)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = "%")) +
  scale_size_continuous(
    range = c(2, 12),
    breaks = c(0.03, 0.05, 0.10),
    labels = scales::label_percent(accuracy = 1),
    name = "Inflação (π)"
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(shape = 21, colour = "grey30", stroke = 0.6, size = 5)),
    size = guide_legend(order = 2, override.aes = list(shape = 21, colour = "grey30", stroke = 0.6, fill = "white"))
  ) +
  theme(
    plot.margin = unit(c(1,0,0,0), "cm"),
    plot.title  = element_text(vjust = 7, size = 15, face = "bold.italic"),
    axis.text   = element_text(face = "bold"),
    legend.title= element_text(face = "bold"),
    axis.title  = element_text(face = "bold"),
    plot.caption= element_text(size = 7, hjust = 0)
  )

ggsave("Grafico_Primario_vs_Divida_com_Regressoes_estetica_unificada_GRANDE.png",
       width = 7.5, height = 3.92, dpi = 600)



### Gráfico 2 ###
asia_fill    <- "steelblue1"        # cor dos pontos/linha da Ásia
asia_halo    <- "steelblue4"        # halo (um tom mais escuro)
nonasia_fill  <- "lightgoldenrod1"   # amarelo claro (preenchimento)
nonasia_line  <- "lightgoldenrod3"   # amarelo um pouco mais escuro p/ a linha

# Base do gráfico: todos os asiáticos (desenv. + subdesenv.) + subdesenv. não-asiáticos
plot_df_mix <- data %>%
  dplyr::filter(Region == "Ásia" | (Region != "Ásia" & Classificação == "Subdesenvolvido")) %>%
  dplyr::mutate(
    Grupo_Ásia2 = ifelse(Region == "Ásia",
                         "Países asiáticos",
                         "Países não-asiáticos (subdesenvolvidos)"),
    Grupo_Ásia2 = factor(Grupo_Ásia2,
                         levels = c("Países asiáticos", "Países não-asiáticos (subdesenvolvidos)"))
  )

# Subconjuntos úteis
brasil        <- dplyr::filter(plot_df_mix, Países == "Brazil")
brasil_focus  <- dplyr::filter(plot_df_mix, Países == "Brazil (Focus)")
pontos_gerais <- dplyr::filter(plot_df_mix, !Países %in% c("Brazil", "Brazil (Focus)"))

ggplot() +
  geom_point(
    data = pontos_gerais,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB, fill = Grupo_Ásia2, size = `Inflação`),
    shape = 21, colour = "grey30", stroke = 0.6, alpha = 0.9
  ) +
  geom_point(
    data = brasil,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB, fill = Grupo_Ásia2, size = `Inflação`),
    shape = 21, colour = "black", stroke = 1.4, show.legend = FALSE
  ) +
  geom_text(
    data = brasil,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    label = "Brasil", fontface = "bold", nudge_y = 1.2
  ) +
  geom_point(
    data = brasil_focus,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB, fill = Grupo_Ásia2, size = `Inflação`),
    shape = 21, colour = "black", stroke = 1.2, alpha = 0.45, show.legend = FALSE
  ) +
  geom_text(
    data = brasil_focus,
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    label = "Brasil (Focus)", fontface = "italic", nudge_y = 1.2
  ) +
  theme_classic() +
  labs(
    title   = "Esforço fiscal vs. dívida/PIB",
    subtitle = "Ásia: desenv. + subdesenv. vs não-Ásia: só subdesenv.",
    x       = "Dívida bruta do governo geral (% do PIB)",
    y       = "Superávit primário estabilizador (% do PIB)",
    fill    = "Grupos",
    size    = "Inflação (π)",
    caption = "Dados: FMI (g e π, médio prazo) e World Government Bonds (juros de 5 anos)."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, col = "red") +
  scale_fill_manual(values = c("Países asiáticos" = asia_fill,
                               "Países não-asiáticos (subdesenvolvidos)" = nonasia_fill)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = "%")) +
  scale_size_continuous(
    range = c(2, 12),
    breaks = c(0.03, 0.05, 0.10),
    labels = scales::label_percent(accuracy = 1)
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(shape = 21, colour = "grey30", stroke = 0.6, size = 5)),
    size = guide_legend(order = 2)
  ) +
  theme(
    plot.margin  = unit(c(1,0,0,0), "cm"),
    plot.title   = element_text(vjust = 7, size = 15, face = "bold.italic"),
    plot.subtitle = element_text(size = 9, margin = margin(t = -20, b = 20)),
    axis.text    = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.title   = element_text(face = "bold"),
    plot.caption = element_text(size = 7, hjust = 0)
  ) +
geom_smooth(
  data = plot_df_mix %>% dplyr::filter(Region == "Ásia",
                                       !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB),
                                       Países != "Brazil (Focus)"),
  aes(x = Divida_PIB, y = Primario_Est_pctPIB),
  method = "lm", formula = y ~ x, se = FALSE,
  colour = scales::alpha(asia_halo, 0.45), linewidth = 3,
  inherit.aes = FALSE, show.legend = FALSE
) +
  geom_smooth(
    data = plot_df_mix %>% dplyr::filter(Region == "Ásia",
                                         !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB),
                                         Países != "Brazil (Focus)"),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = asia_fill, linewidth = 1.2, linetype = "dotted",
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_smooth(
    data = plot_df_mix %>% dplyr::filter(Region != "Ásia",
                                         Classificação == "Subdesenvolvido",
                                         !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB),
                                         Países != "Brazil (Focus)"),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = scales::alpha(nonasia_fill, 0.45), linewidth = 3,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_smooth(
    data = plot_df_mix %>% dplyr::filter(Region != "Ásia",
                                         Classificação == "Subdesenvolvido",
                                         !is.na(Divida_PIB), !is.na(Primario_Est_pctPIB),
                                         Países != "Brazil (Focus)"),
    aes(x = Divida_PIB, y = Primario_Est_pctPIB),
    method = "lm", formula = y ~ x, se = FALSE,
    colour = nonasia_line, linewidth = 1.2, linetype = "dotted",
    inherit.aes = FALSE, show.legend = FALSE
  )

ggsave("Grafico_Primario_vs_Divida_Asia_todos_vs_NaoAsia_subdesenv_duas_regressoes.png",
       width = 7.5, height = 3.92, dpi = 600)


# --------- Shapley de s* (padrão) ---------

# 1) Função s* (em % do PIB)
s_star <- function(i, g, pi, b, pct = TRUE) {
  val <- ((1 + i) / ((1 + g) * (1 + pi)) - 1) * b
  if (pct) 100 * val else val
}

# 2) Gerador de permutações
permute <- function(x) {
  if (length(x) == 1L) return(list(x))
  res <- list(); k <- 1L
  for (i in seq_along(x)) {
    sub <- permute(x[-i])
    for (p in sub) { res[[k]] <- c(x[i], p); k <- k + 1L }
  }
  res
}

# 3) Shapley para 1 linha (target) contra um baseline (base)
shapley_one <- function(target, base, pct = TRUE) {
  factors <- c("i","g","pi","b")
  perms <- permute(factors)
  get_s <- function(vals) s_star(vals["i"], vals["g"], vals["pi"], vals["b"], pct)
  acc <- setNames(numeric(length(factors)), factors)
  
  for (ord in perms) {
    cur <- base
    prev <- get_s(cur)
    for (f in ord) {
      cur[f] <- target[f]
      now <- get_s(cur)
      acc[f] <- acc[f] + (now - prev)
      prev <- now
    }
  }
  acc / length(perms)  # média sobre todas as ordens
}

# 4) Conjunto de pares para construir o baseline

peer_set <- data %>%
 dplyr::filter(Classificação == "Subdesenvolvido",
               Primario_Est_pctPIB > 0,
               !is.na(Bonds_Cinco_Anos), !is.na(PIB_Real),
               !is.na(`Inflação`), !is.na(Divida_PIB))

# 5) Vetor de referência (medianas dos pares)
ref <- peer_set %>%
  dplyr::summarise(
    i  = median(Bonds_Cinco_Anos, na.rm = TRUE),
    g  = median(PIB_Real,         na.rm = TRUE),
    pi = median(`Inflação`,       na.rm = TRUE),
    b  = median(Divida_PIB,       na.rm = TRUE)
  ) %>% as.list()

# 6) Calcula Shapley para linhas filtradas
shapley_df <- data %>%
  dplyr::filter(Classificação == 'Subdesenvolvido',
                Primario_Est_pctPIB > 0) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    s_star_pct = s_star(Bonds_Cinco_Anos, PIB_Real, `Inflação`, Divida_PIB, pct = TRUE),
    .phi = list(
      shapley_one(
        target = c(i = Bonds_Cinco_Anos, g = PIB_Real, pi = `Inflação`, b = Divida_PIB),
        base   = unlist(ref),
        pct    = TRUE
      )
    )
  ) %>%
  dplyr::mutate(
    phi_i  = .phi[["i"]],
    phi_g  = .phi[["g"]],
    phi_pi = .phi[["pi"]],
    phi_b  = .phi[["b"]],
    s_ref  = s_star(ref$i, ref$g, ref$pi, ref$b, pct = TRUE),
    delta  = s_star_pct - s_ref,
    check  = phi_i + phi_g + phi_pi + phi_b  # deve ≈ delta
  ) %>%
  dplyr::select(Países, Classificação, Region,
                s_star_pct, s_ref, delta, phi_i, phi_g, phi_pi, phi_b, check) %>%
  dplyr::ungroup()

# Brasil
shapley_df %>% dplyr::filter(Países == "Brazil")
# Verificar se as contribuições somam o gap (tolerância numérica)
summary(abs(shapley_df$check - shapley_df$delta))

# --- preparar dados ---
comp_cols <- c(
  "i (juros)"        = "#e41a1c",
  "g (crescimento)"  = "#4daf4a",
  "π (inflação)"     = "#ff7f00",
  "b (dívida)"       = "#377eb8"
)

shapley_long <- shapley_df %>%
  dplyr::select(Países, s_star_pct, s_ref, phi_i, phi_g, phi_pi, phi_b) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("phi_"),
    names_to = "componente",
    values_to = "contrib"
  ) %>%
  dplyr::mutate(
    componente = dplyr::recode(componente,
                               phi_i  = "i (juros)",
                               phi_g  = "g (crescimento)",
                               phi_pi = "π (inflação)",
                               phi_b  = "b (dívida)"),
    # ordenar países pelo Δ (s_star_pct - s_ref)
    Países = reorder(Países, shapley_df$delta[match(Países, shapley_df$Países)])
  )


### Gráfico 3 ###

ggplot(shapley_long, aes(x = Países, y = contrib, fill = componente)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_point(
    data = dplyr::distinct(shapley_long, Países, s_star_pct),
    aes(x = Países, y = shapley_df$delta[match(Países, shapley_df$Países)]),
    inherit.aes = FALSE, shape = 21, fill = "white", colour = "black",
    size = 2.8, stroke = 0.8
  ) +
  scale_fill_manual(values = comp_cols, name = "Contribuições") +
  theme_classic() +
  coord_flip() +
  labs(
    title    = "Decomposição Shapley do s* por país",
    x = NULL, y = "% do PIB",
    caption = "Contribuições somam o Δ em relação ao baseline (mediana dos pares)."
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = "%")) +
  theme(
    axis.text    = element_text(face = "bold"),
    plot.title = element_text(face = 'bold.italic'),
    plot.subtitle = element_text(size = 9, margin = margin(b = 20)),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 7, hjust = 0)
  )

ggsave("Grafico_Shapley_empilhado_com_ponto_sstar.png", width = 7.5, height = 3.9, dpi = 600)
