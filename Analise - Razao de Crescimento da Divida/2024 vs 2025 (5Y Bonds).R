### Bibliotecas
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(latex2exp)
library(magrittr)
library(scales)

### Importacao dos Dados
data_pib_real_2025 <- read_excel("GDP Growth.xlsx")
data_inflacao_2025 <- read_excel("Inflation Exp.xlsx")
data_five_y_bonds_2025<- read_excel('Five Year Bonds.xlsx')        
data_divida           <- read_excel('DBGG.xlsx')
data_five_y_bonds_2024<- read_excel('Five Year Bonds 2024.xlsx')
data_pib_real_2024 <- read_excel("GDP Growth 2024.xlsx")
data_inflacao_2024 <- read_excel("Inflation Exp 2024.xlsx")

## Chaves/identidade dos países
chaves <- full_join(
  data_five_y_bonds_2024 %>% select(Countries, Region, Classification),
  data_five_y_bonds_2025 %>% select(Countries, Region, Classification),
  by = "Countries",
  suffix = c("_2024","_2025")
) %>%
  transmute(
    Countries,
    Region         = coalesce(Region_2025, Region_2024),
    Classification = coalesce(Classification_2025, Classification_2024)
  )


## Selecionar as colunas
g2024  <- data_pib_real_2024 %>%
  select(Countries, matches("^2028(\\.0)?$")) %>%
  rename_with(~ "PIB_Real_2024", matches("^2028(\\.0)?$"))

pi2024 <- data_inflacao_2024 %>%
  select(Countries, matches("^2028(\\.0)?$")) %>%
  rename_with(~ "Inflacao_2024", matches("^2028(\\.0)?$"))

g2025  <- data_pib_real_2025 %>%
  select(Countries, matches("^2030(\\.0)?$")) %>%
  rename_with(~ "PIB_Real_2025", matches("^2030(\\.0)?$"))

pi2025 <- data_inflacao_2025 %>%
  select(Countries, matches("^2030(\\.0)?$")) %>%
  rename_with(~ "Inflacao_2025", matches("^2030(\\.0)?$"))


## Base
base <- chaves %>%
  left_join(g2024,  by = "Countries") %>%
  left_join(pi2024, by = "Countries") %>%
  left_join(g2025,  by = "Countries") %>%
  left_join(pi2025, by = "Countries") %>%
  left_join(
    data_divida %>%
      select(Countries, `2030.0`) %>%
      rename(Divida_PIB = `2030.0`),
    by = "Countries"
  ) %>%
  mutate(
    PIB_Real_2024 = as.numeric(PIB_Real_2024)/100,
    Inflacao_2024 = as.numeric(Inflacao_2024)/100,
    PIB_Real_2025 = as.numeric(PIB_Real_2025)/100,
    Inflacao_2025 = as.numeric(Inflacao_2025)/100,
    Divida_PIB    = as.numeric(Divida_PIB),
    Divida_PIB    = ifelse(Divida_PIB > 1, Divida_PIB/100, Divida_PIB)
  ) %>%
  rename(Países = Countries, Classificação = Classification)

## Juros (5Y) 2024 e 2025
y2024 <- data_five_y_bonds_2024 %>%
  select(Countries, Five_Year_Bonds) %>%
  rename(i_2024 = Five_Year_Bonds)

y2025 <- data_five_y_bonds_2025 %>%
  select(Countries, Five_Year_Bonds) %>%
  rename(i_2025 = Five_Year_Bonds)

## s* para 2024 e 2025
cmp <- base %>%
  rename(Countries = Países) %>%
  left_join(y2024, by = "Countries") %>%
  left_join(y2025, by = "Countries") %>%
  mutate(
    Razao_2024 = (1 + i_2024) / ((1 + PIB_Real_2024) * (1 + Inflacao_2024)),
    Razao_2025 = (1 + i_2025) / ((1 + PIB_Real_2025) * (1 + Inflacao_2025)),
    sstar_2024_pctPIB = 100 * (Razao_2024 - 1) * Divida_PIB,
    sstar_2025_pctPIB = 100 * (Razao_2025 - 1) * Divida_PIB
  ) %>%
  rename(Países = Countries) %>%
  filter(!is.na(sstar_2024_pctPIB), !is.na(sstar_2025_pctPIB))

## Limites iguais nos dois eixos
lims <- range(c(cmp$sstar_2024_pctPIB, cmp$sstar_2025_pctPIB), na.rm = TRUE)
pad  <- diff(lims) * 0.05
lims <- c(lims[1] - pad, lims[2] + pad)

## Distinguindo Brasil do resto e informações extras do gráfico
brasil <- cmp %>% filter(Países == "Brazil")
resto  <- cmp %>% filter(Países != "Brazil")
brasil_focus <- cmp %>% dplyr::filter(Países == "Brazil (Focus)")
brfx <- brasil_focus$sstar_2024_pctPIB[1]
brfy <- brasil_focus$sstar_2025_pctPIB[1]
br_x <- brasil$sstar_2024_pctPIB[1]
br_y <- brasil$sstar_2025_pctPIB[1]
dy_mid <- (br_y + br_x) / 2
dx_off <- diff(lims) * 0.035
dx_off_f <- diff(lims) * 0.02
dy_mid_f <- (brfy + brfx) / 2

des_col <- "#F8766D"
sub_col <- "#00BFC4"

pal <- c("Desenvolvido" = des_col, "Subdesenvolvido" = sub_col)

## Gráfico s*2024 (X) vs s*2025 (Y) com cores por Classificação
ggplot(cmp, aes(x = sstar_2024_pctPIB, y = sstar_2025_pctPIB)) +
  geom_point(
    aes(fill = Classificação),
    shape = 21, colour = "grey30", stroke = 0.5, alpha = 0.9, size = 3
  ) +
  # Brasil
  geom_point(
    data = brasil,
    aes(x = sstar_2024_pctPIB, y = sstar_2025_pctPIB),
    shape = 21, fill = NA, colour = "black", stroke = 1.2, size = 3.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = brasil,
    aes(x = sstar_2024_pctPIB, y = sstar_2025_pctPIB),
    label = "Brasil", vjust = -1.2, fontface = "bold"
  ) +
  # seta
  annotate(
    "segment",
    x = br_x, xend = br_x,
    y = br_x, yend = br_y,        
    colour = "black", alpha = 1, linewidth = 0.5,
    arrow = arrow(ends = "both", type = "closed", length = grid::unit(0.18, "cm"))
  ) +
  annotate(
    "text",
    x = br_x - dx_off, y = dy_mid,
    label = "Deterioração 
    fiscal",
    hjust = 1, vjust = 0.5,
    size = 3.5,
    fontface = 'bold',
    colour = "black"   
  ) +
  # Brazil (Focus) — ponto translúcido + rótulo
  geom_point(
    data = brasil_focus,
    aes(x = sstar_2024_pctPIB, y = sstar_2025_pctPIB),
    fill = scales::alpha(pal[brasil_focus$Classificação][1], 0.10),
    shape = 21, colour = "black", stroke = 1.0, size = 3.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = brasil_focus,
    aes(x = sstar_2024_pctPIB, y = sstar_2025_pctPIB),
    label = "Brasil (Focus)", vjust = -1, fontface = "italic",
    nudge_y = 0.1,
    size = 3
  ) +
  # reta 45°
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "red", linewidth = 0.7) +
  coord_equal(xlim = lims, ylim = lims) +
  theme_classic() +
  labs(
    title = "s* 2024 vs s* 2025 (em % do PIB)",
    x = "Superávit primário estabilizador – 2024 (% do PIB)",
    y = "Superávit primário estabilizador – 2025 (% do PIB)",
    fill = "Classificação",
    caption = "Dados: FMI (g, π e dívida/PIB) e World Government Bonds (juros de 5 anos/2024 e 2025)"
  ) +
  scale_fill_manual(
    values = c("Desenvolvido" = des_col, "Subdesenvolvido" = sub_col),
    guide = guide_legend(override.aes = list(shape = 21, colour = "grey30", size = 5))
  ) +
  scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  theme(
    plot.margin  = unit(c(1,0,0,0), "cm"),
    plot.title   = element_text(vjust = 7, size = 15, face = "bold.italic"),
    axis.text    = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(hjust = -0.1, margin = margin(t = 8)),
    plot.caption = element_text(size = 7, hjust = 0)
  )

ggsave("Grafico_sstar_2024_vs_2025_classificacao.png",
       width = 7.5, height = 3.92, dpi = 600)
