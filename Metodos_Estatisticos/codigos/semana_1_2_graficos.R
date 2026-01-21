library(ggplot2)
library(dplyr)


dados <- data.frame(
  Ano = c(2020, 2021, 2022, 2023, 2024),
  Cultura = c("Milho", "Soja", "Trigo", "Milho", "Soja"),
  Qualidade = factor(c("Alta", "Média", "Alta", "Média", "Alta"),
                     levels = c("Baixa", "Média", "Alta"),
                     ordered = TRUE), # ordinal
  Producao_ton = c(500, 620, 450, 580, 700),
  Area_ha = c(200, 250, 180, 220, 260)
)

dados


g_linha <- ggplot(dados, aes(x = Ano, y = Producao_ton)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produção Agropecuária ao Longo dos Anos",
    x = "Ano",
    y = "Produção (toneladas)"
  ) +
  theme_minimal()

ggsave("Metodos_Estatisticos/grafico_linha_producao.png", g_linha, width = 8, height = 5)


g_serie <- ggplot(dados, aes(x = Ano, y = Producao_ton)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Série Temporal da Produção Agropecuária",
    x = "Ano",
    y = "Produção (toneladas)"
  ) +
  theme_classic()

ggsave("Metodos_Estatisticos/serie_temporal_producao.png", g_serie, width = 8, height = 5)


dados_cultura <- dados %>%
  group_by(Cultura) %>%
  summarise(Producao_media = mean(Producao_ton))

g_coluna <- ggplot(dados_cultura, aes(x = Cultura, y = Producao_media)) +
  geom_col() +
  labs(
    title = "Produção Média por Cultura",
    x = "Cultura",
    y = "Produção média (toneladas)"
  ) +
  theme_minimal()

ggsave("Metodos_Estatisticos/grafico_coluna_producao.png", g_coluna, width = 8, height = 5)


dados_area <- dados %>%
  group_by(Cultura) %>%
  summarise(Area_media = mean(Area_ha))

g_barra <- ggplot(dados_area, aes(x = Area_media, y = Cultura)) +
  geom_col() +
  labs(
    title = "Área Média Plantada por Cultura",
    x = "Área média (ha)",
    y = "Cultura"
  ) +
  theme_minimal()

ggsave("Metodos_Estatisticos/grafico_barra_area.png", g_barra, width = 8, height = 5)


pizza <- dados %>%
  group_by(Cultura) %>%
  summarise(Producao_total = sum(Producao_ton)) %>%
  mutate(
    Percentual = Producao_total / sum(Producao_total),
    Label = paste0(
      Cultura, "\n",
      round(Percentual * 100, 1), "%"
    )
  )
g_pizza <- ggplot(pizza, aes(x = "", y = Percentual, fill = Cultura)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Participação da Produção por Cultura"
  ) +
  theme_void()

ggsave("Metodos_Estatisticos/grafico_pizza_producao_simulado.png", g_pizza,  width = 6,  height = 6)



# Histograma em outlier, histograma sem outlier e boxplot





dados <- data.frame(
  Ano = c(2020, 2021, 2022, 2023, 2024),
  Cultura = c("Milho", "Soja", "Trigo", "Milho", "Soja"),
  Qualidade = factor(c("Alta", "Média", "Alta", "Média", "Alta"),
                     levels = c("Baixa", "Média", "Alta"),
                     ordered = TRUE), # ordinal
  Producao_ton = c(500, 620, 450, 580, 700),
  Area_ha = c(200, 250, 180, 220, 260)
)



dados
g_hist <- ggplot(dados, aes(x = Producao_ton)) +
  geom_histogram(
    bins = 6,
    color = "black",
    fill = "lightgray"
  ) +
  labs(
    title = "Histograma da Produção Agropecuária",
    x = "Produção (toneladas)",
    y = "Frequência"
  ) +
  theme_minimal()

g_hist



# Gráficos Simulados COM OUTLIER #
dados <- data.frame(
  Ano = c(2020, 2021, 2022, 2023, 2024, 2025),
  Cultura = c("Milho", "Soja", "Trigo", "Milho", "Soja", "Soja"),
  Producao_ton = c(500, 620, 450, 580, 700, 1500), # 1500 = outlier
  Area_ha = c(200, 250, 180, 220, 260, 300)
)

g_hist <- ggplot(dados, aes(x = Producao_ton)) +
  geom_histogram(
    bins = 6,
    color = "black",
    fill = "lightgray"
  ) +
  labs(
    title = "Histograma da Produção Agropecuária",
    x = "Produção (toneladas)",
    y = "Frequência"
  ) +
  theme_minimal()

g_hist



g_box <- ggplot(dados, aes(y = Producao_ton)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Box-Plot da Produção Agropecuária",
    y = "Produção (toneladas)"
  ) +
  theme_minimal()

g_box
