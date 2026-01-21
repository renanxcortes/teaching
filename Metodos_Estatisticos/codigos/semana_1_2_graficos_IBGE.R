#install.packages("sidrar")
#install.packages("ggplot2")
#install.packages("dplyr")

library(sidrar)
library(ggplot2)
library(dplyr)

# 🌱 Base do IBGE utilizada
# 
# SIDRA – Tabela 5457
# 
# Produção agrícola municipal – quantidade produzida (toneladas)
# 
# Culturas:
#   
#   Soja
# 
# Milho
# 
# Trigo
# 
# Região:
#   
#   Brasil (nível nacional)
# Período:
#   
#   2019 a 2023



dados_ibge <- get_sidra(
  api = "/t/5457/n1/all/v/214/p/2019-2023/c782/40124,40125,40126"
)

# Identificação das culturas
# 
# 40124 → Soja
# 
# 40125 → Milho
# 
# 40126 → Trigo





dados <- dados_ibge %>%
  select(
    Ano = `Ano (Código)`,
    Cultura = `Produto das lavouras temporárias e permanentes`,
    Producao_ton = Valor
  ) %>%
  mutate(
    Qualidade = factor(
      ifelse(Producao_ton > median(Producao_ton), "Alta", "Média"),
      levels = c("Baixa", "Média", "Alta"),
      ordered = TRUE
    )
  )

dados

  