# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# ANOVA com Delineamento de Blocos Completos Casualizados:

# Experimento lúdico realizado em aula

n_alunos_participantes <- 7
n_niveis_fator <- 3 # Qtd. diferentes de sabores

# Preencher com os dados das notas dos sabores
# Cada bloco tem uma repetição de cada nível do fator
# Letícia, Victor, Luna, Gabrielle, Rodrigo, Sara, Renan
dados <- data.frame(Sabor_A = c(10,8,7,10,9,9,5.5),
                    Sabor_B = c(7,9,9,9,10,10,5),
                    Sabor_C = c(8,9,8,7,9,9,6))

# Reorganizando os dados
dados_reorg <- tidyr::pivot_longer(dados, 
                                  cols = c('Sabor_A', 'Sabor_B', 'Sabor_C'),
                                  names_to = 'Fator',
                                  values_to = 'Notas') %>%
  mutate(Fator = factor(Fator)) %>%
  mutate(Bloco = factor(paste0('Participante_',
                               rep(1:n_alunos_participantes,
                                   each = n_niveis_fator)))) # Nomes dos participantes

# Estatísticas Descritivas
dados_reorg %>%
  group_by(Fator) %>%
  summarise(
    count = n(),
    mean = mean(Notas, na.rm = TRUE),
    sd = sd(Notas, na.rm = TRUE),
    median = median(Notas, na.rm = TRUE)
  )

dados_reorg %>%
  group_by(Bloco) %>%
  summarise(
    count = n(),
    mean = mean(Notas, na.rm = TRUE),
    sd = sd(Notas, na.rm = TRUE),
    median = median(Notas, na.rm = TRUE)
  )



#-----------------------------
# Gráficos Exploratórios
#-----------------------------

###########################################
# Explorando médias das notas dos Sabores #
###########################################

# De pontos
ggplot(dados_reorg) +
  aes(x = Fator, y = Notas, color = Fator) +
  geom_jitter(width = 0.0) +
  theme(legend.position = "none") +
  theme_classic()

# Boxplot
ggplot(dados_reorg, aes(x = Fator, y = Notas, fill = Fator)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Distribuição da Notas por nível do fator",
    x = "Fator",
    y = "Notas"
  )

# Com intervalo de confiança
ggplot(dados_reorg, aes(x = Fator, y = Notas)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "Médias com IC 95%",
    x = "Fator",
    y = "Notas"
  )

# Densidade por nível do tratamento
# Means by treatment
medias <- dados_reorg %>%
  group_by(Fator) %>%
  summarise(
    media = mean(Notas, na.rm = TRUE)
  )

# Separate density plots
ggplot(dados_reorg, aes(x = Notas, fill = Fator)) +
  
  # Density curve
  geom_density(alpha = 0.5) +
  
  # Mean line
  geom_vline(
    data = medias,
    aes(xintercept = media),
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # Mean label
  geom_text(
    data = medias,
    aes(
      x = media,
      y = 0,
      label = paste0("Média = ", round(media, 2))
    ),
    angle = 90,
    vjust = -0.75,
    hjust = -0.5,
    size = 3
  ) +
  
  # One panel per treatment
  facet_wrap(~ Fator, scales = "free_y") +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Distribuição da Notas por tratamento",
    subtitle = "Cada painel representa um nível do fator",
    x = "Notas",
    y = "Densidade"
  ) +
  
  theme(
    legend.position = "none"
  )



#################################################
# Explorando médias das notas dos Participantes #
#################################################

# De pontos
ggplot(dados_reorg) +
  aes(x = Bloco, y = Notas, color = Bloco) +
  geom_jitter(width = 0.0) +
  theme(legend.position = "none") +
  theme_classic()

# Boxplot
ggplot(dados_reorg, aes(x = Bloco, y = Notas, fill = Bloco)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Distribuição da Notas por nível do Bloco",
    x = "Bloco",
    y = "Notas"
  )

# Com intervalo de confiança
ggplot(dados_reorg, aes(x = Bloco, y = Notas)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "Médias com IC 95%",
    x = "Bloco",
    y = "Notas"
  )

# Densidade por nível do tratamento
# Means by treatment
medias <- dados_reorg %>%
  group_by(Bloco) %>%
  summarise(
    media = mean(Notas, na.rm = TRUE)
  )

# Separate density plots
ggplot(dados_reorg, aes(x = Notas, fill = Bloco)) +
  
  # Density curve
  geom_density(alpha = 0.5) +
  
  # Mean line
  geom_vline(
    data = medias,
    aes(xintercept = media),
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # Mean label
  geom_text(
    data = medias,
    aes(
      x = media,
      y = 0,
      label = paste0("Média = ", round(media, 2))
    ),
    angle = 90,
    vjust = -0.75,
    hjust = -0.5,
    size = 3
  ) +
  
  # One panel per treatment
  facet_wrap(~ Bloco, scales = "free_y") +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Distribuição da Notas por tratamento",
    subtitle = "Cada painel representa um nível do Bloco",
    x = "Notas",
    y = "Densidade"
  ) +
  
  theme(
    legend.position = "none"
  )




###########################
# Estimando o Modelo ---- #
###########################
anova_blocos <- aov(Notas ~ Fator + Bloco, data=dados_reorg)
summary(anova_blocos)

par(mfrow = c(2, 2))
plot(anova_blocos) # Diagnósticos Gerais
par(mfrow = c(1, 1))

# Comparações múltiplas:
modelo_dcc_blocos <- agricolae::HSD.test(anova_blocos, 'Fator', alpha = 0.05, console=T)
modelo_dcc_blocos
plot(modelo_dcc_blocos)

# Maneira alternativa
plot(TukeyHSD(anova_blocos))

