# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae", "readr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Exemplo de ANOVA #
# Os dados referem-se ao rendimentode 4 variedades de cana-de-açúcar (em t/ha) de um experimento de competição,instalado no delineamento completamente casualizado
# Objetivo: identificar a (ou as) variedade que produz maior rendimento médio

# Ler os dados:
# Maneira 1:
# dados <- readr::read_csv2('dados_anova_dcc.csv')

# Maneira 2:
dados <- data.frame(A = c(64,72,68,77,56,95),
                    B = c(78,91,97,82,85,77),
                    C = c(75,93,78,71,63,76),
                    D = c(55,66,49,64,70,68))

# Reorganizando os dados
dados2 <- tidyr::pivot_longer(dados, 
                              cols = c('A', 'B', 'C', 'D'),
                              names_to = 'Fator',
                              values_to = 'Resposta') %>%
  mutate(Fator = factor(Fator))

# Workflow usual de uma ANOVA #

# Estatísticas Descritivas
dados2 %>%
  group_by(Fator) %>%
  summarise(
    count = n(),
    mean = mean(Resposta, na.rm = TRUE),
    sd = sd(Resposta, na.rm = TRUE),
    median = median(Resposta, na.rm = TRUE)
  )


#-----------------------------
# Gráficos Exploratórios
#-----------------------------

# De pontos
ggplot(dados2) +
  aes(x = Fator, y = Resposta, color = Fator) +
  geom_jitter(width = 0.0) +
  theme(legend.position = "none") +
  theme_classic()

# Boxplot
ggplot(dados2, aes(x = Fator, y = Resposta, fill = Fator)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Distribuição da resposta por nível do fator",
    x = "Fator",
    y = "Resposta"
  )

# Com intervalo de confiança
ggplot(dados2, aes(x = Fator, y = Resposta)) +
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
    y = "Resposta"
  )

# Densidade por nível do tratamento
# Means by treatment
medias <- dados2 %>%
  group_by(Fator) %>%
  summarise(
    media = mean(Resposta, na.rm = TRUE)
  )

# Separate density plots
ggplot(dados2, aes(x = Resposta, fill = Fator)) +
  
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
    title = "Distribuição da resposta por tratamento",
    subtitle = "Cada painel representa um nível do fator",
    x = "Resposta",
    y = "Densidade"
  ) +
  
  theme(
    legend.position = "none"
  )

#-----------------------------
# Ajuste de uma ANOVA de um fator
#-----------------------------

modelo <- aov(Resposta ~ Fator, data = dados2)

# Tabela da decomposição da variância da ANOVA
summary(modelo)

#-----------------------------
# Diagnóstico das suposições
#-----------------------------

# Resíduos
par(mfrow = c(2, 2))
plot(modelo) # Diagnósticos Gerais
par(mfrow = c(1, 1))

# Plot de resíduos mais bonito
# Residuals vs. Fitted Plot
ggplot(modelo, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

# Q-Q Plot for Normality
ggplot(modelo, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")



# Normality of residuals
shapiro.test(residuals(modelo))

# Homogeneity of variances
bartlett.test(Resposta ~ Fator, data = dados2) # Teste de Bartlett
car::leveneTest(Resposta ~ Fator, data = dados2) # Teste de Levene

#-----------------------------
# Group means
#-----------------------------

model.tables(modelo, type = "means")

#-----------------------------
# Post-hoc comparisons
#-----------------------------

post_hoc <- TukeyHSD(modelo)
post_hoc

plot(post_hoc)

# Maneira alternativa usando o agricolae (deve-se instalar o pacote)
tukey.test2 <- agricolae::HSD.test(modelo, trt = 'Fator')
tukey.test2

plot(tukey.test2)

# Alternativa com emmeans (deve-se instalar o pacote)
# library(emmeans)
# emmeans(modelo, pairwise ~ Fator)



