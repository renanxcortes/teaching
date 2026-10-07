# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# ANOVA com Delineamento de Blocos Completos Casualizados:


# # Num ensaio comparativo de cinco cultivares de
# trigo foi usado o delineamento em blocos casualizados, com
# quatro repetições. O bloqueamento teve por finalidade
# controlar diferenças de fertilidade do solo no campo
# experimental.Os rendimentos de grão por parcela, em t/ha,
# aparecem na tabela

# Cada bloco tem uma repetição de cada nível do fator
dados <- data.frame(A = c(1.9, 1.7, 1.7, 1.3),
                    B = c(2.4, 2.8, 2.7, 2.2),
                    C = c(2.4, 1.9, 2.3, 1.7),
                    D = c(3.6, 2.8, 2.5, 2.7),
                    E = c(2.7, 2.3, 2.2, 1.9))

# Reorganizando os dados
cultivares <- tidyr::pivot_longer(dados, 
                              cols = c('A', 'B', 'C', 'D', 'E'),
                              names_to = 'Fator',
                              values_to = 'Resposta') %>%
  mutate(Fator = factor(Fator)) %>%
  mutate(Bloco = factor(rep(1:4, each = 5)))


cultivares %>%
  group_by(Fator) %>%
  summarise(
    count = n(),
    mean = mean(Resposta, na.rm = TRUE),
    sd = sd(Resposta, na.rm = TRUE),
    median = median(Resposta, na.rm = TRUE)
  )


anova_culti <- aov(Resposta ~ Fator + Bloco, data=cultivares)
summary(anova_culti)

par(mfrow = c(2, 2))
plot(anova_culti) # Diagnósticos Gerais
par(mfrow = c(1, 1))

# Comparações múltiplas:
modelo_dcc_blocos <- agricolae::HSD.test(anova_culti, 'Fator', alpha = 0.05, console=T)
modelo_dcc_blocos
plot(modelo_dcc_blocos)

# Maneira alternativa
plot(TukeyHSD(anova_culti))



tukey.test2 <- agricolae::HSD.test(anova_culti, trt = 'Fator')
tukey.test2


post_hoc <- TukeyHSD(anova_culti)
post_hoc




