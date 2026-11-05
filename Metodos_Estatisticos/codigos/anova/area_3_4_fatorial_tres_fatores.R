# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae", "lsmeans", "emmeans")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# ANOVA com Delineamento Fatorial (três fatores)

# Exemplo 5.3 (pg. 208) do Montgomery:
# A soft drink bottler is interested in obtaining more uniform
# fill heights in the bottles produced by his manufacturing
# process. The filling machine theoretically fills each bottle to
# the correct target height,but in practice, there is variation
# around this target,and the bottler would like to understand
# the sources of this variability better and eventually reduce it.
# The process engineer can control three variables during
# the filling process: the percent carbonation (A), the operat
# ing pressure in the filler (B), and the bottles produced per
# minute or the line speed (C). 



# ANOVA com Delineamento Fatorial (três fatores)



dados <- data.frame(
  Resposta = c(-3,-1,-1,1,-1,0,0,1,0,2,2,6,
               1,1,3,5,5,7,7,10,4,6,9,11),
  FatorA = rep(c('10', '12', '14'), c(12,8,4)),
  FatorB = rep(c('25', '25', '30', '30'), times = 6),
  FatorC = rep(c('200', '250'), times = 12)
)

# Transforma os fatores em variáveis categóricas

dados$FatorA <- factor(dados$FatorA)
dados$FatorB <- factor(dados$FatorB)
dados$FatorC <- factor(dados$FatorC)

# Ajuste da ANOVA

anova_drink <- aov(
  Resposta ~ FatorA*FatorB*FatorC,
  data = dados
)

summary(anova_drink)

# Diagnóstico dos resíduos

par(mfrow = c(2,2))
plot(anova_drink)
par(mfrow = c(1, 1))

# ----------------------------------------------------
# Médias ajustadas para os gráficos de interação
# ----------------------------------------------------

medias <- as.data.frame(
  emmeans(anova_drink, ~ FatorA*FatorB*FatorC)
)

# ----------------------------------------------------
# Interação A x B para cada nível de C
# ----------------------------------------------------

p1 <- ggplot(
  medias,
  aes(
    x = FatorA,
    y = emmean,
    color = FatorB,
    group = FatorB
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~FatorC) +
  labs(
    title = "Interação Fator A × Fator B",
    subtitle = "Painéis separados por Fator C",
    x = "Carbonatação (%)",
    y = "Média Ajustada",
    color = "Pressão"
  ) +
  theme_bw()
p1

# ----------------------------------------------------
# Interação A x C para cada nível de B
# ----------------------------------------------------

p2 <- ggplot(
  medias,
  aes(
    x = FatorA,
    y = emmean,
    color = FatorC,
    group = FatorC
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~FatorB) +
  labs(
    title = "Interação Fator A × Fator C",
    subtitle = "Painéis separados por Fator B",
    x = "Carbonatação (%)",
    y = "Média Ajustada",
    color = "Velocidade"
  ) +
  theme_bw()
p2

# ----------------------------------------------------
# Interação B x C para cada nível de A
# ----------------------------------------------------

p3 <- ggplot(
  medias,
  aes(
    x = FatorB,
    y = emmean,
    color = FatorC,
    group = FatorC
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~FatorA) +
  labs(
    title = "Interação Fator B × Fator C",
    subtitle = "Painéis separados por Fator A",
    x = "Pressão",
    y = "Média Ajustada",
    color = "Velocidade"
  ) +
  theme_bw()
p3

# Gerando todos de uma vez:

par(mfrow = c(1,3))

interaction.plot(
  dados$FatorA,
  dados$FatorB,
  dados$Resposta,
  xlab = "Fator A",
  ylab = "Média",
  trace.label = "Fator B"
)

interaction.plot(
  dados$FatorA,
  dados$FatorC,
  dados$Resposta,
  xlab = "Fator A",
  ylab = "Média",
  trace.label = "Fator C"
)

interaction.plot(
  dados$FatorB,
  dados$FatorC,
  dados$Resposta,
  xlab = "Fator B",
  ylab = "Média",
  trace.label = "Fator C"
)
