# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae", "lsmeans")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# ANOVA com Delineamento Fatorial

# Foi conduzido um experimento para avaliar o efeito do implante do 
# hormônio Stilbestrol sobre o ganho de peso de cordeiros machos e 
# fêmeas da raça Corriedale, em um intervalo de 180 dias após o implante.


# Dados

# Fator1: Hormônio Stilbestrol (S: Sem, C: Com)
# Fator2: Sexo (M - Macho, F - Fêmea)

dados <- data.frame(Resposta = c(22,25,27,26, 25,32,29,28, 32,30,28,34, 29,34,36,37),
                    Fator1 =   c('S','S','S','S','S','S','S','S', 'C','C','C','C','C','C','C','C'),
                    Fator2 =   c('F','F','F','F','M','M','M','M', 'F','F','F','F','M','M','M','M'))

# Fazendo a ANOVA

anova_hormo <- aov(Resposta ~ Fator1 + Fator2 + Fator1:Fator2, data=dados)
summary(anova_hormo)

par(mfrow = c(2, 2))
plot(anova_hormo) # Diagnósticos Gerais
par(mfrow = c(1, 1))

# Interaction plot

# Compute means for each combination of factors
medias <- dados %>%
  group_by(Fator1, Fator2) %>%
  summarise(Media = mean(Resposta, na.rm = TRUE), .groups = "drop")
ggplot(medias, aes(x = Fator1,
                   y = Media,
                   color = Fator2,
                   group = Fator2)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Interaction Plot",
    x = "Fator1",
    y = "Mean Response",
    color = "Fator2"
  ) +
  theme_minimal(base_size = 14)

# Gráfico alternativo:
with(dados,
     interaction.plot(
       x.factor = Fator1,
       trace.factor = Fator2,
       response = Resposta,
       fun = mean,
       type = "b",
       pch = 19,
       col = 1:length(unique(Fator2)),
       xlab = "Fator1",
       ylab = "Mean Response",
       trace.label = "Fator2"
     )
)


# Comparações múltiplas:
modelo_dcc_fator1 <- agricolae::HSD.test(anova_hormo, 'Fator1', alpha = 0.05, console=T)
modelo_dcc_fator1
plot(modelo_dcc_fator1)


modelo_dcc_fator2 <- agricolae::HSD.test(anova_hormo, 'Fator2', alpha = 0.05, console=T)
modelo_dcc_fator2
plot(modelo_dcc_fator2)

# Maneira alternativa para interação
plot(TukeyHSD(anova_hormo))


# ANOVA Fatorial + Blocos

# Num experimento fatorial, instalado em blocos casualizados com
# 4 repetições, avaliaram-se os fatores Adubo Mineral (A) (não = a1; sim = a2) e Adubo Orgânico (B) (não = b1; sim= b2). O
# bloqueamento teve por finalidade controlar diferenças de
# fertilidade do solo no campo experimental.


dados <- data.frame(Resposta = c(18, 19.6, 20.6, 19.2, 8.6, 15.0, 21, 19.6, 9.4, 14.6, 18.6, 18.4, 11.4, 15.8, 20.6, 20.2),
                    Bloco =  rep(c('1','2','3','4'), each = 4),
                    FatorA = rep(c('1','1','2','2'), times = 4),
                    FatorB = rep(c('1','2','1','2'), times = 4)) %>%
  mutate(Mineral = FatorA) %>%
  mutate(Organico = FatorB)


# Fazendo a ANOVA

anova_adubo <- aov(Resposta ~ Mineral + Organico + Mineral:Organico + Bloco, data=dados)
summary(anova_adubo)
plot(anova_adubo)


# Gráfico de médias:

# Compute means for each combination of factors
medias <- dados %>%
  group_by(Mineral, Organico) %>%
  summarise(Media = mean(Resposta, na.rm = TRUE), .groups = "drop")
ggplot(medias, aes(x = Mineral,
                   y = Media,
                   color = Organico,
                   group = Organico)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Interaction Plot",
    y = "Mean Response",
    x = "Mineral",
    group = "Orgânico"
  ) +
  theme_minimal(base_size = 14)

# Invertendo cores e eixo x 
ggplot(medias, aes(x = Organico,
                   y = Media,
                   color = Mineral,
                   group = Mineral)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Interaction Plot",
    y = "Mean Response",
    x = "Orgânico",
    group = "Mineral"
  ) +
  theme_minimal(base_size = 14)


# Comparações múltiplas:
modelo_dcc_fator1 <- agricolae::HSD.test(anova_adubo, 'Mineral', alpha = 0.05, console=T)
modelo_dcc_fator1
plot(modelo_dcc_fator1)


modelo_dcc_fator2 <- agricolae::HSD.test(anova_adubo, 'Organico', alpha = 0.05, console=T)
modelo_dcc_fator2
plot(modelo_dcc_fator2)

# Tabela de médias
model.tables(anova_adubo, type= "means")

# Comparações múltiplas Tukey dos níveis dos dois fatores:

comparacoes_post_hoc_fatorial_blocos <- lsmeans(anova_adubo, pairwise ~ Mineral/Organico, adjust = ("tukey"))
comparacoes_post_hoc_fatorial_blocos
plot(comparacoes_post_hoc_fatorial_blocos)
