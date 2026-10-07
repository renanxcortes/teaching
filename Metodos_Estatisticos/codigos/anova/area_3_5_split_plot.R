# Instalando/carregando pacotes necessários
packages <- c("dplyr", "ggplot2", "tidyr", "car", "agricolae", "lsmeans", "emmeans")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# -----------------------------
# Tensile Strength of Paper
# Montgomery Chapter 14
# Split-Plot Design
# -----------------------------


# The tensile strength of paper example in Montgomery's split-plot chapter has:
# 
# Whole-plot factor: Pulp Preparation Method (3 levels)
# Subplot factor: Cooking Temperature (4 levels)
# Replicates (blocks): 3
# Response: Tensile Strength
# 
# The data reported in Table 14.16 are:

paper <- data.frame(
  Replicate = factor(c(
    rep(1,12),
    rep(2,12),
    rep(3,12)
  )),
  
  Method = factor(c(
    rep(c(1,2,3), each = 4),
    rep(c(1,2,3), each = 4),
    rep(c(1,2,3), each = 4)
  )),
  
  Temperature = factor(rep(
    c(200,225,250,275),
    9
  )),
  
  Strength = c(
    # Replicate 1
    30,35,37,36,
    34,41,38,42,
    29,26,33,36,
    
    # Replicate 2
    28,32,40,41,
    31,36,42,40,
    31,30,32,40,
    
    # Replicate 3
    31,37,41,40,
    35,40,39,44,
    32,34,39,45
  )
)

paper <- paper %>% 
  mutate(Replicate = factor(Replicate),
         Method = factor(Method),
         Temperature = factor(Temperature))

paper

paper.aov <- aov(
  Strength ~ Method * Temperature +
    Error(Replicate/Method),
  data = paper
)

summary(paper.aov)
