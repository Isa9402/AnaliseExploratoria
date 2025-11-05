library(readxl)
library(dplyr)
library(ggplot2)

#Importar a base e transformar variáveis qualitativas em fatores
base <- read_excel("Base_trabalho.xlsx")

base <- base %>%
  mutate(across(
    c(escolaridade, reincidente, sexo, casado),
    as.factor
  ))

#Média, 1º quartil, mediana e 3º quartil
analise <- base %>%
  summarise(
    media_score = mean(score_periculosidade, na.rm = TRUE),
    q1_score = quantile(score_periculosidade, 0.25, na.rm = TRUE),
    mediana_score = median(score_periculosidade, na.rm = TRUE),
    q3_score = quantile(score_periculosidade, 0.75, na.rm = TRUE),
    
    media_idade = mean(idade, na.rm = TRUE),
    q1_idade = quantile(idade, 0.25, na.rm = TRUE),
    mediana_idade = median(idade, na.rm = TRUE),
    q3_idade = quantile(idade, 0.75, na.rm = TRUE),
    
    media_tempo = mean(tempo_preso, na.rm = TRUE),
    q1_tempo = quantile(tempo_preso, 0.25, na.rm = TRUE),
    mediana_tempo = median(tempo_preso, na.rm = TRUE),
    q3_tempo = quantile(tempo_preso, 0.75, na.rm = TRUE)
  )

print(analise)

#Gráfico de dispersão entre tempo_preso e score_periculosidade
ggplot(base, aes(x = tempo_preso, y = score_periculosidade)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Dispersão: Tempo Preso x Score de Periculosidade",
    x = "Tempo Preso",
    y = "Score de Periculosidade"
  ) +
  theme_minimal()

#Correlação entre tempo_preso e score_periculosidade
correlacao <- cor(base$tempo_preso, base$score_periculosidade, use = "complete.obs")
cat("Correlação entre tempo_preso e score_periculosidade:", correlacao, "\n")
print(correlacao)


#Variância, desvio padrão e amplitude
var_dp_amp <- base %>%
  summarise(
    var_score = var(score_periculosidade, na.rm = TRUE),
    dp_score = sd(score_periculosidade, na.rm = TRUE),
    amp_score = max(score_periculosidade, na.rm = TRUE) - min(score_periculosidade, na.rm = TRUE),
    
    var_idade = var(idade, na.rm = TRUE),
    dp_idade = sd(idade, na.rm = TRUE),
    amp_idade = max(idade, na.rm = TRUE) - min(idade, na.rm = TRUE),
    
    var_tempo = var(tempo_preso, na.rm = TRUE),
    dp_tempo = sd(tempo_preso, na.rm = TRUE),
    amp_tempo = max(tempo_preso, na.rm = TRUE) - min(tempo_preso, na.rm = TRUE)
  )

print(var_dp_amp)