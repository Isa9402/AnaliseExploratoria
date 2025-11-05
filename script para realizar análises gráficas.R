library(usethis)
use_git_config(
  user.name = "Isa9402",         
  user.email = "isabellarp@id.uff.br"     
)
usethis::create_github_token()
gitcreds::gitcreds_set()

#script para realizar análises gráficas:

library(readxl)     
library(dplyr)      
library(ggplot2)    
library(tidyr)      
library(forcats) 

base <- read_excel("Base_trabalho.xlsx")
head(base)

#mudando oa colunas qualitativas para fatores
base <- base %>%
  mutate(across(
    c(escolaridade, reincidente, sexo, casado), as.factor
  ))

#verificando dados faltantes
dados_faltando <- base %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variável",
               values_to = "Faltantes")
print(dados_faltando) #não há dados faltantes

#histograma da variável idade.
ggplot(base, aes(x = idade)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(base$idade, na.rm = TRUE), by = 10)) +
  labs(
    title = "Distribuição da Idade",
    x = "Idade",
    y = "Frequência"
  ) +
  theme_minimal()

#boxplot da variável tempo_preso
ggplot(base, aes(x = tempo_preso)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Boxplot do Tempo Preso",
       x = "Tempo Preso em meses") +
  theme_minimal()

#boxplot de score_periculosidade por escolaridade
ggplot(base, aes(y = escolaridade, x = score_periculosidade, fill = escolaridade)) +
  geom_boxplot() +
  labs(title = "Score de Periculosidade por Escolaridade",
       y = "Escolaridade",
       x = "Score de Periculosidade") +
  theme_minimal() +
  theme(legend.position = "none")

#gráfico de barras da variável reincidente
ggplot(base, aes(x = reincidente, fill = reincidente)) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Não", "1" = "Sim")) +
  scale_y_continuous(breaks = seq(0, max(table(base$reincidente)), by = 20)) +
  labs(
    title = "Distribuição de Reincidência",
    x = "Reincidente",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")