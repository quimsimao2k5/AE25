library(tidyverse)
library(dplyr)
library(ggplot2)
set.seed(1234)
n <- 200
alunos <- tibble(
  id            = 1:n,
  curso         = sample(c("CC", "SI"), n, replace = TRUE, prob = c(.65, .35)),
  ano           = sample(1:3, n, replace = TRUE, prob = c(.4, .35, .25)),
  sexo          = sample(c("F", "M"), n, replace = TRUE),
  horas_estudo  = round(rlnorm(n, meanlog = log(6), sdlog = 0.5), 1),
  faltas        = pmax(0, round(rpois(n, lambda = 3) - rbinom(n, 1, .1)*3)),
  nota          = pmin(20,
                       pmax(0,
                            round( 8 +
                                     0.6 * pmin(horas_estudo, 20) -
                                     0.4 * pmin(faltas, 10) +
                                     rnorm(n, 0, 2.5), 1)))
) %>%
  mutate(
    # introduzir alguns outliers na nota
    nota = replace(nota, sample(1:n, 3), c(2, 19.5, 0)),
    # introduzir NAs em horas_estudo
    horas_estudo = replace(horas_estudo, sample(1:n, 6), NA_real_)
  )

head(alunos)

mean(alunos$nota)

median(alunos$nota)

IQR(alunos$nota)

sd(alunos$nota)

summary(alunos) 

q1=8.75
q3=13.3
tk1=q1-1.5*IQR(alunos$nota)
tk1
tk3 = q3 + 1.5*IQR(alunos$nota)
tk3
boxplot(alunos$nota)

# exemplo com a coluna nota
boxplot.stats(alunos$nota)$out

outliers <- boxplot.stats(alunos$nota)$out
alunos %>% filter(nota %in% outliers)


ggplot(alunos, aes(x = "", y = nota)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, alpha = 0.5)

ggplot(alunos,aes(x=nota)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color="black") +
  labs(title="Histograma de Notas", x="Nota",y="Frequência")

ggplot(alunos, aes(x = nota)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma com Curva de Densidade",
       x = "Nota",
       y = "Densidade")


ggplot(alunos, aes(x = curso, y = nota, fill = curso)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuição da nota por curso",
       x = "Curso", y = "Nota") +
  theme_minimal()

ggplot(alunos, aes(x = sexo, y = nota, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuição da nota por sexo",
       x = "Sexo", y = "Nota") +
  theme_minimal()

tapply(alunos$nota,alunos$curso,mean)

alunos %>%
  group_by(curso, sexo) %>%
  summarise(
    n       = n(),
    media   = mean(nota, na.rm = TRUE),
    mediana = median(nota, na.rm = TRUE),
    IQR     = IQR(nota, na.rm = TRUE)
  )

alunos_clean <- alunos %>% filter(!is.na(horas_estudo))

ggplot(alunos_clean,aes(x=horas_estudo,y=nota)) +
  geom_point(alpha = 0.6, color = "steelblue")+
  labs(
    title = "Relação entre Horas de Estudo e Nota",
    x = "Horas de Estudo por semana",
    y = "Nota final"
  ) +
  theme_minimal()

# Pearson (linear)
corpearson <- cor(alunos_clean$horas_estudo, alunos_clean$nota, method = "pearson")

# Spearman (ordem/rank)
corspearman <- cor(alunos_clean$horas_estudo, alunos_clean$nota, method = "spearman")

corspearman

corpearson

