#Intalando pacotes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("solitude")
install.packages("ggplot2")
install.packages("readr")
install.packages("caret")

#Carregando os pacotes nessa sesão
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)
library(readr)
library(caret)

#Carregando os dados históricos
dados_historicos = read.csv("dados_historicos.csv")
view(dados_historicos)

#Cria modelo de ML com algoritmo isolationForest
?isolationForest
modelo_ml = isolationForest$new()

modelo_ml$fit(dados_historicos)

previsoes_historico = dados_historicos %>%
  modelo_ml$predict() %>%
  arrange(desc(anomaly_score))

view(previsoes_historico)

plot(density(previsoes_historico$anomaly_score))

#0.62 pois a maioria dos dados estão antes de 0.60
indices_historico = previsoes_historico[which(previsoes_historico$anomaly_score > 0.62)]

anomalias_hsitorico = dados_historicos[indices_historico$id,]
normais_historico = dados_historicos[-indices_historico$id,]

colors()
ggplot() +
  geom_point(data = normais_historico,
             mapping = aes(transacao1, transacao2),
             col = "skyblue3",
             alpha = 0.5) +
  geom_point(data = anomalias_hsitorico,
             mapping = aes(transacao1, transacao2),
             col = "red2",
             alpha = 0.8)

novos_dados = read.csv("novos_dados.csv")
view(novos_dados)


previsoes_novos_dados = modelo_ml$predict(novos_dados)
indices_novos_dado = previsoes_novos_dados[which(previsoes_novos_dados$anomaly_score > 0.62)]

anomalias_novos_dados = novos_dados[indices_novos_dado$id,]
normais_novos_dados = novos_dados[-indices_novos_dado$id,]

ggplot() +
  geom_point(data = normais_novos_dados,
             mapping = aes(transacao1, transacao2),
             col = "skyblue3",
             alpha = 0.5) +
  geom_point(data = anomalias_novos_dados,
             mapping = aes(transacao1, transacao2),
             col = "red2",
             alpha = 0.8)

previsoes_novos_dados = previsoes_novos_dados %>%
  mutate(anomaly_score = round(anomaly_score, 2))

previsoes_novos_dados = previsoes_novos_dados %>%
  mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

library(ggplot2)

ggplot(previsoes_novos_dados, aes(x = status, y = anomaly_score, fill = status)) +
  geom_boxplot() +
  labs(title = "Box Plot de anomalias e normais", 
       x = "Status",
       y = "Anomaly score") +
  theme_minimal() +
  scale_fill_manual(values = c("anomalia" = "red", "normal" = "blue")) + 
  theme(legend.position = "none")

write.csv(previsoes_novos_dados, "previsoes_novos_dados.csv")