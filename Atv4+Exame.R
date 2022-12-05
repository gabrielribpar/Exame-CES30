
library(tidyverse)
library(readr)

Clientes <- read_csv("Clientes.csv")
Clientes$CD_CLIENTE <- as.factor(Clientes$CD_CLIENTE)
Clientes$INADIMPLENTE <- as.factor(Clientes$INADIMPLENTE)
Clientes$TEM_AUXILIO <- as.factor(Clientes$TEM_AUXILIO)
Clientes$TEM_BOLSAFAMILIA <- as.factor(Clientes$TEM_BOLSAFAMILIA)
Clientes$CD_UF <- as.factor(Clientes$CD_UF)
Clientes$CD_MUN <- as.factor(Clientes$CD_MUN)
Clientes$Bancarizado <- as.factor(Clientes$Bancarizado)
Clientes$OCUPACAO <- as.factor(Clientes$OCUPACAO)
Clientes$CD_MESO <- as.factor(Clientes$CD_MESO)

summary(Clientes)

hist(Clientes$SCORE, xlab="Score", main="Dados-Score")
clientes.por.estado <- Clientes %>% group_by(SG_UF) %>% summarise(total_count=n()) %>% as.data.frame()
inadimplentes.por.estado <- Clientes %>% group_by(SG_UF) %>% summarise(count = sum(INADIMPLENTE == 1)) %>% as.data.frame()
inadimplencia <- merge(clientes.por.estado, inadimplentes.por.estado)
inadimplencia$percentage <- (inadimplencia$count / inadimplencia$total_count)

ggplot(inadimplencia, aes(x = SG_UF, y = percentage)) + geom_bar(stat='identity') + labs(title = "Porcentagem de inadimplentes por estado", x = "Estado", y = "% de inadimplentes")
ggplot(inadimplencia, aes(x = SG_UF, y = count)) + geom_bar(stat='identity') + labs(title = "Inadimplentes por estado", x = "Estado", y = "# de inadimplentes")

Clientes.numeric <- read_csv("Clientes.csv")
Clientes.numeric <- Clientes.numeric %>% select(-NM_MUN) %>% select(-SG_UF) %>% select(-NM_REGIAO)
Clientes.numeric$Bancarizado <- Clientes.numeric$Bancarizado %>% as.numeric()
Clientes.numeric$Bancarizado[is.na(Clientes.numeric$Bancarizado)] <- 0
Clientes.numeric$CD_UF <- Clientes.numeric$CD_UF %>% as.numeric()
Clientes.numeric$CD_UF[is.na(Clientes.numeric$CD_UF)] <- 0
corr <- cor(Clientes.numeric)
library(corrplot)
corrplot(corr, method='number')


corplot <- cor(Clientes.numeric)
library(corrplot)
corrplot(corplot, method = "number")



Clientes.numeric$Bancarizado <- NULL
Clientes.numeric$PF_VOL <- NULL
Clientes.numeric$PF_VLR <- NULL
Clientes.numeric$PF_MOD02 <- NULL
Clientes.numeric$PF_TPR <- NULL
Clientes.numeric$CD_MUN <- NULL
Clientes.numeric$CD_UF <- NULL
Clientes.numeric$PF_TDC <- NULL

corr <- cor(Clientes.numeric)
library(corrplot)
corrplot(corr, method='number')


corplot <- cor(Clientes.numeric)
library(corrplot)
corrplot(corplot, method = "number")

