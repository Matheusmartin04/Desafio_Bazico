rm(list = ls())

pacman::p_load(readxl, dplyr, tidyverse,nortest,rpart, cluster, rpart.plot)


# Diretorio de trabalho
setwd("F:\\bazico")


# BASE ====
## Importando as bases ====
clientes <- read_excel("clientes.xlsx")
vendas_de_produtos <- read_excel("vendas_de_produtos.xlsx")

## convertendo as variaveis em numerico e fatores ====
vendas_de_produtos$ID_Cliente = as.factor(vendas_de_produtos$ID_Cliente)
vendas_de_produtos$ID_Produto = as.factor(vendas_de_produtos$ID_Produto)
vendas_de_produtos$ID_Pedido = as.factor(vendas_de_produtos$ID_Pedido)

vendas_de_produtos$Quantidade = as.numeric(vendas_de_produtos$Quantidade)
vendas_de_produtos$Desconto = as.numeric(vendas_de_produtos$Desconto)
vendas_de_produtos$Frete = as.numeric(vendas_de_produtos$Frete)
vendas_de_produtos$Total_do_Pedido = as.numeric(vendas_de_produtos$Total_do_Pedido)
vendas_de_produtos$Data <- as.Date(vendas_de_produtos$Data)

## Calculando total de compra e valor por Cliente e Data; organindo a veriavel Desconto e Frete ====
base1 <- vendas_de_produtos %>% 
  mutate(valor_compra = Quantidade * Preço_Unitário) %>%
  group_by(ID_Cliente, Data) %>% 
  summarise(Quantidade = sum(Quantidade), valor_compra = sum(valor_compra), Desconto = mean(Desconto), 
            Frete = mean(Frete))

## Calculando o valor final da compra por cliente e data; criando a variavel LifeTime value (LTV) ====
base1 <- base1 %>% mutate(valor_final_compra = (valor_compra - Desconto + Frete)) %>% 
  group_by(ID_Cliente) %>% 
  mutate(LTV = sum(valor_final_compra))

## Conta a quantidade de vezes que o cliente visitou a loja ====
base1 <- base1 %>%
  group_by(ID_Cliente) %>%
  mutate(quantidade_visitas = n())

## Criando a variavel 'Recompra" ====
base1 <- base1 %>%
  group_by(ID_Cliente) %>%
  mutate(Recompra_Loja = ifelse(quantidade_visitas >= 2, "Sim","Não"))

## Juntando a base manipulada com a base de clientes e retirando os valores faltantes. ====
base <- merge(clientes,base1 ,by = "ID_Cliente")
base <- na.omit(base)

base$Bairro <-  as.factor(base$Bairro)
base$Cidade <-  as.factor(base$Cidade)
base$Estado <-  as.factor(base$Estado)

base$Recompra_Loja = factor(base$Recompra_Loja, levels = c("Sim","Não"))

# Obtendo Insigths
base %>% 
  group_by(Estado) %>% 
  summarise(quantidade_clientes = n(), 
            percentual_clientes = round(n() / nrow(base) * 100, 2),
            média_LTV = round(mean(LTV),2), 
            Mediana_LTV = round(median(LTV),2)) %>% 
  view()



# definir a proporção do conjunto de treinamento
prop_treino <- 0.8

# definir uma semente aleatória para reprodutibilidade
set.seed(124)

# criar um vetor de índices aleatórios para dividir a base em treino e teste
indices <- sample(nrow(base), nrow(base)*prop_treino)

# selecionar as observações para o conjunto de treinamento
dados_treino <- base[indices,]

# selecionar as observações para o conjunto de teste
dados_teste <- base[-indices,]

# MODELO DE arvore de Decisão ====

fit = rpart(Recompra_Loja ~ Cidade+Estado+Quantidade+Desconto+Frete+valor_final_compra+LTV,
            method = "class",
            data = dados_treino)

barplot(fit$variable.importance)

printcp(fit)
plotcp(fit)


rpart.plot(fit, # method graph
           type = 0,
           extra = 100,
           box.palette = 'GnBu',
           branch.lty = 2,
           shadow.col = "gray",
           nn = TRUE,
           cex = 1)


glimpse(dados_teste)
# fazer a previsão de recompra para o conjunto de teste
predicao_teste <- predict(fit, newdata = dados_teste, type = "class")

# avaliar a precisão do modelo usando o conjunto de teste
matriz_confusao <- table(predicao_teste, dados_teste$Recompra_Loja)
matriz_confusao

acuracia <- sum(diag(matriz_confusao))/sum(matriz_confusao)
acuracia

# fazer a previsão das probabilidades de recompra para o conjunto de teste
probabilidade_teste <- predict(fit, newdata = dados_teste, type = "prob")

# visualizar as probabilidades previstas
View(probabilidade_teste)

# juntar as probabilidades previstas ao conjunto de teste
dados_teste_com_prob <- cbind(dados_teste, probabilidade_teste[,1])

# renomear a coluna das probabilidades previstas
colnames(dados_teste_com_prob)[ncol(dados_teste_com_prob)] <- "probabilidade_recompra"

# Lista dos clientes e a probabilidade de recompra
dados_teste_com_prob %>% 
  select(ID_Cliente, probabilidade_recompra) %>% 
  group_by(ID_Cliente) %>% 
  summarise(probabilidade = round(mean(probabilidade_recompra),4)) %>% 
  arrange(desc(probabilidade)) %>% View()
