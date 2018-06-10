#####################################################################################################
#
#                                              PROJETO
#
#####################################################################################################

# EMPRESA WKD

# Perguntas para esta tarefa
# "Nossa taxa de inadimplência é de 35%. Você acha que com o uso das suas técnicas, conseguimos baixar este índice para pelo menos, 25%?"

# Crie um modelos de machine learning e tente chegar a um índice de inadimplência aproximado, de 25%.

# Descreva algoritmos e técnicas utilizadas, bem como o índice alcançado.


# biblioteca
library(e1071)
# Abrindo Conjunto de dados
credito <- read.csv(file.choose(), sep = ';', header = T)
head(credito)

str(credito)
dim(credito)

# Criando uma amostra
amostra <- sample(2, 1000, replace = T, prob = c(0.7, 0.3))

# treino e teste
treino <- credito[amostra == 1,]
teste <- credito[amostra == 2,]

# Usando Rpart
library(rpart)

arvore <- rpart(CLASSE ~ ., data=credito, method = 'class')
print(arvore)

plot(arvore)
text(arvore, use.n = T, all = T, cex=.8)

# Previsao
teste_credito = predict(arvore, newdata = teste)
teste_credito

# Colocando coluna para ver a probabilidade de cada um
credito_class <- cbind(teste, teste_credito)
head(credito_class)

# Criando coluna se a probabilidade for maior que 0.8
credito_class['Resultado'] = ifelse(credito_class$ruim > 0.75, 'Mal', 'Bom')
fix(credito_class)

# Tabela confusao
confusao_cred <- table(credito_class$CLASSE,credito_class$Resultado)
confusao_cred

# Taxa de acerto
accert <- (confusao_cred[1] + confusao_cred[4]) / sum(confusao_cred)
accert

# Modelo
modelo <- naiveBayes(CLASSE ~ ., treino)
modelo

# Predição
predi_credito <- predict(modelo, teste)
predi_credito

# Tabela Confusao
confusao <- table(teste$CLASSE,predi_credito)
confusao

# Taxa de acerto
acerto <- (confusao[1] + confusao[4])/ sum(confusao)
acerto * 100


