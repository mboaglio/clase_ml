# Arboles 

mydata=iris
attach(mydata)
#View(mydata)
head(mydata)
summary(mydata)

# Separamos el dataset training de test
s<-sample(150,100)
iris_train<-iris[s,]
iris_test<-iris[-s,]


# Le sacamos la clase al dataset de test
iris_test_sin_clase<-iris_test
iris_test_sin_clase$Species<-NULL


# Entrenamos el modelo de arbol
library(rpart)
arboliris<-rpart(Species~., iris_train, method="class")

# Analizamos la estructura del árbol, escribiendo
arboliris
summary(arboliris)

# Graficamos el árbol usando 
plot(arboliris) 
text(arboliris) 

# Otro grafico ... un poc mas lindo
library(rpart.plot)
rpart.plot(arboliris, type=1, extra=101)
rpart.plot(arboliris, type=4,extra=101, shadow.col="gray", branch.lty=3, box.palette="RdYlGn")

# Testeamos el árbol con el conjunto de testeo
p<-predict(arboliris, iris_test_sin_clase, type="class")

# miramos la prediccion
p

# Matriz de confusión
table(p, iris_test[,5])

# Matriz de confusión II
library(caret)
confusionMatrix(p, iris_test[,5])

