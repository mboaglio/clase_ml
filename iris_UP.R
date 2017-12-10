mydata=data(iris)
attach(iris)


# Analizamos el dataset
head(iris)
summary(iris)

# Scatterplots
plot(iris)
pairs(iris[1:4], pch = 21, bg = c("darkblue", "darkgreen", "red")[unclass(iris$Species)])

#boxplot
boxplot(iris$Petal.Length~iris$Species, xlab="Especie", ylab="Longitud del petalo",
        col = c("darkblue","darkgreen","red"))

#histogramas
#par(mfrow = c(2,2))
hist(iris$Petal.Length[1:50], border="darkblue", xlab="Long de Petalo", ylab = "Frec", main="Iris setosa", breaks = "FD")
hist(iris$Petal.Length[51:100], border="darkgreen", xlab="Long de Petalo", ylab = "Frec", main="Iris setosa", breaks = "FD")
hist(iris$Petal.Length[101:150], border="red", xlab="Long de Petalo", ylab = "Frec", main="Iris setosa", breaks = "FD")
hist(iris$Petal.Length, border="red", xlab="Long de Petalo", ylab = "Frec", main="Iris setosa", breaks = "FD")


# Vamos a entrenar un modelo Arbol
# Separo Dataset de Training / Testing
s<-sample(150,100) 
iris_train<-iris[s,] 
iris_test<-iris[-s,]

summary(iris_train$Species)

# Entreno el modelo
library(rpart)
arboliris<-rpart(Species~., iris_train, method="class") # modelo contenedor <- modelo (formula (VAR target ~ (?uflo) V1+v5+v6(variables independiente). En R el . me indica que uso todas las variables), base, parametros)
arboliris

# Muestro el modelo
library(rpart.plot)
rpart.plot(arboliris, type=4, extra=101)

# Predigo la clase del dataset de testing
test_sin_clase<-iris_test
test_sin_clase$Species<-NULL
p<-predict(arboliris, test_sin_clase, type="class")
p

# Muestro matriz de confusion
head(iris_test)
table(iris_test[,5],p) 

confusionMatrix(p[,1], iris_test$V1) # <-- El modelo predice siempre 1 !!!
table(p,iris_test$V1)

