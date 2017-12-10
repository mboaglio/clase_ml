buscarmaximo <- function(vector) {
  mayor = -1
  for (i in 1:length(vector)) {
    if (vector[i] > mayor)
      mayor = vector[i]
  }
  return(mayor)
}

vet = c(3,6,8,9,1,2)
print(buscarmaximo(vet))

