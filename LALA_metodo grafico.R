# Paso 1: Identificar las variables básicas
# x1 = Cantidad de yogurts a producirse 
# x2 = Cantidad de jugos a producirse 

# Paso 2: Identificar las variables "cj"
# c1 = $ 20 / yogur  
# c2 = $ 25 / jugos 
# Paso 3: Función objetivo
# Max z = (20/ yogur) + ($ 25 / jugos)
# Max z = 20x1 + 25x2 

# Paso 4: Identificar "bj"
# b1 = Capacidad máxima de yogur y jugos de fresa producir kg (4500)
# b2 = Capacidad máxima de yogurt y fresa de durazno a producir (6300)
# b3 = cantidad máxima de botellas para los productos (60,00 y 90,000)
# b5 = costo de yogurt y durazno(25 y 20)
# Paso 5: Identificar "aij"
# b3 = cantidad máxima de botellas para yogurt (60,000)
# b4 = cantidad máxima de botellas para jugos (90,000)
# b5 = costo de yogurt (25)
# b6 = costo de fresa (20)

# Paso 6: Armar restricciones
# b1 = Capacidad máxima de yogur y jugos de fresa producir kg (4500)
# (1.5kg/botella) + (1.8kg/botella)
# 1.5x1 + 1.8x2 <= 4500

# b2 = Capacidad máxima de yogurt y fresa de durazno a producir (6300)
# (1.2kg/botella) + (1.1kg/botella)
#1.2x1 + 1.1x2 <= 6300

# b3 = cantidad máxima de botellas para yogurt (60,000)
# x1 <= 60000

#b4 = cantidad máxima de botellas para jugos (90,000)
# x2 <= 90000
#b5 = costo de yogurt (25)
# x1 <= 25
#b6 = costo de fresa (20)
# x2 <= 20

# Paso 7: No negatividad
# x1, x2 >= 0

# Paso 8: Modelo general
# Max z = 20x1 + 25x2 
# s.a.
# 1.5x1 + 1.8x2 <= 4500
#1.2x1 + 1.1x2 <= 6300
# x1 <= 60000
# x2 <= 90000
# x1 <= 25
# x2 <= 20
# x1, x2 >= 0


# Paso 9 Graficar el sistema.

# Importar la libreria
library(matlib)

# Asignar los coeficientes de las restricciones
# 1.5x1  +  1.8x2 <= 4500    1.5  1.8  (1)
# 1.2x1  +  1.1x2 <= 6300    1.2  1.1  (2)
# x1              <= 60000    1    0   (3)
#           x2    <= 90000    0    1   (4)


# Definir el numero de columnas en 2 y el n?mero de filas en 4


A<-matrix(c(1.5,1.2,1,0,1.8,1.1,0,1), ncol = 2, nrow = 4)
A

# Asignar los valores del lado derecho de las desiguldades
# 1.5x1  +  1.8x2 <= 4500      4500    (1)
# 1.2x1  +  1.1x2 <= 6300      6300    (2)
# x1              <= 60000     60000   (3)        
#           x2    <= 90000     90000   (4)
b<- c(4500,6300,60000,90000)

# Se grafican las ecuaciones
plotEqn(A,b, xlim=c(20,10000), labels=TRUE)


# A)La interseccion de la restriccion 1 (linea negra) con el eje x2
# B)La interseccion de la restriccion 1 (linea negra) con el eje x1


# Interseccion A
#restriccion 1 linea negra con el eje x2 (R1-x2)

# Se resuelve de manera matricial como ;
# 1.5x1  +  1.8x2 <= 4500     
# x1              <= 0 

# entonces las matrices A y B quedan como:
# [A=
#             1.5  1.8
#             1     0
# ;B=
#             4500
#             0
# ]

# Definiendo matricialmente las funciones dentro de R, quedar?a como

A <- matrix(c(1.5,1.8,1,0), nrow = 2, ncol = 2, byrow = T)
A

B <- matrix(c(4500,0), nrow = 2, ncol = 1, byrow = F)
B

# Resolviendo nos queda como
r <- solve(t(A)%*%A)%*%t(A)%*%B
r



# Interseccion B
#Restriccion 2 linea negra con el eje x1 (R1-x1)
# 1.5x1  +  1.8x2 <= 4500
#          x2     <= 0

# entonces las matrices A y B quedan como:
# [A=
#             1.5     1.8
#              0       1
# ;B=
#             4500
#             0
# ]

# Definiendo matricialmente las funciones dentro de R, quedar?a como
A <- matrix(c(1.5,1.8,0,1), nrow = 2, ncol = 2, byrow = T)
A

B <- matrix(c(4500,0), nrow = 2, ncol = 1, byrow = F)
B

# Resolviendo nos queda como
r <- solve(t(A)%*%A)%*%t(A)%*%B
r

# Definiendo la funci?n objetivo

# Para ello escribe el c?digo como
val<-matrix(c(0,2500,3,-9.992007), nrow=2, ncol = 2, byrow=T)
val

FO<-matrix(c(20,25), nrow=2, ncol=1)
# Mostrar la matriz FO
FO

r=val%*%FO
r

##       [,1]
## [1,] -186441.21
## [2,] -495000.00
## [3,]  18545.34

# como se observa la soluci?n ?ptima del sistema es 62500 con las variables 
# X1= 0 y x2= 2500


