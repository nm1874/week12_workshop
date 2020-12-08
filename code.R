#1. 

F<-function(v) v[1]*v[3]-v[2]^2
v0 <- c(4,2,1)

g<-function(v) c(v[1]^2, v[1]*v[2]^2, v[2]^4)

v1<-c(2, 1)
#a) 
Dg <- jacobian(g, v1); Dg

#since the basis is the image of Dg 
#the basis is c(4,1,0) c(0,4,4)

#b) #since the basis of the image is in the kernel of dF

DF <- grad(F, v0); DF
DF%*%Dg[,1]
DF%*%Dg[,2]

#c) 
library(lattice)
#install.packages("scatterplot3d")  #comment this out after running it once
library(scatterplot3d)

n <- 18

u <- seq (from = 1.5, to = 2.5, length.out = n+1); u

v <- seq (from = .5, to = 1.5, length.out = 2*n+1); v

g <- expand.grid(u=u,v=v); head(g)
#Next compute x, y, and z as functions of the  parameter pairs
g$x = (g$u)^2
g$y = (g$u)*(g$v)^2
g$z = (g$v)^4
head(g)
#We need to convert the last three columns to matrices
x <- matrix(g$x, length(u))
y <- matrix(g$y, length(u))
z <- matrix(g$z, length(u))
#Now wireframe will plot the parametrized sphere
wireframe(z ~ x * y, scales = list(arrows = FALSE)) 
