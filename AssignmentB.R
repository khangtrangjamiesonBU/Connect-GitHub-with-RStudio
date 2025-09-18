#Assignment B
#Full name: Khang-Trang Nguyen

#-----Exercise 1. Vectors-----

#Problem 1: Create the vectors
#a (1,2,3,....,19,20)
vector1 <- c(1:20)
print(vector1)
#b (20, 19,...,2,1)
vector2 <- c(20:1)
print(vector2)
#c (1,2,3...,19,20,19,18,...,2,1)
vector3as <- c(1:20)
vector3de <- c(19:1)
vector3 <- c(vector3as,vector3de)
print(vector3)
#d (4,6,3)
tmp <- c(4,6,3)
print(tmp)
#e (4,6,3,4,6,3...,4,6,3) where there are 10 occurrences of 4
rep4 <- rep(4, 7)
vector4 <- c(tmp, tmp, rep4, tmp)
print(vector4)
#f (4,6,3,4,6,3...,4,6,3,4) where there are 11 occurrences of 4,
#10 occurrences 6 and 10 occurrences of 3
rep6 <- rep(6, 7)
rep3 <- rep(3, 7)
vector5 <- c(tmp, tmp, rep4, rep6, rep3, tmp, 4)
print(vector5)
#g(4,4,...,4,6,6,...,6,3,3,...,3) where there are 10 occurrences of 4,
#20 occurrences of 6 and 30 occurrences of 3
rep4_10 <- rep(4, 10)
rep6_20 <- rep(6, 20)
rep3_30 <- rep(3, 30)
vector6 <- c(rep4_10, rep6_20, rep3_30)
print(vector6)

#Problem 2
vec <- seq(3, 6, by = 0.1)
equate <- (exp(vec))*(cos(vec))
result <- c(equate)
print(result)

#Problem 3
#a
x <- (3*c(1:12))
y <- x-2
print(x)
print(y)
vec1 <- c((0.1^x)*(0.2^y))
print(vec1)
#b
b <- c(1:25)
equate_1 <- ((2^b)/b)
vect2 <-c(equate_1)
print(vect2)

#Problem 4
#a
i <- c(10:100)
equate_2 <- ((i^3)+(4*(i^2)))
result_1 <- sum(equate_2)
print(result_1)
#b
i1 <- c(1:25)
equate_3 <- (((2^i1)/i1)+((3^i1)/(i1^2)))
result_2 <- sum(equate_3)
print(result_2)

#-----Exercise 2. Matrices-----

#Problem 1
#a
row1 <- c(1, 1, 3)
row2 <- c(5, 2, 6)
row3 <- c(-2, -1, -3)
A <- matrix(c(row1, row2, row3), nrow = 3, ncol = 3, byrow = TRUE)
print(A)
A3 <- A%*%A%*%A
print(A3)
print ("A3 is indeed equal to 0 where 0 is a 3x3 matrix with every entry equal to 0")
#b
col1 <- A[,1]
col2 <- A[,2]
print(col2)
col3 <- A[,3]
print(col3)
new_col3 <- col2+col3
print(new_col3)
new_A <- matrix(c(col1, col2, new_col3), nrow = 3, ncol = 3, byrow = FALSE)
print(new_A)

#Problem 2
row_1 <- c(10, -10, 10)
B <- matrix(c(row_1), nrow = 15, ncol = 3, byrow = TRUE)
print(B)
BtB <- crossprod(B)
print(BtB)

#Problem 3
matE <- matrix(0, nrow = 6, ncol = 6, byrow = TRUE)
print(matE)
rowE <- row(matE)
print(rowE)
colE <- col(matE)
print(colE)
new_mat <- (abs(rowE-colE) == 1)*1 #this part I needed help of chatGPT
print(new_mat)

#Problem 4
o <- 0:4
p <- 0:4
matT <- outer(o, p, "+")
print(matT)

#-----Exercise 3. Simple Functions-----

#Problem 1
#a
#helped by chatGPT
tmpFn1 <- function(xVec){
  n <- length(xVec)
  j1 <- c(1:n)
  xVec^(j1)
}
tmpFn2 <- function(xVec){
  n <- length(xVec)
  j2 <- c(1:n)
  (xVec^j2)/j2
}
#Test functions
xVec <- c(1:9)
tmpFn1(xVec)
tmpFn2(xVec)
#b
#helped by chatGPT; it recommended me using stopifnot
tmpFn3 <- function(x1, n1) {
  stopifnot(is.numeric(x1),length(x1) == 1,
            is.numeric(n1), length(n1) == 1,
            n1 >= 1, n1 == as.integer(n1))
  k <- 1:n1
  1 + sum((x1^k) / k)
}
#Test function
x1 = 4
n1 = 9
tmpFn3(x1,n1)

#Problem 2
new_tmpFn <- function(a){
  n = length(a)
  mva= (a[-c(n-1,n)] + a[-c(1,n)] + a[-c(1,2)])/3
  return(mva)
}
#Test function
a<-c(1:5, 6:1)
new_tmpFn(a)

#Problem 3
f <- function(x) {
  ifelse(x < 0,              x^2 + 2*x + 3,
         ifelse(x < 2,              x + 3,
                x^2 + 4*x - 7))
}
tmpFn<-function(xVec1){
  f(xVec1)
}
#Test function
xVec1<-c(-3, 0, 3)
tmpFn(xVec1)
#Plot function
xs <- seq(-3, 3, by = 0.01)
plot(xs, f(xs), type = "l", xlab = "x", ylab = "f(x)")
abline(v = c(0, 2), lty = 2, col = "red")

#Problem 4
doubleOdds <- function(matrixO) {
  matrixO[matrixO %% 2 != 0] <- matrixO[matrixO %% 2 != 0] * 2
  return(matrixO)
}
matG <- matrix(c(1,5,-2, 1,2,-1, 3,6,-3), nrow = 3, byrow = FALSE)
print(matG)
doubleOdds(matG)

#-----Exercise 4. Harder Functions-----

#Problem 1
#a: Use function outer
zfunctionA <- function(xVec2, yVec2){
  colSums(outer(yVec2, xVec2, "<"))
}
#Test function
xVec2 <- c(1,2,5,7,9)
yVec2 <- c(3,4,7,8,9)
zfunctionA(xVec2, yVec2)
#b: Use function sapply
zfunctionB <- function(xVec3, yVec3){
  rowSums (sapply(yVec3, FUN = function(y1){y1 < xVec3}))
}
#Test function
xVec3 <- c(3,6,10)
yVec3 <- c(1,2,5,7,12)
zfunctionB(xVec3, yVec3)
#c: Use function vapply
zfunctionC <- function(xVec4, yVec4){
  rowSums(vapply(yVec4, FUN=function(y2){y2 < xVec4},
                 FUN.VALUE = seq(along = xVec4)))
}
#Test function
xVec4 <- c(3,6,10)
yVec4 <- c(1,2,5,7,12)
zfunctionC(xVec4,yVec4)
#d
#When one of the arguments is an empty vector (vector with length 0)
xVec2 <- c()
yVec2 <- c(3,4,7,8,9)
zfunctionA(xVec2, yVec2) #Failed

xVec3<-vector()
yVec3 <- c(3,4,7,8,9)
zfunctionB(xVec3, yVec3) #Failed

xVec4 <- vector()
yVec4 <- c(1,2,5,7,12)
zfunctionC(xVec4,yVec4) #Numeric(0)

#When both of the arguments is an empty vector (vector with length 0)
xVec2<-vector()
yVec2 <- vector()
zfunctionA(xVec2, yVec2) #Failed

xVec3<-vector()
yVec3 <- vector()
zfunctionB(xVec3, yVec3) #Failed

xVec4 <- vector()
yVec4 <- vector()
zfunctionC(xVec4,yVec4) #Numeric(0)

#When one of the arguments is matrix
xVec2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec2 <- c(3,4,7,8,9)
zfunctionA(xVec2, yVec2) #Failed

xVec3<-matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec3 <- c(3,4,7,8,9)
zfunctionB(xVec3, yVec3) #Numeric(0) #Returned 0 1 2 0 2 3 0 2 4

xVec4 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec4 <- c(1,2,5,7,12)
zfunctionC(xVec4,yVec4) #Numeric(0) #Returned 0 2 3 1 2 4 2 3 4

#When both of the arguments is matrix
xVec2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec2 <- matrix(11:19, nrow = 3, ncol = 3, byrow = TRUE)
zfunctionA(xVec2, yVec2) #Failed

xVec3<-matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec3 <- c(3,4,7,8,9)
zfunctionB(xVec3, yVec3) #Returned 0 1 2 0 2 3 0 2 4

xVec4 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
yVec4 <- c(1,2,5,7,12)
zfunctionC(xVec4,yVec4) #Returned 0 2 3 1 2 4 2 3 4

#e
relative_speed1 <- rnorm(10000)
relative_speed2 <- rnorm(12000)
system.time(zfunctionA(relative_speed1,relative_speed2))
# user    system  elapsed 
# 0.827   0.551   1.516 
system.time(zfunctionB(relative_speed1,relative_speed2))
# user    system  elapsed 
# 0.482   0.108   0.619
system.time(zfunctionC(relative_speed1,relative_speed2))
# user    system  elapsed 
# 0.351   0.113   0.463 

Print("Conclusion: vapply is the fastest")

#Problem 2
#a
submatFn1 <- function(matA){
  matA[, !apply(is.na(matA), 2, any), drop = FALSE]
}
#Test function
matAr1 <- c(1,NA,3,7)
matAr2 <- c(3,7,4,NA)
matAr3 <- c(4,2,1,5)
matA <- matrix(c(matAr1, matAr2,matAr3), nrow = 3, ncol = 4, byrow = TRUE)
submatFn1 (matA)
#b
submatFn2 <- function(matB){
  matB[!apply(is.na(matB), 1, any), !apply(is.na(matB), 2, any), drop = FALSE]
}
#Test function
matBr1 <- c(1,NA,3,7)
matBr2 <- c(3,7,4,NA)
matBr3 <- c(4,2,1,5)
matB <- matrix(c(matBr1, matBr2,matBr3), nrow = 3, ncol = 4, byrow = TRUE)
submatFn2 (matB)

#Problem 3
#a
empCopula1 <- function( u1, v1, xVec5, yVec5 )
{
  n <- length(xVec5)
  r3 <- rank(xVec5)/(n+1)
  s3 <- rank(yVec5)/(n+1)
  sum( (r3 <= u1) & (s3 <= v1) ) /n
}
#Test function
xVec5 <- c(7,3,1,4)
yVec5 <- c(2,8,5,6)
empCopula1(0.8, 0.6, xVec5, yVec5)
#b
print("Function as the answer to part (a) does not work if u and v are
numeric vectors with the same length and with all values lying in [0,1]",
      col = "red")
empCopula2 <- function( u2, v2, xVec6, yVec6 )
{
  n <- length(xVec6)
  r4 <- rank(xVec6)/(n+1)
  s4 <- rank(yVec6)/(n+1)
  valuesN <- colSums( outer(r4, u2, "<=")&outer(s4, v2, "<=") )
  cbind( uCoord = u2, vCoord = v2, empCop=valuesN/n )
}
#Test function
u2 <- c(0.5,0.8)
v2 <- c(0.5, 0.6)
xVec6 <- c(7,3,1,4)
yVec6 <- c(2,8,5,6)
empCopula2(u2,v2,xVec6,yVec6)

#Problem 4
#a
funA <- function (n)
{
  us <- 0
  for(r2 in 1:n)
    {
    for(s2 in 1:r2)
      us <- us + s2^2/(10+4*r2^3)
    }
  us
}
#b
funB <- function(n)
{
  matC <- matrix(0, ncol = n, nrow = n)
  sum((col(matC)^2)/(10+4*row(matC)^3)*(col(matC)<=row(matC)))
}
#c
funC <- function(n){
  sum(outer(1:n,1:n,FUN=function(r3,s3){
    (s3<=r3)*(s3^2)/(10+4*r3^3)
  }))
}
#d
funD <- function (n){
  tmpfn <- function(r)
    {sum(((1:r)^2)/(10+4*r^3))}
  sum(sapply(1:n, FUN=tmpfn))
}
#Test function
funD(6)

funE <- function (n){
  tmpfn <- function(r)
    {sum(((1:r)^2)/(10+4*r^3))}
  sum(unlist(lapply(1:n, FUN=tmpfn)))
}
#Test function
funE(6)

system.time(funD(50))
# user    system  elapsed 
# 0.003   0.000   0.004 
system.time(funE(50))
# user    system  elapsed 
# 0.004   0.000   0.004

print ("Conclusion: There is no increase in speed gained by using
combination of unlist and lappy")

#e
funF <- function (n){
  tmpf <- function(s,r){(s^2)/(10+4*r^3)*(s<=r)}
  sum(mapply(tmpf, rep(1:n, times=rep(n,n)), 1:n))
}

system.time(funA(50))
system.time(funB(50))
system.time(funC(50))
system.time(funF(50))

print("funB and funC are the fastest")

#-----Exercise 5. Data frame, list, array, and time series-----

#Problem 1
#a
tsEwma <- function( tsDat, m0=0, delta=0.7){
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
    }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}
#b
tsEwma2 <- function( tsDat, m0=0, delta=0.7){
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
    }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}
tmp <- ts(rnorm(400000), start=c(1960,3), frequency=12)
system.time(tsEwma2(tmp))
system.time(tsEwma(tmp))

print("tsEwma2 is faster than tsEwma")

#Problem 2
#a
myListFn <- function(n){
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}
#b I don't know how to do this
#c
myList <- lapply( rep(10,1000), myListFn )
lapply(myList, FUN="[[", "yVec")
#d
sapply(myList, FUN="[[", 2)
#e
myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})
print(myList2)
#f
myList[which( unlist(lapply( myList, function(x){x[[3]]>2} )) )]

#I stop here!