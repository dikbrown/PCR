seed <- 42
set.seed(seed)
# Not included in dataset, but some variables depend on these
a <- 10*runif(1000)
b <- 10*runif(1000)

# Variables found in dataset
# base variables
A <- 10*runif(1000)
B <- 10*runif(1000)
C <- 10*runif(1000)
D <- 10*runif(1000)
E <- 10*runif(1000)
F <- 10*rnorm(1000)
G <- 10*runif(1000, lambda = 4)

# derived variables
N <- a + b + rnorm(1000)
P <- 2 * a + rnorm(1000)
Q <- b + G + rnorm(1000)

R <- B * 2 + rnorm(1000)
S <- E / 3 + rnorm(1000)
T <- (C + F) / 2 + rnorm(1000)
U <- b + runif(1000)
V <- a * b


Z <- (0 * (A + 2 * rnorm(1000)) +
     -1 * (B + 2 * rnorm(1000)) + 
      3 * (C + 2 * rnorm(1000)) +
      0 * (D + 2 * rnorm(1000)) +
     -1 * (E + 2 * rnorm(1000)) +
      2 * (F + 2 * rnorm(1000)) + 
      1 * (G + 2 * rnorm(1000))  +
      1 * (N + 2 * rnorm(1000))  +
      0 * (P + 2 * rnorm(1000))  +
     -2 * (Q + 2 * rnorm(1000))  +
     -3 * (R + 3 * rnorm(1000))  +
      4 * (S + 4 * rnorm(1000))  +
      1 * (T + 2 * rnorm(1000))  +
      1.5 * (U + 2 * rnorm(1000))  +
      0.5 * (V + 2 * rnorm(1000)) +
       20 * rnorm(1000))

corrplot(cor(df))
df <- data.frame(cbind(A, B, C, D, E, F, G, N, P, Q, R, S, T, U, V, Z))


#df <- read.csv("./data/PCR.csv", skip = 2)
#df <- df[,-9]
head(df)
###################################
###  Regular Linear Regression  ###
###################################
set.seed(seed)
nobs <- nrow(df)
train <- sample(nobs, 0.8 * nobs)

trainset <- df[train,]
testset <- df[-train,]

lr1 <- glm(Z ~ ., data = trainset)
summary(lr1)
# fitcorr = 0.7205

lr1.1 <- glm(Z ~ B + C + F + G + N + P + R + T + U + V, data = trainset) # use correlated variables
summary(lr1.1)
# fitcorr = 0.7154

lr1.2 <- glm(Z ~ C + E + F + G + Q + R + S + T + U + V, data = trainset) # use significant variables from lr1
summary(lr1.2) 
# fitcorr = 0.7209

lr1.3 <- glm(Z ~ B + C + E+F+G+N+Q+R+S+T+U+V, data = trainset) # use variable with non-zero defined contribution
summary(lr1.3)
# fitcorr = 0.7210

#test model with the test set
pred1 <- predict(lr1.3, type = "response", newdata = testset)

obs <- subset(testset, select = Z)
fitpoints <- cbind(obs, Predicted = pred1)
fitcorr <- round(cor(fitpoints[,1], fitpoints[,2])^2,4)
fitcorr

plot(fitpoints[,1], fitpoints[,2], ylab = "Predicted", xlab = "Z")
#add best-fit line through points
prline <- lm(fitpoints[,2] ~ fitpoints[,1])
abline(prline, col = "blue")
# add line for pred = obs
abline(0,1, col = "red")


# 
# lr2 <- glm(Z ~ A + B + D + E + F + G + H, data = trainset)
# summary(lr2)
#####################################
###  Create Principal Components  ###
#####################################
# take out the dependent variable
PCAset <- df[,-9]

# Get the principal components
eig <- eigen(cor(PCAset))
eig
### Screeplots
scree <- eig$values
plot(scree)
scree2 <- scree / sum(scree) # sum(scree) = number of variables = 9
plot(scree2)
scree2

tot = 0
for (i in c(1:15)) {
  tot = tot + scree2[i]
  cat("First ", i, " components account for ", round(tot*100, 2), "% of variance.\n" )
}

# Create new variables out of first n principal components
n <- 5
newset <- as.matrix(PCAset) %*% eig$vectors[,c(1:5)]


# Add in dependent variable
newset <- data.frame(transform(newset, Z = df$Z))
head(newset)

#create train and test sets
trainset2 <- newset[train,]
head(trainset2)
testset2 <- newset[-train,]
head(testset2)

lr2 <- glm(Z ~ ., data = trainset2)
summary(lr2)
# fitcorr = 0.9867 w/8 PCs
# fitcorr = 0.9831 w/5 PCs



#test model with the test set
pred2 <- predict(lr2, type = "response", newdata = testset2)

obs2 <- subset(testset2, select = Z)
fitpoints2 <- cbind(obs, Predicted = pred2)
fitcorr2 <- round(cor(fitpoints2[,1], fitpoints2[,2])^2,4)
fitcorr2

plot(fitpoints2[,1], fitpoints2[,2], ylab = "Predicted", xlab = "Z")

#add best-fit line through points
prline2 <- lm(fitpoints2[,2] ~ fitpoints2[,1])
abline(prline2, col = "blue")
# add line for pred = obs
abline(0,1, col = "red")

