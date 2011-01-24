#################
## import data ##
data.set <- as.matrix(read.csv('features.csv', header=T))
P <- ncol(data.set) - 1 # num features
N <- nrow(data.set) # num data points
y <- data.set[,P+1] # variable to predict
X <- data.set[,-(P+1)] # features
cv.groups <- kfold.cv.groups(X, y)

kfold.cv.groups <- function(X, y, k=3) {
	P <- ncol(X)
	N <- length(y)
	N.group <- round(N/k)
	X.training.groups <- list()
	y.training.groups <- list()
	X.test.groups <- list()
	y.test.groups <- list()
	
	# create cv test groups
	for (i in 1:k) {
		N <- length(y)
		group.indices <- sample(N, N.group, replace=F)
		
		X.test.groups[[i]] <- X[group.indices,]
		y.test.groups[[i]] <- y[group.indices]
		
		if (i != k) {
			X <- X[-group.indices,]
			y <- y[-group.indices]
		}
	}
	
	# create cv training groups
	for (i in 1:k) {
		X <- matrix(nrow=0, ncol=P)
		y <- vector(length=0, mode='numeric')
		for (j in 1:k) {
			if (j != i) {
				X <- rbind(X, X.test.groups[[j]])
				y <- c(y, y.test.groups[[j]])
			}
		}
		
		X.training.groups[[i]] <- X
		y.training.groups[[i]] <- y
	}
	
	return (list(k=k, training.X=X.training.groups, training.y=y.training.groups, test.X=X.test.groups, test.y=y.test.groups))
}


########################
## summary statistics ##
means <- vector(length=P, mode='numeric')
for (p in 1:P) means[p] <- mean(X[,p])

standard.deviations <- vector(length=P, mode='numeric')
for (p in 1:P) standard.deviations[p] <- sqrt(var(X[,p]))

mins <- vector(length=P, mode='numeric')
for (p in 1:P) mins[p] <- min(X[,p])

maxes <- vector(length=P, mode='numeric')
for (p in 1:P) maxes[p] <- max(X[,p])

summary.table <- cbind(means, standard.deviations, mins, maxes)


###########
## plots ##
require(lattice); # splom

splom(data.set[,c(16, 1:3)], col='black')
splom(data.set[,c(16, 4:6)], col='black')
splom(data.set[,c(16, 7:9)], col='black')
splom(data.set[,c(16, 10:12)], col='black')
splom(data.set[,c(16, 13:15)], col='black')


############################
## ordinary least squares ##
ols.error <- ols.mean.test.error(cv.groups)

ols.mean.test.error <- function(cv.groups) {
	sum.test.errors <- 0
	
	k <- cv.groups$k
	for (i in 1:k) {
		training.X <- cv.groups$training.X[[i]]
		training.y <- cv.groups$training.y[[i]]
		
		test.X <- cv.groups$test.X[[i]]
		test.y <- cv.groups$test.y[[i]]
		
		test.error <- ols.rss(training.X, training.y, test.X, test.y)
		sum.test.errors <- sum.test.errors + test.error
	}
	
	mean.test.error <- sum.test.errors / k
	return (mean.test.error)
}

ols.rss <- function(training.X, training.y, test.X, test.y) {
	fit <- lm(training.y ~ training.X)
	betas <- fit$coefficients
	predictions <- cbind(1, test.X) %*% betas
	residuals <- predictions - test.y
	rss <- sum(residuals^2)
	
	return (rss)
}


######################
## ridge regression ##
require(MASS)

# best tuning parameters by mean test error
br <- best.ridge(cv.groups)

# plot lambda and errors
re <- ridge.errors(cv.groups)
plot(re$lambdas, re$mean.test.errors)
title('Ridge regression parameter and mean test errors')

# mean test error per parameter value
ridge.errors <- function(cv.groups) {
	lambdas <- seq(0,15,0.005)
	L <- length(lambdas)
	mean.test.errors <- vector(length=L, mode='numeric')
	cv.group.betas <- ridge.cv.group.betas(cv.groups, lambdas)
	
	for (i in 1:L) {
		mean.test.error <- ridge.mean.test.error(cv.groups, lambdas[i], cv.group.betas, i)
		mean.test.errors[i] <- mean.test.error
	}
	
	return (list(lambdas=lambdas, mean.test.errors=mean.test.errors))
}

best.ridge <- function(cv.groups) {
	best.lambda <- -1
	best.mean.test.error <- -1
	
	lambdas <- seq(0,100,0.05)
	L <- length(lambdas)
	cv.group.betas <- ridge.cv.group.betas(cv.groups, lambdas)
	for (i in 1:L) {
		mean.test.error <- ridge.mean.test.error(cv.groups, lambdas[i], cv.group.betas, i)
		
		if (best.lambda == -1 || mean.test.error < best.mean.test.error) {
			best.lambda <- lambdas[i]
			best.mean.test.error <- mean.test.error
		}
	}
	
	return (list(best.lambda=best.lambda, best.mean.test.error=best.mean.test.error))
}

ridge.cv.group.betas <- function(cv.groups, lambdas) {
	beta.sets <- list()
	
	k <- cv.groups$k
	for (i in 1:k) {
		X <- cv.groups$training.X[[i]]
		y <- cv.groups$training.y[[i]]
		
		fit <- lm.ridge(y ~ X, lambda=lambdas)
		betas <- coef(fit)
		beta.sets[[i]] <- betas
	}
	
	return (beta.sets)
}

ridge.mean.test.error <- function(cv.groups, lambda, cv.group.betas, betas.index) {
	k <- cv.groups$k
	sum.test.errors <- 0
	
	for (i in 1:k) {
		test.X <-cv.groups$test.X[[i]]
		test.y <-cv.groups$test.y[[i]]
		beta.set <- cv.group.betas[[i]]
		betas <- beta.set[betas.index,]
		
		test.error <- ridge.rss(test.X, test.y, lambda, betas)
		sum.test.errors <- sum.test.errors + test.error
	}
	
	mean.test.error <- sum.test.errors / k
	return (mean.test.error)
}

ridge.rss <- function(test.X, test.y, lambda, betas) {
	predictions <- cbind(1, test.X) %*% betas
	residuals <- predictions - test.y
	rss <- sum(residuals^2)
	
	return (rss)
}


######################
## lasso regression ##
require(lars)

# best tuning parameters by mean test error
bl <- best.lasso(cv.groups)

# plot lambda and errors
le <- lasso.errors(cv.groups)
plot(le$lambdas, le$mean.test.errors)
title('Lasso regression parameter and mean test errors')

# mean test error per parameter value
lasso.errors <- function(cv.groups) {
	cv.group.fits <- lasso.cv.group.fits(cv.groups)
	lambdas <- seq(0,500,0.5)
	L <- length(lambdas)
	mean.test.errors <- vector(length=L, mode='numeric')
	
	for (i in 1:L) {
		mean.test.error <- lasso.mean.test.error(cv.group.fits, cv.groups, lambdas[i])
		mean.test.errors[i] <- mean.test.error
	}
	
	return (list(lambdas=lambdas, mean.test.errors=mean.test.errors))
}

best.lasso <- function(cv.groups) {
	best.lambda <- -1
	best.mean.test.error <- -1
	
	cv.group.fits <- lasso.cv.group.fits(cv.groups)
	lambdas <- seq(0,100,0.01)
	for (lambda in lambdas) {
		mean.test.error <- lasso.mean.test.error(cv.group.fits, cv.groups, lambda)
		
		if (best.lambda == -1 || mean.test.error < best.mean.test.error) {
			best.lambda <- lambda
			best.mean.test.error <- mean.test.error
		}
	}
	
	return (list(best.lambda=best.lambda, best.mean.test.error=best.mean.test.error))
}

lasso.cv.group.fits <- function(cv.groups) {
	fits <- list()
	
	k <- cv.groups$k
	for (i in 1:k) {
		X <- cv.groups$training.X[[i]]
		y <- cv.groups$training.y[[i]]
		
		fit <- lars(X, y, type='lasso', intercept=T)
		fits[[i]] <- fit
	}
	
	return (fits)
}

lasso.mean.test.error <- function(cv.group.fits, cv.groups, lambda) {
	k <- cv.groups$k
	sum.test.errors <- 0
	
	for (i in 1:k) {
		test.X <-cv.groups$test.X[[i]]
		test.y <-cv.groups$test.y[[i]]
		fit <- cv.group.fits[[i]]
		
		test.error <- lasso.rss(fit, test.X, test.y, lambda)
		sum.test.errors <- sum.test.errors + test.error
	}
	
	mean.test.error <- sum.test.errors / k
	return (mean.test.error)
}

lasso.rss <- function(fit, test.X, test.y, lambda) {
	predictions <- predict(fit, test.X, s=lambda, type='fit', mode='lambda')$fit
	residuals <- predictions - test.y
	rss <- sum(residuals^2)
	
	return (rss)
}


############################
## elastic net regression ##
require(elasticnet)

# best tuning parameters by mean test error
be <- best.enet(cv.groups)

# plot lambda and errors
ene <- enet.errors.for.l1.param(cv.groups)
plot(ene$lambda1s, ene$mean.test.errors)
title('Elastic net regression linear parameter and mean test errors, quadratic parameter fixed')

ene <- enet.errors.for.l2.param(cv.groups)
plot(ene$lambda2s, ene$mean.test.errors)
title('Elastic net regression quadratic parameter and mean test errors, linear parameter fixed')

# mean test error per parameter value
enet.errors.for.l1.param <- function(cv.groups) {
	lambda1s <- seq(1, 70, 0.5) # l1 params
	L <- length(lambda1s)
	mean.test.errors <- vector(length=L, mode='numeric')
	
	lambda2s <- seq(0.01, 1, 0.01)
	best.lambda2 <- -1
	best.mean.test.error <- -1
	for (lambda2 in lambda2s) {
		cv.group.fits <- enet.cv.group.fits(cv.groups, lambda2)
		
		for (i in 1:L) {
			mean.test.error <- enet.mean.test.error(cv.group.fits, cv.groups, lambda1s[i])
			mean.test.errors[i] <- mean.test.error
			
			if (best.lambda2 == -1 || mean.test.error < best.mean.test.error) {
				best.lambda2 <- lambda2
				best.mean.test.error <- mean.test.error
			}
		}
	}
	
	return (list(lambda1s=lambda1s, mean.test.errors=mean.test.errors))
}

# mean test error per parameter value
enet.errors.for.l2.param <- function(cv.groups) {
	lambda2s <- seq(0.1, 100, 0.1)
	L <- length(lambda2s)
	mean.test.errors <- vector(length=L, mode='numeric')
	
	for (i in 1:L) {
		cv.group.fits <- enet.cv.group.fits(cv.groups, lambda2s[i])
		
		lambda1 <- 48 # fixed
		mean.test.error <- enet.mean.test.error(cv.group.fits, cv.groups, lambda1)
		mean.test.errors[i] <- mean.test.error
	}
	
	return (list(lambda2s=lambda2s, mean.test.errors=mean.test.errors))
}

best.enet <- function(cv.groups) {
	best.lambda1 <- -1
	best.lambda2 <- -1
	best.mean.test.error <- -1
	
	lambda2s <- seq(0.01, 2, 0.01) # l2 tuning parameter
	for (lambda2 in lambda2s) {
		cv.group.fits <- enet.cv.group.fits(cv.groups, lambda2)
		
		lambda1s <- seq(1, 60, 0.05) # l1 tuning parameter
		for (lambda1 in lambda1s) {
			mean.test.error <- enet.mean.test.error(cv.group.fits, cv.groups, lambda1)
			
			if (best.lambda1 == -1 || mean.test.error < best.mean.test.error) {
				best.lambda1 <- lambda1
				best.lambda2 <- lambda2
				best.mean.test.error <- mean.test.error
			}
		}
	}
	
	return (list(best.lambda1=best.lambda1, best.lambda2=best.lambda2, best.mean.test.error=best.mean.test.error))
}

enet.cv.group.fits <- function(cv.groups, lambda2) {
	fits <- list()
	
	k <- cv.groups$k
	for (i in 1:k) {
		X <- cv.groups$training.X[[i]]
		y <- cv.groups$training.y[[i]]
		
		fit <- enet(X, y, lambda2)
		fits[[i]] <- fit
	}
	
	return (fits)
}

enet.mean.test.error <- function(cv.group.fits, cv.groups, lambda1) {
	k <- cv.groups$k
	sum.test.errors <- 0
	
	for (i in 1:k) {
		test.X <-cv.groups$test.X[[i]]
		test.y <-cv.groups$test.y[[i]]
		fit <- cv.group.fits[[i]]
		
		test.error <- enet.rss(fit, test.X, test.y, lambda1)
		sum.test.errors <- sum.test.errors + test.error
	}
	
	mean.test.error <- sum.test.errors / k
	return (mean.test.error)
}

enet.rss <- function(fit, test.X, test.y, lambda1) {
	predictions <- predict(fit, test.X, mode='penalty', s=lambda1)$fit
	residuals <- predictions - test.y
	rss <- sum(residuals^2)
	
	return (rss)
}

