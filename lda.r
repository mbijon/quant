get.test.and.training.sets <- function(data, frac.training=0.70) {
	num.points <- nrow(data)
	num.training.points <- round(frac.training * num.points)
	num.test.points <- num.points - num.training.points
	
	training.row.indices <- sample(num.points, num.training.points, replace=F)
	
	training <- matrix(nrow=num.training.points, ncol=ncol(data))
	test <- matrix(nrow=num.test.points, ncol=ncol(data))
	
	training.index <- 1
	test.index <- 1
	for (i in 1:num.points) {
		if (i %in% training.row.indices) {
			training[training.index,] <- data[i,]
			training.index <- training.index + 1
		} else {
			test[test.index,] <- data[i,]
			test.index <- test.index + 1
		}
	}
	sets <- list(training=training, test=test)
	return (sets)
}

get.lda.training.error <- function(g, X) {
	K <- max(g)
	N <- length(g)
	N.ks <- get.lda.N.ks(g, K)
	log.pi.hats <- log(N.ks / N)
	mu.hats <- get.lda.mu.hats(g, X, N.ks, K)
	sigma.hat <- get.lda.sigma.hat(g, X, mu.hats, N, K)
	sigma.hat.inv <- solve(sigma.hat)
	
	a.terms <- matrix(ncol=K, nrow=ncol(X))
	for (k in 1:K) a.terms[,k] <- sigma.hat.inv %*% mu.hats[k,]
	
	b.terms <- vector(length=K, mode='numeric')
	for (k in 1:K) b.terms[k] <- (1/2) %*% t(mu.hats[k,]) %*% sigma.hat.inv %*% mu.hats[k,] + log.pi.hats[k]
	
	g.hats <- vector(length=N, mode='numeric')
	for (i in 1:N) {
		x <- X[i,]
		g.hat <- get.lda.g.hat(x, a.terms, b.terms, K)
		g.hats[i] <- g.hat
	}
	
	num.errors <- 0
	for (i in 1:N)
		if (g[i] != g.hats[i])
			num.errors <- num.errors + 1
	
	training.error <- num.errors / N
	return (training.error)
}

get.lda.test.error <- function(training.g, training.X, test.g, test.X) {
	# training
	g <- training.g; X <- training.X
	K <- max(g); N <- length(g)
	N.ks <- get.lda.N.ks(g, K)
	log.pi.hats <- log(N.ks / N)
	mu.hats <- get.lda.mu.hats(g, X, N.ks, K)
	sigma.hat <- get.lda.sigma.hat(g, X, mu.hats, N, K)
	sigma.hat.inv <- solve(sigma.hat)
	
	a.terms <- matrix(ncol=K, nrow=ncol(X))
	for (k in 1:K) a.terms[,k] <- sigma.hat.inv %*% mu.hats[k,]
	
	b.terms <- vector(length=K, mode='numeric')
	for (k in 1:K) b.terms[k] <- (1/2) %*% t(mu.hats[k,]) %*% sigma.hat.inv %*% mu.hats[k,] + log.pi.hats[k]
	
	
	# test
	g <- test.g; X <- test.X
	N <- length(g)
	g.hats <- vector(length=N, mode='numeric')
	for (i in 1:N) {
		x <- X[i,]
		g.hat <- get.lda.g.hat(x, a.terms, b.terms, K)
		g.hats[i] <- g.hat
	}
	
	num.errors <- 0
	for (i in 1:N)
		if (g[i] != g.hats[i])
			num.errors <- num.errors + 1
	
	training.error <- num.errors / N
	return (training.error)
}

get.lda.N.ks <- function(g, K) {
	N.ks <- vector(length=K, mode='numeric')
	
	for (obs.g in g)
		N.ks[obs.g] <- N.ks[obs.g] + 1
	
	return (N.ks)
}

get.lda.mu.hats <- function(g, X, N.ks, K) {
	sums <- matrix(nrow=K, ncol=ncol(X), data=0)
	
	for (obs.i in 1:length(g)) {
		x <- X[obs.i,]
		class.i <- g[obs.i]
		sums[class.i,] <- sums[class.i,] + x
	}
	
	mu.hats <- sums / N.ks
	return (mu.hats)
}

get.lda.sigma.hat <- function(g, X, mu.hats, N, K) {
	sigma.hat <- 0
	
	for (i in 1:length(g)) {
		x <- X[i,]
		mu.hat <- mu.hats[g[i]]
		
		deviation <- x - mu.hat
		sq.deviation <- (deviation %*% t(deviation))
		sigma.hat <- sigma.hat + sq.deviation
	}
	
	sigma.hat <- sigma.hat / (N - K)
	return (sigma.hat)
}

get.lda.g.hat <- function(x, a.terms, b.terms, K) {
	best.delta.k <- -1
	best.k <- -1
	
	for (k in 1:K) {
		a <- a.terms[, k]
		b <- b.terms[k]
		delta.k <- t(x) %*% a - b
		
		if (best.k == -1 || delta.k > best.delta.k) {
			best.delta.k <- delta.k
			best.k <- k
		}
	}
	
	g.hat <- best.k
	return (g.hat)
}


# import data
data <- as.matrix(read.csv('features', sep=',', header=T))
sets <- get.test.and.training.sets(data)
training.g <- sets$training[,258]
training.X <- sets$training[,2:257]

test.g <- sets$test[,258]
test.X <- sets$test[,2:257]

# plot
require(lattice)
splom(X[,1:5], col='black')
splom(cbind(g, X[,1:3]), col='black')

# calculate errors
training.error <- get.lda.training.error(training.g, training.X)
test.error <- get.lda.test.error(training.g, training.X, test.g, test.X)
