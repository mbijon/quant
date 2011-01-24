get.output.for.local.lda.errors <- function(lambdas) {
	l <- length(lambdas)
	training.errors <- vector(length=l, mode='numeric')
	test.errors <- vector(length=l, mode='numeric')

	for (i in 1:l) {
		lambda <- lambdas[i]
		training.errors[i] <- get.local.lda.training.error(training.g, training.X, lambda)
		test.errors[i] <- get.local.lda.test.error(training.g, training.X, test.g, test.X, lambda)
	}
	
	output <- cbind(lambdas, training.errors, test.errors)
	return (output)
}

get.local.lda.training.error <- function(g, X, lambda) {
	N <- length(g)
	num.errors <- 0
	
	for (i in 1:N) {
		x.o <- X[i,]
		weighted.X <- get.weighted.X(X, x.o, lambda)
		obs.g <- g[i]
		obs.g.hat <- get.local.lda.g.hat(g, weighted.X, x.o)
		
		if (obs.g != obs.g.hat)
			num.errors <- num.errors + 1
	}
	
	training.error <- num.errors / N
	return (training.error)
}

get.local.lda.test.error <- function(training.g, training.X, test.g, test.X, lambda) {
	N <- length(test.g)
	num.errors <- 0
	
	for (i in 1:N) {
		x.o <- test.X[i,]
		weighted.X <- get.weighted.X(training.X, x.o, lambda)
		obs.g <- test.g[i]
		obs.g.hat <- get.local.lda.g.hat(training.g, weighted.X, x.o)
		
		if (obs.g != obs.g.hat)
			num.errors <- num.errors + 1
	}
	
	training.error <- num.errors / N
	return (training.error)
}

get.weighted.X <- function(X, x.o, lambda) {
	weighted.X <- matrix(nrow=nrow(X), ncol=ncol(X), data=0)
	
	for (i in 1:nrow(X)) {
		x <- X[i,]
		deviation <- abs(x - x.o)
		distance <- sqrt(t(deviation) %*% deviation) / lambda
		
		if (distance > 1) weighted.X[i,] <- 0
		else weighted.X[i,] <- x
	}
	
	return (weighted.X)
}

get.local.lda.g.hat <- function(g, X, x.o) {
	K <- 10
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
	
	return (get.lda.g.hat(x.o, a.terms, b.terms, K))
}

get.lda.N.ks <- function(g, K) {
	N.ks <- vector(length=K, mode='numeric')
	
	for (obs.g in g)
		N.ks[obs.g + 1] <- N.ks[obs.g + 1] + 1
	
	return (N.ks)
}

get.lda.mu.hats <- function(g, X, N.ks, K) {
	sums <- matrix(nrow=K, ncol=ncol(X), data=0)
	
	for (obs.i in 1:length(g)) {
		x <- X[obs.i,]
		class.i <- g[obs.i] + 1
		sums[class.i,] <- sums[class.i,] + x
	}
	
	mu.k.hats <- sums / N.ks
	return (mu.k.hats)
}

get.lda.sigma.hat <- function(g, X, mu.hats, N, K) {
	sigma.hat <- 0
	
	for (i in 1:length(g)) {
		x <- X[i,]
		mu.hat <- mu.hats[g[i] + 1]
		
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
	
	g.hat <- best.k - 1
	return (g.hat)
}


training.data <- as.matrix(read.csv('train', sep=' ', header=F))
training.g <- training.data[,1]
training.X <- training.data[,c(-1,-258)]

test.data <- as.matrix(read.csv('test', sep=' ', header=F))
test.g <- test.data[,1]
test.X <- test.data[,-1]

lambdas <- c(5, 10, 15, 20, 30)
output <- get.output.for.local.lda.errors(lambdas)
