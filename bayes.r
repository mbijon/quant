get.data <- function() {
	sigma <- matrix(c(1, 0, 0, 1), ncol=2)
	blue.means <- mvrnorm(n=10, mu=c(1,0), Sigma=sigma)
	orange.means <- mvrnorm(n=10, mu=c(0,1), Sigma=sigma)
	
	blue.points = matrix(nrow=100, ncol=2)
	sigma <- matrix(c(sqrt(0.2), 0, 0, sqrt(0.2)), ncol=2)
	for (k in 1:100) {
		random.blue.mean <- blue.means[sample(1:10, 1),]
		random.blue.point <- mvrnorm(n=1, mu=random.blue.mean, Sigma=sigma)
		blue.points[k,] <- random.blue.point
	}
	
	orange.points =  matrix(nrow=100, ncol=2)
	sigma <- matrix(c(0.2, 0, 0, 0.2), ncol=2)
	for (k in 1:100) {
		random.orange.mean <- orange.means[sample(1:10, 1),]
		random.orange.point <- mvrnorm(n=1, mu=random.orange.mean, Sigma=sigma)
		orange.points[k,] <- random.orange.point
	}
	
	data <- list(blue.points=blue.points, orange.points=orange.points, blue.means=blue.means, orange.means=orange.means)
	return (data)
}

get.bounds <- function(points) {
	blue.x <- points$blue.points[,1]
	blue.y <- points$blue.points[,2]
	orange.x <- points$orange.points[,1]
	orange.y <- points$orange.points[,2]
	
	max.x <- max(c(blue.x, orange.x))
	min.x <- min(c(blue.x, orange.x))
	max.y <- max(c(blue.y, orange.y))
	min.y <- min(c(blue.y, orange.y))
	
	bounds <- list(min.x=min.x, max.x=max.x, min.y=min.y, max.y=max.y)
	return (bounds)
}

plot.points <- function(points) {
	blue.x <- points$blue.points[,1]
	blue.y <- points$blue.points[,2]
	plot(blue.x, blue.y, col='blue', cex=1.3, xlab='x', ylab='y')
	title('Bayes classifier')
	
	orange.x <- points$orange.points[,1]
	orange.y <- points$orange.points[,2]
	points(orange.x, orange.y, col='orange', cex=1.3)
}

plot.grid <- function(grid, bounds) {
	x.values <- seq(from=bounds$min.x, to=bounds$max.x, len=ncol(grid))
	y.values <- seq(from=bounds$min.y, to=bounds$max.y, len=nrow(grid))
	
	for (i in 1:length(x.values)) {
		for (j in 1:length(y.values)) {
			if (grid[i,j] >= 0.5)
				points(y.values[j] ~ x.values[i], pch=1, cex=.1, col='orange')
			else
				points(y.values[j] ~ x.values[i], pch=2, cex=.1, col='blue')
		}
	}
	
	contour(x=x.values, y=y.values, z=grid, add=T, levels=0.5)
}

get.bayes.classifier.points <- function(data, bounds, nrow=70, ncol=70) {
	all.points <- matrix(ncol=ncol, nrow=nrow)
	x.values <- seq(from=bounds$min.x, to=bounds$max.x, len=ncol)
	y.values <- seq(from=bounds$min.y, to=bounds$max.y, len=nrow)
	
	for (i in 1:length(x.values)) {
		for (j in 1:length(y.values)) {
			all.points[i,j] <- get.bayes.classification(data, c(x.values[i], y.values[j]))
		}
	}
	
	return (all.points)
}

get.bayes.classification = function(data, point) {
	blue.means <- data$blue.means
	orange.means <- data$orange.means
	
	blue.density <- get.density(point, blue.means)
	orange.density <- get.density(point, orange.means)
	
	cond.prob.blue <- (1/2*blue.density) / (1/2*blue.density + 1/2*orange.density)
	if (cond.prob.blue > 0.5) {
		return (0) # blue classification
	} else {
		return (1) # orange classification
	}
}

get.density <- function(point, means) {
	densities <- vector(len=ncol(means), mode='numeric')
	for (i in 1:ncol(means)) {
		densities[i] <- get.single.density(point, means[i,], sqrt(0.2))
	}
	
	density <- sum(1/10 * densities)
	return (density)
}

get.single.density <- function(point, mean, sigma) {
	x <- point[1]
	y <- point[2]
	
	mx <- mean[1]
	my <- mean[2]
	
	tmp.vector<- matrix(c(x-mx, y-my), ncol=1)
	cov.matrix<- sigma * diag(c(1,1))
	inverse.cov.matrix <-solve(cov.matrix)

	matrix.product <- t(tmp.vector) %*% inverse.cov.matrix %*% tmp.vector 
	
	density <- (1/(2*pi*2*sigma^2)  ) * exp( (-1/2)*matrix.product )
	return (density)
}

data <- get.data()
bounds <- get.bounds(data)
plot.points(data)
bc.points <- get.bayes.classifier.points(data, bounds)
plot.grid(bc.points, bounds)
