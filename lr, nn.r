get.points <- function() {
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
	
	points <- list(blue.points=blue.points, orange.points=orange.points)
	return (points)
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
	plot(blue.x, blue.y, col='blue', cex=1.3, xlab='x1', ylab='x2')
	title('Linear regression')
	
	orange.x <- points$orange.points[,1]
	orange.y <- points$orange.points[,2]
	points(orange.x, orange.y, col='orange', cex=1.3)
}

get.linear.regression.points = function(training.points, bounds, nrow=70, ncol=70) {
	betas <- get.betas(training.points)
	
	all.points <- matrix(ncol=ncol, nrow=nrow)
	x1.values <- seq(from=bounds$min.x, to=bounds$max.x, len=ncol)
	x2.values <- seq(from=bounds$min.y, to=bounds$max.y, len=nrow)
	
	for (i in 1:length(x1.values)) {
		for (j in 1:length(x2.values)) {
			all.points[i,j] <- sum(betas * c(1, x1.values[i], x2.values[j]))
		}
	}
	
	return (all.points)
}

get.betas <- function(training.points) {
	blue.points <- training.points$blue.points
	num.blue <- nrow(blue.points)
	blue.y <- rep(0, num.blue)
	
	orange.points <- training.points$orange.points
	num.orange <- nrow(orange.points)
	orange.y <- rep(1, num.orange)
	
	X <- matrix(nrow=num.blue+num.orange, ncol=2)
	X[1:num.blue,] <- blue.points
	orange.start = 1 + num.blue
	orange.end = num.blue + num.orange
	X[orange.start:orange.end,] <- orange.points
	X <- cbind(1, X)
	
	Y <- c(blue.y, orange.y)
	
	betas <- solve(t(X) %*% X) %*% t(X) %*% Y # explicit
	return (betas)
}

get.nearest.neighbors.points = function(training.points, bounds, k=1, nrow=70, ncol=70) {
	all.points <- matrix(ncol=ncol, nrow=nrow)
	x.values <- seq(from=bounds$min.x, to=bounds$max.x, len=ncol)
	y.values <- seq(from=bounds$min.y, to=bounds$max.y, len=nrow)
	
	for (i in 1:length(x.values)) {
		for (j in 1:length(y.values)) {
			all.points[i,j] <- get.nn.classification(training.points, c(x.values[i], y.values[j]), k=k)
		}
	}
	
	return (all.points)
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

get.nn.classification <- function(training.points, point, k) {
	blue.points <- training.points$blue.points
	orange.points <- training.points$orange.points
	
	blue.distances <- get.distances(blue.points, point)
	orange.distances <- get.distances(orange.points, point)
	
	num.blue.neighbors <- 0
	blue.index <- 1
	orange.index <- 1
	
	for (i in 1:k) {
		blue.distance <- blue.distances[blue.index]
		orange.distance <- orange.distances[orange.index]
		
		if (blue.distance < orange.distance) {
			num.blue.neighbors <- num.blue.neighbors + 1
			blue.index <- blue.index + 1
		} else {
			orange.index <- orange.index + 1
		}
	}
	
	if (num.blue.neighbors > k / 2) {
		return (0) # blue classification
	} else {
		return (1) # orange classification
	}
}

get.distances <- function(points, point) {
	distances <- vector(len=nrow(points), mode='numeric')
	for (i in 1:nrow(points)) {
		distance <- sqrt(sum((points[i,] - point)^2))
		distances[i] <- distance
	}
	
	distances <- sort(distances)
	return (distances)
}

points <- get.points()
bounds <- get.bounds(points)
plot.points(points)
lr.points <- get.linear.regression.points(points, bounds)
nn1.points <- get.nearest.neighbors.points(points, bounds, k=1)
nn15.points <- get.nearest.neighbors.points(points, bounds, k=15)
plot.grid(lr.points, bounds)
plot.grid(nn1.points, bounds)
plot.grid(nn15.points, bounds)
