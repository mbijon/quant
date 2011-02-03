require(splines)

# import data
data <- as.matrix(read.table('data', sep='\t', strip.white=T, header=T))
N <- nrow(data)
df <- 4
x <- as.numeric(data[,1])
y <- as.numeric(data[,2])
boundaries <- range(x)


# plot cubic spline
fm <- lm(y ~ bs(x, df=df))
prediction.x <- seq(min(x), max(x), length.out=200)
prediction.y <- predict(fm, data.frame(x=prediction.x))
plot(y ~ x)
lines(prediction.x, prediction.y)


# cross validation
get.test.and.training.sets <- function(x, y, frac.training=0.80) {
	N <- length(x)
	num.training.points <- round(frac.training * N)
	num.test.points <- N - num.training.points
	
	training.row.indices <- sample(N, num.training.points, replace=F)
	
	training.x <- vector(length=num.training.points, mode='numeric')
	training.y <- vector(length=num.training.points, mode='numeric')
	test.x <- vector(length=num.test.points, mode='numeric')
	test.y <- vector(length=num.test.points, mode='numeric')
	
	training.index <- 1
	test.index <- 1
	for (i in 1:N) {
		if (i %in% training.row.indices) {
			training.x[training.index] <- x[i]
			training.y[training.index] <- y[i]
			training.index <- training.index + 1
		} else {
			test.x[test.index] <- x[i]
			test.y[test.index] <- y[i]
			test.index <- test.index + 1
		}
	}
	sets <- list(training.x=training.x, training.y=training.y, test.x=test.x, test.y=test.y)
	return (sets)
}

best.df <- -1
best.test.error <- -1
for (df in 3:20) {
	sum.test.error.for.df <- 0
	for (i in 1:100) {
		sets <- get.test.and.training.sets(x, y)
		tr.x <- sets$training.x
		tr.y <- sets$training.y
		te.x <- sets$test.x
		te.y <- sets$test.y
		
		fm <- lm(tr.y ~ bs(tr.x, df=df, Boundary.knots=boundaries))
		predicted.y <- predict(fm, data.frame(tr.x=te.x))
		
		residuals <- predicted.y - te.y
		test.error <- sum(residuals^2)
		sum.test.error.for.df <- sum.test.error.for.df + test.error
	}
	test.error <- sum.test.error.for.df / 100
	
	if (best.df == -1 || test.error < best.test.error) {
		best.df <- df
		best.test.error <- test.error
	}
}


# bootstrap
sample.points <- function(x, y, N) {
	sample.indices <- sample(N, N, replace=T)
	
	sample.x <- vector(length=N, mode='numeric')
	sample.y <- vector(length=N, mode='numeric')
	
	for (i in 1:N) {
		sample.index <- sample.indices[i]
		sample.x[i] <- x[sample.index]
		sample.y[i] <- y[sample.index]
	}
	sets <- list(x=sample.x, y=sample.y)
	return (sets)
}

for (i in 1:100) { # easier to see with 1:10
	sample <- sample.points(x, y, N)
	s.x <- sample$x
	s.y <- sample$y
	
	fm <- lm(s.y ~ bs(s.x))
	prediction.x <- seq(min(s.x), max(s.x), length.out=200)
	prediction.y <- predict(fm, data.frame(s.x=prediction.x))
	lines(prediction.x, prediction.y, col='grey')
}
# bootstrap confidence interval: (2nd+3rd)/2 largest and smallest bmd at each x
