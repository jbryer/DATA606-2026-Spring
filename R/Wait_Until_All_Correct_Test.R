library(ggplot2)

# https://fediscience.org/@DarrinLRogers/113952951823744486
size <- 5 # Test size (i.e. number of questions)
p <- 1/4 # Probability of randomly getting correct answer

# Simulate one test attempt
test <- sample(c(TRUE, FALSE), size = size, prob = c(p, 1 - p), replace = TRUE)
test

#' Simulate how long until all answers are correct
#' @param size test size.
#' @param prob probability of randomly getting correct answer
#' @param stop_score the score on the test we wish to achieve. Value of 1
#'        indicates a perfect score.
simulate_test <- function(size, p, stop_score = 1) {
	n <- 0
	repeat{
		n <- n + 1
		test <- sample(c(TRUE, FALSE),
					   size = size,
					   prob = c(p, 1 - p),
					   replace = TRUE)
		if(mean(test) >= stop_score) {
			break
		}
	}
	return(n)
}

simulate_test(size = size, p = p)

simulations <- integer(1000)
for(i in 1:length(simulations)) {
	simulations[i] <- simulate_test(size = size, p = p)
}

mean(simulations)
median(simulations)
plot(density(simulations))

ggplot(data.frame(x = simulations), aes(x = x)) +
	geom_histogram(aes(y = ..density..), binwidth = 50, fill = 'grey70') +
	geom_density(color = 'blue')


# Geometric distribution
geom_dist <- data.frame(x = 0:5000,
						y = dgeom(0:5000, prob = dbinom(x = size, size = size, prob = p)))
( cut_point <- qgeom(0.5, prob = dbinom(x = size, size = size, prob = p)) )
ggplot(geom_dist, aes(x = x, y = y)) +
	geom_polygon(data = rbind(data.frame(x = 0, y = 0),
							  geom_dist[geom_dist$x < cut_point,],
							  data.frame(x = cut_point, y = 0)),
				 fill = 'grey50') +
	geom_path(stat = 'identity', color = 'blue')


# Probability of getting all questions correct
dbinom(x = size, size = size, prob = p)

# The expected value from the geometric distribution is 1 / p
1 / dbinom(x = size, size = size, prob = p)

# This should be closer to the median from the simulation
qgeom(0.5, prob = dbinom(x = size, size = size, prob = p))

# Plot the binomial distribution
dist <- dbinom(x = 0:size, size = size, prob = p)
ggplot2::ggplot(data.frame(x = 0:size,
						   prob = dist,
						   label = paste0(round(100 * dist, digits = 2), '%')),
				ggplot2::aes(x = x, y = prob, label = label)) +
	ggplot2::geom_bar(stat = 'identity', fill = 'grey50') +
	ggplot2::geom_text(vjust = -0.5)


