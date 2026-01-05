x <- rnorm(500)

sample_var <- function(x) {
	sum( (x - mean(x))^2)  / (length(x) - 1)
}

pop_var <- function(x) {
	sum( (x - mean(x))^2)  / (length(x))
}

# var(x)
sample_var(x)
pop_var(x)

n <- 5:500

diff <- numeric(length(x))
for(i in n) {
	x2 <- x[1:i]
	diff[i] <- abs(sample_var(x2) - pop_var(x2))
}
diff

library(ggplot2)
ggplot(data.frame(x = 1:length(diff), y = diff), aes(x = x, y = y)) + geom_point()


# What position are the vowels?
letters
vowels <- c('a', 'e', 'i', 'o', 'u')
letters %in% vowels
letters %in% vowels |> which()


