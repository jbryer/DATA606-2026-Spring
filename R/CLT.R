library(ggplot2)

N <- 1e5

pop <- runif(N, 0, 1)

pop_mean <- mean(pop)

samp_n <- 30
samp <- sample(pop, size = samp_n)
samp_mean <- mean(samp)
se <- sd(samp) / sqrt(samp_n)

hist.samp <- density(samp,
					 from = mean(samp) - 1.96 * sd(samp),
					 to = mean(samp) + 1.96 * sd(samp))
hist.samp <- data.frame(x = hist.samp$x, y = hist.samp$y)
hist.sampdist <- data.frame(x = seq(mean(samp) - 1.96 * se,
									mean(samp) + 1.96 * se, 0.01))
hist.sampdist$y <- dnorm(hist.sampdist$x, mean = mean(samp), sd = se)

boot.samples <- numeric(1000) # 1,000 bootstrap samples
for(i in seq_along(boot.samples)) {
	tmp <- sample(samp, size = length(samp), replace = TRUE)
	boot.samples[i] <- mean(tmp)
}
hist.boot <- data.frame(x = seq(mean(boot.samples) - 1.96 * sd(boot.samples),
								mean(boot.samples) + 1.96 * sd(boot.samples), 0.01))
hist.boot$y <- dnorm(hist.boot$x, mean = mean(boot.samples), sd = sd(boot.samples))

ggplot(data = data.frame(x = pop)) +
	geom_density(data = data.frame(x = pop), aes(x = x), alpha = 0.2) +
	geom_vline(xintercept = pop_mean) +
	geom_ribbon(data = hist.boot, aes(x = x, ymin = 0, ymax = y), fill = 'green', alpha = 0.5) +
	geom_density(data = data.frame(x = boot.samples), aes(x = x), color = 'green') +
	geom_ribbon(data = hist.samp, aes(x = x, ymin = 0, ymax = y), fill = 'blue', alpha = 0.5) +
	geom_density(data = data.frame(x = samp), aes(x = x), color = 'blue') +
	geom_ribbon(data = hist.sampdist, aes(x = x, ymin = 0, ymax = y), fill = 'maroon', alpha = 0.5) +
	stat_function(fun = dnorm, n = 1000,
				  args = list(mean = mean(samp), sd = se), color = 'maroon') +
	xlim(mean(samp) - 3 * sd(samp), mean(samp) + 3 * sd(samp)) + ylab("") +
	ylim(c(0, max(c(hist.sampdist$y, hist.boot$y)))) +
	theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
	ggtitle('Distribution of Population (in black), Sample (in blue), and Sampling Distribution (in maroon)',
			subtitle = paste0('Population mean = ', round(mean(pop), digits = 3),
							  ' sample n = ', length(samp), ''))


#####
pop <- rnbinom(N, 10, .5)
samp_sizes <- c(10, 20, 40, 80)
samp_dists <- data.frame(samp = 1:1000)
for(i in samp_sizes) {
	col <- paste0('n', i)
	for(j in 1:nrow(samp_dists)) {
		samp <- sample(pop, size = i)
		samp_dists[j,col] <- mean(samp)
	}
}

samp_dists2 <- reshape2::melt(samp_dists, id.vars = 'samp')
ggplot(samp_dists2, aes(x = value, color = variable, group = variable)) +
	geom_density() +
	scale_color_brewer(type = 'qual', palette = 2) +
	theme_minimal()

VisualStats::gg_qq_plot(samp_dists[,1], title = 'n = 10')
VisualStats::gg_qq_plot(samp_dists[,2], title = 'n = 20')
VisualStats::gg_qq_plot(samp_dists[,3], title = 'n = 40')
