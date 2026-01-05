library(VisualStats)
library(ggplot2)

ggplot2::theme_set(ggplot2::theme_minimal())

data(depression, package = 'VisualStats')

lm(depression ~ anxiety, data = depression) |> summary()
lm(depression ~ affect, data = depression) |> summary()

GGally::ggpairs(depression[,c('depression', 'anxiety', 'affect')])

lm_out <- lm(depression ~ anxiety + affect, data = depression)
summary(lm_out)
depression$predicted <- predict(lm_out)

lm_out_interaction <- lm(depression ~ anxiety * affect, data = depression)
summary(lm_out_interaction)
depression$predicted_interaction <- predict(lm_out_interaction)

# Total sum of squares (i.e. the numerator part of the variance)
sum((depression$depression - mean(depression$depression))^2)
# Error sum of squared
sum((depression$depression - predict(lm_out))^2)
# The anova function gives us how each predictor contributes to the total sum of squares
(aov_out <- anova(lm_out))
sum(aov_out$`Sum Sq`)

affect_mean <- mean(depression$affect)
affect_sd <- sd(depression$affect)
min_anxiety <- min(depression$anxiety)
max_anxiety <- max(depression$anxiety)
simple_slopes <- data.frame(
	anxiety = c(min_anxiety, max_anxiety, min_anxiety, max_anxiety),
	affect = c(affect_mean - affect_sd, affect_mean - affect_sd,
			   affect_mean + affect_sd, affect_mean + affect_sd)
)
simple_slopes$depression <- predict(lm_out_interaction, newdata = simple_slopes)
simple_slopes$Moderator <- c(rep('Low Affect (-1 SD)', 2), rep('High Affect (+1 SD)', 2))

ggplot(simple_slopes, aes(x = anxiety, y = depression, color = Moderator)) +
	geom_point(size = 4) +
	geom_line()


library(rpart)
rpart_out <- rpart(depression ~ anxiety + affect, data = depression)
rpart_out
summary(rpart_out)
plot(rpart_out)
text(rpart_out, use.n = TRUE)

# rpart_out <- rpart(poverty ~ white + female_house, data = poverty)

# cor(depression$depression, depression$anxiety) * (sd(depression$depression) / sd(depression$anxiety))
# cov(depression$depression, depression$anxiety) / var(depression$anxiety)
# y <- depression$depression
# x1 <- depression$anxiety
# x2 <- depression$affect

# lm1 <- lm(y ~ x1)
# lm2 <- lm(y ~ x2)
#
# cov(y, resid(lm1)) / var(resid(lm1))
# cov(y, resid(lm2)) / var(resid(lm2))
# cov(cbind(y, x1, x2))

cowplot::plot_grid(
	ggplot(depression, aes(x = depression, y = predicted)) + geom_point(),
	ggplot(depression, aes(x = depression, y = predicted_interaction)) + geom_point(),
	nrow = 1
)

# R-squared
cor(depression$depression, depression$predicted)^2
cor(depression$depression, depression$predicted_interaction)^2

multiple_regression_vis(y = depression$depression,
						x1 = depression$anxiety,
						x2 = depression$affect,
						y_lab = 'Depression',
						x1_lab = 'Anxiety',
						x2_lab = 'Affect',
						plot_residuals = TRUE,
						plot_slopes = 'x2',
						interaction = TRUE)

depression$affect_level <- cut(
	x = depression$affect,
	# breaks = quantile(depression$affect, probs = c(0, 0.3, 0.6, 1)),
	# labels = c('Low Affect', 'Medium Affect', 'High Affect'),
	breaks = quantile(depression$affect, probs = seq(0, 1, 0.25)),
	labels = c('Low Affect', 'Medium-Low Affect', 'Medium-High', 'High Affect'),
	include.lowest = TRUE)

depression$affect_level |> table(useNA = 'ifany')

summary(lm_out)

ggplot(depression, aes(x = anxiety, y = depression, color = affect)) +
	geom_point(alpha = 0.4) +
	geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
	# geom_abline(intercept = lm_out$coefficients[1], slope = lm_out$coefficients[2]) +
	facet_grid(~ affect_level, margins = TRUE) +
	theme(legend.position = 'none')

# ggplot(depression, aes(x = anxiety, y = depression, color = affect)) +
# 	geom_point(alpha = 0.4) +
# 	geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
# 	facet_wrap(~ affect, nrow = 1) +
# 	theme(legend.position = 'none')

high_affect <- depression |> dplyr::filter(affect >= 40)
low_affect <-  depression |> dplyr::filter(affect <= 20)

x_limits <- range(depression$anxiety)
y_limits <- range(depression$depression)


p_high <- ggplot(high_affect, aes(x = anxiety, y = depression)) +
	geom_point() +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
	xlim(x_limits) + ylim(y_limits) +
	ggtitle("High Positive Affect")

p_low <- ggplot(low_affect, aes(x = anxiety, y = depression)) +
	geom_point() +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
	xlim(x_limits) + ylim(y_limits) +
	ggtitle("Low Positive Affect")

cowplot::plot_grid(p_high, p_low, nrow = 1)


data("manufacturing", package = "VisualStats")
multiple_regression_vis(
	y = manufacturing$Strength,
	x1 = manufacturing$Temperature,
	x2 = manufacturing$Pressure,
	y_lab = 'Strength',
	x1_lab = 'Temperature',
	x2_lab = 'Pressure',
	interaction = TRUE
)

data("poverty", package = "VisualStats")
GGally::ggpairs(poverty[,c('poverty', 'white', 'female_house')])
multiple_regression_vis(
	y = poverty$poverty,
	x1 = poverty$white,
	x2 = poverty$female_house,
	y_lab = 'Poverty',
	x1_lab = 'Percent White',
	x2_lab = 'Percent Female Head of Household',
	interaction = FALSE
)
lm(poverty ~ white, data = poverty) |> summary()
lm(poverty ~ female_house, data = poverty) |> summary()
lm(poverty ~ female_house + white, data = poverty) |> summary()

var(poverty$poverty)
sqrt(var(poverty$poverty))
sd(poverty$poverty)

sum((poverty$poverty - mean(poverty$poverty))^2 / (nrow(poverty) - 1))

sum((poverty$poverty - mean(poverty$poverty))^2)

##### Using GLM approach
residual_sum_squares <- function(parameters, predictors, outcome) {
	if(length(parameters) - 1 != ncol(predictors)) {
		stop('Number of parameters does not match number of predictors.')
	}
	predicted <- 0
	for(i in 1:ncol(predictors)) {
		predicted <- predicted + parameters[i] * predictors[i]
	}
	predicted <- predicted + parameters[length(parameters)]
	residuals <- outcome - predicted
	ss <- sum(residuals^2)
	return(ss)
}

optim.rss <- optim_save(
	par = runif(3),
	fn = residual_sum_squares,
	method = "L-BFGS-B",
	predictors = depression[,c('anxiety', 'affect')],
	outcome = depression$depression
)

glm_out <- glm(depression ~ anxiety + affect, data = depression)
sum((depression$depression - predict(glm_out))^2)
residual_sum_squares(parameters = c(0.58300, -0.21048, 5.73635),
					 predictors = depression[,c('anxiety', 'affect')],
					 outcome = depression$depression)
optim.rss$par

optim.rss$iterations_df |> head()

data(mtcars)
lm(mpg ~ wt, data = mtcars) |> summary()
lm(mpg ~ vs, data = mtcars) |> summary()
lm(mpg ~ wt + vs, data = mtcars) |> summary()
cor.test(mtcars$wt, mtcars$vs)

optim.rss <- optim_save(
	par = runif(3),
	fn = residual_sum_squares,
	# method = "L-BFGS-B",
	predictors = mtcars[,c('wt', 'vs')],
	outcome = mtcars$mpg
)
optim.rss$par

# scatterplot3d::scatterplot3d(
# 	x = depression$anxiety,
# 	y = depression$affect,
# 	z = depression$depression,
# 	xlab = "Anxiety Severity",
# 	ylab = "Positive Affect",
# 	zlab = "Depression Severity"
# )

# grid.lines = 21
# fit.dep1b <- lm(depression ~ anxiety * affect, data = depression)
# x.pred <- seq(min(depression$anxiety), max(depression$anxiety), length.out = grid.lines)
# y.pred <- seq(min(depression$affect), max(depression$affect), length.out = grid.lines)
# xy <- expand.grid(anxiety = x.pred, affect = y.pred)
# z.pred <- matrix(predict(fit.dep1b, newdata = xy),
# 				 nrow = grid.lines, ncol = grid.lines)
# fitpoints <- predict(fit.dep1b)
#
# plot3D::scatter3D(
# 	x = depression$anxiety,
# 	y = depression$affect,
# 	z = depression$depression,
# 	pch = 21, cex = .6, cex.lab = .7, bty = "b2",
# 	theta = 25,  phi = -2,
# 	ticktype = "detailed",
# 	zlim = c(-4, 21.5),
# 	xlab = "Anxiety Severity",
# 	ylab = "Positive Affect",
# 	zlab = "Depression Severity",
# 	surf = list(x = x.pred, y = y.pred, z = z.pred,
# 				facets = NA,
# 				col="grey75"),
# 	colkey = F,
# 	col = "steelblue4",
# 	main = "",
# 	plot = T
# )

matrix(c(10, 3, 3, 3, 5, 0, 3, 0, 15), 3, 3)

library(MASS)
?mvrnorm
df <- mvrnorm(
	n = 100,
	mu = c(0, 0, 0),
	Sigma = matrix(c(10, 3, 3, 3, 5, 0, 3, 0, 5), 3, 3)
) |> as.data.frame()
head(df)
lm(V1 ~ V2 + V3, data = df) |> summary()
cor(df)
cor.test(df$V2, df$V3)

lm(V1 ~ V2, data = df)
lm(V1 ~ V3, data = df)
lm(V1 ~ V2 + V3, data = df)
