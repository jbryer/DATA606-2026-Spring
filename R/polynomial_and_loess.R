library(ggplot2)
data("faithful")


ggplot(faithful, aes(x = waiting, y = eruptions)) +
	geom_point() +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, color = 'blue') +
	geom_smooth(method = 'lm', formula = y ~ I(x^2) + x, se = FALSE, color = 'red') +
	geom_smooth(method = 'lm', formula = y ~ I(x^3) + I(x^2) + x, se = FALSE, color = 'darkgreen') +
	geom_smooth(method = 'loess', formula = y ~ x, se = FALSE, color = 'purple') +
	theme_minimal()

lm_out <- lm(eruptions ~ waiting, data = faithful)
summary(lm_out)
lm_out2 <- lm(eruptions ~ I(waiting^2) + waiting, data = faithful)
lm_out3 <- lm(eruptions ~ I(waiting^3) + I(waiting^2) + waiting, data = faithful)

faithful$lm_resid <- resid(lm_out)

loess_out <- loess(eruptions ~ waiting, data = faithful)
faithful$loess_resid <- resid(loess_out)

cor(faithful$eruptions, predict(lm_out))^2
cor(faithful$eruptions, predict(lm_out2))^2
cor(faithful$eruptions, predict(lm_out3))^2
cor(faithful$eruptions, predict(loess_out))^2

ggplot(faithful, aes(x = waiting, y = lm_resid)) +
	geom_point() +
	theme_minimal()
ggplot(faithful, aes(x = lm_resid)) + geom_density()

ggplot(faithful, aes(x = waiting, y = loess_resid)) +
	geom_point() +
	theme_minimal()
ggplot(faithful, aes(x = loess_resid)) + geom_density()
