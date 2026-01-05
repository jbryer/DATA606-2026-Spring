library(ggplot2)
library(googlesheets4)
library(dplyr)
library(VisualStats)

results_url <- 'https://docs.google.com/spreadsheets/d/1lAOM-wRsbD1z38jcc2yN6T4p1Qo13dCQlJcLGMM-yJk/edit?resourcekey&usp=forms_web_b&urp=linked#gid=1432388966'

results <- read_sheet(results_url)
results <- results |>
	dplyr::filter(lubridate::hour(Timestamp) > 12)


results_melt <- results |>
	reshape2::melt(id.vars = 'Timestamp') |>
	rename(color = variable)
head(results_melt)


tab <- psych::describeBy(results_melt$value,
						 group = results_melt$color,
						 mat = TRUE, skew = FALSE) |>
	rename(color = group1)
tab
ggplot(tab, aes(x = color, y = mean, fill = color)) +
	geom_bar(stat = 'identity') +
	scale_fill_manual(breaks = tab$color, values = tab$color) +
	theme_minimal() +
	theme(legend.position = 'none')

aov(value ~ color, data = results_melt) |> summary()







# Expected results
mm_expected <- c(
	'Blue' = 0.24,
	'Orange' = 0.20,
	'Green' = 0.16,
	'Yellow' = 0.14,
	'Red' = 0.13,
	'Brown' = 0.13
)
ggplot(data.frame(color = names(mm_expected), percent = unname(mm_expected)),
	   aes(x = color, y = percent, fill = color)) +
	geom_bar(stat = 'identity') +
	geom_text(aes(label = paste0(100 * percent, '%')), vjust = -0.5) +
	scale_fill_manual(breaks = names(mm_expected), values = names(mm_expected)) +
	scale_y_continuous(labels = scales::label_percent()) +
	xlab('') + ylab('Percent') +
	theme_minimal() +
	theme(legend.position = 'none')
