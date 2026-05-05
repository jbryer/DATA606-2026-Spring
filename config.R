year <- 2026
semester <- 'Spring'
course <- 'DATA 606'
class_day <- 'Monday'
class_time <- '8:00pm to 9:00pm'
description <- paste0(course, ' Statistics & Probability Fall ', semester, ' ', year)
github_link <- "DATA606-2026-Spring"
one_minute_paper <- 'https://forms.gle/Ze19MooQHvZmQE2ZA'
one_minute_paper_results <- 'https://docs.google.com/spreadsheets/d/1NfDt8izNa3jSMqZrTvmEupHR5iEb2s4P5Fwj-6Gl-mw/edit?resourcekey=&gid=661629844#gid=661629844'
formative_assessment <- 'https://forms.gle/te7oDEdSv8JKGEjq8'
formative_assessmnet_results <- 'https://docs.google.com/spreadsheets/d/1Fx4Kj7JgZ8nIZQf5TlNUOFra_PwMLwV31a4j6bTWWzs/edit?resourcekey=&gid=644446617#gid=644446617'
slack_invite_link <- 'https://cuny-msds.slack.com/archives/C0A65FTPNH0'
slack_link <- 'http://cuny-msds.slack.com'
project_signup_link <- 'https://docs.google.com/spreadsheets/d/1qBbziG4PKRT7RjVd0zuGENP3Ikvo3HgtRbt8G8Gn988/edit?usp=sharing'

library("knitr")
library("tidyverse")
library("likert")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("VisualStats")
library("DATA606")
library("reshape2")
library("latex2exp")
library("psych")
library("cowplot")
library("rmarkdown")
library("qrcode")

cuny_colors <- c('#0033A1', '#FFB71B', '#9A3CB0', '#A3C9FF', '#EA0045', '#45C2B1')

opts_chunk$set(digits = 3, width = 120)

knitr::opts_chunk$set(
	warning = FALSE,
	message = FALSE,
	error = FALSE,
	fig.width = 12,
	fig.height = 6,
	# out.width = '90%',
	fig.align = 'center',
	digits = 3,
	# width = 120,
	dev.args = list(bg = 'transparent')
)

##### Configure the default ggplot2 theme.
# This also ensures the background is transparent so it matches the slides
# ggplot2::theme_set(ggplot2::theme_minimal(base_size = 16))
ggplot2::update_theme(
	panel.background = element_rect(fill = "transparent", colour = NA),
	plot.background = element_rect(fill = "transparent", colour = NA),
	legend.background = element_rect(fill = "transparent"),
	legend.box.background = element_rect(fill = "transparent")
)

# The following is to fix a DT::datatable issue with Xaringan
# https://github.com/yihui/xaringan/issues/293
options(htmltools.dir.version = FALSE, htmltools.preserve.raw = FALSE)

# This style was adapted from Max Kuhn: https://github.com/rstudio-conf-2020/applied-ml
# And Rstudio::conf 2020: https://github.com/rstudio-conf-2020/slide-templates/tree/master/xaringan
# This slide deck shows a lot of the features of Xaringan: https://www.kirenz.com/slides/xaringan-demo-slides.html

# To use, add this to the slide title:   `r I(hexes(c("DATA606")))`
# It will use images in the images/hex_stickers directory (i.e. the filename is the parameter)
hexes <- function(x) {
	x <- rev(sort(x))
	markup <- function(pkg) glue::glue('<img src="images/hex/{pkg}.png" class="title-hex">')
	res <- purrr::map_chr(x, markup)
	paste0(res, collapse = "")
}

printLaTeXFormula <- function(fit, digits=2) {
	vars <- all.vars(fit$terms)
	result <- paste0('\\hat{', vars[1], '} = ', prettyNum(fit$coefficients[[1]], digits=2))
	for(i in 2:length(vars)) {
		val <- fit$coefficients[[i]]
		result <- paste0(result, ifelse(val < 0, ' - ', ' + '),
						 prettyNum(abs(val), digits=digits),
						 ' ', names(fit$coefficients)[i])
	}
	return(result)
}

PlotDist <- function(alpha, from = -5, to = 5, n = 1000, filename = NULL,
					 alternative = c("two.tailed", "greater", "lesser"),
					 distribution = c("normal", "t", "F", "chisq", "binomial"),
					 colour = "black", fill = "skyblue2",
					 ...)
{
	alternative <- match.arg(alternative)
	alt.alpha <- switch(alternative, two.tailed = alpha/2, greater = alpha,
						lesser = alpha)
	MyDen <- switch(distribution, normal = dnorm, t = dt, F = df,
					chisq = dchisq, binomial = dbinom)
	MyDist <- switch(distribution, normal = qnorm, t = qt, F = qf,
					 chisq = qchisq, binomial = qbinom)
	crit.lower <- MyDist(p = alt.alpha, lower.tail = TRUE, ...)
	crit.upper <- MyDist(p = alt.alpha, lower.tail = FALSE, ...)
	cord.x1 <- c(from, seq(from = from, to = crit.lower, length.out = 100),
				 crit.lower)
	cord.y1 <- c(0, MyDen(x = seq(from = from, to = crit.lower,
								  length.out = 100), ...), 0)
	cord.x2 <- c(crit.upper, seq(from = crit.upper, to = to,
								 length.out = 100), to)
	cord.y2 <- c(0, MyDen(x = seq(from = crit.upper, to = to,
								  length.out = 100), ...), 0)
	if (!is.null(filename)) pdf(file = filename)
	curve(MyDen(x, ...), from = from, to = to, n = n, col = colour,
		  lty = 1, lwd = 2, ylab = "Density", xlab = "Values")
	if (!identical(alternative, "greater")) {
		polygon(x = cord.x1, y = cord.y1, col = fill)
	}
	if (!identical(alternative, "lesser")) {
		polygon(x = cord.x2, y = cord.y2, col = fill)
	}
	if (!is.null(filename)) dev.off()
}

ompWordCloud <- function(text) {
	if(length(text) < 1) {
		warning('No text to create a word cloud.')
		return()
	}
	docs <- Corpus(VectorSource(text))
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))
	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	# Remove english common stopwords
	docs <- tm_map(docs, removeWords, stopwords("english"))
	# Remove your own stop word
	# specify your stopwords as a character vector
	docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)
	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)
	dtm <- TermDocumentMatrix(docs)
	m <- as.matrix(dtm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	head(d, 10)
	wordcloud(words = d$word, freq = d$freq, min.freq = 2,
			  max.words=200, random.order=FALSE, rot.per=0.35,
			  colors=brewer.pal(8, "Dark2"))
}


##### Meetup banner
meetup_image <- function(title,
						 date = format(Sys.Date(), '%B %d, %Y'),
						 url = paste0('https://', tolower(semester), year, '.data606.net'),
						 out_file) {
	print(here::here())
	cuny_logo <- png::readPNG(paste0(here::here(), '/website/images/CUNY_SPS_Logo_Wide.png')) |>
		grid::rasterGrob(interpolate = TRUE)
	course_logo <- png::readPNG(paste0(here::here(), '/',
									   paste0(sub(' ', '', course), '-', semester, year, '.png'))) |>
						   	grid::rasterGrob(interpolate = TRUE)

	ggplot2::ggplot() +
		ggplot2::annotate(geom = 'text', label = likert:::label_wrap_mod(title, width = 20),
						  x = 0, y = 7.5, color = '#0033A1',
				 hjust = 0, size = 14, size.unit = 'pt', fontface = 'bold') +
		ggplot2::annotate(geom = 'text', label = date, x = 0, y = 5.5, color = '#0033A1',
				 hjust = 0, size = 10, size.unit = 'pt') +
		ggplot2::annotate(geom = 'text', label = url, x = 10, y = 0, color = '#FF9822',
				 hjust = 1, size = 6, size.unit = 'pt') +
		ggplot2::annotation_custom(cuny_logo, xmin = 0, xmax = 4, ymin = 0, ymax = 2) +
		ggplot2::annotation_custom(course_logo, xmin = 6, xmax = 10, ymin = 0, ymax = 10) +
		ggplot2::xlim(0, 10) + ggplot2::ylim(0, 10) +
		ggplot2::geom_point() +
		ggplot2::theme_void() +
		ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'white'))
	ggplot2::ggsave(filename = out_file,
		   width = 1280, height = 720, units = 'px', device = 'png')
}
