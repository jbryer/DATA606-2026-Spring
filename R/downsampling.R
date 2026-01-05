# install.packages('MultilevelPSA')

library(splitstackshape)
library(medley) # remotes::install_github('jbryer/medley') # Not on CRAN yet

# https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success
# student <- read.csv('course_data/students_dropout_and_academic_success.csv', sep = ';')

# https://r4ds.github.io/bookclub-tmwr/class-imbalance.html
# members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv")
members <- read.csv('course_data/members.csv')
table(members$died) |> prop.table()

members$died <- as.factor(members$died)

set.seed(2112); splits <- stratified(members, "died", size = 0.75, bothSets = TRUE)
member_train <- splits[[1]]
member_valid <- splits[[2]]

member_formu <- died ~ season + year + sex + age + solo + oxygen_used
member_lr_out <- glm(member_formu, data = member_train, family = binomial(link = 'logit'))
member_predict <- predict(lr_out, newdata = member_valid, type = 'response')

member_train$died |> table() |> print() |> prop.table()
member_valid$died |> table() |> print() |> prop.table()

table(member_valid$died, member_predict > 0.1)
hist(member_predict)

medley::calculate_roc(member_predict, member_valid$died) |> plot()
medley::confusion_matrix(member_valid$died, member_predict > 0.1)
table(member_valid$died, member_predict > 0.1)

member_ds_out <- downsample_train(member_formu, member_train, ratio = 4)
length(member_ds_out)

member_predict_ds <- predict(member_ds_out, newdata = member_valid, type = 'response')
member_valid$predictions <- apply(member_predict_ds, 1, mean)
medley::calculate_roc(member_valid$predictions, member_valid$died) |> plot()
medley::confusion_matrix(observed = member_valid$died,
						 predicted = (member_valid$predictions > 0.5))

medley::accuracy(observed = member_valid$died, predicted = member_valid$predictions < 0.02)

###### PISA
data(pisana, package = 'multilevelPSA')
data(pisa.psa.cols, package = 'multilevelPSA')

pisana$Public <- pisana$PUBPRIV == 'Public'

formu <- paste0('Public ~ CNT + ', paste0(pisa.psa.cols, collapse = ' + ')) |> as.formula()

set.seed(2112); splits <- splitstackshape::stratified(pisana, "Public", size = 0.75, bothSets = TRUE)
train <- splits[[1]] |> as.data.frame()
valid <- splits[[2]] |> as.data.frame()

# Verify the stratification worked
table(pisana$Public, useNA = 'ifany') |> print() |> prop.table() # Full dataset
table(train$Public, useNA = 'ifany') |> print() |> prop.table()  # Training
table(valid$Public, useNA = 'ifany') |> print() |> prop.table()  # Validation

lr_out <- glm(formu, data = train, family = binomial(link = 'logit'))
summary(lr_out)

valid$predictions <- predict(lr_out, newdata = valid, type = 'response')

table(valid$predictions > 0.5, useNA = 'ifany')
hist(valid$predictions)
median(valid$predictions)

medley::calculate_roc(predictions = valid$predictions,
					  observed = valid$Public) |> plot()

medley::confusion_matrix(observed = valid$Public, predicted = valid$predictions > 0.43)

train_true <- train[train$Public,]
train_false <- train[!train$Public,]

set.seed(2112); train2 <- rbind(train_true[sample(2 * nrow(train_false)),], train_false)

lr_out2 <- glm(formu, data = train2, family = binomial(link = 'logit'))

valid$predictions2 <- predict(lr_out2, newdata = valid, type = 'response')

hist(valid$predictions2)

medley::calculate_roc(predictions = valid$predictions2,
					  observed = valid$Public) |> plot()
medley::confusion_matrix(observed = valid$Public, predicted = valid$predictions2 > .152)


#' Training
downsample_train <- function(formu, data, ratio = 1, ...) {
	data <- as.data.frame(data)
	dep_var <- all.vars(formu)[1]
	tab_out <- table(data[,dep_var])
	big_class <- names(tab_out[tab_out == max(tab_out)])
	small_class <- names(tab_out[tab_out == min(tab_out)])
	train_big <- data[data[,dep_var] == big_class,]
	train_small <- data[data[,dep_var] == small_class,]
	n_models <- floor(nrow(train_big) / (ratio * nrow(train_small)))
	if(n_models == 1) {
		warning('Only training one model. Is this really what you want to do?')
	}
	model <- sample(n_models, size = nrow(train_big), replace = TRUE)
	models <- list()
	for(i in seq_len(n_models)) {
		train_data <- rbind(train_small, train_big[model == i,])
		models[[i]] <- glm(formu, train_data, family = binomial(link = 'logit'))
	}
	class(models) <- c('downsample', 'list')
	return(models)
}

#' Predict values from downsampling
#'
#' @method predict downsample
predict.downsample <- function(object, newdata, ...) {
	predictions <- data.frame(predict(object[[1]], newdata = newdata, ...))
	if(length(object) > 1) {
		for(i in 2:length(object)) {
			predictions <- cbind(predictions, predict(object[[i]], newdata = newdata, ...))
		}
	}
	names(predictions) <- paste0('model', 1:length(object))
	return(predictions)
}


downsample_out <- downsample_train(formu = formu, data = train)
length(downsample_out)
predictions <- predict(downsample_out, newdata = valid, type = 'response')
prediction <- apply(predictions, 1, mean)

medley::calculate_roc(predictions = prediction,
					  observed = valid$Public) |> plot()
medley::confusion_matrix(observed = valid$Public, predicted = prediction > .091)


