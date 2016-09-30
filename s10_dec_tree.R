# this is exploratory

source("libraries.R")

# copy main data file to new data frame
# dec_tree <- decisions
dec.tree <- dec0

# remove non-necessary variables
# dec_tree$abstractWordCount <- NULL
# dec_tree$noAuthSexIdent <- NULL
dec.tree$ms_id <- NULL
dec.tree$sort <- NULL

# data set to analyze
dec.analysis <- dec.tree
nobs <- nrow(dec.analysis)

set.seed(23)
test <- sample(nobs, 0.7*nobs)

set.seed(23)
train <- sample(nobs, 0.3*nobs)

dec.test  <- dec.analysis[order(test),]
dec.train <- dec.analysis[order(train),]

fit.test.1 <- rpart(paper_rejected ~ .,
                    data = dec.test, method = "class")
fit.test.1
summary(fit.test.1)
fancyRpartPlot(fit.test.1)

p1 <- predict(fit.test.1,
              dec.train,
              class = "class")

table(dec.train$paper_rejected, p1)

fit.train.1 <- rpart(paper_rejected ~ .,
                     data = dec.train,
                     method = "class")
fancyRpartPlot(fit.train.1)

# conditional inference tree
dec.train2  <- dec.train %>% na.omit()
fit.train.2 <- ctree(paper_rejected ~ ., data = dec.train2)
plot(fit.train.2)

## ---- Classifying the Review Process ----
library(rpart)
dec.tree <- select(dec.tree, paper_rejected, mean_review_score,
                   senior_auth_geog, first_auth_geog, handling_editor_geog,
                   time_to_decision)
dec.tree <- filter(dec.tree, mean_review_score > 0)
dec.tree <- filter(dec.tree, time_to_decision > 0)
dec.tree <- filter(dec.tree, !is.na(first_auth_geog))
dec.tree <- filter(dec.tree, !is.na(senior_auth_geog))

dec.tree$paper_rejected       <- relevel(dec.tree$paper_rejected, ref = "Yes")
dec.tree$senior_auth_geog     <- relevel(dec.tree$senior_auth_geog, ref = "United Kingdom")
dec.tree$first_auth_geog      <- relevel(dec.tree$first_auth_geog, ref = "United Kingdom")
dec.tree$handling_editor_geog <- relevel(dec.tree$handling_editor_geog, ref = "United Kingdom")

summary(dec.tree)
fit.tree.1 <- rpart(paper_rejected ~ ., data = dec.tree, method = "class")
fit.tree.1
summary(fit.tree.1)
fancyRpartPlot(fit.tree.1)

summary(dec.tree)
fit.tree.2 <- rpart(mean_review_score ~ ., data = dec.tree, method = "anova")
fit.tree.2
summary(fit.tree.2)
fancyRpartPlot((fit.tree.2))

library(party)
fit.tree.2 <- ctree(paper_rejected ~ ., data = dec.tree) 
plot(fit.tree.2)
