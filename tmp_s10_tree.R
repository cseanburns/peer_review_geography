source("libraries.R")

# copy main data file to new data frame
# dec_tree <- decisions
decTree <- dec0

# remove non-necessary variables
# dec_tree$abstractWordCount <- NULL
# dec_tree$noAuthSexIdent <- NULL
decTree$msID <- NULL
decTree$sort <- NULL

# data set to analyze
decAnalysis <- decTree
nobs <- nrow(decAnalysis)

set.seed(23)
test <- sample(nobs, 0.7*nobs)

set.seed(23)
train <- sample(nobs, 0.3*nobs)

decTest  <- decAnalysis[order(test),]
decTrain <- decAnalysis[order(train),]

fit.test.1 <- rpart(paperRejected ~ .,
                    data = decTest, method = "class")
fit.test.1
summary(fit.test.1)
fancyRpartPlot(fit.test.1)

p1 <- predict(fit.test.1,
              decTrain,
              class = "class")

table(decTrain$paperRejected, p1)

fit.train.1 <- rpart(paperRejected ~ .,
                     data = decTrain,
                     method = "class")
fancyRpartPlot(fit.train.1)

# conditional inference tree
decTrain2 <- decTrain %>% na.omit()
fit.train.2 <- ctree(paperRejected ~ ., data = decTrain2)
plot(fit.train.2)

## ---- Classifying the Review Process ----
library(rpart)
decTree <- select(decTree, paperRejected, meanReviewScore,
                   seniorAuthGeog, firstAuthGeog, handlingEditorGeog,
                   timeToDecision)
decTree <- filter(decTree, meanReviewScore > 0)
decTree <- filter(decTree, timeToDecision > 0)
decTree <- filter(decTree, !is.na(firstAuthGeog))
decTree <- filter(decTree, !is.na(seniorAuthGeog))

decTree$paperRejected <- relevel(decTree$paperRejected, ref = "Yes")
decTree$seniorAuthGeog <- relevel(decTree$seniorAuthGeog, ref = "United Kingdom")
decTree$firstAuthGeog <- relevel(decTree$firstAuthGeog, ref = "United Kingdom")
decTree$handlingEditorGeog <- relevel(decTree$handlingEditorGeog, ref = "United Kingdom")

summary(decTree)
fit.tree.1 <- rpart(paperRejected ~ ., data = decTree, method = "class")
summary(fit.tree.1)
fit.tree.1
fancyRpartPlot(fit.tree.1)

summary(decTree)
fit.tree.2 <- rpart(meanReviewScore ~ ., data = decTree, method = "anova")
summary(fit.tree.2)
fit.tree.2
fancyRpartPlot((fit.tree.2))

library(party)
fit.tree.2 <- ctree(paperRejected ~ ., data = decTree) 
plot(fit.tree.2)
