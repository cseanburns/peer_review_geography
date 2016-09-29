source("libraries.R")

dec <- read.csv("decisions-2.csv")

# create submission year variable
dec$submitYear <- substr(dec$ms_id, 1, 4)
dec$submitYear <- as.numeric(dec$submitYear)

dec <- select(dec, msID, firstAuthGeog, corrAuthGeog, submitAuthGeog,
              seniorAuthGeog, handlingEditorGeog, meanReviewScore,
              meanReviewerDaysRespond, meanReviewerDaysReview,
              propReviewersResponding, propReviewersAgreeing,
              sentForReview, paperRejected, noReviewsObtained,
              noReviewersResponded, timeToDecision, submitYear)

simple <- select(dec, firstAuthGeog, corrAuthGeog, submitAuthGeog,
                 seniorAuthGeog, handlingEditorGeog, meanReviewScore,
                 meanReviewerDaysRespond, meanReviewerDaysReview,
                 propReviewersResponding, propReviewersAgreeing,
                 noReviewsObtained, noReviewersResponded, timeToDecision,
                 paperRejected)

set.seed(23)
imputed <- complete(mice(simple))

dec$firstAuthGeog           <- imputed$firstAuthGeog
dec$corrAuthGeog            <- imputed$corrAuthGeog
dec$submitAuthGeog          <- imputed$submitAuthGeog
dec$seniorAuthGeog          <- imputed$seniorAuthGeog
dec$handlingEditorGeog      <- imputed$handlingEditorGeog
dec$meanReviewScore         <- imputed$meanReviewScore
dec$meanReviewerDaysRespond <- imputed$meanReviewerDaysRespond
dec$meanReviewerDaysReview  <- imputed$meanReviewerDaysReview
dec$propReviewersResponding <- imputed$propReviewersResponding
dec$propReviewersAgreeing   <- imputed$propReviewersAgreeing
dec$noReviewsObtained       <- imputed$noReviewsObtained
dec$noReviewersResponded    <- imputed$noReviewersResponded
dec$timeToDecision          <- imputed$timeToDecision
dec$paperRejected           <- imputed$paperRejected

##### Decisions Data #####

# convert to factors
dec$firstAuthGeog      <- factor(dec$firstAuthGeog,
                                 ordered = FALSE)

dec$handlingEditorGeog <- factor(dec$handlingEditorGeog,
                                 ordered = FALSE)

dec$submitYear         <- factor(dec$submitYear,
                                 ordered = FALSE)

dec$sentForReview      <- factor(dec$sentForReview,
                                 levels = c("0", "1"),
                                 labels = c("No", "Yes"),
                                 ordered = FALSE)

dec$paperRejected      <- factor(dec$paperRejected, # NOTE reverse order
                                 levels = c("1", "0"),
                                 labels = c("Yes", "No"),
                                 ordered = FALSE)

# to avoid multicollinearity
# Keep only firstAuthGeog since this is highly correlated with corrAuthGeog, submitAuthGeog, seniorAuthGeog
# Keep only noReviewersObtained since this is highly correlated with NoReviewersResponded

dec <- select(dec, -corrAuthGeog, -submitAuthGeog, -seniorAuthGeog, -noReviewersResponded)

rm(simple, imputed, decisions)

### Additional decisions data #### 

author_decisions <- read.csv("author_decisions.csv")

author_decisions$corresponding_author <- factor(author_decisions$corresponding_author,
                                                levels = c("2", "0", "1"),
                                                labels = c("None", "No", "Yes"),
                                                ordered = TRUE)

author_decisions$submitting_author <- factor(author_decisions$submitting_author,
                                             levels = c("0", "1"),
                                             labels = c("No", "Yes"),
                                             ordered = TRUE)

author_decisions$senior_author <- factor(author_decisions$senior_author,
                                          levels = c("0", "1"),
                                          labels = c("No", "Yes"),
                                          ordered = TRUE)

author_decisions$missing_authors <- factor(author_decisions$missing_authors,
                                           levels = c("0", "1"),
                                           labels = c("No", "Yes"),
                                           ordered = FALSE)

author_decisions$author_country <- as.character(author_decisions$author_country)

author_decisions$submit_date <- as.Date(as.character(author_decisions$submit_date))