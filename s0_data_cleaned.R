source("libraries.R")

dec <- read.csv("decisions-2.csv")

# create submission year variable
dec$submit_year <- substr(dec$ms_id, 1, 4)
dec$submit_year <- as.numeric(dec$submit_year)

# select key variables
dec <- select(dec, ms_id, first_auth_geog, corr_auth_geog, submit_auth_geog,
              senior_auth_geog, handling_editor_geog, mean_review_score,
              mean_reviewer_days_respond, mean_reviewer_days_review,
              prop_reviewers_responding, prop_reviewers_agreeing,
              sent_for_review, paper_rejected, no_reviews_obtained,
              no_reviewers_responded, time_to_decision, submit_year)

# select variables for imputing
simple <- select(dec, first_auth_geog, corr_auth_geog, submit_auth_geog,
                 senior_auth_geog, handling_editor_geog, mean_review_score,
                 mean_reviewer_days_respond, mean_reviewer_days_review,
                 prop_reviewers_responding, prop_reviewers_agreeing,
                 no_reviews_obtained, no_reviewers_responded, time_to_decision,
                 paper_rejected)

# set seed and impute variables
set.seed(23)
imputed <- complete(mice(simple))

# save imputed variables
dec$first_auth_geog            <- imputed$first_auth_geog
dec$corr_auth_geog             <- imputed$corr_auth_geog
dec$submit_auth_geog           <- imputed$submit_auth_geog
dec$senior_auth_geog           <- imputed$senior_auth_geog
dec$handling_editor_geog       <- imputed$handling_editor_geog
dec$mean_review_score          <- imputed$mean_review_score
dec$mean_reviewer_days_respond <- imputed$mean_reviewer_days_respond
dec$mean_reviewer_days_review  <- imputed$mean_reviewer_days_review
dec$prop_reviewers_responding  <- imputed$prop_reviewers_responding
dec$prop_reviewers_agreeing    <- imputed$prop_reviewers_agreeing
dec$no_reviews_obtained        <- imputed$no_reviews_obtained
dec$no_reviewers_responded     <- imputed$no_reviewers_responded
dec$time_to_decision           <- imputed$time_to_decision
dec$paper_rejected             <- imputed$paper_rejected

rm(simple, imputed)

# convert to factors
dec$first_auth_geog      <- factor(dec$first_auth_geog,
                                   ordered = FALSE)

dec$corr_auth_geog       <- factor(dec$corr_auth_geog,
                                   ordered = FALSE)

dec$submit_auth_geog     <- factor(dec$submit_auth_geog,
                                   ordered = FALSE)

dec$senior_auth_geog     <- factor(dec$senior_auth_geog,
                                   ordered = FALSE)

dec$handling_editor_geog <- factor(dec$handling_editor_geog,
                                   ordered = FALSE)

dec$sent_for_review      <- factor(dec$sent_for_review,
                                   levels = c("0", "1"),
                                   labels = c("No", "Yes"),
                                   ordered = FALSE)

dec$paper_rejected      <- factor(dec$paper_rejected, # NOTE reverse order
                                 levels = c("1", "0"),
                                 labels = c("Yes", "No"),
                                 ordered = FALSE)

dec$submit_year         <- factor(dec$submit_year,
                                 ordered = FALSE)

# to avoid multicollinearity
# Keep only first_auth_geog since this is highly correlated with:
# corr_auth_geog, submit_auth_geog, senior_auth_geog
# Keep only no_reviewers_obtained since this is highly correlated with no_reviewers_responded

dec <- select(dec, -corr_auth_geog, -submit_auth_geog, -senior_auth_geog, -no_reviewers_responded)

### Author decisions data #### 

author_decisions <- read.csv("author_decisions.csv")

author_decisions$submit_year          <- factor(author_decisions$submit_year, ordered = FALSE)
author_decisions$submit_month         <- factor(author_decisions$submit_month, ordered = FALSE)

author_decisions$corresponding_author <- factor(author_decisions$corresponding_author,
                                                levels = c("0", "1", "2"),
                                                labels = c("No", "Yes", "None"),
                                                ordered = FALSE)

author_decisions$submitting_author <- factor(author_decisions$submitting_author,
                                             levels = c("0", "1"),
                                             labels = c("No", "Yes"),
                                             ordered = FALSE)

author_decisions$senior_author <- factor(author_decisions$senior_author,
                                          levels = c("0", "1"),
                                          labels = c("No", "Yes"),
                                          ordered = FALSE)

author_decisions$missing_authors <- factor(author_decisions$missing_authors,
                                           levels = c("0", "1"),
                                           labels = c("No", "Yes"),
                                           ordered = FALSE)

author_decisions$author_country <- as.character(author_decisions$author_country)

author_decisions$final_decision <- NULL # the data is not good / correct

author_decisions$submit_date <- as.Date(as.character(author_decisions$submit_date))
