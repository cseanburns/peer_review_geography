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

# filter out manuscripts that were sent for review
dec_sent <- filter(dec, sent_for_review == 1)
dec_unsent <- filter(dec, sent_for_review == 0)

# select variables for imputing
simple <- select(dec_sent, first_auth_geog, corr_auth_geog, submit_auth_geog,
                 senior_auth_geog, handling_editor_geog, mean_review_score,
                 mean_reviewer_days_respond, mean_reviewer_days_review,
                 prop_reviewers_responding, prop_reviewers_agreeing,
                 no_reviews_obtained, no_reviewers_responded, time_to_decision,
                 paper_rejected)

# set seed and impute variables only for sent_for_review is true
imputed <- complete(mice(simple, seed = 23))

# save imputed variables
dec_sent$first_auth_geog            <- imputed$first_auth_geog
dec_sent$corr_auth_geog             <- imputed$corr_auth_geog
dec_sent$submit_auth_geog           <- imputed$submit_auth_geog
dec_sent$senior_auth_geog           <- imputed$senior_auth_geog
dec_sent$handling_editor_geog       <- imputed$handling_editor_geog
dec_sent$mean_review_score          <- imputed$mean_review_score
dec_sent$mean_reviewer_days_respond <- imputed$mean_reviewer_days_respond
dec_sent$mean_reviewer_days_review  <- imputed$mean_reviewer_days_review
dec_sent$prop_reviewers_responding  <- imputed$prop_reviewers_responding
dec_sent$prop_reviewers_agreeing    <- imputed$prop_reviewers_agreeing
dec_sent$no_reviews_obtained        <- imputed$no_reviews_obtained
dec_sent$no_reviewers_responded     <- imputed$no_reviewers_responded
dec_sent$time_to_decision           <- imputed$time_to_decision
dec_sent$paper_rejected             <- imputed$paper_rejected

# rm(simple, imputed)

# convert to factors; sent for review
dec_sent$first_auth_geog      <- factor(dec_sent$first_auth_geog, ordered = FALSE)
dec_sent$corr_auth_geog       <- factor(dec_sent$corr_auth_geog, ordered = FALSE)
dec_sent$senior_auth_geog     <- factor(dec_sent$senior_auth_geog, ordered = FALSE)
dec_sent$handling_editor_geog <- factor(dec_sent$handling_editor_geog, ordered = FALSE)
dec_sent$submit_auth_geog     <- factor(dec_sent$submit_auth_geog, ordered = FALSE)
dec_sent$submit_year          <- factor(dec_sent$submit_year, ordered = FALSE)

dec_sent$sent_for_review <- factor(dec_sent$sent_for_review,
                                   levels = c("0", "1"),
                                   labels = c("No", "Yes"),
                                   ordered = FALSE)

dec_sent$paper_rejected  <- factor(dec_sent$paper_rejected, # NOTE reverse order
                                   levels = c("1", "0"),
                                   labels = c("Yes", "No"),
                                   ordered = FALSE)

# convert to factors; not sent for review
dec_unsent$first_auth_geog      <- factor(dec_unsent$first_auth_geog, ordered = FALSE)
dec_unsent$corr_auth_geog       <- factor(dec_unsent$corr_auth_geog, ordered = FALSE)
dec_unsent$submit_auth_geog     <- factor(dec_unsent$submit_auth_geog, ordered = FALSE)
dec_unsent$senior_auth_geog     <- factor(dec_unsent$senior_auth_geog, ordered = FALSE)
dec_unsent$handling_editor_geog <- factor(dec_unsent$handling_editor_geog, ordered = FALSE)
dec_unsent$submit_auth_geog     <- factor(dec_unsent$submit_auth_geog, ordered = FALSE)
dec_unsent$submit_year          <- factor(dec_unsent$submit_year, ordered = FALSE)

dec_unsent$sent_for_review <- factor(dec_unsent$sent_for_review,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"),
                                     ordered = FALSE)

dec_unsent$paper_rejected  <- factor(dec_unsent$paper_rejected, # NOTE reverse order
                                     levels = c("1", "0"),
                                     labels = c("Yes", "No"),
                                     ordered = FALSE)

dec <- rbind(dec_sent, dec_unsent)

rm(imputed, simple)

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

###

# geocoding author countries ; after loading s1_authors.R
author_country_id <- unique(dec_review$author_country)
country_id        <- geocode(author_country_id,
                             output = "latlon",
                             source = "google")
country_id$author_country <- author_country_id
write.table(country_id, file="country_id.txt", quote=TRUE, sep=",", row.names=FALSE)
rm(author_country_id)