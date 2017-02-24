source("libraries.R")

### There are two data sets: author_decisions.csv and decisons.csv. Both
### datasets are largely equivalent, but the author_decisions.csv data contains
### multiple observations for each author per manuscript, and the decicions.csv
### data is condensed. I also added the HDI and language variables and
### observations to the author_decisions.csv data.

### Author decisions data; non-condensed format
author_decisions <- read.csv("author_decisions.csv")

# set variables to proper types
author_decisions$submit_year  <- factor(author_decisions$submit_year,
                                        ordered = FALSE)
author_decisions$submit_month <- factor(author_decisions$submit_month,
                                        ordered = FALSE)

author_decisions$corresponding_author <- factor(author_decisions$corresponding_author,
                                                levels = c("0", "1", "2"),
                                                labels = c("No", "Yes", "None"),
                                                ordered = FALSE)

author_decisions$submitting_author <- factor(author_decisions$submitting_author,
                                             levels = c("0", "1"),
                                             labels = c("No", "Yes"),
                                             ordered = FALSE)
author_decisions$senior_author     <- factor(author_decisions$senior_author,
                                             levels = c("0", "1"),
                                             labels = c("No", "Yes"),
                                             ordered = FALSE)
author_decisions$missing_authors   <- factor(author_decisions$missing_authors,
                                             levels = c("0", "1"),
                                             labels = c("No", "Yes"),
                                             ordered = FALSE)

author_decisions$author_country <- as.character(author_decisions$author_country)
 
# Remove this variable; the data is not good nor correct
author_decisions$final_decision <- NULL

author_decisions$submit_date <- as.Date(as.character(author_decisions$submit_date))

### Author decisions data; condensed format
dec <- read.csv("decisions-2.csv")

# # Helps to get count of authors per paper
author_data <- inner_join(author_decisions, dec, by = "ms_id")

author_data <- select(author_data, ms_id, submit_year, author_order,
                      corresponding_author, submitting_author, senior_author,
                      missing_authors, author_country, HDI, language,
                      geographic_region, author_count, corr_auth_first_auth,
                      corr_auth_first_auth, submit_auth_first_auth,
                      submit_auth_senior_auth, corr_auth_senior_auth,
                      first_auth_geog, corr_auth_geog, submit_auth_geog,
                      senior_auth_geog, handling_editor, handling_editor_geog,
                      editor_seniority, editor_years, mean_review_score, 
                      prop_reviewers_responding, prop_reviewers_agreeing,
                      sent_for_review, paper_rejected)

author_data_max <- author_data %>%
        group_by(ms_id) %>%
        filter(author_order==max(author_order))

author_data_max$author_count <- author_data_max$author_order
author_data_max$author_order <- NULL

# Remove all papers w/ single authors (we're looking at collaborations)
author_data_max <- author_data_max %>% filter(author_count > 1)

# Convert language to English logical value
author_data_max$english <- author_data_max$language == "English"

dec <- author_data_max

rm(author_data, author_data_max)

# convert to factors

dec$corr_auth_first_auth <- factor(dec$corr_auth_first_auth,
                                   levels = c("0", "1"),
                                   labels = c("No", "Yes"),
                                   ordered = FALSE)
dec$submit_auth_first_auth <- factor(dec$submit_auth_first_auth,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"),
                                     ordered = FALSE)
dec$submit_auth_senior_auth <- factor(dec$submit_auth_senior_auth,
                                      levels = c("0", "1"),
                                      labels = c("No", "Yes"),
                                      ordered = FALSE)
dec$corr_auth_senior_auth <- factor(dec$corr_auth_senior_auth,
                                    levels = c("0", "1"),
                                    labels = c("No", "Yes"),
                                    ordered = FALSE)

dec$first_auth_geog      <- factor(dec$first_auth_geog, ordered = FALSE)
dec$corr_auth_geog       <- factor(dec$corr_auth_geog, ordered = FALSE)
dec$senior_auth_geog     <- factor(dec$senior_auth_geog, ordered = FALSE)
dec$handling_editor_geog <- factor(dec$handling_editor_geog, ordered = FALSE)
dec$submit_auth_geog     <- factor(dec$submit_auth_geog, ordered = FALSE)
dec$submit_year          <- factor(dec$submit_year, ordered = FALSE)
dec$sent_for_review      <- factor(dec$sent_for_review,
                                       levels = c("0", "1"),
                                       labels = c("No", "Yes"),
                                       ordered = FALSE)

# NOTE that the 1 and 0 are reverse order
dec$paper_rejected       <- factor(dec$paper_rejected,
                                       levels = c("1", "0"),
                                       labels = c("Yes", "No"),
                                       ordered = FALSE)

# get counts of authors per paper; and create variable noting whether authors
# are from different countries or the same country
  
auth_tmp <- author_decisions %>% select(ms_id, author_country,
                                   geographic_region, language, author_order)
auth_tmp <- auth_tmp %>% arrange(ms_id, author_order)

auth_tmp <- select(auth_tmp, ms_id, author_country, author_order)

auth_tmp <- auth_tmp %>% select(ms_id, author_country)
auth_tmp <- distinct(auth_tmp)
auth_tmp$mixed <- duplicated(auth_tmp$ms_id)

# note that FALSE for dec$mixed means all authors are from the same country
# note that TRUE for dec$mixed means authors are from separate countries

mixed.true  <- auth_tmp %>% filter(mixed == TRUE)
mixed.false <- auth_tmp %>% filter(mixed == FALSE)

mixed.true$author_country  <- NULL
mixed.false$author_country <- NULL

mixed.false.logic <- mixed.false[!(mixed.false$ms_id %in% mixed.true$ms_id),]
mixed.combined    <- rbind(mixed.true, mixed.false.logic)
mixed.combined    <- unique(mixed.combined)

auth_tmp <- inner_join(mixed.combined, dec, by="ms_id")
auth_tmp <- auth_tmp %>% arrange(ms_id)

dec <- auth_tmp

rm(auth_tmp, mixed.true, mixed.false, mixed.false.logic, mixed.combined)

# Geocoding author countries
dec_review <- author_decisions

# Replace Georgia with Tbilisi ; otherwise Google geocodes for US State
dec_review$author_country <- gsub("Georgia", "Tbilisi",
                                  dec_review$author_country)

## Geocode author countries
author_country_id <- unique(dec_review$author_country)
country_id        <- geocode(author_country_id,
                             output = "latlon",
                             source = "google")
country_id$author_country <- author_country_id
write.table(country_id, file="country_id.csv",
            quote=TRUE, sep=",",
            row.names=FALSE)
rm(author_country_id)