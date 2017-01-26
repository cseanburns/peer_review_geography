source("libraries.R")

dec0 <- dec

# Geographical differences between first and last authors
first_author_tbl_1 <- table(dec0$first_auth_geog)
first_author_tbl_2 <- round(table(dec0$first_auth_geog) /
                                    sum(table(dec0$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

senior_author_tbl_1 <- table(dec0$senior_auth_geog)
senior_author_tbl_2 <- round(table(dec0$senior_auth_geog) /
                                     sum(table(dec0$senior_auth_geog)),4) * 100
senior_author_tbl_1 ; senior_author_tbl_2

chisq.test(first_author_tbl_1, senior_author_tbl_1, simulate.p.value = TRUE)

first_senior_author_tbl_1 <- table(dec0$first_auth_geog, dec0$senior_auth_geog)
first_senior_author_tbl_2 <- round(first_senior_author_tbl_1 /
                                           rowSums(first_senior_author_tbl_1),3)
# this is Table 1 in manuscript
first_senior_author_tbl_1 ; first_senior_author_tbl_2
assocstats(first_senior_author_tbl_1)

rm(first_author_tbl_1, first_author_tbl_2)
rm(senior_author_tbl_1, senior_author_tbl_2)
rm(first_senior_author_tbl_1, first_senior_author_tbl_2)

# Geographical difference between first and corresponding authors
first_author_tbl_1 <- table(dec0$first_auth_geog)
first_author_tbl_2 <- round(table(dec0$first_auth_geog) /
                                    sum(table(dec0$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

corr_author_tbl_1 <- table(dec0$corr_auth_geog)
corr_author_tbl_2 <- round(table(dec0$corr_auth_geog) /
                                   sum(table(dec0$corr_auth_geog)),4) * 100
corr_author_tbl_1 ; corr_author_tbl_2

chisq.test(first_author_tbl_1, corr_author_tbl_1, simulate.p.value = TRUE)

first_corr_author_tbl_1 <- table(dec0$first_auth_geog, dec0$corr_auth_geog)
first_corr_author_tbl_2 <- round(first_corr_author_tbl_1 /
                                           rowSums(first_corr_author_tbl_1),3)
first_corr_author_tbl_1 ; first_corr_author_tbl_2
assocstats(first_corr_author_tbl_1)

rm(first_author_tbl_1, first_author_tbl_2)
rm(corr_author_tbl_1, corr_author_tbl_2)
rm(first_corr_author_tbl_1, first_corr_author_tbl_2)

# Geographical difference between first and submitting authors
first_author_tbl_1 <- table(dec0$first_auth_geog)
first_author_tbl_2 <- round(table(dec0$first_auth_geog) /
                                    sum(table(dec0$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

sub_author_tbl_1 <- table(dec0$submit_auth_geog)
sub_author_tbl_2 <- round(table(dec0$submit_auth_geog) /
                                  sum(table(dec0$submit_auth_geog)),4) * 100
sub_author_tbl_1 ; sub_author_tbl_2

chisq.test(first_author_tbl_1, sub_author_tbl_1, simulate.p.value = TRUE)

first_sub_author_tbl_1 <- table(dec0$first_auth_geog, dec0$submit_auth_geog)
first_sub_author_tbl_2 <- round(first_sub_author_tbl_1 /
                                           rowSums(first_sub_author_tbl_1),3)
first_sub_author_tbl_1 ; first_sub_author_tbl_2
assocstats(first_sub_author_tbl_1)

rm(first_author_tbl_1, first_author_tbl_2)
rm(first_sub_author_tbl_1, first_sub_author_tbl_2)
rm(sub_author_tbl_1, sub_author_tbl_2)

# Set up for Table 2 in manuscript; examines co-authorship among top submitting
# nations
# create subset from working copy
dec_review        <- select(dec0, ms_id, mean_review_score, paper_rejected)
names(dec_review) <- c("ms_id", "mean_review", "paper_rejected")
dec_review        <- inner_join(author_decisions,
                                dec_review,
                                by = "ms_id")
dec_review$submit_month        <- NULL
dec_review$author_institution  <- NULL
# replace Georgia with Tbilisi to distinguish between US State (geocoding issue)
dec_review$author_country      <- gsub("Georgia", "Tbilisi",
                                       dec_review$author_country)

### --- ###
# # geocoding author countries; not using this in the final analysis but it
# # was useful in exploratory work
# author_country_id <- unique(dec_review$author_country)
# country_id        <- geocode(author_country_id,
#                              output = "latlon",
#                              source = "google")
# country_id$author_country <- author_country_id
# write.table(country_id, file="country_id.txt",
#             quote=TRUE, sep=",",
#             row.names=FALSE)
# rm(author_country_id)
### --- ###

### --- ###
# # get basic total authorship characteristics; not explicitly used in manuscript
# # but useful in exploratory analysis
# auth_country_freq        <- sort(table(dec_review$author_country),
#                                  decreasing = TRUE)
# auth_country_freq        <- data.frame(auth_country_freq)
# names(auth_country_freq) <- c("Country", "Freq")
# auth_country_freq$ratios <- auth_country_freq$Freq[1] / auth_country_freq$Freq
# auth_country_freq$count  <- seq(1:length(auth_country_freq$Freq))
# auth_country_freq$perc   <- round(auth_country_freq$Freq /
#                                           sum(auth_country_freq$Freq) * 100, 2)
# View(auth_country_freq)
# rm(auth_country_freq)
### --- ###

# For table 2
auth_dec <- melt(dec_review, id.vars = c("ms_id",
                                         "author_order",
                                         "author_country"))

country_id <- read.csv("~/Dropbox/workspace/geography/country_id.csv")
auth_dec <- auth_dec %>% left_join(country_id)
auth_dec <- filter(auth_dec, variable == "corresponding_author")

first_author <- auth_dec %>% group_by(ms_id) %>%
        filter(author_order == min(author_order)) %>%
        select(ms_id, author_country)

first_author <- first_author %>% inner_join(country_id)

last_author  <- auth_dec %>% group_by(ms_id) %>%
        filter(author_order == max(author_order)) %>%
        select(ms_id, author_country)

last_author  <- last_author %>% inner_join(country_id)

auth_dec        <- inner_join(first_author, last_author, by = "ms_id")
names(auth_dec) <- c("ms_id", "first_author", "lon_fa",
                     "lat_fa", "last_author", "lon_la", "lat_la")
auth_dec        <- auth_dec[,-1] # remove ms_id column
auth_dec        <- data.frame(auth_dec)

## Exploratory graph
# set.seed(1234)
# auth_dec_test         <- select(auth_dec, first_author, last_author)
# ggnet2(auth_dec_test, label = TRUE, label.size = 4)
# rm(auth_dec_test)

# Table 2 in manuscript
auth_freq           <- sort(table(auth_dec$first_author))
auth_freq           <- as.data.frame(auth_freq)
auth_freq$country   <- rownames(auth_freq)
rownames(auth_freq) <- NULL
names(auth_freq)    <- c("country", "Freq", "id")

# Usage: first_last_author("United States") or fala("China")
first_last_author_stats <- function(x) {
        auth_dec_1 <- filter(auth_dec, first_author == x)
        auth_dec_1b <- data.frame(table(auth_dec_1$first_author,
                                        auth_dec_1$last_author))
        auth_dec_1b$Perc <- round(auth_dec_1b$Freq / sum(auth_dec_1b$Freq), 3) * 100
        auth_dec_1b[order(auth_dec_1b$Perc, decreasing = TRUE), ]
}

first_last_author_stats("United States")
first_last_author_stats("China")
first_last_author_stats("Australia")
first_last_author_stats("United Kingdom")
first_last_author_stats("France")
first_last_author_stats("Germany")
first_last_author_stats("Canada")
first_last_author_stats("Spain")

# Get cumulative percentage of contributions by nation
auth_freq$perc <- cumsum(round(auth_freq$Freq / sum(auth_freq$Freq) * 100, 3))

rm(auth_freq, first_last_author_stats)
rm(first_author, last_author)

rm(auth_dec, country_id, dec_review)
