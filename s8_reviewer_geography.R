source('libraries.R')
library("readr")

reviewer_data_by_paper <- read_csv("reviewer_geographies.csv")
rev_dec <- reviewer_data_by_paper %>% filter(ms_id >= 201000001)
rm(reviewer_data_by_paper)

table(rev_dec$reviewer_region)

rev_dec$reviewer_region <- rev_dec$regions

rev_dec$reviewer_region <- gsub("AustandNZ", "Oceania",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("EasternAsia", "Asia",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("EasternEurope", "Europe",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("LatinAmerica", "Latin America",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("NorthAmerica", "North America",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("NorthernEurope", "Europe",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("OtherAsia", "Asia",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("SouthernAsia", "Asia",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("SouthernEurope", "Europe",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("UnitedKingdom", "United Kingdom",
                                rev_dec$reviewer_region)
rev_dec$reviewer_region <- gsub("WesternEurope", "Europe",
                                rev_dec$reviewer_region)

table(rev_dec$reviewer_region)

rev_dec <- filter(rev_dec, reviewer_score > 0)

rev_dec$reviewer_country <- NULL 

rev_dec$reviewer_region <- factor(rev_dec$reviewer_region)
rev_dec$reviewer_region <- relevel(rev_dec$reviewer_region,
                                   ref = "Europe")

contrasts(rev_dec$reviewer_region)

# merge with first auth geog data

auth <- select(author_decisions, ms_id, geographic_region, author_order)
auth <- filter(auth, author_order == 1)
auth$author_region <- auth$geographic_region
auth$geographic_region <- NULL
reviewer_auth <- inner_join(auth, rev_dec, by = "ms_id")
reviewer_auth <- reviewer_auth[complete.cases(reviewer_auth),]

fit.1 <- glm(reviewer_score ~ author_region + reviewer_region + 
                     author_region * reviewer_region,
             data = reviewer_auth)


summary(fit.1)

Anova(fit.1)

rm(auth, rev_dec, reviewer_auth, fit.1)


