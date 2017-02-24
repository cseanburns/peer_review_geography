# Set up for Table 2 in manuscript
dec_review <- author_decisions

# Replaced Georgia with Tbilisi since it refers to nation and not US State (geocodes otherwise)
dec_review$author_country <- gsub("Georgia", "Tbilisi",
                                  dec_review$author_country)

dec_review   <- select(dec_review, ms_id, author_order, author_country)
country_id   <- read.csv("country_id.csv")
dec_review   <- dec_review %>% left_join(country_id)

# Get basic total authorship characteristics ; exploratory analysis
auth_country_freq        <- sort(table(dec_review$author_country),
                                 decreasing = TRUE)
auth_country_freq        <- data.frame(auth_country_freq)
names(auth_country_freq) <- c("Country", "Freq")
auth_country_freq$ratios <- auth_country_freq$Freq[1] / auth_country_freq$Freq
auth_country_freq$count  <- seq(1:length(auth_country_freq$Freq))
auth_country_freq$perc   <- round(auth_country_freq$Freq /
                                          sum(auth_country_freq$Freq) * 100, 2)
View(auth_country_freq)
rm(auth_country_freq)

# Continue setting up Table 2
first_author <- dec_review %>% group_by(ms_id) %>%
        filter(author_order == min(author_order)) %>%
        select(ms_id, author_country)
first_author <- first_author %>% inner_join(country_id)

last_author  <- dec_review %>% group_by(ms_id) %>%
        filter(author_order == max(author_order)) %>%
        select(ms_id, author_country)
last_author  <- last_author %>% inner_join(country_id)

dec_review        <- inner_join(first_author, last_author, by = "ms_id")
names(dec_review) <- c("ms_id", "first_author", "lon_first",
                     "lat_first", "last_author", "lon_last", "lat_last")
dec_review        <- data.frame(dec_review)

rm(country_id, first_author, last_author)

# Usage: first_last_author("United States") or fala("China")
# Comment out the last line in the function to get the sum
first_last_author_stats <- function(x) {
        auth_dec_1 <- filter(dec_review, first_author == x)
        auth_dec_1b <- data.frame(table(auth_dec_1$first_author,
                                        auth_dec_1$last_author))
        auth_dec_1b$Perc <- round(auth_dec_1b$Freq / sum(auth_dec_1b$Freq), 3) * 100
        auth_dec_1b[order(auth_dec_1b$Perc, decreasing = TRUE), ]
        #sum(auth_dec_1b$Freq)
}

first_last_author_stats("United States")
first_last_author_stats("China")
first_last_author_stats("Australia")
first_last_author_stats("United Kingdom")
first_last_author_stats("France")
first_last_author_stats("Germany")
first_last_author_stats("Canada")
first_last_author_stats("Spain")

rm(first_last_author_stats, dec_review)
