source("libraries.R")

dec0 <- dec

# demographics of authors
# countries, regions, languages, HDI for
# first authors, last authors, and interactions between first and last authors

first_author_tbl_1 <- table(dec0$first_auth_geog)
first_author_tbl_2 <- round(table(dec0$first_auth_geog) / sum(table(dec0$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

senior_author_tbl_1 <- table(dec0$senior_auth_geog)
senior_author_tbl_2 <- round(table(dec0$senior_auth_geog) / sum(table(dec0$senior_auth_geog)),4) * 100
senior_author_tbl_1 ; senior_author_tbl_2

chisq.test(first_author_tbl_1, senior_author_tbl_1, simulate.p.value = TRUE)

first_senior_author_tbl_1 <- table(dec0$first_auth_geog, dec$senior_auth_geog)
first_senior_author_tbl_2 <- round(first_senior_author_tbl_1 /
                                           rowSums(first_senior_author_tbl_1),3)
assocstats(first_senior_author_tbl_1)

##### ----- ###### 

dec_review        <- select(dec0, ms_id, mean_review_score, paper_rejected)
names(dec_review) <- c("ms_id", "mean_review", "paper_rejected")
dec_review        <- inner_join(author_decisions,
                                dec_review,
                                by = "ms_id")
dec_review$submit_month        <- NULL
dec_review$author_institution  <- NULL

## Performed one time only
# author_country_id <- unique(dec_review$author_country)
# author_country_id <- edit(author_country_id) # remove NA; change Georgia to Tbilisi (fix state name Georgia)
# country_id        <- geocode(author_country_id,
#                              output = "latlon",
#                              source = "google")
# country_id$author_country <- author_country_id
# write.table(country_id, file="country_id.txt", quote=TRUE, sep=",", row.names=FALSE)
# rm(author_country_id)

auth_dec <- melt(dec_review, id.vars = c("ms_id",
                                         "author_order",
                                         "author_country"))

auth_dec <- auth_dec %>%
        left_join(country_id)

auth_dec <- filter(auth_dec,
                   variable == "corresponding_author")

first_author <- auth_dec %>% group_by(ms_id) %>%
        filter(author_order == min(author_order)) %>%
        select(ms_id, author_country)

last_author  <- auth_dec %>% group_by(ms_id) %>%
        filter(author_order == max(author_order)) %>%
        select(ms_id, author_country)

first_author <- first_author %>% inner_join(country_id)
last_author  <- last_author %>% inner_join(country_id)

auth_dec        <- inner_join(first_author, last_author, by = "ms_id")
names(auth_dec) <- c("ms_id", "first_author", "lon_fa", "lat_fa", "last_author", "lon_la", "lat_la")
auth_dec        <- auth_dec[,-1] # remove ms_id column
auth_dec        <- data.frame(auth_dec)

## Exploratory graph
# set.seed(1234)
# auth_dec_test         <- select(auth_dec, first_author, last_author)
# ggnet2(auth_dec_test, label = TRUE, label.size = 4)
# rm(auth_dec_test)

auth_freq           <- sort(table(auth_dec$first_author))
auth_freq           <- as.data.frame(auth_freq)
auth_freq$country   <- rownames(auth_freq)
rownames(auth_freq) <- NULL
names(auth_freq)    <- c("Freq", "first_author")

# Usage: first_last_author("United States") or fala("China")
first_last_author_stats <- function(x) {
        auth_dec_1 <- filter(auth_dec, first_author == x)
        auth_dec_1b <- data.frame(table(auth_dec_1$first_author, auth_dec_1$last_author))
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

auth_country_freq         <- sort(table(dec_review$author_country), decreasing = TRUE)
auth_country_freq         <- data.frame(auth_country_freq)
names(auth_country_freq)  <- "Freq"
auth_country_freq$cnt     <- seq(1:length(auth_country_freq$Freq))
auth_country_freq$ex      <- (auth_country_freq$Freq[1]/auth_country_freq$cnt)

p <- ggplot(auth_country_freq)
p + geom_jitter(aes(x = cnt, y = Freq)) +
  geom_line(aes(x = cnt, y = ex)) +
  xlab("Nation Count") +
  ylab("Authors per Nation") +
  theme_bw()

# internationality of co-authors?

int_auth_1 <- decisions %>%
  filter(author_count > 1) %>%
  select(first_auth_geog, senior_auth_geog, submit_year) %>%
  table() %>% print()

int_auth_1 <- data.frame(int_auth_1)

int_auth_1 %>%
  filter(first_auth_geog == senior_auth_geog) %>%
  ggplot(aes(x = submit_year, y = Freq)) +
  geom_boxplot() + theme_classic() +
  xlab("Submission Year") +
  ylab("Frequency") +
  ggtitle("First Author and Senior/Last Author from Same Region\n
          For all papers submitted")

int_auth_1 %>%
  filter(first_auth_geog != senior_auth_geog) %>%
  ggplot(aes(x = submit_year, y = Freq)) +
  geom_boxplot() + theme_classic() +
  xlab("Submission Year") +
  ylab("Frequency") +
  ggtitle("First Author and Senior/Last Author from Separate Regions\n
          For all papers submitted")

int_auth_2 <- decisions %>%
  filter(author_count > 1 & paperRejected == "No") %>%
  select(first_auth_geog, senior_auth_geog, submit_year) %>%
  table() %>% print()

int_auth_2 <- data.frame(int_auth_2)

int_auth_2 %>%
  filter(first_auth_geog == senior_auth_geog) %>%
  ggplot(aes(x = submit_year, y = Freq)) +
  geom_boxplot() + theme_classic() +
  xlab("Submission Year") +
  ylab("Frequency") +
  ggtitle("First Author and Senior/Last Author from Same Region\n
          For all papers accepted")

int_auth_2 %>%
  filter(first_auth_geog != senior_auth_geog) %>%
  ggplot(aes(x = submit_year, y = Freq)) +
  geom_boxplot() + theme_classic() +
  xlab("Submission Year") +
  ylab("Frequency") +
  ggtitle("First Author and Senior/Last Author from Separate Regions\n
          For all papers accepted")

rm(int_auth_1, int_auth_2)

# What are the trends in submissions by geographic region of submitting author?
# Identify submitting years to the data set

year_geo           <- data.frame(table(decisions$corr_auth_geog,
                                       decisions$submit_year))

colnames(year_geo) <- c("Region", "Year", "Count")
year_geo$Year      <- as.numeric(as.character(year_geo$Year))

p <- ggplot(year_geo, aes(x = Year,
                         y = Count,
                         linetype = Region))

p + geom_line() + theme_classic() +
  geom_dl(aes(label = Region), method = "smart.grid") +
  xlab("Submission Year") +
  ylab("Count of Corresponding Authors") +
  scale_linetype_discrete(guide = FALSE)

# how international are collaborators?

first_senior <- data.frame(table(decisions$first_auth_geog,
                                 decisions$senior_auth_geog))

ggplot(first_senior, aes(x = Var1,
                         y = Var2)) +
          geom_tile(aes(fill = Freq),
                    colour = "white") +
          theme_classic() +
          scale_fill_gradient(low = "white", high = "black") +
          xlab("Geographic Region of First Author") +
          ylab("Geographic Region of Senior/Last Author")

### Where the submitting author is the first author:

# Submitting author is first author, ergo is different than last author,
# which we assume to be the senior author.

dec0 %>%
  filter(submit_auth_first_auth == "Yes") %>%
  filter(author_count > 1) %>%
  select(first_auth_geog, senior_auth_geog) %>%
  table() %>%
  data.frame() %>%
  ggplot(aes(x = first_auth_geog,
             y = senior_auth_geog)) +
          geom_tile(aes(fill = Freq),
                    colour = "white") +
          theme_classic() +
          scale_fill_gradient(low = "white", high = "black") +
          xlab("Geographic Region of First Author") +
          ylab("Geographic Region of Senior/Last Author")

rm(first_author_tbl_1, first_author_tbl_2, senior_author_tbl_1, 
   senior_author_tbl_2)

rm(first_senior_author_tbl_1, first_senior_author_tbl_2, 
   first_senior_author_tbl_3, first_senior_author_tbl_4,
   first_senior_author_tbl_5, first_senior_author_tbl_6)

rm(dec_scores, dec_review)

rm(mp, map_world)

rm(auth_dec_1, auth_dec_2, auth_dec_3, auth_dec_4,
   auth_dec_4b, auth_dec_5, auth_dec_5b, auth_dec_6)

rm(int_auth_1, int_auth_2)

rm(auth_country_freq, auth_freq, authorcountryid, country_id, fala)

rm(p, year_geo, fala)