source("libraries.R")

# demographics of authors
# countries, regions, languages, HDI for
# first authors, last authors, and interactions between first and last authors

dec0 <- dec

first_author_tbl_1 <- table(dec0$first_auth_geog)
first_author_tbl_2 <- round(table(dec0$first_auth_geog) / sum(table(dec0$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

senior_author_tbl_1 <- table(dec0$senior_auth_geog)
senior_author_tbl_2 <- round(table(dec0$senior_auth_geog) / sum(table(dec0$senior_auth_geog)),4) * 100
senior_author_tbl_1 ; senior_author_tbl_2

chisq.test(first_author_tbl_1, senior_author_tbl_2, simulate.p.value = TRUE)

first_senior_author_tbl_1 <- table(dec0$first_auth_geog, dec$senior_auth_geog)
first_senior_author_tbl_2 <- round(first_senior_author_tbl_1 /
                                           rowSums(first_senior_author_tbl_1),3)
first_senior_author_tbl_3 <- data.frame(round(first_senior_author_tbl_1 /
                                                      rowSums(first_senior_author_tbl_1),3))
names(first_senior_author_tbl_3) <- c("First_Author", "Senior_Author", "Proportion")
assocstats(first_senior_author_tbl_3)

first_senior_author_tbl_4 <- table(dec$senior_auth_geog, dec$first_auth_geog)
first_senior_author_tbl_5 <- round(first_senior_author_tbl_4 /
                                           rowSums(first_senior_author_tbl_4),3)
first_senior_author_tbl_6 <- data.frame(round(first_senior_author_tbl_5 /
                                                      rowSums(first_senior_author_tbl_5),3))

names(first_senior_author_tbl_6) <- c("Senior_Author", "First_Author", "Proportion")
assocstats(first_senior_author_tbl_6)

dec_scores        <- select(dec0, ms_id, mean_review_score, paper_rejected)
names(dec_scores) <- c("ms_id", "mean_review", "paper_rejected")
dec_review        <- inner_join(author_decisions,
                                dec_scores,
                                by = "ms_id")

rm(dec_scores)

dec_review$submit_month        <- NULL
dec_review$author_institution  <- NULL

dec_review$submit_date <- format(dec_review$submit_date,
                                 format = "%m/%d/%Y")

dec_review$submit_date <- as.Date(dec_review$submit_date)

authorcountry_id <- unique(dec_review$author_country)
authorcountry_id <- edit(author_country_id) # remove NA; change Georgia to Tbilisi (fix state name Georgia)
country_id       <- geocode(author_country_id,
                            output = "latlon",
                            source = "google")
country_id$author_country <- author_country_id
write.table(country_id, file="country_id.txt", quote=TRUE, sep=",", row.names=FALSE)

mp        <- NULL
map_world <- borders("world", colour = "black", fill = "#019E73")
mp        <- ggplot() + map_world
mp + geom_point(data =countryid,
                aes(x = lon, y = lat)) + theme_bw()

auth_dec_1 <- melt(dec_review, id.vars = c("ms_id",
                                    "author_order",
                                    "author_country"))

auth_dec_1 <- auth_dec_1 %>%
  left_join(countryid)

auth_dec_2 <- filter(auth_dec_1,
                     variable == "corresponding_author")
rm(auth_dec_1)

first_author <- filter(auth_dec_2,
                       author_order == 1) %>% select(ms_id, author_country)

last_author  <- auth_dec_2 %>% group_by(ms_id) %>%
        filter(author_order == max(author_order, na.rm = TRUE)) %>%
        select(ms_id, author_country)

first_author <- first_author %>% inner_join(countryid)
last_author  <- last_author %>% inner_join(countryid)
last_author  <- data.frame(last_author)

auth_dec_3          <- inner_join(first_author, last_author, by = "ms_id")
names(auth_dec_3)   <- c("ms_id", "first_author", "lon_fa", "lat_fa", "last_author", "lon_la", "lat_la")
head(auth_dec_3)

auth_dec_4          <- auth_dec_3[,-1] # remove ms_id column
head(auth_dec_4)

auth_dec_4b         <- select(auth_dec_4, first_author, last_author)
plot(graph_from_data_frame(auth_dec_4b))

set.seed(1234)
ggnet2(auth_dec_4b, label = TRUE, label.size = 4)

auth_freq           <- sort(table(auth_dec_4$first_author))
auth_freq           <- as.data.frame(auth_freq)
auth_freq$country   <- rownames(auth_freq)
rownames(auth_freq) <- NULL
names(auth_freq)    <- c("Freq", "first_author")

auth_dec_6 <- auth_dec_4 %>% distinct(first_author) %>% select(first_author, lon_fa, lat_fa)
auth_dec_6 <- inner_join(auth_dec_6, auth_freq, by = "first_author")

# identify countries with at least 100 first authors
sort(table(auth_dec_4$first_author))

# Usage: fala("United States") or fala("China")
fala <- function(x) {
        auth_dec_5 <- filter(auth_dec_4, first_author == x)
        auth_dec_5b <- data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))
        auth_dec_5b$Perc <- round(auth_dec_5b$Freq / sum(auth_dec_5b$Freq), 3) * 100
        auth_dec_5b[order(auth_dec_5b$Perc, decreasing = TRUE), ]
}

auth_dec_5 <- filter(auth_dec_4, first_author == "United States")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "China")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "Australia")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "United Kingdom")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "France")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "Germany")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "Canada")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

auth_dec_5 <- filter(auth_dec_4, first_author == "Spain")
data.frame(table(auth_dec_5$first_author, auth_dec_5$last_author))

mp       <- NULL
map_world <- borders("world", colour = "black", fill = "#FFFFFF")
mp       <- ggplot() + map_world

for ( i in 1:nrow(auth_dec_5) ) {
        from <- c(auth_dec_5$lon_fa[i], auth_dec_5$lat_fa[i])
        to   <- c(auth_dec_5$lon_la[i], auth_dec_5$lat_la[i])
        inter <- as.data.frame(gcIntermediate(
                p1 = from,
                p2 = to,
                n = 50,
                addStartEnd = TRUE))
        names(inter) <- c("lon", "lat")
        mp <- mp + geom_line(data = inter, aes(x = lon, y = lat), color = "blue")
}

mp + geom_point(data = auth_dec_6, aes(x = lon_fa, y = lat_fa, size = (Freq/sum(Freq)))) +
        labs(x = "Longitude",
             y = "Latitude") +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme_bw() +
        theme(legend.position = "none") 

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

rm(auth_country_freq, auth_freq, authorcountryid, countryid, fala)

rm(p, year_geo, fala)
