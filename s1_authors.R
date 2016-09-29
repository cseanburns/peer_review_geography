source("libraries.R")

# demographics of authors
# countries, regions, languages, HDI for
# first authors, last authors, and interactions between first and last authors

first_author_tbl_1 <- table(dec$first_auth_geog)
first_author_tbl_2 <- round(table(dec$first_auth_geog) / sum(table(dec$first_auth_geog)),4) * 100
first_author_tbl_1 ; first_author_tbl_2

senior_author_tbl_1 <- table(dec$senior_auth_geog)
senior_author_tbl_2 <- round(table(dec$senior_auth_geog) / sum(table(dec$senior_auth_geog)),4) * 100
senior_author_tbl_1 ; senior_author_tbl_2

chisq.test(first_author_tbl_1, senior_author_tbl_2, simulate.p.value = TRUE)

rm(first_author_tbl_1, first_author_tbl_2,
   senior_author_tbl_1, senior_author_tbl_2)

first_senior_author_tbl_1 <- table(dec$first_auth_geog, dec$senior_auth_geog)
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

rm(first_senior_author_tbl_1,
   first_senior_author_tbl_2,
   first_senior_author_tbl_3,
   first_senior_author_tbl_4,
   first_senior_author_tbl_5,
   first_senior_author_tbl_6)

dec_scores        <- select(dec, ms_id, mean_review_score, paper_rejected)
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

authorcountryid <- unique(dec_review$author_country)
authorcountryid <- edit(authorcountryid) # remove NA; change Georgia to Tbilisi (fix state name Georgia)
countryid       <- geocode(authorcountryid,
                           output = "latlon",
                           source = "google")
countryid$author_country <- authorcountryid

mp       <- NULL
mapWorld <- borders("world", colour = "black", fill = "#019E73")
mp       <- ggplot() + mapWorld
mp + geom_point(data =countryid,
                aes(x = lon, y = lat)) + theme_bw()

ad2 <- melt(dec_review, id.vars = c("ms_id",
                                    "author_order",
                                    "author_country"))

ad2 <- ad2 %>%
    left_join(countryid)

ad3 <- filter(ad2,
              variable == "corresponding_author")
rm(ad2)

first_author <- filter(ad3,
                       author_order == 1) %>% select(msid, author_country)

last_author  <- ad3 %>% group_by(msid) %>%
        filter(author_order == max(author_order, na.rm = TRUE)) %>%
        select(msid, author_country)

first_author <- first_author %>% inner_join(countryid)
last_author  <- last_author %>% inner_join(countryid)
last_author  <- data.frame(last_author)

ad4          <- inner_join(first_author, last_author, by = "msid")
names(ad4)   <- c("ms_id", "first_author", "lon_fa", "lat_fa", "last_author", "lon_la", "lat_la")
head(ad4)

ad5          <- ad4[,-1] # remove ms_id column
head(ad5)
ad5b <- select(ad5, first_author, last_author)
plot(graph_from_data_frame(ad5b))
set.seed(1234)
ggnet2(ad5b, label = TRUE, label.size = 4)

adw <- sort(table(ad5$first_author))
adw <- as.data.frame(adw)
adw$country <- rownames(adw)
rownames(adw) <- NULL
names(adw) <- c("Freq", "first_author")

ad7 <- ad5 %>% distinct(first_author) %>% select(first_author, lon_fa, lat_fa)
ad7 <- inner_join(ad7, adw, by = "first_author")

sort(table(ad5$first_author)) # identify countries with at least 100 first authors

# Usage: fala("United States") or fala("China")
fala <- function(x) {
        ad6 <- filter(ad5, first_author == x)
        ad6a <- data.frame(table(ad6$first_author, ad6$last_author))
        ad6a$Perc <- round(ad6a$Freq / sum(ad6a$Freq), 3) * 100
        ad6a[order(ad6a$Perc, decreasing = TRUE), ]
}

ad6 <- filter(ad5, first_author == "United States") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "China") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "Australia") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "United Kingdom") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "France") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "Germany") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "Canada") ; data.frame(table(ad6$first_author, ad6$last_author))
ad6 <- filter(ad5, first_author == "Spain") ; data.frame(table(ad6$first_author, ad6$last_author))

mp       <- NULL
mapWorld <- borders("world", colour = "black", fill = "#FFFFFF")
mp       <- ggplot() + mapWorld

for ( i in 1:nrow(ad6) ) {
        from <- c(ad6$lon_fa[i], ad6$lat_fa[i])
        to   <- c(ad6$lon_la[i], ad6$lat_la[i])
        inter <- as.data.frame(gcIntermediate(
                p1 = from,
                p2 = to,
                n = 50,
                addStartEnd = TRUE))
        names(inter) <- c("lon", "lat")
        mp <- mp + geom_line(data = inter, aes(x = lon, y = lat), color = "blue")
}

mp + geom_point(data = ad7, aes(x = lon_fa, y = lat_fa, size = (Freq/sum(Freq)))) +
        labs(x = "Longitude",
             y = "Latitude") +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme_bw() +
        theme(legend.position = "none") 

# -----

an1 <- sort(table(dec_review$author_country), decreasing = TRUE)
an1 <- data.frame(an1)
names(an1) <- "Freq"
an1$cnt <- seq(1:length(an1$Freq))
an1$ex  <- (an1$Freq[1]/an1$cnt)
# plot(an1$Freq ~ an1$cnt)
# lines(an1$ex)
p8 <- ggplot(an1)
p8 + geom_jitter(aes(x = cnt, y = Freq)) +
        geom_line(aes(x = cnt, y = ex)) +
        xlab("Nation Count") +
        ylab("Authors per Nation") +
        theme_bw()

# -----

# internationality of co-authors?
int_auth_1 <- decisions %>%
        filter(authorCount > 1) %>%
        select(firstAuthGeog, seniorAuthGeog, submitYear) %>%
        table() %>% print()

int_auth_1 <- data.frame(int_auth_1)

int_auth_1 %>%
        filter(firstAuthGeog == seniorAuthGeog) %>%
        ggplot(aes(x = submitYear, y = Freq)) +
        geom_boxplot() + theme_classic() +
        xlab("Submission Year") +
        ylab("Frequency") +
        ggtitle("First Author and Senior/Last Author from Same Region\n
                For all papers submitted")

int_auth_1 %>%
        filter(firstAuthGeog != seniorAuthGeog) %>%
        ggplot(aes(x = submitYear, y = Freq)) +
        geom_boxplot() + theme_classic() +
        xlab("Submission Year") +
        ylab("Frequency") +
        ggtitle("First Author and Senior/Last Author from Separate Regions\n
                For all papers submitted")

int_auth_2 <- decisions %>%
        filter(authorCount > 1 & paperRejected == "No") %>%
        select(firstAuthGeog, seniorAuthGeog, submitYear) %>%
        table() %>% print()

int_auth_2 <- data.frame(int_auth_2)

int_auth_2 %>%
        filter(firstAuthGeog == seniorAuthGeog) %>%
        ggplot(aes(x = submitYear, y = Freq)) +
        geom_boxplot() + theme_classic() +
        xlab("Submission Year") +
        ylab("Frequency") +
        ggtitle("First Author and Senior/Last Author from Same Region\n
                For all papers accepted")

int_auth_2 %>%
        filter(firstAuthGeog != seniorAuthGeog) %>%
        ggplot(aes(x = submitYear, y = Freq)) +
        geom_boxplot() + theme_classic() +
        xlab("Submission Year") +
        ylab("Frequency") +
        ggtitle("First Author and Senior/Last Author from Separate Regions\n
                For all papers accepted")

rm(int_auth_1, int_auth_2)

# What are the trends in submissions by geographic region of submitting author?
# Identify submitting years to the data set

yearGeo           <- data.frame(table(decisions$corrAuthGeog,
                                      decisions$submitYear))
colnames(yearGeo) <- c("Region", "Year", "Count")
yearGeo$Year      <- as.numeric(as.character(yearGeo$Year))

p <- ggplot(yearGeo, aes(x = Year,
                         y = Count,
                         linetype = Region))

p + geom_line() + theme_classic() +
    geom_dl(aes(label = Region), method = "smart.grid") +
        xlab("Submission Year") +
        ylab("Count of Corresponding Authors") +
        scale_linetype_discrete(guide = FALSE)

# how international are collaborators?

firstSenior <- data.frame(table(decisions$firstAuthGeog,
                                decisions$seniorAuthGeog))

ggplot(firstSenior, aes(x = Var1,
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

dec %>%
        filter(submitAuthFirstAuth == "Yes") %>%
        filter(authorCount > 1) %>%
        select(firstAuthGeog, seniorAuthGeog) %>%
        table() %>%
        data.frame() %>%
        ggplot(aes(x = firstAuthGeog,
                   y = seniorAuthGeog)) +
        geom_tile(aes(fill = Freq),
                  colour = "white") +
        theme_classic() +
        scale_fill_gradient(low = "white", high = "black") +
        xlab("Geographic Region of First Author") +
        ylab("Geographic Region of Senior/Last Author")
