
mp        <- NULL
map_world <- borders("world", colour = "black", fill = "#019E73")
mp        <- ggplot() + map_world
mp + geom_point(data =country_id,
                aes(x = lon, y = lat)) + theme_bw()

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

mp + geom_point(data = auth_dec_5, aes(x = lon_fa, y = lat_fa, size = (Freq/sum(Freq)))) +
        labs(x = "Longitude",
             y = "Latitude") +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme_bw() +
        theme(legend.position = "none") 

#### ---- ####

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
