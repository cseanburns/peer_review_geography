source("libraries.R")

# Import and name data
reviewer_geographies <- read_csv("reviewer_geographies.csv")
names(reviewer_geographies) <- c("ms_id", "reviewer_score",
                                 "reviewer_country", "reviewer_region")

# Fix names of reviewer countries
reviewer_geographies$reviewer_country <- gsub("NewZealand",
                                              "New Zealand",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("SriLanka",
                                              "Sri Lanka",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("BruneiDarussalam",
                                              "Brunei Darussalam",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("CostaRica",
                                              "Costa Rica",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("PuertoRico",
                                              "Puerto Rico",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("SvalbardandJanMayen",
                                              "Svalbard and Jan Mayen",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("CzechRepublic",
                                              "Czech Republic",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("UnitedKingdom",
                                              "United Kingdom",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("RussianFederation",
                                              "Russian Federation",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("UnitedStates",
                                              "United States",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("Korea,Republicof",
                                              "Korea",
                                              reviewer_geographies$reviewer_country)
 
reviewer_geographies$reviewer_country <- gsub("SaudiArabia",
                                              "Saudi Arabia",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("SouthAfrica",
                                              "South Africa",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("FrenchGuiana",
                                              "French Guiana",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("HongKong",
                                              "Hong Kong",
                                              reviewer_geographies$reviewer_country)

reviewer_geographies$reviewer_country <- gsub("NetherlandsAntilles",
                                              "Netherlands",
                                              reviewer_geographies$reviewer_country)

# Fix names of reviewer regions

reviewer_geographies$reviewer_region <- gsub("LatinAmerica",
                                              "Latin America",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("NorthernEurope",
                                              "Europe",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("UnitedKingdom",
                                              "United Kingdom",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("UnitedKingdom",
                                              "United Kingdom",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("OtherAsia",
                                              "Asia",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("AustandNZ",
                                             "Oceania",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("SouthernAsia",
                                             "Asia",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("EasternAsia",
                                             "Asia",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("SouthernEurope",
                                             "Europe",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("EasternEurope",
                                             "Europe",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("WesternEurope",
                                             "Europe",
                                              reviewer_geographies$reviewer_region)

reviewer_geographies$reviewer_region <- gsub("NorthAmerica",
                                             "North America",
                                              reviewer_geographies$reviewer_region)

dec_rev <- select(dec, ms_id, first_auth_geog, sent_for_review, paper_rejected)

dec_rev_combined <- inner_join(dec_rev, reviewer_geographies, by = "ms_id")

####

ma <- table(author_decisions$ms_id, author_decisions$author_country)
ma <- as.data.frame(ma)
ma <- filter(ma, Freq > 0)
ma <- ma[order(ma$Var1),]
