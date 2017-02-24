source("libraries.R")

# Create working copy
dec0 <- dec

## Analysis Section

# The average number of authors per paper is four, but the mode is 3.
summary(dec0$author_count) ; table(dec0$author_count)

# most first authors are corresponding authors
table(dec0$corr_auth_first_auth) /
        sum(table(dec0$corr_auth_first_auth))
# most first authors are submitting authors
table(dec0$submit_auth_first_auth) /
        sum(table(dec0$submit_auth_first_auth))

# Table 1 in manuscript; ratios are computed row-wise
tbl.1 <- round(table(dec0$first_auth_geog, dec0$senior_auth_geog) /
                       rowSums(table(dec0$first_auth_geog, dec0$senior_auth_geog)), 3)
tbl.1
assocstats(tbl.1)
rowSums(tbl.1)
rm(tbl.1)

# Geographical difference between first and corresponding authors
tbl.2a <- round(table(dec0$corr_auth_first_auth) /
                        sum(table(dec0$corr_auth_first_auth)), 3)
tbl.2a
tbl.2b <- round(table(dec0$first_auth_geog, dec0$corr_auth_geog) /
                        rowSums(table(dec0$first_auth_geog, dec0$corr_auth_geog)), 3)
tbl.2b
assocstats(tbl.2b)
rowSums(tbl.2b)
rm(tbl.2a, tbl.2b)

# Geographical difference between first and corresponding authors
tbl.3a <- round(table(dec0$submit_auth_first_auth) /
                        sum(table(dec0$submit_auth_first_auth)), 3)
tbl.3a
tbl.3b <- round(table(dec0$first_auth_geog, dec0$submit_auth_geog) /
                        rowSums(table(dec0$first_auth_geog, dec0$submit_auth_geog)), 3)
tbl.3b
assocstats(tbl.3b)
rowSums(tbl.3b)
rm(tbl.3a, tbl.3b)

# The number of papers that include authors from more than one country or one country
round(table(dec0$mixed) / sum(table(dec0$mixed)), 3)