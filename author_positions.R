# Find the percentage shared locations between first and second authors

# this captures the first two author positions and their location
first_second <- select(author_decisions, ms_id, author_country, author_order)
first_second <- arrange(first_second, ms_id, author_order)
first_second <- filter(first_second, author_order < 3)

# Identify which manuscripts only have 1 author
ms_single      <- as.data.frame(table(first_second$ms_id))
ms_single$Var1 <- as.character(ms_single$Var1)
ms_single$Var1 <- as.integer(ms_single$Var1)

# Save manuscripts that have at least two authors
ms_single        <- filter(ms_single, Freq == 2)
ms_single$Freq   <- NULL
names(ms_single) <- "ms_id"

first_second <- inner_join(first_second, ms_single, by = "ms_id")
first_second <- arrange(first_second, ms_id, author_order)
first_second <- select(first_second, author_order, author_country)
# uses author position as variables
first_second <- first_second %>% unstack(author_country ~ author_order)
first_second_tbl <- first_second$X1 == first_second$X2
table(first_second_tbl)
# percentage of first and second authors at same national location
table(first_second_tbl)/sum(table(first_second_tbl))

rm(first_second, first_second_tbl, ms_single)

# Find the percentage shared locations between first and third authors

# this captures the first and third positions
first_third <- select(author_decisions, ms_id, author_country, author_order)
first_third <- arrange(first_third, ms_id, author_order)
first_third <- filter(first_third, author_order < 4)
first_third <- filter(first_third, author_order != 2)

# Identify which manuscripts only have 1 author
ms_single      <- as.data.frame(table(first_third$ms_id))
ms_single$Var1 <- as.character(ms_single$Var1)
ms_single$Var1 <- as.integer(ms_single$Var1)

# Save manuscripts that have at least two authors
ms_single        <- filter(ms_single, Freq == 2)
ms_single$Freq   <- NULL
names(ms_single) <- "ms_id"

first_third <- inner_join(first_third, ms_single, by = "ms_id")
first_third <- arrange(first_third, ms_id, author_order)
# there's a problem with the data:
# author is repeated likely due to multiple addresses
# upon inspection, the problem ms_id is 201400096
# will remove it from the analysis
x <- table(first_third$ms_id, first_third$author_order)
x <- as.data.frame(x)
table(x$Freq)
x$Var1[x$Freq==0]
first_third[(first_third$ms_id=="201400096"),]
first_third <- first_third[!(first_third$ms_id=="201400096"),]
first_third[(first_third$ms_id=="201400096"),]

first_third <- select(first_third, author_order, author_country)
# uses author position as variables
first_third <- first_third %>% unstack(author_country ~ author_order)
first_third_tbl <- first_third$X1 == first_third$X3
table(first_third_tbl)
# percentage of first and second authors at same national location
table(first_third_tbl)/sum(table(first_third_tbl))

rm(first_third, ms_single, x, first_third_tbl)
