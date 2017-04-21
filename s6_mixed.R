source("libraries.R")

## mixed = FALSE, means that authors are located in same country
## mixed = TRUE, means that authors are located in different countries

### test if multinational authorships are penalized for sent for review###

# Exclude single authors
dec0 <- dec %>% filter(author_count > 1)

# test differences on sent for review, on mixed authorship, and then on sent
# for review + mixed authorship
table(dec0$sent_for_review)
round(table(dec0$sent_for_review) / sum(table(dec0$sent_for_review)), 3)
chisq.test(table(dec0$sent_for_review), p = c(0.50, 0.50))

table(dec0$mixed)
round(table(dec0$mixed) / sum(table(dec0$mixed)), 2)
chisq.test(table(dec0$mixed))

table(dec0$sent_for_review, dec0$mixed)
round(table(dec0$sent_for_review, dec0$mixed) /
              sum(table(dec0$sent_for_review, dec0$mixed)), 2)
chisq.test(table(dec0$sent_for_review, dec0$mixed))

plot(dec0$sent_for_review ~ dec0$mixed)

rm(dec0)

### test if multinational authorships are penalized for paper rejected ###

dec1 <- dec %>% filter(sent_for_review == "Yes" & author_count > 1)

# test differences on paper rejection, on mixed authorship, and then on paper
# rejection + mixed authorship
table(dec1$paper_rejected)
chisq.test(table(dec1$paper_rejected))

table(dec1$mixed)
chisq.test(table(dec1$mixed))

table(dec1$paper_rejected, dec1$mixed)
chisq.test(table(dec1$paper_rejected, dec1$mixed))

plot(dec1$paper_rejected ~ dec1$mixed)

rm(dec1)
