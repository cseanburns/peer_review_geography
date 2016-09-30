source("libraries.R")

# Sent for Review 

dec0 <- dec

dec0                 <- select(dec0, sent_for_review, first_auth_geog)
dec0                 <- filter(dec0, !is.na(first_auth_geog))
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "United Kingdom")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")

summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

author_sent <- table(dec0$sent_for_review, dec0$first_auth_geog)
author_sent
round(author_sent[2,]/author_sent[1,],3)

ggplot(dec0, aes(first_auth_geog, fill = sent_for_review)) +
  geom_bar() + theme_bw() + scale_fill_grey(name = "Sent for Review") +
  xlab("Geographical Region of First Author") +
  ylab("Count") +
  theme(legend.position = c(.9,.8))

rm(dec0, fit.0, author_sent)
