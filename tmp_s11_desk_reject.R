source("libraries.R")

# Interactions between authors and reviewers

## ---- Sent for Review ----

decisions2               <- select(decisions, sentForReview, firstAuthGeog)
summary(decisions2)
decisions2               <- filter(decisions2, !is.na(firstAuthGeog))
# decisions2$sentForReview <- relevel(decisions2$sentForReview, ref = "Yes")
decisions2$sfr2          <- relevel(decisions2$sentForReview, ref = "No")
decisions2$firstAuthGeog <- relevel(decisions2$firstAuthGeog, ref = "United Kingdom")
summary(decisions2)
# contrasts(decisions2$sentForReview)
contrasts(decisions2$sfr2)
contrasts(decisions2$firstAuthGeog)
fit.1                    <- glm(sfr2 ~ firstAuthGeog,
                                data = decisions2, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

fasa <- table(decisions2$sfr2, decisions2$firstAuthGeog)
fasa
round(fasa[2,]/fasa[1,],3)

ggplot(decisions2, aes(firstAuthGeog, fill = sfr2)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Sent for Review") +
        xlab("Geographical Region of First Author") +
        ylab("Count") +
        theme(legend.position = c(.9,.8))

decisions2b <- select(decisions, firstAuthGeog, seniorAuthGeog, sentForReview)
decisions2b <- filter(decisions2b, !is.na(seniorAuthGeog))
decisions2b <- filter(decisions2b, !is.na(firstAuthGeog))
summary(decisions2b)

decisions2b$sentForReview <- relevel(decisions2b$sentForReview, ref = "No")
decisions2b$firstAuthGeog <- relevel(decisions2b$firstAuthGeog, ref = "United Kingdom")
decisions2b$seniorAuthGeog <- relevel(decisions2b$seniorAuthGeog, ref = "United Kingdom")
contrasts(decisions2b$sentForReview)
contrasts(decisions2b$firstAuthGeog)
contrasts(decisions2b$seniorAuthGeog)

fit.1b                    <- glm(sentForReview ~ firstAuthGeog:seniorAuthGeog,
                                 data = decisions2b, family = "binomial")
summary(fit.1b)
round(exp(cbind(OR = coef(fit.1b), confint(fit.1b))), 3)

# 
# ggplot(decisions2, aes(seniorAuthGeog, fill = sentForReview)) +
#         geom_bar() + theme_bw() + scale_fill_grey(name = "Sent for Review") +
#         xlab("Geographical Region of Senior Author") +
#         ylab("Count") +
#         theme(legend.position = c(.9,.8))
