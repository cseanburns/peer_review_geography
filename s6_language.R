source("libraries.R")

dec0 <- dec

dec0  <- select(dec0, ms_id, first_auth_geog, sent_for_review, paper_rejected)
lang <- filter(author_decisions, author_order == 1) %>% select(ms_id, language)

dec_lang <- inner_join(dec0, lang, by = "ms_id")
rm(lang)

dec_lang$english <- ifelse(dec_lang$language == "English", TRUE, FALSE)
dec_lang$english <- factor(dec_lang$english, ordered = FALSE)

dec_lang$first_auth_geog <- relevel(dec_lang$first_auth_geog, ref = "Europe")
dec_lang$english <- relevel(dec_lang$english, ref = "FALSE")
dec_lang$sent_for_review <- relevel(dec_lang$sent_for_review, ref = "No")
dec_lang$paper_rejected <- relevel(dec_lang$paper_rejected, ref = "Yes")

contrasts(dec_lang$first_auth_geog)
contrasts(dec_lang$english)
contrasts(dec_lang$sent_for_review)
contrasts(dec_lang$paper_rejected)

fit.0 <- glm(sent_for_review ~ english, family = "binomial", data = dec_lang)
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# The reduction in the deviance; results in the chi square statistic 
fit.chi    <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df     <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statistic. If (e.g.) less than 
# 0.05, we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob <- 1 - pchisq(fit.chi, chi.df) 
# display results
fit.chi ; chi.df ; chisq.prob
rm(fit.0, fit.chi, chi.df, chisq.prob)

# look at proportions
sum.sent.english <- sum(table(dec_lang$english, dec_lang$sent_for_review))
round(table(dec_lang$english, dec_lang$sent_for_review) / sum.sent.english, 3) * 100
chisq.test(table(dec_lang$english, dec_lang$sent_for_review))
rm(sum.sent.english)

# look at final decision set only
dec_lang_sent <- filter(dec_lang, sent_for_review == "Yes")
table(dec_lang_sent$english)

fit.1 <- glm(paper_rejected ~ english, family = "binomial", data = dec_lang_sent)
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

# The reduction in the deviance; results in the chi square statistic 
fit.chi    <- fit.1$null.deviance - fit.1$deviance
# The degrees of freedom for the chi square statistic
chi.df     <- fit.1$df.null - fit.1$df.residual
# The probability associated with the chi-square statistic. If (e.g.) less than 
# 0.05, we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob <- 1 - pchisq(fit.chi, chi.df) 
# display results
fit.chi ; chi.df ; chisq.prob
rm(fit.1, fit.chi, chi.df, chisq.prob)

# look at proportions
sum.final.english <- sum(table(dec_lang_sent$english,
                               dec_lang_sent$paper_rejected))
round(table(dec_lang_sent$english, dec_lang_sent$paper_rejected) /
              sum.final.english, 3) * 100
chisq.test(table(dec_lang_sent$english, dec_lang_sent$paper_rejected))
rm(sum.final.english)

rm(dec_lang, dec_lang_sent, dec0)
