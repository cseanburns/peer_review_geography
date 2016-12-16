source("libraries.R")

dec0 <- dec

dec0  <- select(dec0, ms_id, first_auth_geog, sent_for_review, paper_rejected)
lang <- filter(author_decisions, author_order == 1) %>% select(ms_id, language)

dec_lang <- inner_join(dec0, lang, by = "ms_id")
rm(lang)

dec_lang$english <- ifelse(dec_lang$language == "English", TRUE, FALSE)
dec_lang$english <- factor(dec_lang$english, ordered = FALSE)

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

dec_lang$prob <- predict(fit.0, type = c("response"))
g <- roc(paper_rejected ~ prob, data = dec_lang) ; g
plot(g)

###

dec_lang$sent_for_review <- relevel(dec_lang$sent_for_review, ref = "No")
dec_lang$first_auth_geog <- relevel(dec_lang$first_auth_geog, ref = "Europe")
dec_lang$english <- relevel(dec_lang$english, ref = "TRUE")
contrasts(dec_lang$sent_for_review)
contrasts(dec_lang$first_auth_geog)
contrasts(dec_lang$english)

fit.2 <- glm(sent_for_review ~ first_auth_geog + english,
             family = "binomial", data = dec_lang)
summary(fit.2)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)

##

dec_lang_sent$paper_rejected <- relevel(dec_lang_sent$paper_rejected,
                                        ref = "Yes")
dec_lang_sent$first_auth_geog <- relevel(dec_lang_sent$first_auth_geog,
                                         ref = "Europe")
dec_lang_sent$english <- relevel(dec_lang_sent$english, ref = "TRUE")

contrasts(dec_lang_sent$paper_rejected)
contrasts(dec_lang_sent$first_auth_geog)
contrasts(dec_lang_sent$english)

fit.3 <- glm(paper_rejected ~ first_auth_geog + english,
             family = "binomial", data = dec_lang_sent)
summary(fit.3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
