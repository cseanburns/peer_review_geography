source("libraries.R")

dec0 <- dec

dec0  <- select(dec0, ms_id, first_auth_geog, paper_rejected)
lang <- filter(author_decisions, author_order == 1) %>% select(ms_id, language)

dec_lang <- inner_join(dec0, lang, by = "ms_id")

dec_lang$english <- ifelse(dec_lang$language == "English", TRUE, FALSE)

dec_lang$english <- factor(dec_lang$english, ordered = FALSE)

fit.0 <- glm(paper_rejected ~ english, family = "binomial", data = dec_lang)
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

ggplot(dec_lang, aes(x = english, fill = paper_rejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        xlab("English (Official or Common)") +
        ylab("Count") +
        theme(legend.position = c(.9,.9))

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

dec_lang$prob <- predict(fit.0, type = c("response"))
g <- roc(paper_rejected ~ prob, data = dec_lang) ; g
plot(g)

rm(dec0, lang, dec_lang)
rm(fit.0, fit.chi, chi.df, chisq.prob, g)
