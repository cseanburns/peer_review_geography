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

#### Examinging relationship between HDI and Mean Review Score ####

dec0 <- dec

hdi_test.1 <- author_decisions %>%
        filter(author_order == 1) %>%
        select(ms_id, author_country, HDI)
hdi_test.2 <- dec0 %>% select(ms_id, mean_review_score)

hdi_test.3 <- inner_join(hdi_test.1, hdi_test.2, by = "ms_id")

rm(hdi_test.1, hdi_test.2)

# look at all data

plot(hdi_test.3$mean_review_score ~ hdi_test.3$HDI)
cor.test(hdi_test.3$mean_review_score, hdi_test.3$HDI,
         method = "spearman", conf.level = 0.95, exact = FALSE)

# parse out data into upper category and lower category

hdi_lower <- hdi_test.3 %>% filter(HDI <= 0.85)
plot(hdi_lower$mean_review_score ~ hdi_lower$HDI)
cor.test(hdi_lower$HDI, hdi_lower$mean_review_score, method = "spearman",
         conf.level = 0.95, exact = FALSE)

hdi_upper <- hdi_test.3 %>% filter(HDI > 0.85)
plot(hdi_upper$mean_review_score ~ hdi_upper$HDI)
cor.test(hdi_upper$HDI, hdi_upper$mean_review_score, method = "spearman",
         conf.level = 0.95, exact = FALSE)

# just the last two columns (score & HDI)
hdi_test.4 <- hdi_test.3[,3:4]
boot_hdi <- function(hdi_test.4,i) {
        cor(hdi_test.4$HDI[i], hdi_test.4$mean_review_score[i],
            method = "spearman")
}

library(boot)
boot_hdi_spearman <- boot(hdi_test.4, boot_hdi, 10000, parallel = "multicore")
boot.ci(boot_hdi_spearman)