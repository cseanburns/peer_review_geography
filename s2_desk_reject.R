source("libraries.R")

# create working copy
dec0 <- dec

# Sent for Review ; looking at desk rejects in this section
dec0                 <- select(dec0, sent_for_review, first_auth_geog, english, HDI)
dec0$HDI_10          <- dec0$HDI * 10
dec0                 <- dec0[complete.cases(dec0),]
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")
dec0$english         <- factor(dec0$english)
dec0$english         <- relevel(dec0$english, ref = "TRUE")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)
contrasts(dec0$english)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")

fit.1 <- glm(sent_for_review ~ first_auth_geog + english,
             data = dec0, family = "binomial")

fit.2 <- glm(sent_for_review ~ first_auth_geog + english + HDI_10,
             data = dec0, family = "binomial")

fit.3 <- glm(sent_for_review ~ english + HDI_10,
             data = dec0, family = "binomial")

fit.4 <- glm(sent_for_review ~ HDI_10,
             data = dec0, family = "binomial")

fit.5 <- glm(sent_for_review ~ english,
             data = dec0, family = "binomial")

summary(fit.0)
summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 7)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# 1. The reduction in the deviance; results in the chi square statistic
# 2. The degrees of freedom for the chi square statistic
# 3. The probability associated with the chi-square statistic. If (e.g.) less
# than 0.05, we can reject the null hypothesis that the model is not better
# than chance at predicting the outcome
# 4. Display the results
fit.chi <- fit.0$null.deviance - fit.0$deviance
chi.df  <- fit.0$df.null - fit.0$df.residual
chisq.prob <- 1 - pchisq(fit.chi, chi.df)
fit.chi; chi.df; chisq.prob

reorder_size <- function(x) {
        factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(dec0, aes(x = reorder_size(first_auth_geog), fill = sent_for_review)) +
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent) + theme_bw() +
        # geom_bar() + theme_bw() +
        scale_fill_grey(name = "Sent for Review") +
        labs(x = "Geographical Region of First Author",
        y = "Count") +
        theme(axis.text.y = element_text(size = 12,
                                    colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                    colour = "black")) +
        theme(legend.position = c(.8,.8))

rm(dec0, fit.0, fit.1, fit.2, fit.3, fit.4, fit.5,
   fit.chi, chi.df, chisq.prob)
