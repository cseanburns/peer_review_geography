source("libraries.R")

dec0$paperRejected      <- relevel(dec0$paperRejected, ref = "Yes")
dec0$firstAuthGeog      <- relevel(dec0$firstAuthGeog, ref = "North America")

contrasts(dec0$paperRejected)
contrasts(dec0$firstAuthGeog)

# ---

fit.1 <- glm(paperRejected ~ firstAuthGeog:meanReviewerDaysRespond, data = dec0, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:8) # test the ranking
1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fitChi     <- fit.1$null.deviance - fit.1$deviance
chiDF      <- fit.1$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.1, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

# ---

fit.2 <- glm(paperRejected ~ firstAuthGeog:meanReviewerDaysReview, data = dec0, family = "binomial")
summary(fit.2)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)

wald.test(b = coef(fit.2), Sigma = vcov(fit.2), Terms = 2:8) # test the ranking
1 - pchisq(fit.2$deviance, fit.2$df.residual) # test the model

fitChi     <- fit.2$null.deviance - fit.2$deviance
chiDF      <- fit.2$df.null - fit.2$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.2, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

# ---

fit.3 <- glm(paperRejected ~ firstAuthGeog:propReviewersResponding, data = dec0, family = "binomial")
summary(fit.3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)

wald.test(b = coef(fit.3), Sigma = vcov(fit.3), Terms = 2:7) # test the ranking
1 - pchisq(fit.3$deviance, fit.3$df.residual) # test the model

fitChi     <- fit.3$null.deviance - fit.3$deviance
chiDF      <- fit.3$df.null - fit.3$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.3, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

# ---

fit.4 <- glm(paperRejected ~ firstAuthGeog:propReviewersAgreeing, data = dec0, family = "binomial")
summary(fit.4)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)

wald.test(b = coef(fit.4), Sigma = vcov(fit.4), Terms = 2:7) # test the ranking
1 - pchisq(fit.4$deviance, fit.4$df.residual) # test the model

fitChi     <- fit.4$null.deviance - fit.4$deviance
chiDF      <- fit.4$df.null - fit.4$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.4, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

## ---

fit.5 <- glm(paperRejected ~ firstAuthGeog + meanReviewerDaysReview +
                     firstAuthGeog:meanReviewerDaysReview,
             data = dec0, family = "binomial")
summary(fit.5)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)

wald.test(b = coef(fit.5), Sigma = vcov(fit.5), Terms = 2:8) # test the ranking
1 - pchisq(fit.5$deviance, fit.5$df.residual) # test the model

fitChi     <- fit.5$null.deviance - fit.5$deviance
chiDF      <- fit.5$df.null - fit.5$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.5, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

# ---

ggplot(dec0, aes(x = firstAuthGeog, y = meanReviewerDaysReview)) +
        geom_boxplot() + facet_grid(. ~ paperRejected) + theme_bw() +
        xlab("Geographical Region of First Author") +
        ylab("Days to Review") +
        ggtitle("Paper Rejected") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

fit.0 <- glm(paperRejected ~ 1, data = dec0, family = "binomial")

anova(fit.0, fit.1, test = "Chisq")
anova(fit.0, fit.2, test = "Chisq")
anova(fit.0, fit.3, test = "Chisq")
anova(fit.0, fit.4, test = "Chisq")
anova(fit.0, fit.5, test = "Chisq")

rm(fit.0, fit.1, fit.2, fit.3,fit.4, fit.5, fitChi, chiDF, chisq.prob, prob, g)
