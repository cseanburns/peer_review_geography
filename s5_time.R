source('libraries.R')

dec0$paperRejected      <- relevel(dec0$paperRejected, ref = "Yes")
dec0$firstAuthGeog      <- relevel(dec0$firstAuthGeog, ref = "North America")

contrasts(dec0$paperRejected)
contrasts(dec0$firstAuthGeog)

# ---

fit.1 <- glm(paperRejected ~ firstAuthGeog + timeToDecision +
                     firstAuthGeog:timeToDecision,
             data = dec0, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:7) # test the ranking
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 9:14) # test the ranking
1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fitChi     <- fit.1$null.deviance - fit.1$deviance
chiDF      <- fit.1$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.1, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

ggplot(decisions3, aes(x = paperRejected, y = timeToDecision)) +
        geom_boxplot() + theme_bw() +
        xlab("Paper Rejected") +
        ylab("Time to Decision, in Days")

ggplot(decisions3, aes(x = paperRejected, y = timeToDecision)) +
        geom_boxplot() + theme_bw() +
        facet_grid(. ~ handlingEditorGeog) +
        xlab("Paper Rejected") +
        ylab("Time to Decision, in Days") +
        ggtitle("Handling Editor Region")

ggplot(decisions3, aes(x = paperRejected, y = timeToDecision)) +
        geom_boxplot() + theme_bw() +
        facet_grid(. ~ firstAuthGeog) +
        xlab("Paper Rejected") +
        ylab("Time to Decision, in Days") +
        ggtitle("First Author Region")

rm(fit.1, fitChi, chiDF, chisq.prob, prob, g)
