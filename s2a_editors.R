source("libraries.R")

# demographics of editors 
# countries, regions, languages

fa1 <- table(dec0$firstAuthGeog, dec0$handlingEditorGeog)
assocstats(fa1) ; rm(fa1)

# --- 

dec0$paperRejected      <- relevel(dec0$paperRejected, ref = "Yes")
dec0$handlingEditorGeog <- relevel(dec0$handlingEditorGeog, ref = "North America")
dec0$firstAuthGeog      <- relevel(dec0$firstAuthGeog, ref = "Europe")

contrasts(dec0$paperRejected)
contrasts(dec0$handlingEditorGeog)
contrasts(dec0$firstAuthGeog)

fit.1 <- glm(paperRejected ~ handlingEditorGeog, data = dec0, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

ggplot(dec0, aes(x = handlingEditorGeog, fill = paperRejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        xlab("Geographical Region of Handling Editor") +
        ylab("Count") +
        theme(legend.position = c(.9,.8))

wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:7) # test the ranking
1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fit.0      <- glm(paperRejected ~ 1, data = dec0, family = "binomial")
fitChi     <- fit.1$null.deviance - fit.1$deviance
chiDF      <- fit.1$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.1, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

rm(fit.0, fit.1, fitChi, chiDF, chisq.prob, prob, g)