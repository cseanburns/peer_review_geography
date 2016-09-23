source('libraries.R')

dec0$paperRejected      <- relevel(dec0$paperRejected, ref = "Yes")
dec0$firstAuthGeog      <- relevel(dec0$firstAuthGeog, ref = "North America")
dec0$handlingEditorGeog <- relevel(dec0$handlingEditorGeog, ref = "North America")

contrasts(dec0$paperRejected)
contrasts(dec0$firstAuthGeog)
contrasts(dec0$handlingEditorGeog)

fit.1 <- glm(paperRejected ~ firstAuthGeog:handlingEditorGeog, data = dec0, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:50) # test the ranking
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

# ---

fit.2 <- glm(paperRejected ~ firstAuthGeog + handlingEditorGeog +
                     firstAuthGeog:handlingEditorGeog,
             data = dec0, family = "binomial")
summary(fit.2)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)

wald.test(b = coef(fit.2), Sigma = vcov(fit.2), Terms = 2:50) # test the ranking
1 - pchisq(fit.2$deviance, fit.2$df.residual) # test the model

fit.0      <- glm(paperRejected ~ 1, data = dec0, family = "binomial")
fitChi     <- fit.2$null.deviance - fit.1$deviance
chiDF      <- fit.2$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.2, type = c("response"))
dec0$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)


rm(fit.0, fit.1, fitChi, chiDF, chisq.prob, prob, g)
