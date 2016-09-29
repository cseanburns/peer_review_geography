# Time to decision

source('libraries.R')

dec0 <- dec

dec0$paper_rejected   <- relevel(dec0$paper_rejected, ref = "Yes")
dec0$first_auth_geog  <- relevel(dec0$first_auth_geog, ref = "North America")

contrasts(dec0$paper_rejected)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(paper_rejected ~ first_auth_geog + time_to_decision,
             data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7) # test the ranking
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 9:14) # test the ranking
1 - pchisq(fit.0$deviance, fit.0$df.residual) # test the model

fit.chi     <- fit.0$null.deviance - fit.0$deviance
chi.df      <- fit.0$df.null - fit.0$df.residual
chi.prob    <- 1 - pchisq(fit.chi, fit.df)
fit.chi ; chi.df ; chisq.prob

dec0$prob <- predict(fit.0, type = c("response"))

g <- roc(paper_rejected ~ prob, data = dec0) ; g
plot(g)

ggplot(dec0, aes(x = paper_rejected, y = time_to_decision)) +
  geom_boxplot() + theme_bw() +
  xlab("Paper Rejected") +
  ylab("Time to Decision, in Days")

ggplot(dec0, aes(x = paper_rejected, y = time_to_decision)) +
  geom_boxplot() + theme_bw() +
  facet_grid(. ~ handling_editor_geog) +
  xlab("Paper Rejected") +
  ylab("Time to Decision, in Days") +
  ggtitle("Handling Editor Region")

ggplot(dec0, aes(x = paper_rejected, y = time_to_decision)) +
  geom_boxplot() + theme_bw() +
  facet_grid(. ~ first_auth_geog) +
  xlab("Paper Rejected") +
  ylab("Time to Decision, in Days") +
  ggtitle("First Author Region")

rm(fit.0, fit.chi, chi.df, chisq.prob, g)
