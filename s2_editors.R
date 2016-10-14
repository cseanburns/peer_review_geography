# Editors by geographic regions and comparing to paper rejections

source("libraries.R")

dec0 <- dec

table(dec0$handling_editor_geog)
sort(round(table(dec0$handling_editor_geog) / sum(table(dec0$handling_editor_geog)),3), decreasing = TRUE)

table(dec0$first_auth_geog)
table(dec0$handling_editor_geog)
round(table(dec0$first_auth_geog) / table(dec0$handling_editor_geog),2)
round(table(dec0$handling_editor_geog) / table(dec0$first_auth_geog),2)

f.h.tbl <- table(dec0$first_auth_geog, dec0$handling_editor_geog)
assocstats(y) ; rm(f.h.tbl)

dec0$paper_rejected       <- relevel(dec0$paper_rejected, ref = "Yes")
dec0$handling_editor_geog <- relevel(dec0$handling_editor_geog, ref = "North America")

contrasts(dec0$paper_rejected)
contrasts(dec0$handling_editor_geog)

fit.0 <- glm(paper_rejected ~ handling_editor_geog, data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

ggplot(dec0, aes(x = handling_editor_geog, fill = paper_rejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        xlab("Geographical Region of Handling Editor") +
        ylab("Count") +
        theme(legend.position = c(.9,.8))

wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7) # test the ranking
1 - pchisq(fit.0$deviance, fit.0$df.residual) # test the model

fit.1       <- glm(paper_rejected ~ 1, data = dec0, family = "binomial")
fit.chi     <- fit.1$null.deviance - fit.1$deviance
chi.df      <- fit.1$df.null - fit.1$df.residual
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
fit.chi ; chi.df ; chisq.prob

dec0$prob      <- predict(fit.1, type = c("response"))

g <- roc(paper_rejected ~ prob, data = dec0) ; g
plot(g)

rm(dec0, fit.0, fit.1, fit.chi, chi.df, chisq.prob, g)
