source("libraries.R")

# create working copy
dec0 <- dec

# Sent for Review ; looking at desk rejects in this section
dec0                 <- select(dec0, sent_for_review, first_auth_geog)
dec0                 <- dec0[complete.cases(dec0),]
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")

summary(fit.0)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Investigate ROC curve ; be sure to substitute "sent_for_review" out if using
# in other functions
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(sent_for_review ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.0, dec0)

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

rm(dec0, fit.0, fit.chi, chi.df, chisq.prob, roc_curve)
