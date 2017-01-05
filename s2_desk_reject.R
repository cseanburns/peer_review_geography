source("libraries.R")

# create working copy
dec0 <- dec

# Sent for Review ; looking at desk rejects in this section
dec0                 <- select(dec0, sent_for_review, first_auth_geog)
# remember here that I only imputed first_auth_geog for dec_sent subset
dec0                 <- filter(dec0, !is.na(first_auth_geog))
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df  <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statistic; If (e.g.) less than
# 0.05, we can reject the null hypothesis that the model is not better than
# chance at predicting the outcome
chisq.prob <- 1 - pchisq(fit.chi, chi.df)
# Display the results
fit.chi; chi.df; chisq.prob

# Below not used but for exploratory analysis ; primarily for looking at 
# odds w/o a control/reference group
author_sent <- table(dec0$sent_for_review, dec0$first_auth_geog)
author_sent
round(author_sent[2,]/author_sent[1,],3)

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

rm(dec0, fit.0, fit.chi, chi.df, chisq.prob, author_sent)
