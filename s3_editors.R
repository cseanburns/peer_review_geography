source("libraries.R")

# Editors by geographic regions, comparing to paper rejections 
# Focus on sent for review data and not all data

# Get ratio of handling editors by region
sort(table(dec_sent$handling_editor_geog), decreasing = TRUE)
sort(round(table(dec_sent$handling_editor_geog) /
                   sum(table(dec_sent$handling_editor_geog)),3),
     decreasing = TRUE)

# Get ratio of author geographies to handling editor geographies
table(dec_sent$first_auth_geog) ; table(dec_sent$handling_editor_geog)
round(table(dec_sent$first_auth_geog) / table(dec_sent$handling_editor_geog),2)
round(table(dec_sent$handling_editor_geog) / table(dec_sent$first_auth_geog),2)
author.editor.tbl <- table(dec_sent$first_auth_geog, dec_sent$handling_editor_geog)
assocstats(author.editor.tbl)
rm(author.editor.tbl)

dec_sent$paper_rejected       <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$handling_editor_geog <- relevel(dec_sent$handling_editor_geog,
                                         ref = "Europe")

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$handling_editor_geog)

fit.0 <- glm(paper_rejected ~ handling_editor_geog, data = dec_sent, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statisitc
# If (e.g.) less than 0.05, then we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# Display the results
fit.chi ; chi.df ; chisq.prob

dec_tmp <- select(dec_sent, handling_editor_geog, paper_rejected)
table(dec_tmp)

reorder_size <- function(x) {
        factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(dec_tmp, aes(x = reorder_size(handling_editor_geog), fill = paper_rejected)) +
        geom_bar(stat = "count") + theme_bw() +
        scale_fill_grey(name = "Revision Invited / Declined") +
        labs(x = "Geographical Region of Handling Editor",
        y = "Count") +
        theme(axis.text.y = element_text(size = 12,
                                    colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                    colour = "black")) +
        theme(legend.position = c(.8,.8))

rm(fit.0, fit.chi, chi.df, chisq.prob, dec_tmp)
