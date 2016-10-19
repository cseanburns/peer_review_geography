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

dec0$prob      <- predict(fit.0, type = c("response"))

g <- roc(paper_rejected ~ prob, data = dec0) ; g
plot(g)

rm(dec0, fit.0, f.h.tbl, fit.chi, chi.df, chisq.prob, g)

### in progress -- test if multinational authorships are penalized ###
### need to remove singleton authorships ###

dec0 <- dec

dec_handling_editors <- select(dec0, ms_id, paper_rejected)
dec_author_country   <- select(author_decisions, ms_id, author_country)
dec_mixed_countries  <- inner_join(dec_handling_editors,
                                   dec_author_country,
                                   by = "ms_id")
dec_mixed_countries  <- distinct(dec_mixed_countries)
dec_mixed_countries$mixed  <- duplicated(dec_mixed_countries$ms_id)

rm(dec0, dec_handling_editors, dec_author_country)

mixed.true  <- dec_mixed_countries %>% filter(mixed == TRUE)
mixed.false <- dec_mixed_countries %>% filter(mixed == FALSE)

rm(dec_mixed_countries)

mixed.true$author_country  <- NULL
mixed.false$author_country <- NULL

mixed.false.logic <- mixed.false[!(mixed.false$ms_id %in% mixed.true$ms_id),]
mixed.combined    <- rbind(mixed.true, mixed.false.logic)
mixed.combined    <- unique(mixed.combined)

rm(mixed.true, mixed.false, mixed.false.logic)
