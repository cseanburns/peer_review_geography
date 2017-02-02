source("libraries.R")

dec_sent <- filter(dec, sent_for_review == "Yes")
dec_sent <- select(dec_sent, ms_id, paper_rejected, first_auth_geog)

# Authors by regions and comparing to paper rejections
# Focus on data set filtered by sent for review

dec_sent$paper_rejected       <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$first_auth_geog      <- relevel(dec_sent$first_auth_geog, ref = "Europe")

# remove rows with NAs
dec_sent <- dec_sent[complete.cases(dec_sent),]

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$first_auth_geog)

fit.0 <- glm(paper_rejected ~ first_auth_geog, data = dec_sent, family = "binomial")
summary(fit.0, digits = 3)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statistic
# If (e.g.) less than 0.05, we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# display the results
fit.chi ; chi.df ; chisq.prob

reorder_size <- function(x) {
        factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(dec_sent, aes(x = reorder_size(first_auth_geog), fill = paper_rejected)) +
        geom_bar() + theme_bw() + 
        scale_fill_grey(name = "Likely Not Published") +
        labs(x = "Geographical Region of First Author",
             y = "Count") +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(legend.position = c(.8,.8))

rm(fit.0, fit.chi, chi.df, chisq.prob, dec_sent)

### test if multinational authorships are penalized ###
### identifies all manuscripts where authors are from the same nation and
### authors are from different nations
### need to remove single authorships ###
### mixed.combined$mixed = FALSE, means that authors are located in same country
### mixed.combined$mixed = TRUE, means that authors are located in different countries

# create new dataframe with manuscript IDs, paper rejection status, and author
# country.

dec_handling_editors <- dec %>% filter(sent_for_review == "Yes") %>%
        select(ms_id, paper_rejected)
dec_author_country   <- author_decisions %>% select(ms_id, author_country)
dec_mixed_countries  <- inner_join(dec_handling_editors,
                                   dec_author_country,
                                   by = "ms_id")
dec_mixed_countries  <- distinct(dec_mixed_countries)
dec_mixed_countries$mixed  <- duplicated(dec_mixed_countries$ms_id)

rm(dec_handling_editors, dec_author_country)

mixed.true  <- dec_mixed_countries %>% filter(mixed == TRUE)
mixed.false <- dec_mixed_countries %>% filter(mixed == FALSE)

rm(dec_mixed_countries)

mixed.true$author_country  <- NULL
mixed.false$author_country <- NULL

mixed.false.logic <- mixed.false[!(mixed.false$ms_id %in% mixed.true$ms_id),]
mixed.combined    <- rbind(mixed.true, mixed.false.logic)
mixed.combined    <- unique(mixed.combined)

# identify single authorships
singles.df <- select(author_decisions, ms_id)
singles.df <- data.frame(table(singles.df))
singles.df <- singles.df %>% filter(Freq == 1)
singles.df$singles.df <- as.integer(as.character(singles.df$singles.df))
singles <- singles.df$singles.df

# remove singler authorships from data for analysis

mixed.combined <- mixed.combined[!(mixed.combined$ms_id %in% singles),]

rm(mixed.false, mixed.false.logic, mixed.true, singles.df)

# run chi square test on mixed authorship and then on rejection status
chisq.test(table(mixed.combined$mixed))
chisq.test(table(mixed.combined$paper_rejected, mixed.combined$mixed))

# regression
table(mixed.combined$mixed, mixed.combined$paper_rejected)
mixed.combined$mixed <- as.factor(mixed.combined$mixed)
mixed.combined$paper_rejected <- relevel(mixed.combined$paper_rejected, ref = "Yes")
mixed.combined$mixed          <- relevel(mixed.combined$mixed, ref = "FALSE")
contrasts(mixed.combined$paper_rejected)
contrasts(mixed.combined$mixed)

fit.0 <- glm(paper_rejected ~ mixed,
             data = mixed.combined, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

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

rm(mixed.combined, chi.df, chisq.prob, fit.0, fit.chi, singles)
