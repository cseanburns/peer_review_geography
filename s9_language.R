source("libraries.R")

dec0 <- dec

dec0  <- select(dec0, ms_id, first_auth_geog, paper_rejected)
lang0 <- select(author_decisions, ms_id, author_order, language) %>% filter(author_order == 1)

#### variable names need to be fixed ####

names(lang0) <- c("ms_id", "first_auth_geog", "paper_rejected")

dec_language1 <- inner_join(auth_language, dec_language, by = "msid")
dec_language2 <- select(dec_language1, msid, firstAuthGeog, paperRejected, english)

dec_language2$english <- factor(dec_language2$english, ordered = FALSE)

fit.0 <- glm(paper_rejected ~ english, family = "binomial", data = dec_language2)
       
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

ggplot(dec_language2, aes(x = english, fill = paperRejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        xlab("English (Official or Common)") +
        ylab("Count") +
        theme(legend.position = c(.9,.8))

1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fit.0      <- glm(paper_rejected ~ 1, data = dec_language2, family = "binomial")
fit.chi    <- fit.0$null.deviance - fit.0$deviance
chi.df     <- fit.0$df.null - fit.0$df.residual
chisq.prob <- 1 - pchisq(fit.chi, chi.df) 
fit.chi ; chi.df ; chisq.prob

dec_lang_2$prob <- predict(fit.0, type = c("response"))
g <- roc(paper_rejected ~ prob, data = dec0) ; g
plot(g)

rm(fit.0, fit.chi, chi.df, chisq.prob, g)
