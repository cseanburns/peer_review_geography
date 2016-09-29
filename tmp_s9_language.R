source("libraries.R")

dec_language <- select(dec0,msID, firstAuthGeog, paperRejected)
auth_language <- select(author_decisions, msid, author_order, english) %>% filter(author_order == 1)

names(dec_language) <- c("msid", "firstAuthGeog", "paperRejected")

dec_language1 <- inner_join(auth_language, dec_language, by = "msid")
dec_language2 <- select(dec_language1, msid, firstAuthGeog, paperRejected, english)

dec_language2$english <- factor(dec_language2$english, ordered = FALSE)

fit.1 <- glm(paperRejected ~ english, family = "binomial", data = dec_language2)
       
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

# ggplot(dec_language2, aes(x = paperRejected, fill = english)) +
#         geom_bar() + theme_bw() + scale_fill_grey(name = "English") +
#         xlab("Paper Rejected") +
#         ylab("Count") +
#         theme(legend.position = c(.9,.8))

ggplot(dec_language2, aes(x = english, fill = paperRejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        xlab("English (Official or Common)") +
        ylab("Count") +
        theme(legend.position = c(.9,.8))

1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fit.0      <- glm(paperRejected ~ 1, data = dec_language2, family = "binomial")
fitChi     <- fit.1$null.deviance - fit.1$deviance
chiDF      <- fit.1$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fitChi, chiDF) 
fitChi ; chiDF ; chisq.prob

prob <- predict(fit.1, type = c("response"))
dec_language2$prob <- prob
g <- roc(paperRejected ~ prob, data = dec0) ; g
plot(g)

rm(fit.0, fit.1, fitChi, chiDF, chisq.prob, prob, g)
