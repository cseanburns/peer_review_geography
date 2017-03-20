# Scores
# Focus on data set filtered by sent for review

# Restart R to avoid package conflicts
require("car")
require("ggplot2")
require("MASS")
require("Hmisc")
require("plyr")
require("dplyr")

dec_sent <- dplyr::filter(dec, sent_for_review == "Yes")
dec_sent <- dplyr::select(dec_sent, mean_review_score, first_auth_geog, english)

dec_sent$first_auth_geog <- relevel(dec_sent$first_auth_geog, ref = "Europe")
# remove rows with NAs
dec_sent <- dec_sent[complete.cases(dec_sent),]

contrasts(dec_sent$first_auth_geog)

plot(density(dec_sent$mean_review_score, na.rm = TRUE))
plot(cut(dec_sent$mean_review_score, breaks = 3))

mean.rs <- cut(dec_sent$mean_review_score, breaks = 3)
mean.rs <- revalue(mean.rs, c("(0.997,2]" = "1",
                              "(2,3]" = "2",
                              "(3,4]" = "3"))

mean.rs <- factor(mean.rs, labels = c("Low", "Middle", "High"))
dec_sent$mean.rs <- mean.rs

rm(mean.rs)

contrasts(dec_sent$mean.rs)

# help from UCLA site: http://www.ats.ucla.edu/stat/r/dae/ologit.htm

ftable(xtabs(~ mean.rs + first_auth_geog, data = dec_sent))
ftable(xtabs(~ mean.rs + english, data = dec_sent))
plot(dec_sent$mean.rs ~ as.factor(dec_sent$english))

p <- ggplot(dec_sent, aes(x = first_auth_geog, y = mean_review_score))
p + geom_boxplot() + geom_jitter(size = 0.75)

m <- polr(mean.rs ~ first_auth_geog, data = dec_sent, Hess = TRUE)
summary(m, digits = 3)
Anova(m)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))
round(exp(cbind(OR = coef(m), ci)), 3)
sf <- function(y) {
        c('Y>=1' = qlogis(mean(y >= 1)),
          'Y>=2' = qlogis(mean(y >= 2)),
          'Y>=3' = qlogis(mean(y >= 3)))
}
round(s <- with(dec_sent,
                summary(as.numeric(mean.rs) ~ first_auth_geog, fun = sf)),3)

ggplot(as.data.frame(table(m$model)),
       aes(x = first_auth_geog, y = Freq, fill = mean.rs)) +
       geom_bar(stat="identity") + scale_fill_grey(name = "Review Score") +
       theme_bw() +
       labs(x = "Geographic Region of First Author",
            y = "Count") +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(legend.position = c(.8,.8))

plot(s, which=1:3, pch=1:3,
     xlab = "Logit",
     ylab = "",
     main = " ", xlim=range(s[,3:4]))

s.tbl <- as.table(s)
s.tbl <- s.tbl[1,]
s.df  <- data.frame(s.tbl)
s.df  <- dplyr::filter(s.df, Var2 != "Y>=1" & Var2 != "N")
s.df  <- dplyr::filter(s.df, Var1 != "Europe")
names(s.df) <- c("Region", "Level", "OR")

n.sdf           <- data.frame(exp(coef(m)))
n.sdf$Region    <- rownames(n.sdf)
rownames(n.sdf) <- NULL
names(n.sdf)    <- c("OR", "Region")
Level           <- rep("Overall", 6)
n.sdf$Level     <- Level
n.sdf$Region    <- gsub("first_auth_geogAfrica", "Africa", n.sdf$Region)
n.sdf$Region    <- gsub("first_auth_geogAsia", "Asia", n.sdf$Region)
n.sdf$Region    <- gsub("first_auth_geogLatin America", "Latin America", n.sdf$Region)
n.sdf$Region    <- gsub("first_auth_geogNorth America", "North America", n.sdf$Region)
n.sdf$Region    <- gsub("first_auth_geogOceania", "Oceania", n.sdf$Region)
n.sdf$Region    <- gsub("first_auth_geogUnited Kingdom", "United Kingdom", n.sdf$Region)
n.sdf$Region    <- as.factor(n.sdf$Region)
n.sdf$Level     <- as.factor(n.sdf$Level)
s.df$Region     <- droplevels(s.df$Region)
s.df$Level      <- droplevels(s.df$Level)
t.sdf           <- rbind(s.df, n.sdf)

ggplot(t.sdf, aes(x = OR, y = Region)) +
       geom_point(aes(shape = factor(Level))) +
       scale_shape(solid = FALSE, name = "OR Level") + 
       theme_bw() +
        labs(x = "Logits",
             y = "Geographical Region of First Authors") +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(legend.position = c(0.95,0.85))

rm(ci, ctable, n.sdf, s.df, t.sdf, Level, m, p, s, s.tbl, sf)
