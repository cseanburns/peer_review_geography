# Scores
# Focus on data set filtered by sent for review

# source("libraries.R")
require("ggplot2")
require("MASS")
require("Hmisc")

dec_sent$paper_rejected  <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$first_auth_geog <- relevel(dec_sent$first_auth_geog, ref = "Europe")

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$first_auth_geog)

plot(density(dec_sent$mean_review_score))
plot(cut(dec_sent$mean_review_score, breaks = 3))

mean.rs <- cut(dec_sent$mean_review_score, breaks = 3)
mean.rs <- revalue(mean.rs, c("(0.997,2]" = "1",
                              "(2,3]" = "2",
                              "(3,4]" = "3"))

mean.rs <- factor(mean.rs, labels = c("Low", "Middle", "High"))
dec_sent$mean.rs <- mean.rs
rm(mean.rs)

# help from UCLA site: http://www.ats.ucla.edu/stat/r/dae/ologit.htm

ftable(xtabs(~ paper_rejected + mean.rs + first_auth_geog, data = dec_sent))

m <- polr(mean.rs ~ first_auth_geog, data = dec_sent, Hess = TRUE)
summary(m)
(ctable <- -coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))
exp(coef(m))
exp(cbind(OR = coef(m), ci))
sf <- function(y) {
        c('Y>=1' = qlogis(mean(y >= 1)),
          'Y>=2' = qlogis(mean(y >= 2)),
          'Y>=3' = qlogis(mean(y >= 3)))
}
(s <- with(dec_sent, summary(as.numeric(mean.rs) ~ first_auth_geog, fun = sf)))





###
  
mean.rs.1 <- polr(mean.rs ~ first_auth_geog, data = dec_sent, Hess = TRUE)
(ctable <- coef(summary(mean.rs.1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(mean.rs.1))

exp(cbind(OR = coef(mean.rs.1), ci))
table(dec_sent$paper_rejected, dec_sent$mean.rs)
table(mean.rs.1$model)

sf <- function(y) {
        c('Y>=1' = qlogis(mean(y >= 1)),
          'Y>=2' = qlogis(mean(y >= 2)),
          'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dec_sent, aggregate(as.numeric(mean.rs) ~ first_auth_geog, FUN = sf)))

ggplot(as.data.frame(table(mean.rs.1$model)),
       aes(x = first_auth_geog, y = sort(Freq, decreasing = TRUE), fill = mean.rs)) +
       geom_bar(stat="identity") + scale_fill_grey(name = "Review Score") +
       theme_bw() +
       labs(x = "Geographic Region of First Author",
            y = "Count") +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 12,
                                         colour = "black")) +
        theme(legend.position = c(.8,.8))

# The code below stopped working -- perhaps due to a version upgrade of R? Will
# have to work out, perhaps use the aggregrate function used, as above.

(s <- with(dec_sent, summary(as.numeric(mean.rs) ~ first_auth_geog, FUN = sf)))

plot(s, which=1:3, pch=1:3,
     xlab = "Logit",
     ylab = "",
     main = " ", xlim=range(s[,3:4]))

s.tbl <- as.table(s)
s.tbl <- s.tbl[1,]
s.df  <- data.frame(s.tbl)
s.df  <- filter(s.df, Var2 != "Y>=1" & Var2 != "N")
s.df  <- filter(s.df, Var1 != "Europe")
names(s.df) <- c("Region", "Level", "OR")

n.sdf           <- data.frame(exp(coef(mean.rs.1)))
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
       xlab("Logits") +
       ylab("Geographical Regions of First Authors")

rm(dec_sent, mean.rs, ctable, p, ci, sf, s, s.tbl, s.df, n.sdf, s.df, t.sdf) 