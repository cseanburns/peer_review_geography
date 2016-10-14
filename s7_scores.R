# Scores

source("libraries.R")

dec0 <- dec

dec0$paper_rejected  <- relevel(dec0$paper_rejected, ref = "Yes")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")

contrasts(dec0$paper_rejected)
contrasts(dec0$first_auth_geog)

plot(density(dec0$mean_review_score))
plot(cut(dec0$mean_review_score, breaks = 3))

mean.rs <- cut(dec0$mean_review_score, breaks = 3)
mean.rs <- revalue(mean.rs, c("(0.997,2]" = "1",
                              "(2,3]" = "2",
                              "(3,4]" = "3"))

mean.rs      <- factor(mean.rs, labels = c("Low", "Middle", "High"))
dec0$mean.rs <- mean.rs

mean.rs.1 <- polr(mean.rs ~ first_auth_geog, data = dec0, Hess = TRUE)
(ctable <- coef(summary(mean.rs.1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(mean.rs.1))

exp(cbind(OR = coef(mean.rs.1), ci))
table(dec0$paper_rejected, dec0$mean.rs)
table(mean.rs.1$model)

sf <- function(y) {
        c('Y>=1' = qlogis(mean(y >= 1)),
          'Y>=2' = qlogis(mean(y >= 2)),
          'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dec0, summary(as.numeric(mean.rs) ~ first_auth_geog, fun = sf)))

plot(s, which=1:3, pch=1:3,
     xlab = "Logit",
     ylab = "",
     main = " ", xlim=range(s[,3:4]))

ggplot(as.data.frame(table(mean.rs.1$model)),
       aes(x = first_auth_geog, y = Freq, fill = mean.rs)) +
       geom_bar(stat="identity") + scale_fill_grey(name = "Review Score") +
       theme_bw() +
       xlab("Geographic Region of First Author") +
       ylab("Frequency") +
       theme(legend.position = c(.9,.8))

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
n.sdf           <- edit(n.sdf) # Change Region names to match region names in SDF
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

rm(dec0, mean.rs, ctable, p, ci, sf, s, s.tbl, s.df, n.sdf, s.df, t.sdf) 