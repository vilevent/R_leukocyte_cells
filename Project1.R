
# Project 1
gr1 <- c(3500, 3500, 3500, 4000, 4000, 4000, 4300, 4500, 4500, 4900, 5200, 6000, 6750, 8000)
gr2 <- c(5710, 6110, 8060, 8080, 11400)
gr3 <- c(2930, 3330, 3580, 3880, 4280, 5120)
gr4 <- c(6320, 6860, 11400, 14000)
gr5 <- c(3230, 3880, 7640, 7890, 8280, 16200, 18250, 29900)

n1 <- length(gr1)
n2 <- length(gr2)
n3 <- length(gr3)
n4 <- length(gr4)
n5 <- length(gr5)

# Checking normality
shapiro.test(gr1)
shapiro.test(gr2)
shapiro.test(gr3)
shapiro.test(gr4)
shapiro.test(gr5)

gr.sites <- data.frame(gr.sites.val=c(gr1, gr2, gr3, gr4, gr5),
                       group=rep(c("grp 1", "grp 2", "grp 3", "grp 4", "grp 5"), 
                                 c(n1, n2, n3, n4, n5)))

# descriptive stats: mean and variance
mean.gr1 <- mean(gr1)
mean.gr2 <- mean(gr2)
mean.gr3 <- mean(gr3)
mean.gr4 <- mean(gr4)
mean.gr5 <- mean(gr5)

var.gr1 <- var(gr1)
var.gr2 <- var(gr2)
var.gr3 <- var(gr3)
var.gr4 <- var(gr4)
var.gr5 <- var(gr5)

descrip.stats <- data.frame(group=c("grp 1", "grp 2", "grp 3", "grp 4", "grp 5"),
                            mean=c(mean.gr1, mean.gr2, mean.gr3, mean.gr4, mean.gr5),
                            variance=c(var.gr1, var.gr2, var.gr3, var.gr4, var.gr5))

# Box plot
library("ggpubr")
ggboxplot(gr.sites, x = "group", y = "gr.sites.val", 
          color = "group",
          order = c("grp 1", "grp 2", "grp 3", "grp 4", "grp 5"))

# Kruskal-Wallis test
kruskal.test(gr.sites.val ~ as.factor(group), data = gr.sites)

# Fligner-Wolfe test
gr.sites$trt <- gr.sites$group != "grp 1"
library(coin)
wilcox_test(gr.sites.val~as.factor(trt),
            data=gr.sites, alternative="less") # control group < trt groups

# Steel-Dwass-Critchlow-Fligner test
library("PMCMRplus")
dscfAllPairsTest(gr.sites.val~as.factor(group), data=gr.sites)

# Nemenyi-Damico-Wolfe test
library("PMCMRplus")
kwManyOneNdwTest(gr.sites.val~as.factor(group),
                 data=gr.sites, alternative="greater") 


