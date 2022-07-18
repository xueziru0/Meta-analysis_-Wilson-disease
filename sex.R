library(meta)
library(Matrix)
library(metafor)
library(readr)
# 20210101 from the website:https://zhuanlan.zhihu.com/p/351400164
library(dmetar)
library(tidyverse)
library(meta)

m_sex <- metabin(Events.E,Total.E,Events.C,Total.C,
                 data = metagen_sex,
                 studlab = paste(metagen_sex$`Studies`,sep = ","),
                 label.e = "R778L",
                 label.c = "Non-R778L",
                 sm= "OR",
                 method = "SSW",
                 fixed = TRUE,
                 random = FALSE,
                 tau.common = FALSE)
summary(m_sex)
forest(m_sex,family="sans",fontsize = 9.5,
       lwd =2, col.diamond.fixed = "gray33",
       col.diamond.lines.fixed = "gray33",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "maroon", col.study = "gray33",
       lty.fixed = 4, plotwidth = "11cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)
funnel(m_sex,comb.fixed=FALSE, level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkseagreen4", "darkseagreen", "darkseagreen1"),
       lwd = 2.5, cex = 1.3, pch = 16)
legend(x = 7, y = 0.04, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

metabias( m_sex,
         sm="OR",
         method.bias = "Harbord",
         plotit = TRUE)
dev.new()
##subgroup
m_sex_subgroup1 <- metabin(Events.E,Total.E,Events.C,Total.C,
                          data = metagen_sex,
                          sm="OR",
                          subgroup = metagen_sex$`Grey literature`,
                          studlab = paste(data_sex$`Studies`,sep = ","),
                          label.e = "R778L",
                          label.c = "Non-R778L",
                          method = "SSW",
                          tau.common = FALSE,
                          fixed = FALSE,
                          random = TRUE) 

forest(m_sex_subgroup1,family="sans",fontsize = 9.5,
       lwd =2, col.diamond.fixed = "gray33",
       col.diamond.lines.fixed = "gray33",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "skyblue", col.study = "gray33",
       lty.fixed = 4, plotwidth = "10cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)

m_sex_subgroup2 <- metabin(Events.E,Total.E,Events.C,Total.C,
                           data = metagen_sex,
                           sm="OR",
                           subgroup = metagen_sex$`Data Source`,
                           studlab = paste(data_sex$`Studies`,sep = ","),
                           label.e = "R778L",
                           label.c = "Non-R778L",
                           method = "SSW",
                           tau.common = FALSE,
                           fixed = FALSE,
                           random = TRUE) 

forest(m_sex_subgroup2,family="sans",fontsize = 9.5,
       lwd =2, col.diamond.fixed = "gray33",
       col.diamond.lines.fixed = "gray33",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "skyblue", col.study = "gray33",
       lty.fixed = 4, plotwidth = "10cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)

m_sex_subgroup3 <- metabin(Events.E,Total.E,Events.C,Total.C,
                           data = metagen_sex,
                           sm="OR",
                           subgroup = metagen_sex$`Region`,
                           studlab = paste(data_sex$`Studies`,sep = ","),
                           label.e = "R778L",
                           label.c = "Non-R778L",
                           method = "SSW",
                           tau.common = FALSE,
                           fixed = FALSE,
                           random = TRUE) 

forest(m_sex_subgroup3,family="sans",fontsize = 9.5,
       lwd =2, col.diamond.fixed = "gray33",
       col.diamond.lines.fixed = "gray33",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "skyblue", col.study = "gray33",
       lty.fixed = 4, plotwidth = "10cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)



library(metafor)
m_sex_regression <- update(m_sex,byvar=metagen_sex$`Publication data`,
                           tau.common = FALSE, comb.fixed = FALSE)
m_sex_regression_bubble <- metareg(m_sex_regression)
m_sex_regression_bubble
bubble(m_sex_regression_bubble,
       ylim = c(-2,3),xlim = c(2000,2020),lwd = 1.5,lty =1,
       pch = 1,col = "deepskyblue4",
       max.cex = 10,min.cex = 4,
       col.line = "brown",
       xlab = "Publication Year",
       cex.studlab = 0.5,
       offset = -4,
       regline = TRUE)

metainf(m_sex)
forest(metainf(m_sex),family="sans",fontsize = 9.5,
       lwd = 2, col.diamond.fixed = "lightslategray",
       col.diamond.lines.fixed = "lightslategray",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "moccasin", col.study = "lightslategray",
       lty.fixed = 4, plotwidth = "8cm", colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", just.forest="right", colgap.left = "0.5cm",
       colgap.right = "0.5cm")

