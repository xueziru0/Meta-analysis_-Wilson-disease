library(meta)
library(Matrix)
library(metafor)
library(readr)
library(dmetar)
# 20210101 from the website:https://zhuanlan.zhihu.com/p/351400164

##measurement data
m_count_age <- metacont(n.e = metagen_age$R778L_count,
                        mean.e = metagen_age$R778L_mean,
                        sd.e = metagen_age$R778L_sd,
                        n.c = metagen_age$Control_count,
                        mean.c = metagen_age$Control_mean,
                        sd.c =metagen_age$Control_sd,
                        studlab=paste(metagen_age$Studies,sep = ","), 
                        data = metagen_age,
                        sm="SMD",
                        method.smd = "Hedges",
                        fixed = TRUE,
                        random = FALSE,
                        tau.common = FALSE)

summary(m_count_age)

forest(m_count_age,fontsize = 9.5,
       lwd =2, col.diamond.fixed = "gray33",
       col.diamond.lines.fixed = "gray33",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "maroon", col.study = "gray33",
       lty.fixed = 4, plotwidth = "11cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm",
       print.tau2= FALSE)


funnel(m_count_age,comb.fixed=FALSE, level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkseagreen4", "darkseagreen", "darkseagreen1"),
       lwd = 2.5, cex = 1.3, pch = 16)
legend(x = 1.4, y = 0.05, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
metabias(m_count_age,method.bias = "Egger",plotit = T)
