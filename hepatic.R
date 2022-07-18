library(meta)
library(Matrix)
library(metafor)
library(readr)
# 20210101 from the website:https://zhuanlan.zhihu.com/p/351400164

m_hepatic <- metabin(Events.E,Total.E,Events.C,Total.C,
                 data = metagen_hepatic,sm="OR",
                 studlab = paste(metagen_hepatic$`Studies`),
                 label.e = "R778L",
                 label.c = "Non-R778L",
                 method = "SSW",
                 fixed = TRUE,
                 random = FALSE,
                 tau.common = FALSE)
summary(m_hepatic)
forest(m_hepatic,family="sans",fontsize = 9.5,
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

funnel(m_hepatic,comb.fixed=FALSE, level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkseagreen4", "darkseagreen", "darkseagreen1"),
       lwd = 2.5, cex = 1.3, pch = 16,
       )
legend(x = 7, y = 0.05, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
metabias( m_hepatic,
          sm="OR",
          method.bias = "Harbord",
          plotit = TRUE)

forest(metainf(m_hepatic),comb.random = TRUE,family="sans",fontsize = 9.5,
       lwd = 2, 
       col.diamond.fixed = "lightslategray",
       col.diamond.lines.fixed = "lightslategray",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "moccasin", col.study = "lightslategray",
       lty.fixed = 4, plotwidth = "8cm", colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", just.forest="right", colgap.left = "0.5cm",
       colgap.right = "0.5cm")
