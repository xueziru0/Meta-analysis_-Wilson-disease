library(meta)
library(Matrix)
library(metafor)
library(readr)
# 20210101 from the website:https://zhuanlan.zhihu.com/p/351400164

##measurement data
m_count_cp <-metacont(n.e = metagen_cp$R778L_count,
                      mean.e = metagen_cp$R778L_mean,
                      sd.e = metagen_cp$R778L_sd,
                      n.c = metagen_cp$Control_count,
                      mean.c = metagen_cp$Control_mean,
                      sd.c =metagen_cp$Control_sd,
                      studlab=paste(metagen_cp$Studies,sep = ","), 
                      data = metagen_cp,
                      sm="SMD",
                      method.smd = "Hedges",
                      fixed = FALSE,
                      random = TRUE)
summary(m_count_cp)

forest(m_count_cp,fontsize = 9.5,
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
funnel(m_count_cp,comb.fixed=FALSE, level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkseagreen4", "darkseagreen", "darkseagreen1"),
       lwd = 2.5,cex = 1.3, pch = 16)
legend(x = 0.68, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
metabias(m_count_cp,method.bias = "Egger",plotit = T)
legend(x=9,y=2,
       inset=.05,
       bty = "o",
       box.lwd = 1,
       legend = c("p-value = 0.5535")
       )


##subgroup 
m_cp_subgroup <- metacont(n.e = metagen_cp$R778L_count,
                          mean.e = metagen_cp$R778L_mean,
                          sd.e = metagen_cp$R778L_sd,
                          n.c = metagen_cp$Control_count,
                          mean.c = metagen_cp$Control_mean,
                          sd.c =metagen_cp$Control_sd,
                          studlab=paste(metagen_cp$Studies,sep = ","), 
                          data = metagen_cp,
                          sm="SMD",
                          method.smd = "Hedges",
                          fixed = FALSE,
                          random = TRUE,
                          subgroup  =metagen_cp$`Grey leterature`) 
forest(m_cp_subgroup,
       print.tau2 = FALSE,
       family="sans",fontsize = 9.5,
       lwd =2, 
       col.diamond.fixed = "maroon",
       col.diamond.lines.fixed = "maroon",
       col.square = "skyblue", col.study = "gray33",
       lty.fixed = 4, plotwidth = "10cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)

m_cp_subgroup2<- metacont(n.e = metagen_cp$R778L_count,
                          mean.e = metagen_cp$R778L_mean,
                          sd.e = metagen_cp$R778L_sd,
                          n.c = metagen_cp$Control_count,
                          mean.c = metagen_cp$Control_mean,
                          sd.c =metagen_cp$Control_sd,
                          studlab=paste(metagen_cp$Studies,sep = ","), 
                          data = metagen_cp,
                          sm="SMD",
                          method.smd = "Hedges",
                          fixed = FALSE,
                          random = TRUE,
                          subgroup  =metagen_cp$`Region`) 
    
forest(m_cp_subgroup2,
       print.tau2 = FALSE,
       family="sans",fontsize = 9.5,
       lwd =2, 
       col.diamond.fixed = "maroon",
       col.diamond.lines.fixed = "maroon",
       col.square = "skyblue", col.study = "gray33",
       lty.fixed = 4, plotwidth = "10cm", 
       colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", 
       just.forest="right", 
       colgap.left = "0.5cm",
       colgap.right = "0.5cm"
)

library(metafor)
m_cp_regression <- update(m_count_cp,byvar=metagen_cp$`Publication data`,
                           tau.common = TRUE, comb.fixed = FALSE)
m_cp_regression_bubble <- metareg(m_cp_regression)
m_cp_regression_bubble
bubble(m_cp_regression_bubble,
       ylim = c(-2,3),xlim = c(2000,2020),lwd = 1.5,lty =1,
       pch = 1,col = "deepskyblue4",
       max.cex = 10,min.cex = 4,
       col.line = "brown",
       xlab = "Publication Year",
       cex.studlab = 0.5,
       offset = -4,
       regline = TRUE,
       )
legend(x=2014,y=2.8,
       inset=.05,
       bty = "n",

)


forest(metainf(m_count_cp),comb.random = TRUE,family="sans",fontsize = 9.5,
       lwd = 2, 
       col.diamond.fixed = "lightslategray",
       col.diamond.lines.fixed = "lightslategray",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "moccasin", col.study = "lightslategray",
       lty.fixed = 4, plotwidth = "8cm", colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", just.forest="right", colgap.left = "0.5cm",
       colgap.right = "0.5cm")

