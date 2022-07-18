

m_neurologic <- metabin(Events.E,Total.E,Events.C,Total.C,
                     data = metagen_neurologic,sm="OR",
                     studlab = paste(metagen_neurologic$`Studies`,sep = ","),
                     label.e = "R778L",
                     label.c = "Non-R778L",
                     method = "SSW",
                     fixed = TRUE,
                     random = FALSE)
summary(m_neurologic)
forest(m_neurologic,family="sans",fontsize = 9.5,
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

funnel(m_neurologic,comb.fixed=FALSE, level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkseagreen4", "darkseagreen", "darkseagreen1"),
       lwd = 2.5, cex = 1.3, pch = 16,
       )
legend(x = 6
       , y = 0.05, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

metabias( m_neurologic,
          sm="OR",
          method.bias = "Harbord",
          plotit = TRUE)
forest(metainf(m_neurologic),comb.random = TRUE,family="sans",fontsize = 9.5,
       lwd = 2, 
       col.diamond.fixed = "lightslategray",
       col.diamond.lines.fixed = "lightslategray",
       col.diamond.random = "maroon", col.diamond.lines.random = "maroon",
       col.square = "moccasin", col.study = "lightslategray",
       lty.fixed = 4, plotwidth = "8cm", colgap.forest.left = "1cm",
       colgap.forest.right = "1cm", just.forest="right", colgap.left = "0.5cm",
       colgap.right = "0.5cm")
