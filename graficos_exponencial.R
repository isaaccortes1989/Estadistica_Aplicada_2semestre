rm(list=ls())
library(latex2exp)
t     <- seq(0,20,0.0001)
lambda <- c(0.25,0.5,1,1.5)
n     <- length(t)

f_1  <- dexp(t,rate = lambda[1])
f_2  <- dexp(t,rate = lambda[2])
f_3  <- dexp(t,rate = lambda[3])
f_4  <- dexp(t,rate = lambda[4])


par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(t, f_1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, 
panel.first = grid(col = "lightgray", lty = "dotted"),ylim = c(0,1.5),
lwd = 3, xlim = c(0,20), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "#1b9e77")
lines(t,f_2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#d95f02")
lines(t,f_3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#7570b3")
lines(t,f_4, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#e7298a")

mtext(text = "t",side = 1,line = 2, cex = 1.6)
mtext(text = "f(t)",side = 2,line = 2, cex = 1.6)

legend(x = c(10,15), y = c(1,1.5), box.lty = 0, horiz = FALSE, cex = 1.4, lty=c(1,1,1,1), lwd = 2, col=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
legend = c(text=TeX("$\\lambda=0.25$"),text=TeX("$\\lambda=0.50$"),text=TeX("$\\lambda=1$"),
           text=TeX("$\\lambda=1.5$")))


#==================================#
#=====FDA==========================#
#==================================#

g_1  <- pexp(t,rate = lambda[1])
g_2  <- pexp(t,rate = lambda[2])
g_3  <- pexp(t,rate = lambda[3])
g_4  <- pexp(t,rate = lambda[4])

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(t, g_1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, 
panel.first = grid(col = "lightgray", lty = "dotted"),ylim = c(0,1),
lwd = 3, xlim = c(0,20), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "#1b9e77")
lines(t,g_2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#d95f02")
lines(t,g_3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#7570b3")
lines(t,g_4, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#e7298a")

mtext(text = "t",side = 1,line = 2, cex = 1.6)
mtext(text = "F(t)",side = 2,line = 2, cex = 1.6)

legend(x = c(10,15), y = c(0.4,0.6), box.lty = 0, horiz = FALSE, cex = 1.4, lty=c(1,1,1,1), lwd = 2, col=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
legend = c(text=TeX("$\\lambda=0.25$"),text=TeX("$\\lambda=0.50$"),text=TeX("$\\lambda=1$"),
           text=TeX("$\\lambda=1.5$")))


#==================================#
#=====R(T)==========================#
#==================================#

r_1  <- 1-pexp(t,rate = lambda[1])
r_2  <- 1-pexp(t,rate = lambda[2])
r_3  <- 1-pexp(t,rate = lambda[3])
r_4  <- 1-pexp(t,rate = lambda[4])

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(t, r_1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, 
panel.first = grid(col = "lightgray", lty = "dotted"),ylim = c(0,1),
lwd = 3, xlim = c(0,20), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "#1b9e77")
lines(t,r_2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#d95f02")
lines(t,r_3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#7570b3")
lines(t,r_4, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#e7298a")

mtext(text = "t",side = 1,line = 2, cex = 1.6)
mtext(text = "R(t)",side = 2,line = 2, cex = 1.6)

legend(x = c(10,15), y = c(0.4,0.6), box.lty = 0, horiz = FALSE, cex = 1.4, lty=c(1,1,1,1), lwd = 2, col=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
legend = c(text=TeX("$\\lambda=0.25$"),text=TeX("$\\lambda=0.50$"),text=TeX("$\\lambda=1$"),
           text=TeX("$\\lambda=1.5$")))

#==================================#
#=====h(t)==========================#
#==================================#

h_1  <- rep(lambda[1],n)
h_2  <- rep(lambda[2],n)
h_3  <- rep(lambda[3],n)
h_4  <- rep(lambda[4],n)

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(t, h_1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, 
panel.first = grid(col = "lightgray", lty = "dotted"),ylim = c(0,2.5),
lwd = 3, xlim = c(0,20), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "#1b9e77")
lines(t,h_2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#d95f02")
lines(t,h_3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#7570b3")
lines(t,h_4, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "#e7298a")

mtext(text = "t",side = 1,line = 2, cex = 1.6)
mtext(text = "h(t)",side = 2,line = 2, cex = 1.6)

legend(x = c(10,15), y = c(2,2.5), box.lty = 0, horiz = FALSE, cex = 1.4, lty=c(1,1,1,1), lwd = 2, col=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
legend = c(text=TeX("$\\lambda=0.25$"),text=TeX("$\\lambda=0.50$"),text=TeX("$\\lambda=1$"),
           text=TeX("$\\lambda=1.5$")))















