# This code produces Figures 3, 4, and 5. The code for figure 2 is in "Parameter_estimation.R", and the map in Figure 1
# was developed using QGIS.

##########FIGURE 3

par(xpd = NA, oma = c(0, 2, 0, 0))

layout(matrix(c(1, 2, 3, 4, 3, 4), ncol = 3, byrow = FALSE))

# Figure 3A requires running the sensitivity analysis

par(mar = c(2.5, 7, 7, 0) + 0.1)
plot(c(150, 250, 350, 450), 1 - N5s, xlab = "", 
     ylab = "", cex.lab = 2, cex.axis = 1.8, lwd = 2, ylim = c(0, 1), type = "o", yaxt = "n", col = "darkgreen")
axis(2, seq(0, 1, 0.2), cex.axis = 1.8, las =2)

points(c(150, 250, 350, 450), 1 - N10s, lwd = 2, type = "o", col = "gold4")
points(c(150, 250, 350, 450), 1 - N20s, lwd = 2, type = "o", col = "red2")
points(c(150, 250, 350, 450), 1 - N40s, lwd = 2, type = "o", col = "brown")
mtext("A", side = 3, cex = 2, line = 1, at = 100)

# Figure 3B requires running the long-term validation analysis

par(mar = c(7, 7, 2.5, 0) + 0.1)
plot(c(250, 300, 350, 400, 450), c((1 - t250cap), (1 - t300cap), (1 - t350cap), (1 - t400cap), (1 - t450cap)), 
     xlab = "", 
     ylab = "", cex.lab = 2, cex.axis = 1.8, lwd = 2, ylim = c(0, 1), type = "o", xlim = c(235, 465), yaxt = "n",
     xaxt = "n")
axis(2, seq(0, 1, 0.2), cex.axis = 1.8, las = 2)
axis(1, seq(250, 450, 50), cex.axis = 1.8)

points(c(250, 300, 350, 400, 450), c((1 - t250ph), (1 - t300ph), (1 - t350ph), (1 - t400ph), (1 - t450ph)), lwd = 2, type = "o", col = "blue")
mtext("B", side = 3, cex = 2, line = 1, at = 190)

title(xlab = "Degree-days when\nprediction is made", cex.lab = 2, line = 5)



title(ylab = "Accuracy", cex.lab = 2.5, outer = TRUE, line = -1)


x.axis = c(40, 60, 120)
y.axis = c(250, 300, 350, 400, 450)
z.axis = seq(0.9, 1, 0.05)

# Figure 3C requires running the short-term validation analysis

par(mar = c(0, 0, 5, 0) + 0.1, xaxs = "i", yaxs = "i")
pmat <- persp(y = c(250, 300, 350, 400, 450), x = c(40, 60, 120), (1 - t_ph), zlim = c(0.9, 1), theta = 50, phi = 25, expand = 0.6,
              col = "skyblue", xlab = "", ylab = "", cex.axis = 2,
              cex.lab = 2, zlab = "", lwd  = 2, ticktype = "detailed", axes = FALSE)

tick.start <- trans3d(40, 250, z.axis, pmat)
tick.end <- trans3d((40 - 10), 250, z.axis, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

labels <- as.character(z.axis)
label.pos <- trans3d((40 - 20), (250), z.axis, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex = 1.5)

tick.start <- trans3d(x.axis, 250, 0.9, pmat)
tick.end <- trans3d(x.axis, (250 - 10), 0.9, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

tick.start <- trans3d(120, y.axis, 0.9, pmat)
tick.end <- trans3d(125, y.axis, 0.9, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

text(x = limits[1] + 0.1, y = grconvertY(0.95, from = "ndc") - 0.1,
     labels = "C", xpd = NA, cex = 3)

par(mar = c(5, 0, 0, 0) + 0.1)

limits <- par("usr")
pmat <- persp(y = c(250, 300, 350, 400, 450), x = c(40, 60, 120), (1 - t_cap), zlim = c(0.9, 1), theta = 50, phi = 25, expand = 0.6,
              col = "grey", ticktype = "detailed", xlab = "", ylab = "", cex.axis = 2,
              cex.lab = 2, zlab = "", lwd  = 2, axes = FALSE)


tick.start <- trans3d(x.axis, 250, 0.9, pmat)
tick.end <- trans3d(x.axis, (250 - 10), 0.9, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

labels <- as.character(x.axis)
label.pos <- trans3d(x.axis, (250 - 20), 0.9, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt = 320, cex = 1.5)


tick.start <- trans3d(40, 250, z.axis, pmat)
tick.end <- trans3d((40 - 10), 250, z.axis, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

labels <- as.character(z.axis)
label.pos <- trans3d((40 - 20), 250, z.axis, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex = 1.5)

tick.start <- trans3d(120, y.axis, 0.9, pmat)
tick.end <- trans3d(125, y.axis, 0.9, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

labels <- as.character(y.axis)
label.pos <- trans3d((120 + 8), y.axis, 0.9, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex = 1.5)


text(x = limits[1] - (limits[1] * 0.45), y = grconvertY(0.12, from = "ndc"),
     labels = "Length of prediction\n(degree-days)", xpd = NA, srt = 310, cex = 2)

text(x = limits[1] - (limits[1] * 1.55), y = grconvertY(0.12, from = "ndc"),
     labels = "Degree-days when\nprediction is made", xpd = NA, srt = 35, cex = 2)


##########FIGURE 4

# Figure 4A requires running the long-term validation analysis

par(mfrow = c(1, 2), xpd = NA)

par(mar = c(6, 7, 2.2, 2) + 0.1)
boxplot(wi250capST, wi250phST, wi300capST, wi300phST, wi350capST, wi350phST,  wi400capST, wi400phST,  wi450capST, wi450phST,
        xaxt = "n", cex.axis = 1.8, lwd  = 2, ylab = "", cex.lab = 2, at = c(1, 2, 4, 5,
                                                                             7, 8, 10, 11,
                                                                             13, 14),
        xlab = "", col = rep(c("grey", "skyblue"), 4), ylim = c(0, 12), yaxt = "n")

title(xlab = "Degree-days when\nprediction is made", line = 5, cex.lab = 2)

axis(2, seq(0, 10, 2), cex.axis = 1.8, las = 2)
axis(1, at = c(1.5, 4.5, 7.5, 10.5, 13.5), labels = c(250, 300, 350, 400, 450), cex.axis = 1.8)
mtext("A", side = 3, cex = 2, line = 0.8, at = -5)

title(ylab = "Standardized width\nof the prediction band", line = 3.5, cex.lab = 2)
par(new = TRUE)
plot(seq(250, 450, 1), coef(lin1)[1] + coef(lin1)[2] * seq(250, 450, 1), lwd = 3, type = "l", ylim = c(0, 12), xaxt = "n",
     yaxt = "n", ylab = "", xlab = "", lty = 3)

# Figure 4A requires running the short-term validation analysis

par(mar = c(6, 2, 2.2, 7) + 0.1)

boxplot(As_cap, As_ph, Bs_cap, Bs_ph, Cs_cap, Cs_ph,
        xaxt = "n", cex.axis = 2, lwd  = 2, ylab = "", cex.lab = 2,
        xlab = "", at = c(1, 2, 4, 5,
                          7, 8),
        col = rep(c("grey", "skyblue"), 3), ylim = c(0, 12), yaxt = "n")
axis(2, seq(0, 10, 2), labels = FALSE, cex.axis = 1.8)
axis(1, at = c(1.5, 4.5, 7.5), labels = c(40, 60, 120), cex.axis = 2)
title(xlab = "Length of prediction\n(degree-days)", line = 5, cex.lab = 2)

mtext("B", side = 3, cex = 2, line = 0.8, at = -1)


############FIGURE 5

# This figure requires running the validation analysis of the means

par(mfrow = c(1, 2))
par(mar = c(4, 8.5, 0.5, 0) + 0.1)
plot(t350phMOb1, t350phMPr1, xlim = c(0, 1800), ylim = c(0, 1750), xaxt = "n", cex.axis = 1.8, lwd  = 2, ylab = "", cex.lab = 2,
     xlab = "Observed mean moth captures", yaxt = "n")
abline(0, 1, lwd = 2)
abline(mod_val_ph, lwd = 2, lty = 2)

axis(2, seq(0, 1800, 250), cex.axis = 1.8, las = 2)
axis(1, at = seq(0, 1800, 250), cex.axis = 1.8)
mtext("A", side = 3, cex = 2, line = -1, at = -500)

title(ylab = "Predicted\nmean moth captures", line = 5, cex.lab = 2)

par(mar = c(4, 5, 0.5, 3.5) + 0.1)
plot(t350capMOb1, t350capMPr1, xlim = c(0, 1800), ylim = c(0, 1750), xaxt = "n", cex.axis = 1.8, lwd  = 2, ylab = "", cex.lab = 2,
     xlab = "Observed mean moth captures", yaxt = "n")
abline(0, 1, lwd = 2)
abline(mod_val_cap, lwd = 2, lty = 2)

axis(2, seq(0, 1800, 250), labels = NA, cex.axis = 1.8, las = 2)
axis(1, at = seq(0, 1800, 250), cex.axis = 1.8)
mtext("B", side = 3, cex = 2, line = -1, at = -300)
