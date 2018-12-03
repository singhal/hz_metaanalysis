library(plotrix)
library(cowplot)
library(gridGraphics)

space = seq(0, 1, 0.1)
star = 0.8 * space + 0.1
circle = -0.8 * space + 0.9
d1 = data.frame(space = c(space, space), 
               fitness = c(star, circle),
               type = c(rep("star", length(star)),
                        rep("circle", length(circle))))
pts1 = data.frame(x = c(-0.075, -0.075), 
                  y = c(star[1], circle[1]), 
                  type = c("star", "circle"))

star = rep(0.75, length(space))
circle = rep(0.25, length(space))
d2 = data.frame(space = c(space, space), 
                fitness = c(star, circle),
                type = c(rep("star", length(star)),
                         rep("circle", length(circle))))
pts2 = data.frame(x = c(-0.12, -0.025, -0.075, -0.075), 
                  y = c(star[1], star[1], circle[1], circle[1]), 
                  type = c("star", "circle", "star", "circle"))

plot_graph <- function(dd, ptsxx, xlabtxt, ylabtxt) {
ggplot(dd, aes(space, fitness)) + xlim(-0.15, 1) +
  ylim(0, 1) + geom_line(aes(color = type)) +
  xlab(xlabtxt) + ylab(ylabtxt) + 
  scale_color_manual(values=c("black", "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  geom_point(data = ptsxx, 
  mapping = aes(x = x, y = y, 
  shape = type, color = type, cex = 0.3)) + 
  scale_shape_manual(values=c(8, 1)) +
  theme(legend.position="none") + 
  theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.95), units = "cm"))
}

b = plot_graph(d1, pts1, "", "fitness")
# c = plot_graph(d2, pts2, expression(paste(sigma ^ o, "= ", sigma ^"*")), "fitness")
c = plot_graph(d2, pts2, "", "fitness")
c + draw_label(expression(paste(sigma ^ o, "= ", sigma ^"*")), 0.9, 0.05)
d = plot_graph(d1, pts1, "space", expression("settlement \nprobability"))

pts = data.frame(x = runif(50, 0.05, 0.95), 
                 y = runif(50, 0.05, 0.95),
                 type = rep(NA, 50))
b1 = 0.37
b2 = 0.63
pts[pts$x < b1, "type"] = 8
pts[pts$x > b2, "type"] = 1
for (i in 1:nrow(pts)) {
  if (is.na(pts[i, "type"])) {
    rand = runif(1, 0, 1)
    if (rand < 0.25) {
      pts[i, "type"] = 1
    } else if (rand > 0.75) {
      pts[i, "type"] = 8
    } else {
      pts[i, "type"] = 20
    }
  }
}

dev.new()
par(xpd = NA, bg = "transparent", mar = c(0, 5, 0, 0))
plot(NULL, xlim=c(0, 1), ylim=c(0, 1), axes = F, xlab = "", ylab = "")
gradient.rect(0, 0, 1, 1,
              col = gray.colors(100, start = 0.1, end = 0.9, alpha = 0.9),
              border = NA)
lines.default(x = c(b1, b1), y = c(0, 1), lty = 2, lwd = 2)
lines.default(x = c(b2, b2), y = c(0, 1), lty = 2, lwd = 2)
points(pts[pts$type != 20, ]$x, 
       pts[pts$type != 20, ]$y, 
       pch = pts[pts$type != 20, ]$type,
       cex = 1)
points(pts[pts$type == 20, ]$x, 
       pts[pts$type == 20, ]$y, 
       pch = 8,
       cex = 1)
points(pts[pts$type == 20, ]$x, 
       pts[pts$type == 20, ]$y, 
       pch = 1,
       cex = 1)
a <- recordPlot()
dev.off()

xx = plot_grid(a, b, c, d, nrow = 4, labels = c("A", "B", "C", "D"), 
          rel_heights = c(1.5, 1, 1, 1))
save_plot("~/Desktop/figure1.pdf", xx, nrow=4, base_height = 1.5, base_width = 4)
