png(file="genuary/2021/2021-5/2021-5a.png", width = 800, height = 800)
plot(1:200 * sin(1:10), 1:200 * cos(1:10), col = "dark red", type = "s", lwd = 2, axes = FALSE, ann = FALSE)
dev.off()

png(file="genuary/2021/2021-5/2021-5b.png", width = 800, height = 800)
plot(1:100 * sin(1:50), 1:100 * cos(1:10), col = "dark red", type = "s", lwd = 2, axes = FALSE, ann = FALSE)
dev.off()