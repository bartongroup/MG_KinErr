
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


theme_blank <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),legend.position="none",
                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),plot.background=element_blank())


plotSisterChromatids <- function(sc) {
  Sides <- c("L", "R")
  
  spndl <- data.frame(
    x = c(-1, 1),
    y = c(0.3, 0.3)
  )
  rownames(spndl) <- Sides
  
  kts <- data.frame(
    x = c(-0.2, 0.2),
    y = c(0, 0)
  )
  rownames(kts) <- Sides
  
  mts <- list()
  id <- 1
  for(side in Sides) {
    KT <- sc$KT[[side]]
    if(!is.null(KT$spindle)) {
      p1 <- spndl[KT$spindle, ]
      p2 <- kts[side, ]
      if(KT$contact == "lateral") p2 <- p2 + c(-p1$x * 0.05, -0.05)
      p <- rbind(p1, p2)
      mts[[id]] <- p
      id <- id + 1
    }
  }
  
  g <- ggplot() +
    theme_blank +
    xlim(-1.2, 1.2) +
    ylim(-0.5, 0.5) +
    geom_point(data=spndl, aes(x, y), shape=21, colour="black", fill="black", size=5) +
    geom_point(data=kts, aes(x, y), shape=21, colour="black", fill="orange", size=5)
  for(m in mts) g <- g + geom_line(data=m, aes(x, y), colour="black")
  
  g
}