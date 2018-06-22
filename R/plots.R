
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


theme_blank <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),legend.position="none",
                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),plot.background=element_blank())

push <- function(lst, elem) {
  lst[[length(lst) + 1]] <- elem
  lst
}


plotSisterChromatids <- function(sc, dx=0.1, dy=0.08) {
  Sides <- c("L", "R")
  
  spndl <- data.frame(
    x = c(-1, 1),
    y = c(0.1, 0.1)
  )
  rownames(spndl) <- Sides
  
  kts <- data.frame(
    x = c(-0.2, 0.2),
    y = c(0, 0)
  )
  rownames(kts) <- Sides

  # find empty spindles
  emptys <- list("L"=TRUE, "R"=TRUE)
  # build a list of MT coordinates  
  mts <- list()
  for(side in Sides) {
    KT <- sc$KT[[side]]
    if(KT$spindle != "none") {
      emptys[[KT$spindle]] <- FALSE
      p1 <- spndl[KT$spindle, ]
      p2 <- kts[side, ]
      if(KT$contact %in% c("lateral", "dual")) p2 <- p2 + c(-p1$x * dx, -dy)
      p <- rbind(p1, p2)
      mts <- push(mts, p)
      if(KT$contact == "dual") {
        p <- rbind(spndl[KT$dual.spindle, ], kts[side, ])
        mts <- push(mts, p)
      }
    }
  }
  for(side in Sides) {
    if(emptys[[side]]) {
      p1 <- spndl[side, ]
      p2 <- p1 + p1 * c(-0.3, 0.1)
      mts <- push(mts, rbind(p1, p2))
    }
  }
  
  
  g <- ggplot() +
    theme_blank +
    theme(panel.background = element_rect(fill="grey90", colour="grey50")) +
    xlim(-1.2, 1.2) +
    ylim(-0.5, 0.5) +
    geom_point(data=spndl, aes(x, y), shape=21, colour="black", fill="black", size=5) +
    geom_point(data=kts, aes(x, y), shape=21, colour="black", fill="orange", size=5)
  for(m in mts) g <- g + geom_line(data=m, aes(x, y), colour="black")
  
  g
}



plotStateHistory <- function(sc, max.time=NULL) {
  Sides <- c("L", "R")
  sy <- list(L = 1, R = 2)
  dy <- c(L = -0.4, R = 0.4, none=0)
  
  sh <- sc$state.history
  sh <- sh[sh$state != "none",]
  sh <- droplevels(sh)
  sh$state <- factor(sh$state, levels=c("endon", "lateral", "dual"))
  if(is.null(max.time)) max.time <- max(sh$end)
  
  g <- ggplot() +
    theme_classic() +
    theme(panel.background = element_rect(fill="grey90"), axis.line = element_blank()) +
    scale_y_continuous(breaks = 1:2, labels=c("L", "R")) +
    xlim(0, max.time) +
    scale_fill_manual(values=cbPalette) +
    labs(x="Time (min)", y="Kinetochore")
  
  for(side in Sides) {
    s <- sh[sh$KT.side == side, ]
    df <- data.frame(
      xmin = s$start,
      xmax = s$end,
      ymin = sy[[side]],
      ymax = sy[[side]] + dy[as.character(s$spindle)],
      state = s$state
    )
    g <- g + geom_rect(data=df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), colour="black", alpha=0.8)
  }
  g <- g + geom_hline(yintercept = c(1,2))
  g
}
