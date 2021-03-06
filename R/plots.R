
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


plotSisterChromatids <- function(sc, dx=0.1, dy=0.08, title=NULL) {
  SIDES <- c("L", "R")
  KTS <- c("A", "B")
  
  spndl <- data.frame(
    x = c(-1, 1),
    y = c(0.1, 0.1)
  )
  rownames(spndl) <- SIDES
  
  kts <- data.frame(
    x = c(-0.2, 0.2),
    y = c(0, 0)
  )
  rownames(kts) <- KTS

  # find empty spindles
  emptys <- list("L"=TRUE, "R"=TRUE)
  # build a list of MT coordinates  
  mts <- list()
  for(id in KTS) {
    KT <- sc$KT[[id]]
    if(KT$spindle != "none") {
      emptys[[KT$spindle]] <- FALSE
      p1 <- spndl[KT$spindle, ]
      p2 <- kts[id, ]
      if(KT$contact %in% c("lateral", "ambilink", "synlink")) p2 <- p2 + c(-p1$x * dx, -dy)
      p <- rbind(p1, p2)
      mts <- push(mts, p)
      if(KT$contact %in% c("ambilink", "synlink", "biend")) {
        p <- rbind(spndl[KT$link.spindle, ], kts[id, ])
        if(KT$contact == "biend") p <- p + c(0, -0.02)
        mts <- push(mts, p)
      }
    }
  }
  for(side in SIDES) {
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
  if(!is.null(title)) g <- g + annotate("text", label=title, x=-1.2, y=0.5, hjust=0, vjust=1)
  
  g
}



plotStateHistory <- function(sc, max.time=NULL) {
  KTS <- c("A", "B")
  sy <- list(A = 1, B = 2)
  dy <- c(L = -0.4, R = 0.4, none=0)
  
  sh <- sc$state.history
  sh <- sh[sh$state != "none",]
  sh <- droplevels(sh)
  sh$state <- factor(sh$state, levels=c("endon", "lateral", "ambilink", "synlink", "biend"))
  if(is.null(max.time)) max.time <- max(sh$end)
  
  g <- ggplot() +
    theme_classic() +
    theme(panel.background = element_rect(fill="grey90"), axis.line = element_blank()) +
    scale_y_continuous(breaks = 1:2, labels=c("A", "B")) +
    xlim(0, max.time) +
    scale_fill_manual(values=cbPalette) +
    labs(x="Time (min)", y="Kinetochore")
  
  P <- lapply(KTS, function(id) {
    s <- sh[sh$KT.id == id, ]
    df <- data.frame(
      xmin = s$start,
      xmax = s$end,
      ymin = sy[[id]],
      ymax = sy[[id]] + dy[as.character(s$spindle)],
      state = s$state
    )
  })
  df <- do.call(rbind, P)
  g <- g + geom_rect(data=df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), colour="black", alpha=0.8)
  g <- g + geom_hline(yintercept = c(1,2))
  g
}


logScale <- function(show=2) {
  ticks <- 2:10
  # define the OOMs (orders of magnitudes)
  ooms <- 10^(-3:2)
  breaks <- as.vector(ticks %o% ooms)
  
  # select the labels to show
  if(show == 2) {
    show.labels <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  } else if(show == 5) {
    show.labels <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  } else if(show == 10) {
    show.labels <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  }
  labels <- as.character(breaks * show.labels)
  labels <- gsub("^0$", "", labels)
  list(labels=labels, breaks=breaks)
}

plotParameterMedianTime <- function(sim, parnames) {
  P <- lapply(parnames, function(m) {
    df <- data.frame(x=as.factor(sim[, m]), y=sim$median.time)
    s <- logScale()
    ggplot(df, aes(x, y)) +
      theme_classic() +
      geom_boxplot(fill="moccasin", outlier.shape=NA, coef=0) +
      geom_jitter(width=0.1, height=0) +
      labs(x=m, y="Median time (min)") +
      scale_y_log10(labels=s$labels, breaks=s$breaks) +
      #annotation_logticks(sides = "l") +
      theme(panel.grid.major.y = element_line(color="grey80")) +
      xlab(paste(m, "rate"))
  })
  grid.arrange(grobs=P, ncol=3)
}

plotBestTime <- function(selsim, par, ylim=c(NA,NA)) {
  s <- logScale()
  selsim[[par]] <- as.factor(selsim[[par]])
  ggplot(selsim, aes_string(par, "time")) +
    theme_classic() +
    geom_boxplot(fill="moccasin", outlier.shape=NA) +
    #geom_jitter(width=0.1, height=0) +
    labs(x=par, y="Median time (min)") +
    scale_y_log10(labels=s$labels, breaks=s$breaks, limits=ylim) +
    theme(panel.grid.major.y = element_line(color="grey80")) 
}


plotFCsumDet <- function(sim) {
  sim$fc <- 1/sim$formation + 1/sim$conversion
  sim$detachment <- as.factor(sim$detachment)
  s <- logScale()
  ggplot(sim) +
    theme_classic() +
    geom_point(aes(x=fc, y=median.time, colour=detachment)) +
    geom_line(aes(x=fc, y=median.time, colour=detachment)) +
    scale_y_log10(labels=s$labels, breaks=s$breaks) +
    annotation_logticks(sides = "l") +
    theme(panel.grid.major.y = element_line(color="grey80")) +
    xlab(expression(1/R[form]~+~1/R[conv]~(min))) +
    ylab("Median time (min)") +
    labs(colour = substitute(R[det]~(min^-1))) +
    scale_color_viridis(discrete=TRUE)
}

shorten <- list(
  formation = "form",
  conversion = "conv",
  detachment = "det",
  knockoff = "ko",
  replacement = "rep"
)

rlab <- function(s) {
  t <- 
  s <- t[[s]]
  substitute(R[x]~(min^-1), list(x=t[[s]]))
}


plotTwoPar <- function(sim, par.x, par.y, par.f, val.f, val="median.time", val.name="Median time (min)", ylim=c(NA,NA), log.scale=TRUE) {
  sim <- sim[sim[[par.f]] == val.f, ]
  sim[[par.y]] <- as.factor(sim[[par.y]])
  s <- logScale()
  g <- ggplot(sim) +
    theme_classic() +
    geom_point(aes_string(x=par.x, y=val, colour=par.y)) +
    geom_line(aes_string(x=par.x, y=val, colour=par.y)) +
    theme(panel.grid.major.y = element_line(color="grey80")) +
    xlab(substitute(R[x]~(min^-1), list(x=shorten[[par.x]]))) +
    ylab(val.name) +
    scale_color_viridis(discrete=TRUE) +
    ggtitle(substitute(R[p] == v~min^-1, list(p=shorten[[par.f]], v=val.f))) +
    labs(colour = substitute(R[x]~(min^-1), list(x=shorten[[par.y]])))
  if(log.scale) {
    g <- g + 
      scale_y_log10(labels=s$labels, breaks=s$breaks, limits=ylim) +
      annotation_logticks(sides = "l")
  }
  g
}

plotTwoHeat<- function(sim, par.x, par.y, par.f, val.f, val="median.time", val.name="Median time (min)", midpoint=NULL, limits=NULL, gtext.size=3, with.legend=TRUE) {
  sim <- sim[sim[[par.f]] == val.f, ]
  sim[[par.x]] <- as.factor(sim[[par.x]])
  sim[[par.y]] <- as.factor(sim[[par.y]])
  if(is.null(limits)) limits <- c(min(sim[[val]]), max(sim[[val]]))
  if(is.null(midpoint)) midpoint <- mean(limits)
  sim$text.colour <- "black"
  sim[sim[[val]] < midpoint, "text.colour"] <- "white"
  sim$lab <- signif(sim[[val]], 2)
  g <- ggplot(sim) +
    geom_tile(aes_string(x=par.x, y=par.y, fill=val)) +
    xlab(substitute(R[x]~(min^-1), list(x=shorten[[par.x]]))) +
    ylab(substitute(R[y]~(min^-1), list(y=shorten[[par.y]]))) +
    labs(fill=val.name) +
    #scale_fill_viridis() +
    scale_fill_gradientn(colours=c("blue4", "gold", "red"), values=c(0, midpoint, 1), limits=limits) +
    ggtitle(substitute(R[p] == v~min^-1, list(p=shorten[[par.f]], v=val.f))) +
    geom_text(aes_string(x=par.x, y=par.y, label="lab"), size=gtext.size, colour="palegreen")
  if(!with.legend) g <- g + theme(legend.position = "none")
  g
}


plotDistributionGrid <- function(sim, grid.par, fixed.par, value="time", xlab="Time (min)", xlim=c(0.2, 500), bins=50) {
  par.x <- names(grid.par[1])
  par.y <- names(grid.par[2])
  par.f <- names(fixed.par)
  range.x <- grid.par[[1]]
  range.y <- grid.par[[2]]
  val.f <- fixed.par[[1]]
  s <- sim[sim[[par.x]] %in% range.x & sim[[par.y]] %in% range.y & sim[[par.f]] == val.f,]
  s$val <- s[[value]]
  m <- s %>% group_by_(par.x, par.y) %>% summarise(median=median(val))
  m$smedian = sprintf("%.2g", m$median)
  
  sc <- logScale(show=10)
  
  ggplot(s, aes(x=val, y=..density..)) +
    geom_histogram(bins=bins) +
    facet_grid(reformulate(par.x, par.y)) +
    ggtitle(paste0("x = ", par.x, " rate, y = ", par.y, " rate, ", par.f, " rate = ", val.f)) +
    labs(x=xlab, y="Density") +
    geom_vline(data=m, aes(xintercept=median), colour="orange2") +
    scale_x_log10(labels=sc$labels, breaks=sc$breaks, limits=xlim)
    #geom_text(data=m, aes(x=xlim[1], y=0.9, label=smedian))
}

plotDistributionPar <- function(selsim, xlim=c(0.2, 500), ymax.log=NA, ymax.lin=NA, bins=50, show=10, lines=NULL, line.colour="lightblue3") {
  sc <- logScale(show=show)
  med <- median(selsim$time)
  
  g1 <- ggplot(selsim, aes(x=time, y=..density..)) +
    theme_classic() +
    geom_histogram(bins=bins) +
    labs(x="Time (min)", y="Density") +
    scale_x_log10(labels=sc$labels, breaks=sc$breaks, limits=xlim) +
    ylim(0, ymax.log)
  g2 <- ggplot(selsim, aes(x=time, y=..density..)) +
    theme_classic() +
    geom_histogram(bins=bins) +
    labs(x="Time (min)", y="Density") +
    scale_x_continuous(limits=xlim) +
    ylim(0, ymax.lin)
  if(!is.null(lines)) {
    g1 <- g1 + geom_vline(xintercept=lines, colour=line.colour, linetype="dotted")
    g1 <- g1 + geom_vline(xintercept=med, colour="orange3")
    g2 <- g2 + geom_vline(xintercept=lines, colour=line.colour, linetype="dotted")
    g2 <- g2 + geom_vline(xintercept=med, colour="orange3")
  }
  grid.arrange(g1, g2, ncol=2)
}

plotDistributionPar2 <- function(s1, s2, xlim=c(NA, NA), bins=50, show=10, alpha=0.5, lines=NULL, line.colour="lightblue3") {
  sc <- logScale(show=show)
  
  s1$model <- "M1"
  s2$model <- "M2"
  cols <- c("model", "time")
  s <- rbind(s1[, cols], s2[, cols])

  
  g1 <- ggplot(s, aes(x=time, y=..density.., fill=model)) +
    theme_classic() +
    geom_histogram(bins=bins, alpha=alpha, position="identity") +
    labs(x="Time (min)", y="Density") +
    scale_x_log10(labels=sc$labels, breaks=sc$breaks, limits=xlim) +
    scale_fill_manual(values=cbPalette) +
    theme(legend.position="none")
  g2 <- ggplot(s, aes(x=time, y=..density.., fill=model)) +
    theme_classic() +
    geom_histogram(bins=bins, alpha=alpha, position="identity") +
    labs(x="Time (min)", y="Density") +
    scale_x_continuous(limits=xlim) +
    scale_fill_manual(values=cbPalette)
  if(!is.null(lines)) {
    g1 <- g1 + geom_vline(xintercept=lines, colour=line.colour, linetype="dotted")
    #g1 <- g1 + geom_vline(xintercept=med, colour="orange3")
    g2 <- g2 + geom_vline(xintercept=lines, colour=line.colour, linetype="dotted")
    #g2 <- g2 + geom_vline(xintercept=med, colour="orange3")
  }
  grid.arrange(g1, g2, ncol=2)
}


defparPlot <- function(deftime) {
  brks <- c(0.001, 0.01, 0.1, 1, 10, 100)
  ggplot(deftime, aes(time)) +
    geom_histogram(bins=60) +
    facet_grid(. ~ set) +
    scale_x_log10(breaks=brks, labels=brks, limits=c(0.1, 100)) +
    labs(x = "Time (min)", y = "Count")
}

defparRidgePlot <- function(deftime, log.scale=TRUE, limits=NULL) {
  brks <- c(0.001, 0.01, 0.1, 1, 10, 100)
  deftime$set <- as.factor(deftime$set)
  g <- ggplot(deftime, aes(x=time, y=model, fill=set)) +
    geom_density_ridges(alpha=0.3, color="white", scale=0.95) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_manual(values=c("#ff0000", "#0000ff")) +
    labs(x = "Time (min)", y = "Model") +
    theme_ridges(center_axis_labels = TRUE)
  if(log.scale) g <- g + scale_x_log10(breaks=brks, labels=brks, limits=c(0.1, 100))
  if(!is.null(limits)) g <- g + xlim(limits)
  g
}

pdfFigure <- function(g, file, width=NA, height=NA) {
  pfile <- paste0(pdf.dir, file)
  ggsave(pfile, plot=g, device="pdf", width=width, height=height, units="in")
  
  link <- paste0("[PDF](", public_html, "pdf/", file, ")") 
}

figData <- function(d, file) {
  dfile <- paste0(figdata.dir, file)
  write.csv(d, dfile, quote=FALSE, sep=",", row.names=FALSE)
  
  link <- paste0("[source data](", public_html, "figdata/", file, ")") 
}

