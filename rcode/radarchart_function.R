### ----------------------------------------------------------
### Code for
### "Really Useful Synthetic Data -- 
###  A Framework to Evaluate the Quality of Differentially Private Synthetic Data"
###
### https://arxiv.org/abs/2004.07740
###
### Christian Arnold & Marcel Neunhoeffer
###
### This file produces the radar chart with fake metrics.
###
### ----------------------------------------------------------

# Last tested 27 May 2020 on:
### ----------------------------------------------------------
# platform       x86_64-apple-darwin15.6.0
# arch           x86_64
# os             darwin15.6.0
# system         x86_64, darwin15.6.0
# status
# major          3
# minor          6.1
# year           2019
# month          07
# day            05
# svn rev        76782
# language       R
# version.string R version 3.6.1 (2019-07-05)
# nickname       Action of the Toes
### ----------------------------------------------------------
# RStudio       1.2.1335

# Save package names as a vector of strings
pkgs <-
  c("viridis")


# Install needed packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

# Load all packages 
print(sapply(pkgs, require, character.only = TRUE))


# Adapt the radarchart function from fmsb package
radarchart_col <- function(df,
                           axistype = 0,
                           seg = 4,
                           pty = 16,
                           pcol = 1:8,
                           plty = 1:6,
                           plwd = 1,
                           pdensity = NULL,
                           pangle = 45,
                           pfcol = NA,
                           cglty = 3,
                           cglwd = 1,
                           cglcol = "navy",
                           axislabcol = "blue",
                           title = "",
                           maxmin = TRUE,
                           na.itp = TRUE,
                           centerzero = FALSE,
                           vlabels = NULL,
                           vlcex = NULL,
                           caxislabels = NULL,
                           calcex = NULL,
                           paxislabels = NULL,
                           palcex = NULL,
                           color_categories = NULL,
                           ...) {
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(
    c(-1.2, 1.2),
    c(-1.2, 1.2),
    type = "n",
    frame.plot = FALSE,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title,
    asp = 1,
    ...
  )
  
  theta <- seq(90, 450, length = n + 1) * pi / 180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  centerzero <- F
  CGap <- ifelse(centerzero, 0, 1)
  
  if (!is.null(color_categories)) {
    if (length(color_categories) != n) {
      cat("The color categories must be of same length as n.\n")
      return()
    }
    for (col_pol in 1:(n - 1)) {
      polygon(
        c((xx / (seg + CGap))[col_pol], xx[col_pol], xx[col_pol + 1], (xx / (seg + CGap))[col_pol +
                                                                                            1]),
        c((yy / (seg + CGap))[col_pol], yy[col_pol], yy[col_pol + 1], (yy / (seg + CGap))[col_pol +
                                                                                            1]),
        border = NA,
        col = adjustcolor(color_categories[col_pol], alpha = 0.5)
      )
    }
    cat(col_pol)
    polygon(
      c((xx / (seg + CGap))[n], xx[n], xx[1], (xx / (seg + CGap))[1]),
      c((yy / (seg + CGap))[n], yy[n], yy[1], (yy / (seg + CGap))[1]),
      border = NA,
      col = adjustcolor(color_categories[n], alpha = 0.5)
    )
  }
  
  for (i in 0:seg) {
    polygon(
      xx * (i + CGap) / (seg + CGap),
      yy * (i + CGap) / (seg +
                           CGap),
      lty = cglty,
      lwd = cglwd,
      border = cglcol
    )
    if (axistype == 1 | axistype == 3)
      CAXISLABELS <- paste(i / seg * 100, "(%)")
    if (axistype == 4 | axistype == 5)
      CAXISLABELS <- sprintf("%3.2f", i / seg)
    if (!is.null(caxislabels) & (i < length(caxislabels)))
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | axistype ==
        5) {
      if (is.null(calcex))
        text(-0.05, (i + CGap) / (seg + CGap), CAXISLABELS,
             col = axislabcol)
      else
        text(-0.05,
             (i + CGap) / (seg + CGap),
             CAXISLABELS,
             col = axislabcol,
             cex = calcex)
    }
  }
  if (centerzero) {
    arrows(
      0,
      0,
      xx * 1,
      yy * 1,
      lwd = cglwd,
      lty = cglty,
      length = 0,
      col = cglcol
    )
  }
  else {
    arrows(
      xx / (seg + CGap),
      yy / (seg + CGap),
      xx * 1,
      yy *
        1,
      lwd = cglwd,
      lty = cglty,
      length = 0,
      col = cglcol
    )
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels))
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex))
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol,
           cex = palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels))
    VLABELS <- vlabels
  if (is.null(vlcex))
    text(xx * 1.2, yy * 1.2, VLABELS)
  else
    text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <-
      CGap / (seg + CGap) + (df[i, ] - df[2, ]) / (df[1, ] - df[2, ]) * seg /
      (seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 1)
            }
            xxleft <- xx[left] * CGap / (seg + CGap) +
              xx[left] * (df[i, left] - df[2, left]) / (df[1,
                                                           left] - df[2, left]) * seg /
              (seg + CGap)
            yyleft <- yy[left] * CGap / (seg + CGap) +
              yy[left] * (df[i, left] - df[2, left]) / (df[1,
                                                           left] - df[2, left]) * seg /
              (seg + CGap)
            xxright <- xx[right] * CGap / (seg + CGap) +
              xx[right] * (df[i, right] - df[2, right]) / (df[1,
                                                              right] - df[2, right]) * seg /
              (seg + CGap)
            yyright <- yy[right] * CGap / (seg + CGap) +
              yy[right] * (df[i, right] - df[2, right]) / (df[1,
                                                              right] - df[2, right]) * seg /
              (seg + CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright *
                                 xxleft) / (yy[j] * (xxright - xxleft) - xx[j] *
                                              (yyright - yyleft))
            yys[j] <- (yy[j] / xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap / (seg + CGap) + xx[j] *
            (df[i, j] - df[2, j]) / (df[1, j] - df[2, j]) *
            seg / (seg + CGap)
          yys[j] <- yy[j] * CGap / (seg + CGap) + yy[j] *
            (df[i, j] - df[2, j]) / (df[1, j] - df[2, j]) *
            seg / (seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(
          xxs,
          yys,
          lty = pltys[i - 2],
          lwd = plwds[i -
                        2],
          border = pcols[i - 2],
          col = pfcols[i -
                         2]
        )
      }
      else {
        polygon(
          xxs,
          yys,
          lty = pltys[i - 2],
          lwd = plwds[i -
                        2],
          border = pcols[i - 2],
          density = pdensities[i -
                                 2],
          angle = pangles[i - 2],
          col = pfcols[i -
                         2]
        )
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2],
             col = pcols[i - 2])
    }
  }
}
