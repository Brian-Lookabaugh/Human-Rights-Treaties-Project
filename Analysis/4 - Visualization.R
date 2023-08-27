############################################################################
##################--------------Visualization-------------##################
############################################################################

dev.off()

############################################################################
###############--------------Balance Scatterplot-------------###############
############################################################################

#####------Physical Violence------#####

png("Graphics/cbplot.phy.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,3),
    pty = "s")

balance_scatter(
  phy.mah.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  phy.mah.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.psm.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  phy.psm.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.psw.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  phy.psw.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  phy.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.17, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.5, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.83, outer = TRUE, cex = .8)

dev.off()

#####-----Torture-----#####
png("Graphics/cbplot.tor.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,3),
    pty = "s")

balance_scatter(
  tor.mah.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  tor.mah.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  tor.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  tor.psm.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  tor.psm.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  tor.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  tor.psw.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  tor.psw.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  tor.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.17, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.5, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.83, outer = TRUE, cex = .8)

dev.off()

#####-----Female Political Empowerment-----######
png("Graphics/cbplot.pol.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,3),
    pty = "s")

balance_scatter(
  pol.mah.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  pol.mah.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.psm.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  pol.psm.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.psw.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  pol.psw.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  pol.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.17, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.5, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.83, outer = TRUE, cex = .8)

dev.off()

#####-----Female Civil Liberties-----#####
png("Graphics/cbplot.civ.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,3),
    pty = "s")

balance_scatter(
  civ.mah.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  civ.mah.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.psm.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n"
)

balance_scatter(
  civ.psm.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.psw.1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = ""
)

balance_scatter(
  civ.psw.2,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  civ.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("One Year Lag",
      line = 0, at = 0.17, outer = TRUE, cex = .8)
mtext("Two Year Lag",
      line = 0, at = 0.5, outer = TRUE, cex = .8)
mtext("Three Year Lag",
      line = 0, at = 0.83, outer = TRUE, cex = .8)

dev.off()

#####-----Combined Scatterplot-----#####
png("Graphics/combined.scatter.png", width = 1600, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,4),
    pty = "s")

balance_scatter(
  phy.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
)

balance_scatter(
  tor.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.mah.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
)

balance_scatter(
  tor.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  pol.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  civ.psm.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  xaxt = "n",
  yaxt = "n"
)

balance_scatter(
  phy.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
)

balance_scatter(
  tor.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  pol.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

balance_scatter(
  civ.psw.3,
  xlim = c(0, 1),
  ylim = c(0, 1),
  data = final,
  covariates = c("polity", "milper", "cso", "t.bal"),
  main = "",
  x.axis.label = "",
  y.axis.label = "",
  yaxt = "n"
)

mtext(1,text = "Standardized Mean Difference \n Before Refinement",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n After Refinement",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("Physical \n Violence",
      line = -1.5, at = 0.125, outer = TRUE, cex = .8)
mtext("Torture",
      line = -1.5, at = 0.375, outer = TRUE, cex = .8)
mtext("Womens' Political \n Empowerment",
      line = -1.5, at = 0.625, outer = TRUE, cex = .8)
mtext("Womens' Civil \n Liberties",
      line = -1.5, at = 0.875, outer = TRUE, cex = .8)

dev.off()

############################################################################
###################--------------Line Plots-------------####################
############################################################################

png("Graphics/line.plot.png", width = 2300, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(3,4),
    pty = "s")

get_covariate_balance(phy.mah.3$att,
                      data = final,
                      covariates = c("p.vio"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      xaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(tor.mah.3$att,
                      data = final,
                      covariates = c("torture"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      xaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(pol.mah.3$att,
                      data = final,
                      covariates = c("w.pol.emp"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      xaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(civ.mah.3$att,
                      data = final,
                      covariates = c("w.civ.lib"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      xaxt = "n",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(phy.psm.3$att,
                      data = final,
                      covariates = c("p.vio"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(tor.psm.3$att,
                      data = final,
                      covariates = c("torture"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(pol.psm.3$att,
                      data = final,
                      covariates = c("w.pol.emp"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(civ.psm.3$att,
                      data = final,
                      covariates = c("w.civ.lib"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(phy.psw.3$att,
                      data = final,
                      covariates = c("p.vio"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(tor.psw.3$att,
                      data = final,
                      covariates = c("torture"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(pol.psw.3$att,
                      data = final,
                      covariates = c("w.pol.emp"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

get_covariate_balance(civ.psw.3$att,
                      data = final,
                      covariates = c("w.civ.lib"),
                      plot = TRUE,
                      ylim = c(-0.5, 0.5),
                      ylab = "",
                      legend = FALSE)
abline(v = 3, lty = "dotted")

mtext(1,text = "Time Until Treatment",
      line = 3.5,
      at = 0.52, outer = TRUE, cex = 1)
mtext(2, text = "Standardized Mean Difference \n in Outcome",
      line = 4, outer = TRUE)
mtext(2, text = "NN Matching \n (Mahalanobis)",
      line = 1.15, at = .82, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Matching",
      line = 1.15, at = .5, outer = TRUE,
      cex = .8)
mtext(2, text = "Propensity Score \n Weighting",
      line = 1.15, at = .16, outer = TRUE,
      cex = .8)
mtext("Physical \n Violence",
      line = -1.5, at = 0.125, outer = TRUE, cex = .8)
mtext("Torture",
      line = -1.5, at = 0.375, outer = TRUE, cex = .8)
mtext("Womens' Political \n Empowerment",
      line = -1.5, at = 0.625, outer = TRUE, cex = .8)
mtext("Womens' Civil \n Liberties",
      line = -1.5, at = 0.875, outer = TRUE, cex = .8)

dev.off()
