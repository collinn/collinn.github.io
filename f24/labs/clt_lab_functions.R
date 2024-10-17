## Functions
sampleNormalData <- function(n, mu, sig, samples = 1000L) {
  xbar <- replicate(samples, {
    mean(rnorm(n, mu, sig))
  })
  data.frame(xbar = xbar, sample = seq_len(samples))
}

getSampleMean <- function(data, variable, n, samples = 1000L) {
  ## Check is var name or character
  var <- substitute(variable)
  if (is.name(var)) {
    var <- as.character(var)
  }
  
  xbar <- replicate(samples, {
    idx <- sample(1:nrow(data), size = n, replace = FALSE)
    x <- data[idx, ][[var]]
    mean(x)
  })
  data.frame(xbar = xbar, sample = seq_len(samples))
}



simulateConfInt <- function(n = 20, numSD = 2, sd = 5, N = 25) {
  x <- rnorm(n, 50, sd)
  sp <- numSD
  
  mm <- vector("numeric",  length = N)
  vv <- vector("numeric",  length = N)
  
  for (i in seq_len(N)) {
    xb <- rnorm(n, 50, sd)
    mm[i] <- mean(xb)
    vv[i] <- sd(xb)
  }
  
  df <- data.frame(sim = seq_len(N), mean = mm, sd = vv / sqrt(n))
  df <- mutate(df, ci = sign(mm - sp*sd - 50) != sign(mm + sp*sd - 50))
  
  df$ci <- factor(df$ci, levels = c(TRUE, FALSE))
  
  mmin <- with(df, min(35, mean - sp*sd - 1))
  mmax <- with(df, max(65, mean + sp*sd + 1))
  
  ggplot(df, aes(sim, mean, color = ci)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 50) + ylim(mmin, mmax) +
    geom_errorbar(width = 0.05, aes(ymin = mean - sp*sd,
                                    ymax = mean + sp*sd)) +
    labs(x = "Simulation", y = "Sample Mean Interval") +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = c(`TRUE` = "#00BFC4", `FALSE` = "#F8766D")) 
}