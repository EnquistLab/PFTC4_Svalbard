
trait.flex.anova <-
  function(formula, specif.avg, const.avg, ...) 
  {
    # Formula into string form
    form.string<-deparse(substitute(formula))
    
    # Check formula parameter
    form.parts<-unlist(strsplit(form.string,"~"))
    if(length(form.parts) != 2)
      stop("First parameter must be valid one-sided formula, like ~A*B");
    if(nchar(form.parts[1])>0)
      warning("Left side of the formula was ignored!");
    
    # The two average variables into string form
    spec.av <- deparse(substitute(specif.avg))
    cons.av <- deparse(substitute(const.avg))
    
    test.has.onelevel<-function(aov.summ)
    {
      (length(aov.summ) == 1);
    }
    
    test.has.resid<-function(aov.one)
    {
      if(class(aov.one)[1] != "anova")
        warning("specified object is not aov result!");
      nrows <- dim(aov.one)[1]
      if(nrows == 0)
        return( FALSE);
      last.row.lbl <- dimnames(aov.one)[[1]][nrows]
      if(unlist(strsplit(last.row.lbl, " "))[1] != "Residuals")
        return( FALSE);
      if(dim(aov.one)[2] < 5)  # P and F are missing
        return( FALSE);
      TRUE;
    }
    
    # Specific averages ANOVA
    form.1 <- as.formula(
      paste(spec.av,form.parts[2],sep="~"))
    res.1 <- summary( aov(form.1,...))
    if(test.has.onelevel( res.1) == FALSE)
      stop("Cannot evaluate ANOVAs with multiple error levels!")
    res.1 <- res.1[[1]]
    if(test.has.resid( res.1) == FALSE)
      stop("No residual DFs left, cannot continue");
    
    # Constant averages ANOVA
    form.2 <- as.formula(
      paste(cons.av,form.parts[2],sep="~"))
    # no need to test for multilevels by now
    res.2 <- summary( aov(form.2,...))[[1]]
    if(test.has.resid( res.2) == FALSE)
      stop("No residual DFs left in constant averages ANOVA, cannot continue");
    
    
    # Now the differences:
    spec.const.diff <- paste("I(", spec.av, "-", cons.av, ")", sep="")
    
    form.3 <- as.formula(
      paste(spec.const.diff,form.parts[2],sep="~"))
    # no need to test for multilevels by now
    res.3 <- summary( aov(form.3,...))[[1]]
    if(test.has.resid( res.3) == FALSE)
      stop("No residual DFs left in (specif-const) ANOVA, cannot continue");
    
    
    if((dim(res.1) != dim(res.2)) || (dim(res.1) != dim(res.3)))
      stop("Tables from the three ANOVAs have incompatible sizes");
    
    # Create sum of squares table: add SS(Tot) except for null models    
    nrows <- dim(res.1)[1]
    ss.turn <- res.2[,2]
    ss.var  <- res.3[,2]
    ss.tot  <- res.1[,2]
    ss.covar<- ss.tot - ss.turn - ss.var
    ss.row.names <- dimnames(res.1)[[1]]
    if(nrows > 1)
    {
      ss.turn <- c(ss.turn, sum(ss.turn))
      ss.var  <- c(ss.var,  sum(ss.var))
      ss.tot  <- c(ss.tot,  sum(ss.tot))
      ss.covar<- c(ss.covar,sum(ss.covar))
      ss.row.names <- c(ss.row.names, "Total")
      nrows   <- nrows + 1
    }
    else
    {
      # replace row title
      ss.row.names[1] <- "Total"
    }
    SS.tab <- data.frame( Turnover = ss.turn, Intraspec. = ss.var,
                          Covariation = ss.covar, Total = ss.tot,
                          row.names = ss.row.names)
    # Calculate relative fractions
    TotalSS <- SS.tab[nrows, 4] # lower right corner
    SS.tab.rel <- SS.tab / TotalSS
    
    # Collect significances
    if(nrows > 1)  # get rid of the "Total" label again
      ss.row.names <- ss.row.names[-nrows]
    P.tab <- data.frame( Turnover = res.2[,5], Intraspec. = res.3[,5],
                         Total = res.1[,5], row.names = ss.row.names)
    
    res <- list( SumSq=SS.tab, RelSumSq=SS.tab.rel, Pvals=P.tab, 
                 anova.turnover=res.2, anova.total=res.1, anova.diff=res.3)
    class(res)<- "trait.flex"
    res
  }


print.trait.flex <-
  function(x,...) 
  {
    op <- options();
    cat("\n Decomposing trait sum of squares into composition turnover\n");
    cat(  " effect, intraspecific trait variability, and their covariation:\n");
    options(digits=5)
    print(x$SumSq);
    cat("\n Relative contributions:\n");
    options(digits=4)
    print(x$RelSumSq)
    nPvals <- dim(x$Pvals)[1]
    if(nPvals > 1)
    {
      cat("\n Significance of testable effects:\n");
      options(digits=5)
      print(x$Pvals[-nPvals,]);
    }
    options(op)
    invisible(x)
  }


plot.trait.flex <-
  function( x, plot.total = FALSE, use.percentage = FALSE,
            plot.covar = FALSE,
            legend.pos = if(plot.total) "topleft" else "topright", ...) 
  { 
    if(use.percentage)
      SumSq <- 100 * x$RelSumSq
    else
      SumSq <- x$SumSq
    if(legend.pos == "none")
      legend.txt <- NULL
    else
      legend.txt <- colnames(SumSq)[-4]
    
    nrows <- dim(SumSq)[1]
    plot.tab <- as.matrix(SumSq)
    
    if(nrows > 1)
    {
      if(plot.covar)
      {
        if(plot.total)
          plot.tab <- plot.tab[,-4]
        else
          plot.tab <- plot.tab[-nrows,-4]
      }
      else
      {
        if(plot.total)
          plot.tab <- plot.tab[,1:2]
        else
          plot.tab <- plot.tab[-nrows, 1:2]   
        if(legend.pos != "none")
          legend.txt <- legend.txt[1:2]
      }
      if(plot.total)
        ymax <- max(SumSq[,4]) * 1.2
      else
        ymax <- max(SumSq[-nrows,4]) * 1.2
    }
    else
    {
      ymax <- SumSq[1,4] * 1.2
      legend.pos <- "none"
      legend.txt <- NULL
    }
    # get pretty maximum value
    ymax <- pretty( c(0,ymax)) # vector of tickmark positions
    ymax <- ymax[length(ymax)] # take the last entry
    if(use.percentage)
    {
      if(max(SumSq[,4]) < 100.1)  # make sure that percentage range is not exceeded when no reason
      {
        if(ymax > 100)
          ymax = 100
      }
      xpos <- barplot( t(plot.tab[,]), ylim = c(0,ymax),
                       ylab = "Explained variation (%)", ...) 
    }
    else
      xpos <- barplot( t(plot.tab[,]), ylim = c(0,ymax),
                       ylab = "Sum of squares of analysed trait", ...) 
    
    if(nrows > 1)
    {
      if(!plot.covar)
      { if(length(xpos) > 1)
        line.half <- 0.4*(xpos[2] - xpos[1])
      else
        line.half <- 0.4*(xpos[1])
      segments( xpos-line.half, SumSq[,4],
                xpos+line.half, SumSq[,4], lwd=3)
      if(legend.pos != "none")
      { NL <- length(legend.txt)       
      legend( legend.pos, legend=c(legend.txt,"Total"),
              fill=c(gray.colors(NL), NA), 
              lty=c(rep( 0,NL),1),
              lwd=c(rep( 0,NL),3))
      }
      }
      else
      {
        if(legend.pos != "none")
          legend( legend.pos, legend=legend.txt, 
                  fill=gray.colors(length(legend.txt)))
      }
    }
  }


example.trait.flex <- function()
{
  # create example data frame
  height.trait<- data.frame(MOWING=as.factor(c(0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0)))
  height.trait$FERTIL <- as.factor(c(0,1,0,1,1,0,1,0,1,0,1,0))
  height.trait$specific <- c( 58.16354, 62.34342, 31.43701, 62.14333, 51.98859,
                              29.95968, 55.48009, 50.68146, 51.75618, 31.13289, 47.53024, 56.44128)
  height.trait$nonspec<- c( 47.93985, 57.09998, 43.06760, 51.58106, 44.52435,
                            40.85160, 50.85945, 44.48371, 43.20859, 43.92655, 45.15222, 47.83641)
  
  # calculate decomposition of total trait variance
  
  x1<-trait.flex.anova(~1, specific, nonspec, data=height.trait)
  print(x1)
  par(ask=TRUE)
  plot(x1)
  
  # calculate decomposition of the factorial model
  x2<-trait.flex.anova(~MOWING*FERTIL, specific, nonspec, data=height.trait)
  print(x2)
  plot(x2)
  plot(x2, legend.pos="none")
  plot(x2, plot.total=TRUE)
  plot(x2, plot.covar=TRUE)
  # and now the relative values ...
  
  plot(x1, use.percentage=T)
  plot(x2, use.percentage=T)
  plot(x2, legend.pos="none", use.percentage=T)
  plot(x2, plot.total=TRUE, use.percentage=T)
  plot(x2, plot.covar=TRUE, use.percentage=T, main="Height")
  par(ask=FALSE)
}