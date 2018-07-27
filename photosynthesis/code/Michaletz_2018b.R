# Michaletz, S.T. Evaluating the kinetic basis of plant growth from organs to ecosystems. New Phytologist.

# Notes S3 R code for fitting the Sharpe-Schoolfield model (TPCfitting_stm.R).

# The following code was used for Sharpe-Schoolfield model fits (Eqn (2)). This code was devleoped in the 
# laboratory of Samraat Pawar (s.pawar@imperial.ac.uk), and is provided here with his permission. The code
# was modified by Sean Michaletz (michaletz@email.arizona.edu).


###########################################################################################################################

# This code is meant to fit thermal performance curves using various 
# thermal performance curve (TPC) models. 

# The main function starts after model functions have been loaded. You 
# we need to specify the Data file, the Model you want to run 
# ("Boltzmann", "Schoolfield" or "all" if we want to run both). You also 
# need to specify if you want to save plots (PLOT=TRUE) (one figure will be 
# plot for each model) or if you are running both models we can choose the 
# Overplot=TRUE selection in case that you want a unique figure with both 
# models overlaid.

# Depending on if you want to use the Schoolfield model with explicit
# T_pk parameter or not you have to set the SchoolTPk as TRUE or FALSE

# Requires package minpack.lm for the NLLS (which implements the more 
# robust Levenberg-Marqualdt algorithm) and ggplot2 for plotting, neither 
# of which is not part of the R standard libraries -- Please install if 
# necessary

#### Tref is specified at the beginning as GlobalEnvironment, so change 
# the value here in case you need it.

### To call the function, all variables should be given (trait,ID,temper,species,traitName). If you do not have values for any of these variables (for instance species name or trait name), add a blank column for each to your dataset prior to run the code.


# Required packages...
library(ggplot2)
library(lattice)
library(minpack.lm)  ### The nls is now run under this package using the nlsLM instead of nls
library(sme)
library(truncnorm)

#############################
# F  U  N  C  T  I  O  N  S #
#############################

#### assign Tref as GlobalEnv
# Tref is the standardization temperature (in K). 
# This needs to be any value below the peak of the curve.
assign("Tref", Inf, envir = .GlobalEnv) ## Set to Inf if you want to remove the normalization, so 1/Tref will be 0 

#### Estimate STARTING VALUES for the nls

GetE <- function(tmp, rate, T.p, k=8.62e-5)
{
  # Estimate starting value for E, taking linear regression using the rise part
  # of the curve only.
  # ~~~ Parameters ~~~
  # tmp  : temperature data (in K).
  # rate : rate data corresponding to temperature above.
  # T.p  : temperature at which rate peaks, used as a cutoff point.
  # k    : Boltzmann constant.
  
  
  tmp.w <- which(tmp <= T.p)
  if (length(tmp.w) > 1 & length(unique(tmp[tmp.w]))>1)
  {
    m <- lm(log(rate[tmp.w]) ~ I(1 / (k * (tmp[tmp.w]))))
    return(abs(summary(m)$coefficients[2, c('Estimate', 'Std. Error')]))
  } else
  {
    return(c(0.7,2))  # Arbitrary estimate if we can't do regression.
  }
  
}

GetlnB0 <- function(tmp, rate)
{
  # Estimate starting value for the normalising constant.
  # ~~~ Parameters ~~~
  # tmp   : temperature data (in K).
  # rate  : rate data corresponding to temperature above.
  # T.ref : estimate normalising constant at this temperature (in K).
  
  if (min(tmp,na.rm=TRUE) > Tref)
  {
    return(log(min(rate[1],na.rm=TRUE)))
  } else
  {
    return(log(max(rate[which(tmp <= Tref)],na.rm=TRUE)))
  }
}


GetTpk <- function(tmp, rate)
{
  # Temperature at which the rate is maximised (estimate of T.peak).
  # ~~~ Parameters ~~~
  # tmp  : Temperature data (in K).
  # rate : Rate data corresponding to temperature above.
  
  return(max(tmp[which.max(rate)]))
}




###################### Boltzmann - Arrhenius model.
Boltzmann.Arrhenius <- function(lnB0, E, temp) {
  
  # Boltzmann's constant. Units imply that E is in eV.
  k <- 8.62e-5 
  
  # lnB0 is the normalization constant.  
  # E is the activation energy.
  # Tref is the standardization temperature (in K).
  
  calc <- lnB0 - E/k * (1/temp - 1/Tref)
  
  return(calc)
}

###################### Schoolfield type models ######################

# Schoolfield function runs two different schoolfield models, one with
# explicit T_pk parameter (then we need to set SchoolTpk as TRUE),
# or the original one.

Schoolfield <- function(lnB0, E, E_D, T_h, temp, SchoolTpk=TRUE)
{ 
  # PARAMETERS/INPUTS (all temperatures in Kelvin) -
  # Boltzmann's constant. Units imply that E is in eV.
  k <- 8.62e-5 
  # temp   : temperature values to evaluate function at (single, scalar or vector of values)
  # lnB0     : Normalisation constant (log transformed)
  # E      : Activation energy (> 0)
  # E_D    : High temperature de-activation energy (> 0) 
  # Tref   : Standardization (reference) temperature; set to 0 if not wanted. To do    # this the whole term 1/Tref should be 0, so Tref has to be set to Inf
  # T_h (if SchoolTpk=TRUE): Temperature at which trait reaches peak value (Tpk)    # T_h (if SchoolTpk=FALSE)   : High temperature at which have of the enzyme units, on average are inactiveated.
  
  
  if (SchoolTpk==TRUE) # Sharpe-Schoolfield model with explicit T_pk parameter
  {
    return(lnB0 + log(exp((-E/k) * ((1/temp)-(1/Tref)))/(1 + (E/(E_D - E)) * exp(E_D/k * (1/T_h - 1/temp)))))
    
  } else { # Standardised Sharpe-Schoolfield's model with low-temp inactivation term removed, slightly modified                                                                                                                     
    return(lnB0 + log(exp((-E/k) * ((1/temp) - (1/Tref)))/(1 + exp((E_D/k) * (1/T_h - 1/temp))))) }
  
}

####################################################################################
############################# M  A  I  N    C  O  D  E #############################
####################################################################################

TPCFit <- function(Data,trait,ID,temper,species,traitName,PLOT=TRUE,OverPLOT=FALSE,Model="Schoolfield",SchoolTpk=TRUE,rand.st=FALSE, n.rand=20){ 
  # MODEL can be also "Boltzmann" or "all" for both
  # If you do not have a column with species name or trait Name, add a blank column to your dataset for these two.
  
  # School can be TRUE (explicit Tpk parameter) or FALSE (Standardised Schoolfield)
  # rand.st    : boolean - use random starting values?
  # n.rand     : numeric - number of random starting values used (if
  #              `rand.st=TRUE`).
  
  if (OverPLOT==TRUE) PLOT <- FALSE
  
  
  # Loads Data
  curvespt <- Data
  curvespt$StandardisedTraitValue <- Data[,trait]
  curvespt$ConTemp <- Data[,temper]
  curvespt$FinalID <- Data[,ID]
  curvespt$Consumer <- Data[,species]
  curvespt$StandardisedTraitName <- Data[,traitName]
  
  curvespt$StandardisedTraitValue <- as.numeric(curvespt$StandardisedTraitValue)
  curvespt$FinalID <- as.character(curvespt$FinalID)
  
  # Merges ConTemp and AmbientTemp in the same column
  if (any(names(curvespt)=="AmbientTemp")) {
    NATemp <- which(is.na(curvespt$ConTemp))
    NAAmbientTemp <- which(!is.na(curvespt$AmbientTemp[NATemp]))
    curvespt$ConTemp[NATemp][NAAmbientTemp] <- curvespt$AmbientTemp[NATemp][NAAmbientTemp]}
  
  curvespt$ConTemp <- as.numeric(curvespt$ConTemp)
  
  # Transform temperatures to Kelvin and log-transform the
  # trait values.
  curvespt$K <- curvespt$ConTemp + 273.15
  
  
  ###################################################################
  # Create unique species/individual IDs, with a series of vectors. #
  ###################################################################
  
  # Initialize the current ID at 0. You'll see why later...
  current_ID <- 0
  
  # Initialize an empty vector of species IDs (as a number).
  id <- c()
  
  # Initialize an empty vector of species names.
  id_spp <- c()
  
  # Initialize an empty vector of processes (e.g., photosynthesis, respiration).
  id_process <- c()
  
  # Read each row of the data frame.
  for (k in 1:nrow(curvespt))
  {
    
    # If the ID of this row is different from the current ID...
    if ( current_ID != curvespt$FinalID[k])
    {
      
      # Change the current ID to the one found in this row.
      current_ID <- curvespt$FinalID[k]
      
      # Add information for this species to the 3 vectors 
      # that we initialized above.
      id <- c(id, current_ID)
      id_spp <- c(id_spp, curvespt$Consumer[k])
      id_process <- c(id_process, curvespt$StandardisedTraitName[k])
    }
  }
  
  # Initialize empty vectors to store the parameter estimates
  # that will be obtained.
  
  if (Model=="Boltzmann" | Model=="all")
  {
    E_boltz <- c()
    lnB0_boltz <- c()
    B0_boltz <- c()
    T_pk_boltz <- c()
    P_pk_boltz <- c()
    AIC_boltz <- c()
    r_sq_boltz <- c()
  }
  
  if (Model=="Schoolfield" | Model=="all")
  {
    lnB0_sch <- c()
    lnB0_SE_sch <- c()
    B0_sch <- c()
    E_sch <- c()
    E_SE_sch <- c()    
    E_D_sch <- c()
    E_D_SE_sch <- c()
    T_h_sch <- c()
    T_h_SE_sch <- c()
    T_pk_sch <- c()
    P_pk_sch <- c()
    AIC_sch <- c()
    r_sq_sch <- c()
    lnB0exp1_sch <- c()
    lnB0exp2_sch<- c()
    lnB0exp5_sch<- c()
    lnB0exp10_sch<- c()
  }
  
  if (Model=="all"){
    selected_model<- c()}
  
  P_pkBug <- c()
  lnB0Bug <- c()
  AftPk <-c()
  BefPk <- c()
  
  
  # Go through every single species ID and try to fit the the selected models.
  grDevices::pdf(paste("TPCFit_plots_", Model, ".pdf", sep="" ))
  for(i in 1:length(id)) 
  { 
    
    # Get only the part of the data that correspond to that particular ID.
    current_dataset <- curvespt[curvespt$FinalID == id[i],]
    
    
    ## If there are negative values, substract the minimum value. This value will be stored in the results table and the columns lnB0_Bug and PpkBug will be TRUE
    MinVal <- NA
    if (min(current_dataset$StandardisedTraitValue,na.rm=TRUE)<=0){
      MinVal <- min(current_dataset$StandardisedTraitValue)
      current_dataset$StandardisedTraitValue <-current_dataset$StandardisedTraitValue - MinVal
      current_dataset <-current_dataset[-which(current_dataset$StandardisedTraitValue==0),]}
    
    ## Calculates number of data points after and before the Tpk
    Order <- order(current_dataset$K)
    Max <- which.max(current_dataset$StandardisedTraitValue[Order])
    if (length(Max)>0){
      AftPk <- c(AftPk,(length(current_dataset$StandardisedTraitValue)-Max))
      BefPk <- c(BefPk,(Max-1))} else {
        AftPk <- c(AftPk,NA)
        BefPk <- c(BefPk,NA)} 
    
    # Runs only if we have at least 5 data points and more than 5 diff temperatures
    if (length(unique(current_dataset$StandardisedTraitValue))>=5 && length(unique(current_dataset$K))>=5)
    {
      
      # Estimate T.h as being approximately T.peak.
      T.h.st  <- GetTpk(tmp=current_dataset$K, rate=current_dataset$StandardisedTraitValue)
      E.st    <- GetE(tmp=current_dataset$K, rate=current_dataset$StandardisedTraitValue, T.p=T.h.st)
      lnB.st <- GetlnB0(tmp=current_dataset$K, rate=current_dataset$StandardisedTraitValue)
      
      if (Model=="Boltzmann" | Model=="all"){
        ###############################
        # Boltzmann - Arrhenius model #
        ###############################
        
        boltzmann_nls <- NA
        
        if (rand.st)
        {
          # Create randomised starting points.
          E.st.pe <- E.st[1]  # Slope value.
          T.h.st  <- c(T.h.st, rnorm(n.rand-1, mean=T.h.st, sd=15))
          # We need truncated normal to ensure we don't get negative values of E.
          E.st   <- c(E.st.pe, rtruncnorm(n.rand-1, a=0, b=Inf, mean=E.st[1], sd=2 * E.st[2]))
          B.st   <- exp(lnB.st)
          # Randomise on linear scale. Again, we don't want negative rates.
          lnB.st <- c(lnB.st, log(rtruncnorm(n.rand-1, a=0, b=Inf, mean=B.st, sd=B.st / 2)))
          
          # We'll select the best model using AICc. Many of these turn out to be
          # similar.
          aics.out <- rep(NA, n.rand)
          
          for (h in 1:n.rand)
          {
            
            # Try and fit the model.
            
            boltzmann_nls <- try(nlsLM(
              log(StandardisedTraitValue) ~ Boltzmann.Arrhenius(lnB0, E, temp = K),
              start = c(lnB0 = lnB.st[h], E = E.st[h]),
              lower=c(lnB0=-Inf, E=0),
              upper=c(lnB0=Inf,  E=Inf),
              control=list(minFactor=1 / 2^16, maxiter=1e4),
              data = current_dataset, 
              na.action=na.omit),
              silent=TRUE
            )
            
            if (class(boltzmann_nls) != 'try-error')
            {
              aics.out[h] <- AICc(boltzmann_nls)
            }
          }
          w <- which.min(aics.out)
          
          if (length(w) > 0)
          {
            boltzmann_nls <- try(nlsLM(
              log(StandardisedTraitValue) ~ Boltzmann.Arrhenius(lnB0, E, temp = K),
              start = c(lnB0 = lnB.st[w], E = E.st[w]),
              lower=c(lnB0=-Inf, E=0),
              upper=c(lnB0=Inf,  E=Inf),
              control=list(minFactor=1 / 2^16, maxiter=1e4),
              data = current_dataset, 
              na.action=na.omit),
              silent=TRUE
            )
          } else
          {
            boltzmann_nls <- NA
          }
        } else
        {
          
          
          boltzmann_nls <- try(nlsLM(
            log(StandardisedTraitValue) ~ Boltzmann.Arrhenius(lnB0, E, temp = K),
            start = c(lnB0 = lnB.st, E = E.st[1]),
            lower=c(lnB0=-Inf, E=0),
            upper=c(lnB0=Inf,  E=Inf),
            control=list(minFactor=1 / 2^16, maxiter=1e4),
            data = current_dataset, 
            na.action=na.omit),
            silent=TRUE
          )
          
        }
        
        # If fitting worked ...
        if(!is.na(boltzmann_nls[1])) 
        { 
          
          # Collect the parameter estimates...
          
          if (!is.na(MinVal)){ 
            if (Model=="Boltzmann") lnB0Bug <- c(lnB0Bug,TRUE) ## When both models are fit, lnB0Bug is attached in Schoolfield
          }else {
            if (Model=="Boltzmann") lnB0Bug <- c(lnB0Bug,FALSE)
          }
          lnB0_boltz <- c(lnB0_boltz, coef(boltzmann_nls)["lnB0"])
          E_boltz <- c(E_boltz, coef(boltzmann_nls)["E"])
          AIC_boltz<- c(AIC_boltz, AIC(boltzmann_nls))
          
          # Calculate the R squared value as: 1 - (rss/tss)
          rss <- sum((exp(predict(boltzmann_nls)) - 
                        current_dataset$StandardisedTraitValue)^2, 
                     na.rm = TRUE)
          tss <- sum(
            (current_dataset$StandardisedTraitValue - 
               mean(current_dataset$StandardisedTraitValue, na.rm = TRUE))^2, 
            na.rm = TRUE)
          
          if ( tss != 0 )
          {
            r_sq_boltz <- c(r_sq_boltz, 1 - (rss/tss))
          } else
          {
            r_sq_boltz <- c(r_sq_boltz, 1)
          }
          
          # Calculate the peak of the curve and its 
          # corresponding temperature value.
          curr_prediction <- predict(boltzmann_nls)
          for (j in 1:length(curr_prediction))
          {
            
            # If we found the maximum performance, exit the loop.
            if (curr_prediction[j] == max(curr_prediction))
            {
              break
            }
          }
          
          T_pk_boltz <- c(T_pk_boltz, current_dataset$K[j])
          P_pk_boltz <- c(P_pk_boltz, (curr_prediction[j]))
          
          if (!is.na(MinVal)){ ## Add MinVal if it was substracted
            if (Model=="Boltzmann") P_pkBug <- c(P_pkBug,TRUE) ## When both models are fit, PpkBug is attached in Schoolfield
            
          }else {
            if (Model=="Boltzmann") P_pkBug <- c(P_pkBug,FALSE)
          }
          
          
          
          
          #######################################
          # Plotting Boltzmann - Arrhenius' fit #
          #######################################
          
          # Create a name for the output file using:
          #	- the original id number
          #   - the species name
          #   - the model
          output_name <- paste(
            current_dataset$FinalID[1], 
            current_dataset$Consumer[1], 
            'Boltzmann_Arrhenius',
            sep = "_"
          )
          
          # Remove any characters that won't look good in a file name,
          # using a regular expression.
          output_name <- gsub("[^\\w|\\s](|)", "", output_name, perl=TRUE)
          # Convert spaces to underscores.
          output_name <- gsub("\\s+", "_", output_name, perl=TRUE)
          
          # CHANGE THIS to set an alternative output directory.
          outdir <- "./TPCFit_plots/"
          # Check if output dir exists; if not, make it
          ifelse(!dir.exists(file.path(getwd(), "TPCFit_plots")), dir.create(file.path(getwd(), "TPCFit_plots")), FALSE)
          
          # Generate predictions from the model fit...
          tmp_temps <- seq(min(
            floor(current_dataset$K)), 
            ceiling(max(current_dataset$K)
            ), length = 200)
          
          tmp_model <- exp(Boltzmann.Arrhenius(
            coef(boltzmann_nls)["lnB0"],
            coef(boltzmann_nls)["E"],
            tmp_temps
          ))
          
          ModelToPlotB <- data.frame(
            Temperature = tmp_temps - 273.15, 
            TraitValue = tmp_model
          )
          
          
          # Prepare the data points of the original values.
          DataToPlot <- data.frame(
            Temperature = current_dataset$K - 273.15, 
            TraitValue = current_dataset$StandardisedTraitValue
          )
          DataToPlot <- na.omit(DataToPlot)
          
          if (!is.na(MinVal)){
            tmp_model <- tmp_model+MinVal
            DataToPlot$TraitValue <- DataToPlot$TraitValue
          }
          
          #### If we want individual plots
          if (PLOT==TRUE) {
            # Plot!
            p <- ggplot() + geom_point(data = DataToPlot, aes(x = Temperature, 
                                                              y = TraitValue), size = 3, col = "black", bg = "lightcyan2", 
                                       alpha = 0.7, pch = 21) + 
              geom_line(data = ModelToPlotB, 
                        aes(x = Temperature, y = TraitValue), colour = "#1b9e77", 
                        lwd = 1.3) +
              #ggtitle(paste(current_dataset$Consumer[1])) +
              ggtitle(paste("curveID=", current_dataset$FinalID[1], ", ", current_dataset$Consumer[1], ", ", 
                            'Boltzmann-Arrhenius', sep="")) +
              xlab(expression(paste("Temperature (", degree, C, ")"))) + 
              ylab(current_dataset$StandardisedTraitName[1]) +
              theme_bw() + theme(plot.title = element_text(size = 12), 
                                 axis.title = element_text(size = 10)) +
              annotate("text", size = 3, label=             
                         paste("R^2 boltz=", sprintf("%.2f", r_sq_boltz[i]), "\nE boltz=", format(coef(boltzmann_nls)["E"], digits = 3),"\nAIC boltz=",format(AIC(boltzmann_nls),digits=3)), 
                       x = min(DataToPlot[, "Temperature"]),
                       y = mean(DataToPlot[, "TraitValue"]),
                       hjust=0,
                       fontface = 3)
            print(p)
            # Save it as a pdf file.
            #pdf_file <- paste(outdir, gsub("/|#", "", output_name), ".pdf", sep="")
            #ggsave(filename = pdf_file, plot = p, height = 4, width = 4.2)
          }
          
          
        } else # If fitting failed ...
        {
          # Populate the vectors with missing values.
          E_boltz <- c(E_boltz, NA)
          lnB0_boltz <- c(lnB0_boltz, NA)
          T_pk_boltz <- c(T_pk_boltz, NA)
          P_pk_boltz <- c(P_pk_boltz, NA)
          AIC_boltz <- c(AIC_boltz, NA)
          r_sq_boltz <- c(r_sq_boltz, NA)
          if (Model=="Boltzmann") { ## When both models are fit, this is attached in Schoolfield
            lnB0Bug <- c(lnB0Bug,NA)
            P_pkBug <- c(P_pkBug,NA)}
        }
      }
      
      if (Model=="Schoolfield" | Model=="all"){
        
        #####################
        # Schoolfield model #
        #####################
        
        
        if (rand.st)
        {
          # Create randomised starting points.
          E.st.pe <- E.st[1]  # Slope value.
          T.h.st  <- c(T.h.st, rnorm(n.rand-1, mean=T.h.st, sd=15))
          # We need truncated normal to ensure we don't get negative values of E.
          E.st   <- c(E.st.pe, rtruncnorm(n.rand-1, a=0, b=Inf, mean=E.st[1], sd=2 * E.st[2]))
          B.st   <- exp(lnB.st)
          # Randomise on linear scale. Again, we don't want negative rates.
          lnB.st <- c(lnB.st, log(rtruncnorm(n.rand-1, a=0, b=Inf, mean=B.st, sd=B.st / 2)))
          
          # We'll select the best model using AICc. Many of these turn out to be
          # similar.
          aics.out <- rep(NA, n.rand)
          
          for (h in 1:n.rand)
          {
            schoolfield_nls <- try(nlsLM(
              log(StandardisedTraitValue) ~ Schoolfield(lnB0, E, E_D, T_h, temp = K,SchoolTpk=SchoolTpk), 
              start=c(lnB0=lnB.st[h], E=E.st[h], E_D=4*E.st[h],T_h=T.h.st[h]),
              lower=c(lnB0=-Inf, E=0,  E_D=0,  T_h=250),
              upper=c(lnB0=Inf,  E=30, E_D=50, T_h=350),
              data=current_dataset,control=list(minFactor=1 / 2^16, maxiter=1024)),
              silent=TRUE)
            
            if (class(schoolfield_nls) != 'try-error')
            {
              aics.out[h] <- AICc(schoolfield_nls)
            }
          }
          w <- which.min(aics.out)
          
          if (length(w) > 0)
          {
            schoolfield_nls <- try(nlsLM(
              log(StandardisedTraitValue) ~ Schoolfield(lnB0, E, E_D, T_h, temp = K,SchoolTpk=SchoolTpk), 
              start=c(lnB0=lnB.st[w], E=E.st[w], E_D=4*E.st[w],T_h=T.h.st[w]),
              lower=c(lnB0=-Inf, E=0,  E_D=0,  T_h=250),
              upper=c(lnB0=Inf,  E=30, E_D=50, T_h=350),
              data=current_dataset,control=list(minFactor=1 / 2^16, maxiter=1024)),
              silent=TRUE)
            
          } else
          {
            schoolfield_nls <- NA
          }
        } else
        {
          
          
          schoolfield_nls <- NA
          try( 
            schoolfield_nls <- nlsLM(
              log(StandardisedTraitValue) ~ Schoolfield(lnB0, E, E_D, T_h, temp = K,SchoolTpk=SchoolTpk), 
              start=c(lnB0 = lnB.st, E = E.st, E_D = 4*E.st, T_h=T.h.st),
              lower=c(lnB0=-Inf,   E=0,    E_D=0, T_h=0),
              upper=c(lnB0=Inf,    E=Inf,  E_D=Inf, T_h=Inf),
              data=current_dataset, control=list(minFactor=1 / 2^16, maxiter=1024)),
            silent=TRUE)
          
        }
        
        # If fitting worked ...
        if(!is.na(schoolfield_nls[1])) 
        { 
          
          # Collect the parameter estimates..
          if (!is.na(MinVal)){ 
            lnB0Bug <- c(lnB0Bug,TRUE)      
          }else {
            lnB0Bug <- c(lnB0Bug,FALSE)       
          }
          lnB0_sch <- c(lnB0_sch, coef(schoolfield_nls)["lnB0"])
          lnB0_SE_sch <- c(lnB0_SE_sch, summary(schoolfield_nls)$coefficients[,'Std. Error']['lnB0'])  
          E_sch <- c(E_sch, coef(schoolfield_nls)["E"])
          E_SE_sch <- c(E_SE_sch, summary(schoolfield_nls)$coefficients[,'Std. Error']['E'])
          E_D_sch <- c(E_D_sch, coef(schoolfield_nls)["E_D"])
          E_D_SE_sch <- c(E_D_SE_sch, summary(schoolfield_nls)$coefficients[,'Std. Error']['E_D'])
          T_h_sch <- c(T_h_sch, coef(schoolfield_nls)["T_h"])
          T_h_SE_sch <- c(T_h_SE_sch, summary(schoolfield_nls)$coefficients[,'Std. Error']['T_h'])
          AIC_sch<- c(AIC_sch, AIC(schoolfield_nls))
          
          # Calculate the R squared value as: 1 - (rss/tss)
          rss <- sum((exp(predict(schoolfield_nls)) - 
                        current_dataset$StandardisedTraitValue)^2, 
                     na.rm = TRUE)
          tss <- sum(
            (current_dataset$StandardisedTraitValue - 
               mean(current_dataset$StandardisedTraitValue, na.rm = TRUE))^2, 
            na.rm = TRUE)
          #--Approach from Michaletz et al. (2016)
          #--Calculate R2, although R2 not defined for nonlinear regression, see http://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-model
          #tss <- length(current_dataset$StandardisedTraitValue)*var(current_dataset$StandardisedTraitValue, na.rm = T)
          
          if ( tss != 0 )
          {
            r_sq_sch <- c(r_sq_sch, 1 - (rss/tss))
          } else
          {
            r_sq_sch <- c(r_sq_sch, 1)
          }
          
          # Calculate the peak of the curve and its 
          # corresponding temperature value.
          curr_prediction <- predict(schoolfield_nls)
          for (j in 1:length(curr_prediction))
          {
            # If we found the maximum performance, exit the loop.
            if (curr_prediction[j] == max(curr_prediction))
            {
              break
            }
          }    
          
          T_pk_sch <- c(T_pk_sch, current_dataset$K[j])
          P_pk_sch <- c(P_pk_sch, curr_prediction[j])
          if (!is.na(MinVal)){ 
            P_pkBug <- c(P_pkBug,TRUE)
          }else {
            P_pkBug <- c(P_pkBug,FALSE)
          }
          
          
          
          
          ##############################
          # Plotting Schoolfield's fit #
          ##############################
          
          # Create a name for the output file using:
          #	- the original id number
          #   - the species name
          #   - the model
          output_name <- paste(
            current_dataset$FinalID[1], 
            current_dataset$Consumer[1], 
            'Schoolfield',
            sep = "_"
          )
          
          
          # Remove any characters that won't look good in a file name,
          # using a regular expression.
          output_name <- gsub("[^\\w|\\s](|)", "", output_name, perl=TRUE)
          
          # Convert spaces to underscores.
          output_name <- gsub("\\s+", "_", output_name, perl=TRUE)
          
          # CHANGE THIS to set an alternative output directory.
          outdir <- "./TPCFit_plots/"
          # Check if output dir exists; if not, make it
          ifelse(!dir.exists(file.path(getwd(), "TPCFit_plots")), dir.create(file.path(getwd(), "TPCFit_plots")), FALSE)
          
          # Generate predictions from the model fit...
          tmp_temps <- seq(min(
            floor(current_dataset$K)), 
            ceiling(max(current_dataset$K)
            ), length = 200)
          
          tmp_model <- exp(Schoolfield(
            coef(schoolfield_nls)["lnB0"],
            coef(schoolfield_nls)["E"],
            coef(schoolfield_nls)["E_D"],
            coef(schoolfield_nls)["T_h"],
            tmp_temps
          ))
          
          ModelToPlotS <- data.frame(
            Temperature = tmp_temps - 273.15, 
            TraitValue = tmp_model
          )
          
          # Prepare the data points of the original values.
          DataToPlot <- data.frame(
            Temperature = current_dataset$K - 273.15,
            TraitValue = current_dataset$StandardisedTraitValue
          )
          DataToPlot <- na.omit(DataToPlot)
          
          
          #### If we want individual plots
          if (PLOT==TRUE) {
            # Plot!
            p <- ggplot() + 
              geom_point(data = DataToPlot, aes(x = Temperature, y = TraitValue), 
                         shape = 21, size = 2.75, col = "black", fill = "white") + 
              geom_line(data = ModelToPlotS,aes(x = Temperature, y = TraitValue), 
                        colour = "green") +                           
              #ggtitle(paste(current_dataset$Consumer[1])) +
              ggtitle(paste("curveID=", current_dataset$FinalID[1], ", ", 
                            current_dataset$Consumer[1], ", ", 'Schoolfield', sep="")) +
              xlab(expression(paste("Leaf temperature (", degree, C, ")"))) + 
              ylab(current_dataset$StandardisedTraitName[1]) +
              theme_bw(base_size=12) +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")) +
              annotate("text", size = 3, 
                       label=paste("R^2","sch=", sprintf("%.2f", r_sq_sch[i]),"\nE sch=", format(coef(schoolfield_nls)["E"], digits = 3),"\nAIC sch=",format(AIC(schoolfield_nls),digits=3)), 
                       x = min(DataToPlot[, "Temperature"]),
                       y = mean(DataToPlot[, "TraitValue"]),
                       hjust=0,
                       fontface = 3)
            
            print(p)
            
            p2 <- ggplot() + 
              geom_point(data = DataToPlot, aes(x = Temperature, y = TraitValue), 
                         shape = 21, size = 2.75, col = "black", fill = "white") + 
              geom_line(data = ModelToPlotS, aes(x = Temperature, y = TraitValue), 
                        colour = "green") +                           
              #ggtitle(paste(current_dataset$Consumer[1])) +
              ggtitle(paste("curveID=", current_dataset$FinalID[1], ", ", 
                            current_dataset$Consumer[1], ", ", 'Schoolfield', sep="")) +
              xlab(expression(paste("Leaf temperature (", degree, C, ")"))) + 
              #ylab(current_dataset$StandardisedTraitName[1]) +
              ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
              scale_x_continuous(sec.axis = sec_axis(trans = ~ 1/(0.00008617*(.+273.15)) , name = expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')')))) +
              scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                                 labels = trans_format("log", math_format(e^.x))) +
              theme_bw(base_size=12) +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")) #+
            #annotate("text", size = 3, label=             
            #           paste("R^2","sch=", sprintf("%.2f", r_sq_sch[i]),"\nE sch=", format(coef(schoolfield_nls)["E"], digits = 3),"\nAIC sch=",format(AIC(schoolfield_nls),digits=3)), 
            #         x = min(DataToPlot[, "Temperature"]),
            #         y = mean(DataToPlot[, "TraitValue"]),
            #         hjust=0,
            #         fontface = 3)
            
            print(p2)
            
            p3 <- ggplot() + 
              geom_point(data = DataToPlot, aes(x = 1/(0.00008617*(Temperature + 273.15)), y = TraitValue), 
                         shape = 21, size = 2.75, col = "black", fill = "white") + 
              geom_line(data = ModelToPlotS, aes(x = 1/(0.00008617*(Temperature + 273.15)), y = TraitValue), 
                        colour = "green") +                           
              #ggtitle(paste(current_dataset$Consumer[1])) +
              ggtitle(paste("curveID=", current_dataset$FinalID[1], ", ", current_dataset$Consumer[1], ", ", 
                            'Schoolfield', sep="")) +
              #xlab(expression(paste("Leaf temperature (", degree, C, ")"))) +
              xlab(expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')'))) +
              #ylab(current_dataset$StandardisedTraitName[1]) +
              ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
              scale_x_continuous(sec.axis = sec_axis(trans = ~ (1/(.*0.00008617))-273.15 , name = expression(paste("Leaf temperature (", degree, C, ")")))) +
              scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                                 labels = trans_format("log", math_format(e^.x))) +
              theme_bw(base_size=12) +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")) #+
            #annotate("text", size = 3, label=             
            #           paste("R^2","sch=", sprintf("%.2f", r_sq_sch[i]),"\nE sch=", format(coef(schoolfield_nls)["E"], digits = 3),"\nAIC sch=",format(AIC(schoolfield_nls),digits=3)), 
            #         x = min(DataToPlot[, "Temperature"]),
            #         y = mean(DataToPlot[, "TraitValue"]),
            #         hjust=0,
            #         fontface = 3)
            
            print(p3)
            # Save it as a pdf file.
            #pdf_file <- paste(outdir, gsub("/|#", "", output_name), ".pdf", sep="")
            #ggsave(filename = pdf_file, plot = p, height = 4, width = 4.2)
            
          }
        } else # If fitting failed ...
        {
          # Populate the vectors with missing values.
          lnB0_sch <- c(lnB0_sch, NA)
          lnB0_SE_sch <- c(lnB0_SE_sch, NA)   
          lnB0Bug <- c(lnB0Bug,NA)
          E_sch <- c(E_sch, NA)
          E_SE_sch <- c(E_SE_sch, NA)
          E_D_sch <- c(E_D_sch, NA)
          E_D_SE_sch <- c(E_D_SE_sch, NA)
          T_h_sch <- c(T_h_sch, NA)
          T_h_SE_sch <- c(T_h_SE_sch, NA)
          T_pk_sch <- c(T_pk_sch, NA)
          P_pk_sch <- c(P_pk_sch, NA)
          P_pkBug <- c(P_pkBug,NA)
          AIC_sch <- c(AIC_sch, NA)
          r_sq_sch <- c(r_sq_sch, NA)
          
        }
        
      }
      
      if (Model=="all")
      {
        
        if (OverPLOT==TRUE) { ## In case we want both models in the same figure with the overlaid fits.
          ##############################
          # Plotting both fits #
          ##############################
          
          # Create a name for the output file using:
          #	- the original id number
          #   - the species name
          #   - the model
          output_name <- paste(
            current_dataset$FinalID[1], 
            current_dataset$Consumer[1], 
            'Schoolfield_Boltz',
            sep = "_"
          )
          
          
          # Remove any characters that won't look good in a file name,
          # using a regular expression.
          output_name <- gsub("[^\\w|\\s](|)", "", output_name, perl=TRUE)
          
          # Convert spaces to underscores.
          output_name <- gsub("\\s+", "_", output_name, perl=TRUE)
          
          # CHANGE THIS to set an alternative output directory.
          outdir <- "./TPCFit_plots/"
          # Check if output dir exists; if not, make it
          ifelse(!dir.exists(file.path(getwd(), "TPCFit_plots")), dir.create(file.path(getwd(), "TPCFit_plots")), FALSE)
          
          
          # Prepare the data points of the original values.
          DataToPlot <- data.frame(
            Temperature = current_dataset$K - 273.15, 
            TraitValue = current_dataset$StandardisedTraitValue
          )
          DataToPlot <- na.omit(DataToPlot)
          
          
          
          ## Trait Units for plot
          Unit <- current_dataset$StandardisedTraitUnit[1]
          if (is.na(Unit)) Unit <- current_dataset$StandardisedTraitUnit[1]
          
          # Plot!
          p <- ggplot() + geom_point(data = DataToPlot, aes(x = Temperature, 
                                                            y = TraitValue), size = 3, col = "black", bg = "lightcyan2", 
                                     alpha = 0.7, pch = 21) + 
            geom_line(data = ModelToPlotB, 
                      aes(x = Temperature, y = TraitValue), colour = "#1b9e77", 
                      lwd = 1.3) +            
            geom_line(data = ModelToPlotS, 
                      aes(x = Temperature, y = TraitValue), colour = "red", 
                      lwd = 1.3) +             
            #ggtitle(paste(current_dataset$Consumer[1])) +
            ggtitle(paste("curveID=", current_dataset$FinalID[1], ", ", current_dataset$Consumer[1], ", ", 
                          'Boltzmann-Arrhenius & Schoolfield', sep="")) +
            xlab(expression(paste("Temperature (", degree, C, ")"))) + 
            ylab(paste(current_dataset$StandardisedTraitName[1],"\n",Unit)) +
            theme_bw() + theme(plot.title = element_text(size = 12), 
                               axis.title = element_text(size = 10)) +
            annotate("text", size = 3, label=             
                       paste("R^2","sch=", sprintf("%.2f", r_sq_sch[i]),"\nE sch=", format(coef(schoolfield_nls)["E"], digits = 3),"\nAIC sch=",format(AIC(schoolfield_nls),digits=3),"\nR^2 boltz=", sprintf("%.2f", r_sq_boltz[i]), "\nE boltz=", format(coef(boltzmann_nls)["E"], digits = 3),"\nAIC boltz=",format(AIC(boltzmann_nls),digits=3)), 
                     x = min(DataToPlot[, "Temperature"]),
                     y = mean(DataToPlot[, "TraitValue"]),
                     hjust=0,
                     fontface = 3)
          
          print(p)
          # Save it as a pdf file.
          #pdf_file <- paste(outdir, gsub("/|#", "", output_name), ".pdf", sep="")
          #ggsave(filename = pdf_file, plot = p, height = 4, width = 4.2)
          
        }     
        
        ##################################################################
        # Compare the two models using the Akaike Information Criterion. #
        ##################################################################
        
        # If both models failed to fit, add NA. 
        if (is.na(AIC_sch[i]) && is.na(AIC_boltz[i]))
        {
          selected_model <- c(selected_model, NA)
          
          # If only one of the two models could be fit, that is 
          # automatically the winner!
        } else if (is.na(AIC_sch[i]) && !is.na(AIC_boltz[i]))
        {
          selected_model <- c(selected_model, 'boltzmann')
        } else if (is.na(AIC_boltz[i]) && !is.na(AIC_sch[i]))
        {
          selected_model<- c(selected_model, "schoolfield")	
          
          # If both models were able to fit and Schoolfield's AIC
          # was lower, then that is the better model for this curve.
        } else if (AIC_sch[i] < AIC_boltz[i])
        {
          selected_model<- c(selected_model,  "schoolfield")
          
          # And the opposite for the Boltzmann - Arrhenius model.
        } else
        {
          selected_model<- c(selected_model,  "boltzmann")
        }
      }
      
    }  else # If there are not enough values
    {
      
      # Populate the vectors with missing values.
      if (Model=="Schoolfield" | Model=="all"){
        lnB0_sch <- c(lnB0_sch, NA)
        lnB0_SE_sch <- c(lnB0_SE_sch, NA)   
        lnB0Bug <- c(lnB0Bug,NA)
        E_sch <- c(E_sch, NA)
        E_SE_sch <- c(E_SE_sch, NA)
        E_D_sch <- c(E_D_sch, NA)
        E_D_SE_sch <- c(E_D_SE_sch, NA)
        T_h_sch <- c(T_h_sch, NA)
        T_h_SE_sch <- c(T_h_SE_sch, NA)
        T_pk_sch <- c(T_pk_sch, NA)
        P_pk_sch <- c(P_pk_sch, NA)
        P_pkBug <- c(P_pkBug,NA)
        AIC_sch <- c(AIC_sch, NA)
        r_sq_sch <- c(r_sq_sch, NA)
        
      }    
      # Populate the vectors with missing values.
      if (Model=="Boltzmann" | Model=="all"){
        E_boltz <- c(E_boltz, NA)
        lnB0_boltz <- c(lnB0_boltz, NA)
        T_pk_boltz <- c(T_pk_boltz, NA)
        P_pk_boltz <- c(P_pk_boltz, NA)
        AIC_boltz <- c(AIC_boltz, NA)
        r_sq_boltz <- c(r_sq_boltz, NA)}
      
      if (Model=="Boltzmann") { ## When both models are fit, this is attached in Schoolfield
        lnB0Bug <- c(lnB0Bug,NA)
        P_pkBug <- c(P_pkBug,NA)}
    }
  }
  grDevices::dev.off()
  
  ##################################################################
  # RESULTS FILE #
  ##################################################################
  
  
  if (Model=="all"){
    # Compile all data into a data frame.
    results <- data.frame(
      id, id_spp, id_process, E_boltz, E_sch, lnB0_boltz, lnB0_sch, lnB0Bug, E_D_sch, 
      T_h_sch, T_pk_boltz, T_pk_sch, P_pk_boltz, P_pk_sch, P_pkBug,AIC_boltz, 
      AIC_sch, r_sq_boltz, r_sq_sch, AftPk,BefPk,MinVal, selected_model
    )}
  
  if (Model=="Schoolfield"){
    results <- data.frame(
      id, id_spp, id_process, E_sch, E_SE_sch, lnB0_sch,lnB0_SE_sch,
      lnB0Bug, E_D_sch, E_D_SE_sch,
      T_h_sch, T_h_SE_sch,  T_pk_sch, P_pk_sch, P_pkBug,
      AIC_sch,  r_sq_sch, AftPk,BefPk, MinVal
    )}
  
  
  if (Model=="Boltzmann"){
    results <- data.frame(
      id, id_spp, id_process, E_boltz, lnB0_boltz,  lnB0Bug,
      T_pk_boltz, P_pk_boltz, P_pkBug,
      AIC_boltz,  r_sq_boltz, AftPk,BefPk, MinVal    )}
  
  
  # CHANGE THIS to set an alternative output directory.
  outdir <- "./"
  # Define path and name of results output file.
  results_file <- paste(outdir, gsub("/|#", "", "TPCFit_results_"), Model, ".csv", sep="")
  # Write the results as a CSV file.
  write.csv(results, file = results_file, row.names = FALSE)
  
  return(results)
}
