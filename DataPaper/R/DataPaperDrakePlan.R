#######################
### Auds Drake Plan ###
#######################

### LOAD LIBRARIES
# install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("drake")
library("PFTCFunctions")
library("tidyverse")
library("readxl")
library("vegan")
library("ggvegan")
# library("lubridate")
# library("broom")
# 
# library("lme4")
# 
# library("grid")
# library("patchwork")

# tricks
pn <- . %>% print(n = Inf)

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_bw(base_size = 15))


### SOURCE FUNCTIONS
# Plans
source("DataPaper/R/DownloadAndImport.R")
source("DataPaper/R/AnalysisPlan.R")

# Functions
source("DataPaper/R/Functions/ItexAnalysis.R")

### MAKE DRAKE PLAN


# knit manuscript
ManuscriptDrakePlan <- drake::drake_plan(
  manuscript = {
    rmarkdown::render(
      input = knitr_in("DataPaper/Writing/Manuscript.Rmd"),
      knit_root_dir = "../",
      clean = FALSE)
  }
)


### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(DataDownloadPlan,
                              DataImportPlan,
                              AnalysisFigurePlan)
                              #ManuscriptDrakePlan)

#configure and make drake plan
config <- drake::drake_config(MasterDrakePlan)

config