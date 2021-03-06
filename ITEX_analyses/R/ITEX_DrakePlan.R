#######################
### Auds Drake Plan ###
#######################

### LOAD LIBRARIES
# install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
#remotes::install_github("Between-the-Fjords/dataDownloader")
library(drake)
library(PFTCFunctions)
library(dataDownloader)
library(tidyverse)
library(lubridate)
library(vegan)
library(ggvegan)
library(readxl)
library(broom)
library(lme4)
library(patchwork)
library(ggpubr)
library(ggfortify)
library(traitstrap)
library(nlme)
library(ape)
library(broom.mixed)
library(ggpattern)
library(MuMIn)

# tricks
pn <- . %>% print(n = Inf)

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_bw(base_size = 15))


### SOURCE FUNCTIONS
# Plans
source("ITEX_analyses/R/DownloadAndImport.R")
source("ITEX_analyses/R/AnalysisPlan.R")
source("ITEX_analyses/R/Figure_Plan.R")
source("ITEX_analyses/R/FluxPlan.R")

# Functions
source("ITEX_analyses/R/Functions/Community_analyses.R")
source("ITEX_analyses/R/Functions/CommunityFigures.R")
source("ITEX_analyses/R/Functions/Trait_analyses.R")
source("ITEX_analyses/R/Functions/TraitFigures.R")
source("ITEX_analyses/R/Functions/inter_intra_anova.R")
source("ITEX_analyses/R/Functions/Climate_analyses.R")
source("ITEX_analyses/R/Functions/Flux_analyses.R")
source("ITEX_analyses/R/Functions/Flux_figures.R")

### MAKE DRAKE PLAN


# # knit manuscript
# ManuscriptDrakePlan <- drake::drake_plan(
#   manuscript = {
#     rmarkdown::render(
#       input = knitr_in("DataPaper/Writing/Manuscript.Rmd"),
#       knit_root_dir = "../",
#       clean = FALSE)
#   }
# )


### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(DataDownloadPlan,
                              DataImportPlan,
                              AnalysisPlan,
                              FigurePlan,
                              FluxPlan)

#configure and make drake plan
config <- drake::drake_config(MasterDrakePlan)

config