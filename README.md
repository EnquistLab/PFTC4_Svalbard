
<style>
p.caption {
  font-size: 0.8em;
}
</style>

# PFTCOURSES, ELEVATIONAL GRADIENT, BIRD CLIFF AND ITEX EXPERIMENT, LONGYEARBYEN, SVALBARD

This is the git repository for the paper: XXX et al. (not written yet).
Plant traits, vegetation and carbon flux data from an elevation
gradient, a bird cliff and a warming experiment in Longyearbyen,
Svalbard.

## PROJECT AND SITE INFORMATION

This project reports on plant functional traits, vegetation,
ecophysiology, ecosystem, climate and remote sensing data. The data was
collected across 7 sites along a 250 m elevational gradient and across 5
sites along a 160 m elevational gradient providing a strong nutrient
gradient (bird cliff) near Longyearbyen, Svalbard. Data was also
collected in a ITEX warming experiment with Open Top chamber (OTC). The
ITEX experiment was set up in 2001 in three habitats (Dryas heath,
Cassiope heath and snowbed). The data were collected between 2003 and
2018 as part of Norwegian research projects and the international Plant
Functional Traits Courses 4 (PFTC4).

## The experimental set-up

#### Elevational and nutrient (Bird Cliff) gradients

For the elevational gradient, we established seven transects (Table 1)
at different elevations along the slope of Lindholmhøgda, near Isdammen.
Transects were established in herb-like vegetation, in between snow beds
(Cassiope heath) and ridge communities (rocky with very scarce
vegetation).

The nutrient deposition gradient was established under a little auk
(Alle alle, but also a few black guillemots, Cepphus grylle) colony near
Bjørndalen, up the slope of Platåfjellet. The birds breeding on the
cliffs above the site deposit nutrients in the form of guano. Nutrient
deposition will therefore be highest closest to the bird nests as the
number of bird droppings per surface area will increase.

In both gradients we established 75 x 75 cm plots arranged in transects
(running perpendicular to the slope) containing 7 plots each. In the
bird cliff gradient, we established in total 5 transects, in the
elevational gradient we established 7 transects.

#### ITEX warming experiment

The experiment was carried out in the high Arctic, in Endalen (78°11‘N,
15°45‘E), a valley situated approximately four kilometers east of
Longyearbyen, Svalbard, at 80-90 m elevation. Average annual temperature
is –5.2°C (1981–2010) and the average annual precipitation is 191 mm
(Førland et al., 2011). The prevailing wind direction is from the east.
The soils are typical Cryosols with thin organic layer on top of
inorganic sediments (Jones et al., 2010). The experiment was first
established in 2002 in three different habitats which are located in the
south–southeast–facing hillside of the valley. The habitats differ in
vegetation composition and the time of snowmelt (and hence the length of
the growing season). These habitats include a relatively dry Dryas heath
with thin snow cover (ca. 10 cm) and early snowmelt, a mesic Cassiope
heath habitat with intermediate snow depth and melting date, and a moist
snowbed community with deep snow (\> 100 cm) and late snowmelt.

In 2001 30 plots (75x75 cm), were selected, ten in each of the three
habitats (Dryas heath, Cassiope heath and snowbed) based on a priory
criteria. Half of the plots were randomly assigned to the warming
treatment in 2002 and the other half to control.

#### Saxifraga oppositifolia polyploidy sites

Saxifraga oppositifolia data was collected in the Endalen Valley. The
Endalen site consists of three three plots representing three different
habitats with 48 marked plants in each (144 total). The first habitat
type (H1; River) has the lowest elevation and is located on the west
side of the river in Quaternary alluvium gravels (Table 1). The second
habitat type (H2; Heath) has the middle elevation and is located to
further to the west from the river site. Cassiope tetragona is the
dominant vascular plant, indicating snow bed conditions (concave
landscape). The third site (Scree) is located at the highest elevation
above a break in the slope. The site’s substrate is composed of
sandstone and siltstone scree.

#### Snowfence?

## DATASETS, CODE AND ANALYSES

The raw and cleaned datasets are stored on OSF PFTCourses, Elevational
Gradient, Bird Cliff and ITEX Experiment, Longyearbyen, Svalbard:
<https://osf.io/smbqh/>

The data was processed and analysed using R. All code is stored on
github: <https://github.com/EnquistLab/PFTC4_Svalbard>

### Download data

All raw data is available on the OSF repo and the code used to clean the
data is in the folder cleaningRawData in the git repository. The cleaned
data are also available on OSF, and we strongly suggest to use these
files.

To download the data, the following function can be used:

``` r
#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")
```

``` r
#Download files from OSF
download_PFTC_data(country = "Svalbard", 
                   datatype = "community", 
                   path = "community/cleaned_data")
#> # A tibble: 2 x 3
#>   node  file                                          remote_path
#>   <chr> <chr>                                         <chr>      
#> 1 smbqh PFTC4_Svalbard_2018_Community_cleaned.csv     Community  
#> 2 smbqh ITEX_Svalbard_2003_2015_Community_cleaned.csv Community
```

It is also possible to download several files at the same time:

``` r
#Download files from OSF
download_PFTC_data(country = "Svalbard", 
                   datatype = c("community", "trait"), 
                   path = "cleaned_data")
#> # A tibble: 3 x 3
#>   node  file                                      remote_path
#>   <chr> <chr>                                     <chr>      
#> 1 smbqh PFTC4_Svalbard_2018_ITEX.csv              Traits     
#> 2 smbqh PFTC4_Svalbard_Traits_2018_Saxy.csv       Traits     
#> 3 smbqh PFTC4_Svalbard_2018_Community_cleaned.csv Community
```

Use the following specifications to download the data:

``` r
data("location", package = "PFTCFunctions")
```

| Country  | DataType  | Remark |
| :------- | :-------- | :----- |
| Svalbard | community |        |
| Svalbard | community |        |
| Svalbard | trait     |        |
| Svalbard | flux      |        |
| Svalbard | meta      |        |

## Data collection

| ResponseVariable                  | Elevation\_N\_Gradient | ITEX\_Warming | Polyploidy | FluxTower | SnowFence |
| :-------------------------------- | ---------------------: | ------------: | ---------: | --------: | --------: |
| PlantCommunity                    |                      1 |             1 |         NA |        NA |        NA |
| PlantFunctionalTraits             |                      1 |             1 |          1 |        NA |        NA |
| BryophyteTrais                    |                      1 |            NA |         NA |        NA |        NA |
| ClimateData                       |                      1 |             1 |         NA |        NA |        NA |
| LeafPhysiology                    |                      1 |            NA |         NA |        NA |        NA |
| C-Fluxes                          |                      1 |             1 |         NA |        NA |        NA |
| SaxifragaPoliploidy               |                     NA |            NA |          1 |        NA |        NA |
| RemoteSensing-SpectralReflectance |                      1 |             1 |         NA |         1 |         1 |

### Community Dataset

#### Elevational and nutrient gradients

In each plot, we identified and estimated cover percentage of all forb
and graminoid species, and if they are flowering. Median vegetation
height was estimated in the field by measuring the height of plants that
appeared to be around the average height for the plot. In addition, we
estimated cover percentage of moss/bryophytes, biocrust, lichens,
litter, rocks and bare ground, and marked presence and identification of
scat, and weather conditions.

#### ITEX warming experiment

To estimate the abundance of different plant species in the plots, the
point intercept method (PIM) was used in the summer of 2003, 2009 and
2015 following the protocols from the ITEX manual (Molau & Mølgaard,
1996). A 75 × 75 cm frame with 100 equally distributed points was used.
All hits within the canopy were recorded until the pin hit the cryptogam
layer (composed of bryophytes and lichens), bare ground, or litter.
Species of vascular plants, bryophytes and lichens were recorded (some
bryophytes and lichens only at the genus level). Dead plant tissue on
the ground was recorded as litter and unvegetated surface was recorded
as rock or soil (bare ground).

To download the clean database use: `XXX.R` To load the community data
start with the R script: `XXX.R`

#### Data cleaning steps

All data was manually entered from into digital worksheets, and manually
proofread.

All data cleaning and checking was done using code. The data was checked
and corrected for spelling mistakes and mislabeled. Missing information
(e.g. PlotID, Site) were added if possible. The data was then checked
visually to detect apparent measurement errors.

#### Diversity along elevational gradient

Add some plots here…

### Plant functional leaf traits

The dataset contains eleven functional traits related to potential
physiological rates and environmental tolerance of plants. These
include:

  - leaf area (LA, cm<sup>2</sup> )
  - leaf thickness (LT, mm)
  - leaf dry matter content (LDMC, g/g)
  - specific leaf area (SLA, cm<sup>2</sup> /g)
  - carbon (C, %)
  - nitrogen (N, %)
  - phosphorus (P, %)
  - carbon:nitrogen ratio (C:N)
  - nitrogen:phosphorus (N:P)
  - carbon13 isotope (δ <sup>13</sup>C, ‰)
  - nitrogen15 isotope (δ <sup>15</sup>N, ‰)

The traits were measured according to Pérez-Harguindeguy et al. (2012)
as well as the Enquist Macrosystems protocol with the following
modifications:

#### Sampling

For each of the gradient and ITEX experimental plots, we collected three
individuals of each species covering more than 1 % of each plot for leaf
functional trait measurements. Plant material for trait measurements was
collected outside but within the close surroundings of each plot because
the Carbon flux group still had to perform measurements, we could not
sample destructively from within the plots. Some additional samples
collected from the Saxifraga and reflectance studies (see below) were
also measured for traits following this protocol.

#### Sample storage

The samples were transported from the field to the lab as whole plants,
including roots and soil, to keep them fresh and fit as long as
possible. We used ziplock plastic bags with plot numbers, date, height,
species ID and sample ID on them. Samples were stored outdoors
(temperature around +6 degrees Celcius) to ensure they stay fresh and
water saturated until the measurements started.

#### Sample preparation

The first step was to check the species identification and taxonomy of
plant samples. Then, plant height was measured (in cm) from the bottom
of each individual plant to the highest tip of a photosynthetic leaf,
excluding florescences but including stem leaves for graminoids. Then,
leaves were separated from the plants using tweezers and sorted into
paper envelopes with a standardized labelling system. Because leaf sizes
and shapes vary significantly between species, some standardized rules
were applied (see Table below). For Equisetum sp., which lacks obvious
leaves, an 8 cm section was cut off including side shoots. For every
plant sample, we collected at least three leaves. If the leaves were
very small, we collected an amount of leaves equivalent to approx 3 cm2.
Small leaves were rolled into a small white paper envelope to ensure no
leaves were lost during the measurements. At all times, the plant
material was kept in moist conditions (sprayed with water, stored
outside and processed within
24h).

| Nr | Rule                                                                                          |
| -: | :-------------------------------------------------------------------------------------------- |
|  1 | Obey the rules and keep the leaves wet                                                        |
|  2 | Sample 3 leaves per ind or fill up 3cm<sup>2</sup>                                            |
|  3 | Write \# of leaves; if \> 10 write bulk                                                       |
|  4 | Sample leaf to where it joins the stem (Inlcude leaflets for Dryas; Exclude sheaths/stipules) |
|  5 | Plant height: from ground to highest photosynthetic unit. Exclude florescenses                |
|  6 | Equisetum length should be width of envelope, and include branches if needed                  |
|  7 | For folding graminoids tape the leaves to the scanner                                         |
|  8 | Scanning: avoid overlap, stay away from edges, face down                                      |
|  9 | Wear slipper in the lab                                                                       |
| 10 | Check species list regularly (Dupontia)                                                       |
| 11 | NO MINING\! = trading for better plants                                                       |
| 12 | For small leaves use folded paper envelopes                                                   |
| 13 | Do not unroll Festuca                                                                         |

#### Fresh mass in g

We weighed the fresh, moist leaves in the lab (in grams, rounded to four
decimals). A weighing boat was used for small leaves. Blances used were
Mettler AE200, Mettler TOLEDO, and AG204 DeltaRange (0.1 mg precision).

#### Leaf surface area in cm<sup>2</sup>

To measure leaf surface area, leaves were placed on a scanner (Canon
Lide 220). Leaves were placed so that the upper sides of the leaves
faces downwards. In case of folded leaves, we used transparent Scotch
tape to flatten them out and fix them on the scanner. A measurement tape
was attached on each scanner as reference scale. Each scan was named
according to its ID number (standardized tag system, written on the
paper bag). The leaf area was calculated using R package “LeafArea”
(Katabuchi 2017).

#### Leaf thickness in mm

Then, leaf thickness was measured three times with electronic
micrometers (Mitutoyo 293-348) on each leaf (i.e. three) per sample. If
fewer leaves were available, several measurements were taken on the same
leaf. We avoided measuring on veins, but that was not possible for all
species, e.g. for very small leaves or in the case of Equisetum species.

#### Leaf thermal constants

We calculated thermal time constants from the measured functional
traits. The thermal time constant is a critical trait underlying leaf
carbon economics (Michaletz et al. 2015 TREE, 2016 Nature Plants). It is
a composite leaf trait that comprises several additional traits,
including dry matter mass, water mass, specific heat capacity, total
(two-sided) surface area, width, geometry (e.g., broadleaf or
needle-leaf), and stomatal conductance. These traits are then combined
into a single ratio tau that quantifies the ability of the leaf to store
heat versus its ability to exchange heat with the environment. Thermal
time constants were calculated from wet and dry mass.

#### Dry mass in g and sample storage

Samples were stored in their envelopes and dried in a drying cupboard
(Thermo Scientific Heraeus®, USA) at 60°C for approximately 24 hours to
prepare them for shipment to the University of Arizona (UoA). At UoA,
the samples were dried at 60°C for 72 hours, weighed, and sent for C and
N analyses.

#### Specific leaf area

Whole leaf SLA. Specific leaf area is calculated from the leaf area for
the whole leaf (measured with the scanner), divided by the dry mass
(measured after 72 hours drying) for the whole leaf. SLA = leaf area
(cm<sup>2</sup>)/dry mass (g).

#### Leaf Dry Matter Content

LDMC was measured with the leaf dry mass divided by the leaf wet mass.
LDMC = Leaf dry mass (g)/ leaf wet mass (g).

To download the clean data use: `XXX.R`

#### Traits Distributions and Values

Add some plots here…

#### Data processing and cleaning

All data was manually entered from into digital worksheets, and manually
proofread.

All data cleaning and checking was done using code. The data was checked
and corrected for spelling mistakes and mislabelling. Missing or
mislabeled information (e.g. elevation, site, taxon, individual number,
project) were added or corrected if possible. Wrong and missing site,
plotID and individual number were imputed if possible. For some leaves
it was more a guess to which plot the leaf belongs. These leaves are
flagged. Leaves where this was not possible were excluded. Duplicated
entries were removed.

The taxonomy was checked according to TNRS
(<http://tnrs.iplantcollaborative.org/>).

The data was then checked visually to detect apparent measurement
errors. Unrealistic values were removed. For the trait data this
included leaves where leaf dry matter values higher than 1 g/g and
leaves with specific leaf area values greater than 500 cm<sup>2</sup>
/g.

This still needs to be checked\!\!\! and leaf nitrogen values higher
than 6.4%. The nitrogen cutoff value was chosen based on the highest
published leaf nitrogen values found in the Botanical Information and
Ecology Network (Enquist et al., 2009) for the genera in our study.

### Bryophyte traits

Bryophyte samples (tussocks) were collected at different elevational
gradient from two sites, bird cliff (B) and elevational gradient (C).
Site B had five elevational transects and site C had seven elevational
transects. At each transect, a survey of dominant moss species
determined which species to collect. Three tussocks of the (maximum)
three most dominant species were sampled at both ends (plot A and G) and
the middle (plot D) of each transect, for a maximum of nine samples per
transect. To avoid destructive sampling, we samples were collected from
the surrounding of each plot, adjacent to the transects.

The following taxa were sampled: Aulacomnium turgidum, Dicranum sp.,
Hylocomium splendens, Polytrichum sp., Polytrichum piliferum,
Racomitrium sp., Racomitrium canescens, Sanionia uncinata, Syntrichia
ruralis, and Tomentypnum nitens.

Aulacomnium turgidum, Hylocomium splendens, and Sanionia uncinata were
sampled at both sites at multiple elevational gradients. Polytrichum sp.
(assuming this is P. piliferum?) and P. piliferum were sampled at both
sites at one elevational transect. Racomitrium sp. and R. canescens were
sampled at both sites, however R. sp. was sampled only from one transect
at site B while R. canescens was sampled across multiple elevational
transects at site C. Dicranum sp. and Tomentypnum nitens were sampled
only at site C, however T. nitens was sampled at multiple elevational
transects. Syntrichia ruralis was only sampled at site B at the highest
elevational transect.

#### Bryophyte trait measurements

Due to the inherent differences between bryophytes and vascular plants,
bryophyte samples did not enter the trait wheel (see 3.2) but followed a
separate methodology described below.

#### Sample collection, transport, and storage

For each sample, we collected tussock of about 25cm2 in the field. These
were stored in plastic bags, transported to UNIS and stored outside (at
+6 degrees Celsius) away from direct sunlight until further processing.

#### Sorting and preparation of samples

From each tussock, we collected at least five living (i.e. green) shoots
(considering approximate biomass needed for chemical analysis) including
any dead lower parts. These shoots we carefully cleaned from soil and
debris under a stereo microscope using tweezers if necessary. Samples
were then labelled as described for vascular plants in section 1.7.

#### Shoot length in cm

Next, both the length of the living part (green) and complete shoot was
measured on three shoots per sample (in cm). In case of a “split tip”
with multiple green shoots, the longest was measured (usually the main
shoot).

#### Wet mass in g

Before measuring the wet weight of all the shoots in each sample,
samples were soaked in petri dishes with demineralized water for 30
minutes. Then, the water was removed and samples were kept in sealed
petri dishes lined with moist tissue paper over night. Before weighing
(d=0.1 mg), samples were blotted dry with tissue paper.

#### Sample storage and further measurements

Samples were oven dried (60 ºC) and stored before shipment to the
University of Arizona for further analysis.

### Climate data

Climate data was recorded along both elevational gradients and in the
ITEX warming experiment.

#### Elevational gradient

##### Temperature and moisture (TomsT logger)

Info needs to be added

##### Short term microclimate data

Along both gradients, we collected soil micro-climatic data. Soil
moisture and temperature were measured on 19.7.2018 from all plots in
the elevation (n = 46) and nutrient gradient (n = 35). We used hand-held
time-domain reflectometry sensors to measure volumetric water content
(VWC%) up to a depth of 7,5 cm (FieldScout TDR 300; Spectrum
Technologies, Plainfield, IL, USA). Each plot was measured from three
points, for covering possible moisture variation within the plot.
Average of the three points was used. We used high-accuracy digital
thermometers measure soil temperature (°C) up to a depth of 7,5 cm (TD
11 Thermometer; VWR International bcba; Leuven, Belgium). Each plot was
measured from the centre once. In addition, miniature temperature
loggers (ThermoChron iButtons, San Jose, CA, USA) were buried c. 7,5 cm
below the soil surface, measuring at 4-hour intervals throughout
19.7.-10.8.2018. On the elevation gradient, the loggers were buried 10
cm to the left of the upper-left corner (seen from the bottom of the
slope) of the plot. On the nutrient gradient, the loggers were buried in
the middle of the plot. Loggers were buried in the following plots (n =
67): Elevation gradient (n = 36): Transect 1: A, B, C, D, F, G Transect
2: A, B, C, D, F, G Transect 3: A, B, D, F, G Transect 4: A, B, D, F, G
Transect 5: A, B, D, F, G Transect 6: A, B, D, F, G Transect 7: A, B, C,
D

Nutrient gradient (n = 31): Transect 1: A, B, C, D, F, G Transect 2: A,
B, C, E, F, G Transect 3: A, B, C, E, F, G Transect 4: A, B, C, D, F, G
Transect 5: A, B, C, D, E, F, G

#### ITEX warming experiment

  - There is a climate station at the ITEX site recording air
    temperature at 2m (°C Tair). Data are recorded (usually) every 10
    minutes and there are data from 2015-2018.
  - Soil temperature (-5 cm) from iButtions recorded between 2004-2005
    and 2015-2018. This data was recorded inside the OTC and in the
    control plots in all 3 habitats (X plots per habitat).

To download the clean data use: `XXX.R`

#### Data processing

The data was provided in excel or csv files. The data was checked
visually for outliers. Unrealistic values were removed.

### Leaf Photosynthesis Temperature Response

…

### CO2 Efflux

…

### Polyploidy in Saxifraga oppositifolia

…

### Remote sensing and spectral reflectance

…
