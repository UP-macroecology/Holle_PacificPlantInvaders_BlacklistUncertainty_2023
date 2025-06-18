# Uncertainty in blacklisting potential Pacific plant invaders using species distribution models

Valén Holle<sup>1</sup>, Anna Rönnfeldt<sup>1</sup>, Katrin Schifferle<sup>1</sup>, Juliano Sarmento Cabral<sup>2,3</sup>, Dylan Craven<sup>4,5</sup>, Tiffany Knight<sup>6,7,8,9</sup>, Hanno Seebens<sup>10</sup>, Patrick Weigelt<sup>11</sup>, Damaris Zurell<sup>1</sup>  

1. University of Potsdam, Institute of Biochemistry and Biology, Am Neuen Palais 10, 14469 Potsdam, Germany
2. School of Biosciences, College of Life and Environmental Sciences, University of Birmingham, Birmingham, B15 2TT, UK
3. Ecological Modelling, Bonner Institute for Organismal Biology - Dept. of Plant Biodiversity, University of Bonn, Bonn, Germany
4. GEMA Center for Genomics, Ecology & Environment, Universidad Mayor, Camino La Pirámide 5750, Huechuraba, Santiago, Chile
5. Data Observatory Foundation, ANID Technology Center No. DO210001, Eliodoro Yáñez 2990, 7510277, Providencia, Santiago, Chile 
6. Department of Species Interaction Ecology, Helmholtz Centre for Environmental Research – UFZ, Permoserstrasse 15, 04318 Leipzig, Germany
7. German Centre for Integrative Biodiversity Research (iDiv), Halle-Jena-Leipzig, Puschstrasse 4, 04103 Leipzig, Germany
8. Institute of Biology, Martin Luther University Halle-Wittenberg, Am Kirchtor 1, 06108 Halle (Saale), Germany
9. Department of Science and Conservation, National Tropical Botanical Garden, Kalāheo, HI, USA
10. Department of Animal Ecology & Systematics, Justus Liebig University, Heinrich-Buff-Ring 26, 35392 Giessen, Germany
11. Department of Environmental Science, Radboud Institute for Biological and Environmental Sciences (RIBES), Radboud University, Heyendaalseweg 135, 6525AJ Nijmegen, The Netherlands



### ABSTRACT:
1.	Invasive alien species pose a growing threat to global biodiversity, underscoring the need for evidence based prevention strategies. Species distribution models (SDMs) are a widely used tool to estimate the potential distribution of alien species and to inform blacklists based on establishment risk. Yet, data limitations and modelling decisions can introduce uncertainty in these predictions. Here, we aim to quantify the contribution of four key sources of uncertainty in SDM-based blacklists: species occurrence data, environmental predictors, SDM algorithms, and thresholding methods for binarising predictions.
2.	Focusing on 82 of the most invasive plant species on the Hawaiian Islands, we built SDMs to quantify their establishment potential in the Pacific region. To assess uncertainty, we systematically varied four modelling components: species occurrence data (native vs. global), environmental predictors (climatic vs. edapho-climatic), four SDM algorithms, and three thresholding methods. From these models, we derived blacklists using three alternative blacklisting definitions and quantified the variance in establishment risk scores and resulting species rankings attributable to each source of uncertainty.
3.	SDMs showed fair predictive performance overall. Among the sources of uncertainty, thresholding method had the strongest and most consistent influence on risk scores across all three blacklist definitions but resulted in only minor changes in blacklist rankings. In contrast, algorithm choice had the most pronounced effect on blacklist rankings, followed by smaller but important effects of species occurrence data and environmental predictors. Notably, models based only on native occurrences often underestimated establishment potential.
4.	SDMs can provide valuable support for planning the preventive management of alien species. However, our findings show that blacklist outcomes are highly sensitive to modelling decisions. While ensemble modelling across multiple algorithms is a recommended best practice, our results reinforce the importance of incorporating global occurrence data when available and carefully evaluating the trade-offs of including additional environmental predictors. Given the strong influence of thresholding on risk scores, we emphasise the need for transparent, context-specific threshold selection. More broadly, explicitly assessing uncertainty in SDM outputs can improve the robustness of blacklists and support scientifically informed, precautionary decision-making, particularly in data-limited situations where pragmatic modelling choices must be taken.

Keywords: Blacklisting, ecological niche, ensemble predictions, habitat suitability models, islands, plant invasions, uncertainty

This repository contains the R scripts needed to reproduce all results and plots.


Funding: This study was supported by the German Research Foundation DFG (grant no. ZU 361/3-1 to DZ)


---------------------------------------------------------------
**Workflow**
---------------------------------------------------------------
The analysis built on the global occurrence data of the plant species listed in the [PaciFLora](https://bdj.pensoft.net/article/67318/) dataset. Each occurrence point matched with a biogeographic status (native or introduced). Code for the data preparation can be found in a separate [repository](https://github.com/UP-macroecology/StatusAssignment).

From these species, we included an initial subset of known plant populations of 122 plant species that occur as most invasive on at least one of the Hawaiian Islands according to the [PIER](http://www.hear.org/pier/) database, derived from the compiled data set by [Wohlwend et al. (2021)](#2), in our study.

We detail all data preparation and modelling steps following the [ODMAP protocol](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/ODMAP_protocol_BlacklistUncertainty.csv)

### 0 - Data setup
scripts [folder structure](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/00_create_folder_structure.R), [functions](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/00_functions.R), [spatial and environmental data prep](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/00_environmental_spatial_data_prep.R)

The required folder structure is set up, the needed functions are listed, and the spatial and environmental data are prepared.


### 1 - Initial species selection
scripts [01](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/01_initial_species_selection.R)

The initial species selection was based on cleaned and thinned native occurrence numbers using two criteria to ensure the consideration of two different niches:  A lower limit of 40 native presences per species as well as a minimum difference of 40 between native and global occurrences. Additionally, species were excluded when having native assignments on the Hawaiian Islands. The spatial thinning of the occurrences pursued to avoid spatial autocorrelation using a thinning distance of 3 km. 

### 2 - Background data generation and thinning
scripts [02a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/02a_background_data_nat.R), [02b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/02b_background_data_glob.R)

For the selected study species, background data was derived. This was done by a random selection of points excluding the presence locations within a buffer distance of 200 km using a presence-absence ratio of 1:10. Spatial thinning was repeated for the generated background data using the same thinning distance of 3 km. As a last step for species data preparation, the sampled thinned background data was merged with the cleaned and thinned presence data. This described species processing steps were separately carried out based on native and global occurrences.

### 3 - Relation of environmental data to occurrence data
scripts [03a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/03a_species_env_data_nat.R), [03b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/03b_species_env_data_glob.R)

For each study species, the occurrence information was related to the environmental data. Environmental data comprised 15 climatic variables (CHELSA, version 2.1, [Karger et al. (2018)](#1)), and 14 edaphic variables ([SoilGrids](https://www.soilgrids.org), version 2.0) at a resolution of 1 km. These environmental data were related in a way that each of the two distribution datasets (based on native and global occurrences) was separately related to purely climatic and combined climatic and edaphic predictor types. As output, four differently combined datasets a joined distribution and environmental data were generated that acted as input for the SDMs. Based on the occurrence numbers of the four differently combined data sets of joined distribution and environmental data per species, the final species selection was made. Once again, a lower limit of 40 native presences and a minimum difference of 40 between native and global occurrences were applied as criteria. 

### 4 - Variable selection
scripts [04a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/04a_variable_selection_nat.R), [04b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/04b_variable_selection_glob.R)

For each of the four SDM input datasets, the four most important and weakly correlated variables based on the Boyce index were extracted from the ranked predictor output that were then considered in the upcoming model-building process. For the models that used both predictor types, the two climatic and edaphic variables that were ranked first in the predictor output were considered.

### 5 - SDM fitting and validation
scripts [05a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/05a_model_fitting_validation_clim_nat.R), [05b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/05b_model_fitting_validation_edaclim_nat.R), [05c](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/05c_model_fitting_validation_clim_glob.R), [05d](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/05d_model_fitting_validation_edaclim_glob.R), [05e](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/05e_model_validation_plot.R)

Based on each of the four SDM input datasets, four different modelling algorithms were applied to fit the models: GLM, GAM, RF, and BRT. For each model, the performance was assessed using 5-fold cross validation across five performance measures: The area under the receiver operating characteristic curve (AUC), true skill statistic (TSS), sensitivity, specificity, and the Boyce index. In line with our study objectives, three different thresholding methods were applied to derive threshold-dependent performance metrics (TSS, sensitivity, specificity). These included: the threshold that maximises TSS (maxTSS), the mean probability of training points (meanProb), and the 10th percentile of probabilities at training presences (tenthPer). Additionally, the ensemble performance of the models using the same niche in the input occurrence data, environmental data, and thresholding method was assessed.

### 6 - Model predictions
scripts [06a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/06a_model_predictions_clim_nat.R), [06b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/06b_model_predictions_edaclim_nat.R), [06c](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/06c_model_predictions_clim_glob.R), [06d](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/06d_model_predictions_edaclim_glob.R)

For each species, predictions were made to the investigated Pacific island groups considering models fitted with the four different algorithms for each SDM input dataset. Moreover, ensemble models were created by averaging the continuous predictions from the individual algorithms using the arithmetic mean. The predicted occurrence probabilities were then converted into binary predictions based on the three different thresholding methods, translating into predicted presences and absences. In a last step, the fraction of predicted suitable habitat in % per island group as well as Pacific-wide was quantified.

### 7 - Blacklist construction
scripts [07a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/07a_blacklists_clim_nat.R), [07b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/07b_blacklists_edaclim_nat.R), [07c](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/07b_blacklists_edaclim_nat.R). [07d](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/07d_blacklists_edaclim_glob.R)

Blacklists, reflecting the species with the highest Pacific-wide establishment potential, were constructed using three different definitions: The total fraction of predicted suitable habitat, the mean suitable habitat fraction over all island groups, and the total number of island groups with suitable habitats. This was done considering the predictions of the different predictor sets, the applied algorithms, and thresholding methods, resulting in 48 blacklists each based on unique information.

### 8 - Quantification of uncertainty 
scripts [08a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/08a_quantification_uncertainty.R), [08b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/08b_quantification_uncertainty_plot.R)

First, Random Forest regression was used to assess whether the establishment risk scores underlying blacklist rankings were associated with the studied key sources of uncertainty: the niche considered in the input occurrence data (native and global), the type of environmental predictor used (purely climatic and combined climatic and edaphic), the modelling algorithms (GLM, GAM, RF, BRT), and the thresholding methods applied (maxTSS, meanProb, tenthPer). Variable importance measures from the Random Forest models were used to quantify the relative contribution of each uncertainty source. This was separately done for all three blacklist definitions. Second, we directly evaluated the impact of each uncertainty factor on the blacklist rankings themselves. Specifically, we calculated the mean difference in species rankings attributable to each factor across the three blacklisting definitions. Additionally, we quantified the proportion of cases exhibiting stability or shifts in their ranking within decile bins (i.e., groups of 10).

### 9 - Unrealised colonisation potential
scripts [09a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/09a_unreal_col_pot_clim_nat.R), [09b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/09b_unreal_col_pot_edaclim_nat.R), [09c](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/09c_unreal_col_pot_clim_glob.R), [09d](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/09d_unreal_col_pot_edaclim_glob.R), [09e](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/09e_unreal_col_pot_plot.R)

Additionally, it was quantified how many non-native species could establish on currently unoccupied island groups. This was done by calculating the Pacific-wide predicted unrealised colonisation potential as the False Positive Rate for all study species, with an exclusive focus on the prediction results of ensemble models. The measure accounted for predicted presence and absence information as well as actual presence and absence information on the studied Pacific island groups. Here, the very complete occurrence data set, gathered within the study of Wohlwend et al. (2021), was used as independent validation data.

### 10 - Final blacklisting
scripts [10a](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/10a_final_blacklisting.R), [10b](https://github.com/UP-macroecology/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/blob/main/scripts/10b_final_blacklisting_plot.R)

Final blacklist rankings for all three blacklist definitions were generated considering ensemble model predictions based on the four different input datasets and the three different thresholding methods.


---------------------------------------------------------------
**Required folder structure**
---------------------------------------------------------------

```

input_data/
├── environmental_data/
│   ├── Chelsa_V2/
│   ├── SoilGrids_V2_raw/
│   └── SoilGrids_V2/
├── spatial_data/

output_data/
├── presences_thinned/
├── absences_thinned/
│   ├── native/
│   └── global/
├── distribution_data/
│   ├── native/
│   └── global/
├── distribution_env_data/
│   ├── native/
│   │   ├── clim/
│   │   └── edaclim/
│   └── global/
│       ├── clim/
│       └── edaclim/
├── distribution_env_data_subset/
│   ├── native/
│   │   ├── clim/
│   │   └── edaclim/
│   └── global/
│       ├── clim/
│       └── edaclim/
├── variable_selection/
│   ├── native/
│   │   ├── clim/
│   │   └── edaclim/
│   └── global/
│       ├── clim/
│       └── edaclim/
├── models/
│   ├── native/
│   │   ├── clim/
│   │   └── edaclim/
│   └── global/
│       ├── clim/
│       └── edaclim/
├── validation/
│   ├── native/
│   │   ├── clim/
│   │   └── edaclim/
│   └── global/
│       ├── clim/
│       └── edaclim/
├── model_predictions/
│   ├── native/
│   │   ├── clim/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   ├── clim_comp/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   └── edaclim/
│   │       ├── maxTSS/
│   │       ├── meanProb/
│   │       └── tenthPer/
│   └── global/
│       ├── clim/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       ├── clim_comp/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       └── edaclim/
│           ├── maxTSS/
│           ├── meanProb/
│           └── tenthPer/
├── blacklists/
│   ├── native/
│   │   ├── clim/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   ├── clim_comp/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   └── edaclim/
│   │       ├── maxTSS/
│   │       ├── meanProb/
│   │       └── tenthPer/
│   └── global/
│       ├── clim/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       ├── clim_comp/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       └── edaclim/
│           ├── maxTSS/
│           ├── meanProb/
│           └── tenthPer/
├── uncertainty_quantification/
├── unrealized_col_pot/
│   ├── native/
│   │   ├── clim/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   ├── clim_comp/
│   │   │   ├── maxTSS/
│   │   │   ├── meanProb/
│   │   │   └── tenthPer/
│   │   └── edaclim/
│   │       ├── maxTSS/
│   │       ├── meanProb/
│   │       └── tenthPer/
│   └── global/
│       ├── clim/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       ├── clim_comp/
│       │   ├── maxTSS/
│       │   ├── meanProb/
│       │   └── tenthPer/
│       └── edaclim/
│           ├── maxTSS/
│           ├── meanProb/
│           └── tenthPer/
├── final_blacklisting/
└── plots/
    ├── presence_absence_plots/
    ├── validation/
    ├── uncertainty_quantification/
    ├── unrealized_col_pot/
    ├── final_blacklisting/
    └── study_region/
    
scripts

```
                                                

---------------------------------------------------------------
**Required data**
---------------------------------------------------------------
All data are publicly available:
* Data frame containing information on the biogeographic status of global occurrences for plant species listed in the [PaciFlora](https://bdj.pensoft.net/article/67318/) dataset (output of this separate [repository](https://github.com/UP-macroecology/StatusAssignment)) using plant species occurrences at [GBIF](https://www.gbif.org/) and [BIEN](https://bien.nceas.ucsb.edu/bien), and biogeographic status information at [WCVP](https://powo.science.kew.org), [GIFT]( https://gift.uni-goettingen.de/home) and [GIoNAF]( https://glonaf.org,) - store in input_data (occ_status_resolved.RData)
* Initial species selection and the used validation data are based on published data by [Wohlwend et al. (2021)](#2) - store in input_data
* Initial island group selection was based on published data by [Wohlwend et al. (2021)](#2) - store in input_data/spatial_data
* Climate data is available at [CHELSA](https://chelsa-climate.org) - store in input_data/environmental_data/Chelsa_V2
* Soil data is available at [SoilGrids](https://www.soilgrids.org) - store in input_data/environmental_data/SoilGrids_V2_raw


---------------------------------------------------------------
**Operating system info**
---------------------------------------------------------------
* R version 4.3.1 (2023-06-16 ucrt)
* Platform: x86_64-w64-mingw32/x64 (64-bit)
* Running under: Windows 10 x64 (build 19045)

* Attached packages:
[1] dismo_1.3-14  [2] doParallel_1.0.17  [3] ecospat_4.0.0 [4] foreach_1.5.2  [5] gbm_2.1.8.1  [6] ggplot2_3.5.1  [7] ggtext_0.1.2  [8] maps_3.4.1  [9] mgcv_1.8-42  [10] PresenceAbsence_1.1.11  [11] randomForest_4.7-1.1  [12] readr_2.1.4  [13] sf_1.0-16  [14] sfheaders_0.4.3  [15] showtext_0.9-7  [16] terra_1.7-55  [17] tidyverse_2.0.0  [18] viridis_0.6.4



---------------------------------------------------------------
**References**
---------------------------------------------------------------
<a id="1"></a>
Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2018). Data from: Climatologies at high resolution for the earth’s land surface areas (Version 1, p. 7266970904 bytes) [Dataset]. Dryad. https://doi.org/10.5061/DRYAD.KD1D4

<a id="2"></a>
Wohlwend, MR, Craven, D, Weigelt, P, et al. (2021). Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. Divers Distrib.; 27: 1120– 1133. https://doi.org/10.1111/ddi.13260

