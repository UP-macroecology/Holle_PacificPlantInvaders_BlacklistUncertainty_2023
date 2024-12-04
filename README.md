# Uncertainty in blacklisting potential Pacific plant invaders using species distribution models

Valén Holle<sup>1</sup>, Anna Roennfeldt<sup>1</sup>  Katrin Schifferle<sup>1</sup>, Hanno Seebens<sup>2</sup>, Dylan Craven<sup>3,4</sup>, Patrick Weigelt<sup>5</sup>, Tiffany Knight<sup>6,7,8,9</sup>, Juliano Sarmento Cabral<sup>10</sup>, Damaris Zurell<sup>1</sup>  

1. University of Potsdam, Institute of Biochemistry and Biology, Am Neuen Palais 10, 14469 Potsdam, Germany
2. Department of Animal Ecology & Systematics, Justus Liebig University, Heinrich-Buff-Ring 26, 35392 Giessen, Germany
3. GEMA Center for Genomics, Ecology & Environment, Universidad Mayor, Camino La Pirámide 5750, Huechuraba, Santiago, Chile
4. Data Observatory Foundation, ANID Technology Center No. DO210001, Eliodoro Yáñez 2990, 7510277, Providencia, Santiago, Chile
5. Department of Environmental Science, Radboud Institute for Biological and Environmental Sciences (RIBES), Radboud University, Heyendaalseweg 135, 6525AJ Nijmegen, The Netherlands
6. Department of Species Interaction Ecology, Helmholtz Centre for Environmental Research – UFZ, Permoserstrasse 15, 04318 Leipzig, Germany
7. German Centre for Integrative Biodiversity Research (iDiv), Halle-Jena-Leipzig, Puschstrasse 4, 04103 Leipzig, Germany
8. Institute of Biology, Martin Luther University Halle-Wittenberg, Am Kirchtor 1, 06108 Halle (Saale), Germany
9. Department of Science and Conservation, National Tropical Botanical Garden, Kalāheo, HI, USA
10. School of Biosciences, College of Life and Environmental Sciences, University of Birmingham, Birmingham, B15 2TT, UK

Funding: This study was supported by the German Research Foundation DFG (grant no. ZU 361/3-1 to DZ)

Code: **insert Zenodo DOI**

Preprint: **insert DOI**


### ABSTRACT:
1. Invasive alien species pose a growing threat to global biodiversity, necessitating evidence-based prevention measures. Species distribution models (SDMs) are a useful tool for quantifying the potential distribution of alien species in non-native areas and deriving blacklists based on establishment risk. Yet, uncertainties due to different modelling decisions may affect predictive accuracy and the robustness of such blacklists. We thus aim to assess the relevance of three distinct sources of uncertainty in SDM based blacklists: species data, environmental data and SDM algorithms.
2. Focusing on 82 of the most invasive plant species on the Hawaiian Islands, we built SDMs to quantify their establishment potential in the Pacific region. We considered two different species datasets (native vs. global occurrences), two environmental predictor sets (climatic vs. edapho-climatic), and four different SDM algorithms. Based on SDM predictions, we derived blacklists using three distinct blacklisting definitions and quantified the variance in blacklist rankings associated with each source of uncertainty.
3. On average, SDMs showed fair predictive performance. SDM algorithm choice resulted in the largest variation in blacklist ranks while the relevance of species and environmental data was lower and varied across blacklist definitions. Nevertheless, using only native occurrences led to a clear underestimation of the establishment potential for certain species and to lower predictive performance, including high ranking species on blacklists.
4. SDMs can serve as a robust decision support tool to plan preventive management strategies. To establish robust model-aided blacklists, we recommend ensemble models using multiple SDM algorithms that rely on global rather than native occurrences only. The relevance of environmental predictors additional to climate should be carefully considered and weighed against spatial coverage of those data to ensure sufficiently large sample sizes and predictive accuracy. We advocate for explicit assessment of uncertainty to increase confidence in blacklists and allow more reliable decision making.  


---------------------------------------------------------------
**Workflow**
---------------------------------------------------------------
The analysis built on the global occurrence data of the plant species listed in the [PaciFLora](https://bdj.pensoft.net/article/67318/) dataset. Each occurrence point matched with a biogeographic status (native or introduced). Code for the data preparation can be found in a separate [repository](https://github.com/UP-macroecology/StatusAssignment).

From these species, we included an initial subset of known plant populations of 122 plant species that occur as most invasive on at least one of the Hawaiian Islands according to the [PIER](http://www.hear.org/pier/) database, derived from the compiled data set by [Wohlwend et al. (2021)](#2), in our study.


### 1 - Initial species selection
The initial species selection was based on cleaned and thinned native occurrence numbers using two criteria to ensure the consideration of two different niches:  A lower limit of 40 native presences per species as well as a minimum difference of 40 between native and global occurrences. Additionally, species were excluded when having native assignments on the Hawaiian Islands. The spatial thinning of the occurrences pursued to avoid spatial autocorrelation using a thinning distance of 3 km. 

### 2 - Background data generation and thinning
For the selected study species, background data was derived. This was done by a random selection of points excluding the presence locations within a buffer distance of 200 km using a presence-absence ratio of 1:10. Spatial thinning was repeated for the generated background data using the same thinning distance of 3 km. As a last step for species data preparation, the sampled thinned background data was merged with the cleaned and thinned presence data. This described species processing steps were separately carried out based on native and global occurrences.

### 3 - Relation of environmental data to occurrence data
For each study species, the occurrence information was related to the environmental data. Environmental data comprised 15 climatic variables (CHELSA, version 2.1, [Karger et al. (2018)](#1)), and 14 edaphic variables ([SoilGrids](https://www.soilgrids.org), version 2.0) at a resolution of 1 km. These environmental data were related in a way that each of the two distribution datasets (based on native and global occurrences) was separately related to purely climatic and combined climatic and edaphic predictor types. As output, four differently combined datasets a joined distribution and environmental data were generated that acted as input for the SDMs. Based on the occurrence numbers of the four differently combined data sets of joined distribution and environmental data per species, the final species selection was made. Once again, a lower limit of 40 native presences and a minimum difference of 40 between native and global occurrences were applied as criteria. 

### 4 - Variable selection
For each of the four SDM input datasets, the four most important and weakly correlated variables based on the Boyce index were extracted from the ranked predictor output that were then considered in the upcoming model-building process. For the models that used both predictor types, the two climatic and edaphic variables that were ranked first in the predictor output were considered.

### 5 - SDM fitting and validation
Based on each of the four SDM input datasets, four different model algorithms were applied to fit the models: GLM, GAM, RF, and BRT. For each model, the performance was assessed using the 5-fold cross validation based on five different performance measures: The area under the receiver operating characteristic curve (AUC), true skill statistic (TSS), sensitivity, specificity, and the Boyce index. Additionally, the ensemble performance of the four different models using the same niche in the input occurrence data and environmental data was assessed.

### 6 - Model predictions
For each species, predictions were made to the investigated Pacific island groups considering models fitted with the four different algorithms per SDM input data set. Moreover, an ensemble model was established by combining the continuous predictions of each algorithm using the arithmetic mean. The predicted occurrence probabilities were then converted into binary predictions, translating into predicted presences and absences. In a last step, the fraction of predicted suitable habitat in % per island group as well as Pacific-wide was quantified.

### 7 - Blacklist construction
Blacklists, reflecting the species with the highest Pacific-wide establishment potential, were constructed using three different definitions: The total fraction of predicted suitable habitat, the mean suitable habitat fraction over all island groups, and the total number of island groups with suitable habitats. This was done considering the predictions of the different predictor sets as well as the applied algorithms, resulting in 16 blacklists each based on unique information.

### 8 - Quantification of uncertainty
Using random forest regression, it was assessed whether the blacklist rankings were associated with the considered niche in the input occurrence data (native and global), the used predictor type (purely climatic and combined climatic and edaphic), and the applied algorithms (GLM, GAM, RF, BRT). The output of variable importance was considered for the uncertainty quantification of the studied sources. This was separately done for all three blacklist definitions.

### 9 - Unrealised colonisation potential
Additionally, it was quantified how many non-native species could establish on currently unoccupied island groups. This was done by calculating the Pacific-wide predicted unrealised colonisation potential as the False Positive Rate for all study species, with an exclusive focus on the prediction results of ensemble models. The measure accounted for predicted presence and absence information as well as actual presence and absence information on the studied Pacific island groups. Here, the very complete occurrence data set, gathered within the study of Wohlwend et al. (2021), was used as independent validation data.

### 10 - Final blacklisting
Final blacklist rankings were created for all three blacklist definitions based on the ensemble predictions.


---------------------------------------------------------------
**Required folder structure**
---------------------------------------------------------------

```

input_data
├── environmental_data
├── spatial_data

output_data
├── presences_thinned
├── absences_thinned
│   ├── native
│   └── global
├── distribution_data
│   ├── native
│   └── global
├── distribution_env_data
│   ├── native
│   │   ├── clim
│   │   └── edaclim
│   └── global
│       ├── clim
│       └── edaclim
├── distribution_env_data_subset
│   ├── native
│   │   ├── clim
│   │   └── edaclim
│   └── global
│       ├── clim
│       └── edaclim
├── variable_selection
│   ├── native
│   │   ├── clim
│   │   └── edaclim
│   └── global
│       ├── clim
│       └── edaclim
├── models
│   ├── native
│   │   ├── clim
│   │   └── edaclim
│   └── global
│       ├── clim
│       └── edaclim
├── validation
│   ├── native
│   │   ├── clim
│   │   └── edaclim
│   └── global
│       ├── clim
│       └── edaclim
├── model_predictions
│   ├── native
│   │   ├── clim
│   │   ├── clim_comp
│   │   └── edaclim
│   └── global
│       ├── clim
│       ├── clim_comp
│       └── edaclim
├── blacklists
│   ├── native
│   │   ├── clim
│   │   ├── clim_comp
│   │   └── edaclim
│   └── global
│       ├── clim
│       ├── clim_comp
│       └── edaclim
├── uncertainty_quantification
├── unrealized_col_pot
│   ├── native
│   │   ├── clim
│   │   ├── clim_comp
│   │   └── edaclim
│   └── global
│       ├── clim
│       ├── clim_comp
│       └── edaclim
├── final_blacklisting
│   └── algorithm_blacklisting
└── plots
    ├── presence_absence_plots
    ├── validation
    ├── response_plots
    ├── uncertainty_quantification
    ├── unrealized_col_pot
    ├── final_blacklisting
    └── study_region

scripts

```
                                                

---------------------------------------------------------------
**Required data**
---------------------------------------------------------------
All data are publicly available:
* Data frame containing information on the biogeographic status of global occurrences for plant species listed in the [PaciFlora](https://bdj.pensoft.net/article/67318/) dataset (output of this separate [repository](https://github.com/UP-macroecology/StatusAssignment)) using plant species occurrences at [GBIF](https://www.gbif.org/) and [BIEN](https://bien.nceas.ucsb.edu/bien), and biogeographic status information at [WCVP](https://powo.science.kew.org), [GIFT]( https://gift.uni-goettingen.de/home) and [GIoNAF]( https://glonaf.org,).
* Initial species selection, island group selection, and the used validation data are based on published data by [Wohlwend et al. (2021)](#2)
* Climate data is available at [CHELSA](https://chelsa-climate.org)
* Soil data is available at [SoilGrids](https://www.soilgrids.org)


---------------------------------------------------------------
**Operating system info**
---------------------------------------------------------------
* R version 4.3.1 (2023-06-16 ucrt)
* Platform: x86_64-w64-mingw32/x64 (64-bit)
* Running under: Windows 10 x64 (build 19045)

* Attached packages:
[1] terra_1.7-55  [2] tidyverse_2.0.0  [3] sf_1.0-16  [4] sfheaders_0.4.3  [5] ecospat_4.0.0  [6] mgcv_1.8-42  [7] randomForest_4.7-1.1  [8] dismo_1.3-14  [9] PresenceAbsence_1.1.11  [10] gbm_2.1.8.1  [11] viridis_0.6.4  [12] maps_3.4.1  [13] ggplot2_3.5.1   


---------------------------------------------------------------
**References**
---------------------------------------------------------------
<a id="1"></a>
Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2018). Data from: Climatologies at high resolution for the earth’s land surface areas (Version 1, p. 7266970904 bytes) [Dataset]. Dryad. https://doi.org/10.5061/DRYAD.KD1D4

<a id="2"></a>
Wohlwend, MR, Craven, D, Weigelt, P, et al. (2021). Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. Divers Distrib.; 27: 1120– 1133. https://doi.org/10.1111/ddi.13260

