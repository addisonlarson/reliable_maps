# reliable_maps
materials on MOEs and reducing error through aggregation

## things that actually work:
**The Map Reliability Calculator.** Find it online [here](https://aplarson.shinyapps.io/MapClassificationAutoreporter/). Check out how to use it [here](../master/how_to.pdf).

## the rest of the things in this repository:
* [sample_dat.R](../master/sample_dat.R) downloads data from the Census API and TIGER/LINE at the county, tract, and block group level, computes the percentage of Hispanic and Latino residents, and recalculates the margin of error. The first 75 lines are what's important; the back half is used for testing the size of CVs over different land areas and population densities.
* [visualize_impact.R](../master/visualize_impact.R) shows how the reliability changes across geographies, number of classes, and classification schemes.
* [classification_example.R](../master/classification_example.R) randomly selects 7 census tracts and shows the math behind the Map Reliability Calculator.
* ...all of which were used to make [this presentation](https://drive.google.com/open?id=1QZMH2_5PcD4EShTkIN8_5bNf3WQP8xmNKn63inls-Wc).
