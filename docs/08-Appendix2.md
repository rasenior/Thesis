




# Supporting information for Chapter 4

## Sampling methods for forest structure {#text-B-1}

\begin{figure}[H]

{\centering \includegraphics{./output/fig-B-1-1} 

}

\caption{Sampling design schematic.}(\#fig:fig-B-1)
\end{figure}

Several different variables have been previously identified as efficiently capturing overall forest structure [@hamer_ecology_2003; @lucey_spillover_2012]. Each plot (background circle in the schematic) was divided into quadrants (Q1-Q4). Within each quadrant we measured the distance to and circumference at breast height of the two nearest mature trees (circumference > 0.6 m) and saplings (circumference 0.1-0.6 m). Stand basal area (m^2^/ha) was calculated separately for trees and for saplings. In the above schematic, tree/sapling individuals are depicted as points: there can be zero, one or two individuals in each quadrant; the nearest individual is represented by a cross, and the furthest individual as a star. To estimate stand basal area, we calculated the basal area of each individual from its circumference at breast height, summed this across all observed individuals, divided by the true area of forest that was surveyed and multiplied by 10000 to convert units into the standard m^2^/ha. The true area surveyed is depicted by coloured quadrants; this was calculated for each quadrant individually and then summed together. Each true quadrant area was calculated using the equation:

\begin{center}
$A = \frac{1}{4} \pi r^2$
\end{center}

Where A is the area (m^2^) and r is the distance to the furthest individual (tree or sapling; m). 

To capture plot-level variation in basal area we calculated the coefficient of variation for trees and for saplings, and we also noted the proportion of observed tree individuals that were in the family Dipterocarpaceae, given the association of these species with mature, complex forest.  

Finally, to capture the overall density of vegetation at the plot centre we measured percentage canopy cover using a spherical densiometer [@lemmon_spherical_1956], and the same observer estimated percentage vegetation cover at three distinct forest strata: ground (1.5 m above ground), understorey (15 m above ground) and canopy (the main mat of leaf cover > 15 m above ground). Visual estimates of vegetation cover were made by imagining a horizontal gridded plane intersecting vegetation at the three different heights, and then estimating the percentage of grid cells occupied by vegetation. 

\pagebreak

## Extracting and processing data from thermal images {#text-B-2}

Using infrared cameras to sample microclimates in the terrestrial realm is a relatively novel methodology [@scheffers_extreme_2017; but see: @caillon_warming_2014; @faye_toolbox_2016]. There is, as yet, no standardised protocol, and there are numerous different choices of hardware. In this study, we used a FLIR Systems, model E40 camera. A single thermal image comprised 19200 distinct measurements from the infrared sensor (one per pixel). These raw data can be extracted and converted to temperature in &deg;C using the freely available software [FLIR Tools](http://www.flir.com/instruments/display/?id=54865) [cf. @scheffers_extreme_2017]. However, it is easier, faster and more thorough to use the R package `Thermimage` [@tattersall_thermimage:_2017].

Raw data were first extracted from thermal images using the function `readflirJPG`, which produces a numeric matrix of the same dimensions as the original jpeg (160 x 120). The function `raw2temp` was then used to convert raw data into temperature using standard equations from infrared thermography (see `?Thermimage::raw2temp` for more details). At this point it is possible to specify various parameters that likely differ from the default settings. For emissivity we used a value of 0.986, which represents the mean of the range (0.982 to 0.990) for bare soil, leaf litter, live tree leaves and the bark of tree trunks in green broadleaf forests [@snyder_classification-based_1998]. For atmospheric temperature and relative humidity, we used measurements taken using a whirling hygrometer  immediately prior to each sampling event at each plot. We defined the distance between the camera and the surface as the hypotenuse of an isosceles right triangle with its vertical length equal to breast height: $1.3\times\sqrt{2} =$ 1.84 m. Finally, there are five different calibration constants (PlanckR1, PlanckB, PlanckF, PlanckO and PlanckR2) that are specific to each camera, and we retrieved these from thermal images using the function `flirsettings`.

\pagebreak

## Sampling methods for microhabitat volume {#text-B-3}

We measured the volume of leaf litter in five 1 x 1 m quadrats, centred 2 m to the left of the transect edge, at 0, 10 and 20 m from the plot centre. Leaf litter was compressed inside a purpose-built compression cylinder with a plunger, and the volume read directly from a graduated scale on the cylinder [@parsons_volume_2009]. 

Within the subplot we measured the length and circumference at both ends of all intact deadwood (> 10 cm diameter). If only a portion of the deadwood was contained within the subplot, we measured that portion only. We calculated volume using Smalian’s volume formula [@waddell_sampling_2002]:

\begin{center}
$V = \frac{l \cdot (\frac{\pi}{8}) \cdot (D_S^2 + D_L^2)}{10000}$
\end{center}

Where V is volume (m^3^), l is the length (m), D~S~ is the small-end diameter (cm), D~L~ the large-end diameter (cm). We also measured the maximum and minimum diameter of entrances to all tree holes (maximum entrance diameter > 2 cm and < 2 m high), and their internal volume. Approximating the entrance to an ellipse shape, we calculated entrance area using the standard equation for area of an ellipse:

\begin{center}
$A = \pi \times a \times b$
\end{center}

Where A is entrance area (cm^2^), a is the maximum diameter of the entrance (cm) and b is the minimum diameter (cm). Internal volume could not be adequately measured for one very large tree hole, hence the plot in which it was located was excluded from analyses.

\pagebreak

## Impact of logging on macroclimate {#text-B-4}

### Methods

To interpret the impact of selective logging on thermal buffering by microclimates in a meaningful way it is also necessary to know whether macroclimate conditions are affected by selective logging. As discussed in the Materials and Methods, macroclimate temperature was measured prior to thermal image collection using a whirling hygrometer, and also by a temperature datalogger suspended at the centre of each plot (HOBO pendant datalogger, Onset, model UA-001-64K or model UA-002-64K). 

The necessity for thermoregulation, however, is dependent not only on temperature, but also on water availability. Vapour pressure deficit (VPD) encompasses both temperature and relative humidity. We measured VPD in two ways. First, using dry-bulb (i.e. macroclimate temperature) and wet-bulb temperature from the whirling hygrometer. We also suspended one hygrochron iButton datalogger (Maxim, model DS1923) 1.5 m above the ground in the plot centre of a subset of plots, alongside the HOBO dataloggers measuring macroclimate temperature. We attempted to distribute our limited number of hygrochrons as evenly as possible; ultimately we collected data from 15 plots across all six sites in primary forest, and from 13 plots across five sites in logged forest. As there were five plots in each site (Fig. 1), we placed dataloggers either in plots one, three and five, or plots one and five, depending on the number of hygrochrons available. Uneven sample sizes resulted because several hygrochrons were lost or broken. Hygrochrons measured relative humidity every 20 minutes for six days and, as in the main text (see Materials and Methods), a unique datapoint was the median value across each two-hourly increment from 04:40-14:40 hrs, on each day of recording for each of the 60 total plots.

Macroclimate VPD was calculated from saturated vapour pressure and relative humidity using the formula:

\begin{center}
$VPD = \frac{100 - RH}{100} \times SVP$
\end{center}

Where VPD is vapour pressure deficit (Pa), RH is relative humidity (%) and SVP is saturated vapour pressure (Pa). SVP was calculated from temperature:

\begin{center}
$SVP = 610.7 \times 10^ \frac{7.5 \times T_d}{237.3 + T_d}$ 
\end{center}

Where T~d~ is macroclimate (dry-bulb) temperature (&deg;C). Relative humidity can be estimated directly from a whirling hygrometer, but to reduce human error we calculated relative humidity using the equation:

\begin{center}
$RH = \frac{p}{SVP}$
\end{center}

Where p is partial vapour pressure (Pa), estimated assuming ambient pressure of 1 atm:

\begin{center}
$p = SVR_w - 66.86 \cdot (1+0.00115 \cdot(T_w)) \cdot (T_d - T_w)$
\end{center}

Where T~d~ is dry-bulb temperature (&deg;C), T~w~ is wet-bulb temperature and SVP~w~ is saturated vapour pressure at the wet-bulb temperature, calculated in the same way as SVP, but substituting in T~w~ for T~d~.   

### Statistical analysis

All supplementary analyses were carried out in an analogous way to the main analyses of microclimate temperature (see Statistical analyses). The response variables (macroclimate temperature or VPD, from either the hygrometer or dataloggers) were modelled against the fixed effects forest quality (measured as tree stand basal area; m^2^/ha) and forest type (categorical: primary forest or logged forest), using linear mixed effects models implemented in the `nlme` package [@pinheiro_nlme:_2017] in R [@r_core_team_2017]. Plot nested in site was included as a random intercept term, to account for spatial pseudoreplication. Temporal autocorrelation of residuals was evident (function `acf`), and we therefore included date and time in a correlation structure, with the best structure determined using AIC [@zuur_mixed_2009]. Statistical significance was inspected using likelihood ratio tests [see Materials and Methods; @zuur_mixed_2009], and diagnostic plots were assessed to confirm model fit.

### Results

Macroclimate temperature was comparable between primary and logged forest whether measured using a whirling hygrometer (LR = 0.081, *P* = 0.776; Fig. S2a) or suspended datalogger (LR = 0, *P* = 0.983; Fig. S2b), and was also unaffected by forest quality for both the hygrometer (LR = 0.022, *P* = 0.883; Fig. S2a) and datalogger measurements (LR = 0.527, *P* = 0.468; Fig. S2b). Similarly, macroclimate VPD did not differ according to forest type for either method of VPD measurement: hygrometer (LR = 1.344, *P* = 0.246; Fig. S2c) and suspended datalogger (LR = 3.489, *P* = 0.062; Fig. S2d). Neither did the two measures of macroclimate VPD vary with forest quality (*P* > 0.05; Fig. S2c-d).Thus, we found no evidence that selective logging impacted macroclimate temperature or macroclimate VPD.

\pagebreak

## Impact of logging on microclimate over 24 hours {#text-B-5}

### Introduction

We were primarily interested in the impact of selective logging on thermal buffering at times when buffering from extremes of heat is most necessary. In the main analyses, therefore, we limited our study to temperatures recorded between the coolest part of the day (around sunrise) and the hottest part of the day [around noon; cf. @scheffers_extreme_2017]. However, the wealth of data recorded by dataloggers also enables us to investigate how thermal buffering varies over the full 24-hour period, and particularly during the day versus during the night. In the same way that we would expect logged forests to receive more incoming solar radiation during the day -- because of reduced structural complexity and canopy cover [@okuda_effect_2003; @kumar_effects_2005] -- we would also expect these forests to radiate heat more freely at night [@chen_growing-season_1995]. Night-time conditions, although less thermally challenging, are still important biologically because nocturnal species can be inactive inside refugia during the heat of the day, but they must forage and seek mates at night if they are to survive and reproduce in the long-term.

### Statistical anlaysis

We assessed the impact of selective logging on microclimate temperature in the same way as in the main text (see Materials and Methods), but using the full datalogger dataset. Each unique datapoint was the median of six repeated measures taken every 20 minutes for each two-hourly interval, for each of six sequential days and in each of the 60 total plots (5 plots x 12 sites). As these analyses were not compared alongside results from thermal images, the two-hourly intervals began from 00:00 hrs (rather than 04:40 hrs). For simplicity, data recorded between 06:00-18:00 hrs were defined as being during the day, and 18:00-06:00 as during the night. Analyses were carried out separately for day and night and for each microhabitat: deadwood, tree holes and leaf litter. Thus, for each analysis (out of six), there was a maximum of 4320 unique datapoints: 12 time intervals x 6 days x 5 plots x 12 sites.

As in the main text, we used mixed effects models to analyse microclimate temperature as a function of forest quality (measured as tree stand basal area; m^2^/ha), forest type (primary or logged forest) and macroclimate temperature, with an interaction between the latter two variables. Models were implemented in the `nlme` package [@pinheiro_nlme:_2017] in R [@r_core_team_2017]. We included plot nested within site as a random intercept to account for spatial pseudoreplication, and both date and time in a correlation structure to account for temporal autocorrelation [the best structure was determined using AIC; @zuur_mixed_2009]. Statistical significance was inspected using likelihood ratio tests, first dropping the interaction and comparing to the full model, and then dropping main effects in turn and comparing to a model without the interaction term [@zuur_mixed_2009].

### Results

We found no effect of either forest quality or forest type on microclimates at the surface or inside deadwood and leaf litter (*P* > 0.05; Fig. S3). We found a very small effect of both variables on the absolute temperature of microclimates inside tree holes, during the day. At the median value of tree basal area, tree hole temperature in primary forest was 24.8&deg;C compared to 24.9&deg;C in logged forest (LR = 58.202, *P* < 0.001; Fig. S3b), and with an increase in forest quality (i.e. tree stand basal area) of 1 m^2^/ha, tree hole temperature increased by 0.00504&deg;C (LR = 57.814, *P* < 0.001). Evidently, these effects were extremely small, and therefore unlikely to be relevant to the majority of organisms.

Similarly, any effects of forest type on the relationship between microclimate and macroclimate temperature, while statistically significant, were small in real terms. During the day, 1&deg;C of warming in the macroclimate (from its median temperature) corresponded to more warming in primary forest than in logged forest for tree holes (LR = 18.214, *P* < 0.001; Fig. S3b) and leaf litter (LR = 40.957, *P* < 0.001; Fig. S3c), but there was no difference for microclimates inside deadwood (LR = 0.254, *P* = 0.614; Fig. S3a). At night, 1&deg;C of cooling in the macroclimate corresponded to more cooling in primary forest than in logged forest for microclimates inside deadwood (LR = 8.589, *P* < 0.01; Fig. S3d) and leaf litter (LR = 861.623, *P* < 0.001; Fig. S3f), but there was no longer any observed difference for microclimates inside tree holes (LR = 1.359, *P* = 0.244; Fig. S3e).

Overall, there is some evidence that thermal buffering from warming and cooling is slightly enhanced for microclimates in logged forest compared to primary forest, but in reality the size of these effects was so small that they are unlikely to have much biological relevance. This is also evident from the large confidence intervals of Fig. S4, which demonstrate that for most values of macroclimate temperature, primary and logged forests did not differ in microclimate temperature.

\pagebreak

## Supplementary figures

(ref:cap-B-3) The influence of forest type (primary or logged forest) and forest quality (measured as tree stand basal area; m^2^/ha) on macroclimate temperature (top row) and macroclimate vapour pressure deficit (VPD; bottom row). Macroclimate measurements collected using a whirling hygrometer are shown in the left column, and from dataloggers in the right column. Datapoints from primary forest points are depicted as blue circles, and from logged forest as orange triangles. Shaded bands are 95% confidence intervals.

\begin{figure}[H]

{\centering \includegraphics{./output/fig-B-3-1} 

}

\caption{(ref:cap-B-3)}(\#fig:fig-B-3)
\end{figure}

(ref:cap-B-4) Comparison of the relationship between microclimate temperature and macroclimate temperature within primary forest (blue circles) and logged forest (orange triangles), during the day (top row) and night (bottom row), and for three microhabitats: deadwood (left column), tree holes (centre column) and leaf litter (right column). The grey dashed line indicates zero temperature buffering, where the microclimate temperature is equal to the macroclimate temperature. Shaded bands are 95% confidence intervals.

\begin{figure}[H]

{\centering \includegraphics{./output/fig-B-4-1} 

}

\caption{(ref:cap-B-4)}(\#fig:fig-B-4)
\end{figure}

(ref:cap-B-2) Comparison between primary forest (blue) and logged forest (orange) for the nine forest structure measures: the stand basal area of trees (a) and saplings (b); the coefficient of variation for tree basal area (c) and sapling basal area (d); the proportion of trees that were in the family Dipterocarpaceae (e); the percentage canopy cover (f); and visual estimates of percentage vegetation at 1.5 m above ground (g), 15 m above ground (h) and > 15 m above ground (i). Statistically significant differences are indicated by asterisks, differentiating between: 0.01 < *P* < 0.05 (\*); 0.001 < *P* < 0.01 (\*\*) and *P* < 0.0001 (\*\*\*). Error bars are 95% confidence intervals.

\begin{figure}[H]

{\centering \includegraphics{./output/fig-B-2-1} 

}

\caption{(ref:cap-B-2)}(\#fig:fig-B-2)
\end{figure}

\clearpage
\fancyhead[R]{Bibliography}
