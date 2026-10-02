# BackTrajectory-ClusterAnalysis
---

<b>This repository houses code to generate cluster-associated representative air parcel trajectories associated with cloud water samples collected at the summit of Whiteface Mountain (NY).</b>

## Methods
This repository houses code that generates HYSPLT back trajectories for each hour that the cloud water collector was deployed during a sample period. 

> [!NOTE]
> **Sample Periods for 2009-2013 are 3-hour intervals. Sample Periods for 2014 onward are 12-hour intervals.**

An associated trajectory for each cloud water sample is determined by running 144-hour back trajectories **_for each hour in-cloud_** (avg LWC > 0.05g/cm3) and taking the average. Cluster analysis is conducted for the associated sample trajectories, using a trajectory angle distance matrix, to find a representative cluster corresponding to each sample. Finally, an average trajectory and the percentage of overall trajectories associated with each cluster are determined. 

![Flow chart for HYSPLIT cluster analysis.](/images/ClusterAnalysis_Flowchart.png)

---

![Back trajecory Cluster Analysis for CW samples from 2014-2021](/images/HYSPLIT_Cluster_Analyis_2014-2021.png)
![Heat map of back trajectories associated with each cluster from 2014-2021](/images/ClusterBackTracjectoryHeatMap_2014-2021.png)
Note: While the trajectory data were processed using the R code in this directory, this plots were generated in Python

---

## Code Requirements

This code uses several packages from the R library:

- scales
- tidyverse
- lubridate
- tibbletime
- data.table
- splitr (from [rich-iannone/splitr](https://github.com/rich-iannone/splitr))
- openair

### Running the Code

> [!WARNING]
> **This code will monopolize your computer and time, as the back trajectories require pulling copious amounts of NAM data from NOAA. While R does generally pick up where it left off when there's a timeout, it still takes an exceptionally large amount of time to run even a single year's cloud water season (June-Sept).**

R scripts need to be run in this order:
1. LWC_HYSPLIT_Cleanup_Script.R
2. HypslitTrajectoryScript.R
3. CloudClusterProcessingScriptCorrected.R

> [!IMPORTANT]
> The trajectories and clustering for 2009-2025 have already been run, and can be accessed in this repository.

### Plotting the Trajectories

An attached Jupyter Notebook allows for ease of plotting trajectories by cluster, sample dump time, or sampleID.

---

The initial version of this code was developed by [Dr. Christopher Lawrence](https://github.com/ChrisLaw08).

