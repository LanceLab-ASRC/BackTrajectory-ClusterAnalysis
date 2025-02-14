# BackTrajectory-ClusterAnalysis
---

<b>This repository houses code to generate cluster-associated representative air parcel trajectories associated with cloud water samples collected at the summit of Whiteface Mountain (NY).</b>

## Methods
This repository houses code that generates HYSPLT back trajectories for each hour that the cloud water collector was deployed during a 12-hour cloud event, then finds an average trajectory for each 12-hour cloud water sample. Cluster analysis is then done on trajectories, using partitioning around medoids, to find a representative trajectory that samples could be associated with. 
---

## Code Requirements
This code uses several packages from the R library:

- scales
- tidyverse
- lubridate
- tibbletime
- data.table
- splitr
- openair

## Running the Code

R scripts need to be run in this order:

1. HysplitMetCleanupScript.R
1. HypslitTrajectoryScript.r
1. CloudClusterProcessingScriptCorrected.R

### Reach Out For Met Data Files
---

This code was originally written and compiled by [Dr. Christopher Lawrence](https://github.com/ChrisLaw08)

