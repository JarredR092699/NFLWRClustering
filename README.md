# NFLWRClustering
Welcome to the **NFLWRClustering** repository! This project contains the code for a K-Means clustering algorithm that clusters 2024 NFL Wide Receivers together based on their physical attributes and performance metrics. 

**Overview**

The **NFLWRClustering** model leverages nfl play-by-play data from the `nflfastR` package along with advanced data from [FantasyPros.com](https://www.fantasypros.com/nfl/advanced-stats-wr.php). 

![image](https://github.com/user-attachments/assets/81ebb0e6-2675-448f-a2d1-9f851185662b)
[Dashboard Link](https://public.tableau.com/app/profile/jarred.robidoux4256/viz/NFLReceiverClusters/ClusterDash)

**Repository Structure**
- **wr_clustering.R**: Main file containing the K-means clustering model.
- **download_logos.R**: Secondary file that downloads all 32 NFL team logos for the Tableau Dashboard shown above.
- **pc2_csv**: Data that feeds the Tableau Dashboard.
