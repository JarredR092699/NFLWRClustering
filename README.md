# NFLWRClustering
Welcome to the **NFLWRClustering** repository! explores how NFL wide receivers in 2024 can be grouped based on their phyiscal attributes and performance metrics. Using K-Means clustering, the algorithm identifies patterns and similarities among WRs to help analyze their roles, skill sets, and contributions to their teams. The analysis focuses on players with at least 350 snaps through the first nine games of the 2024 season. 

**Overview**

The **NFLWRClustering** model leverages nfl play-by-play data from the `nflfastR` package along with advanced data from [FantasyPros.com](https://www.fantasypros.com/nfl/advanced-stats-wr.php). 

![image](https://github.com/user-attachments/assets/81ebb0e6-2675-448f-a2d1-9f851185662b)
[Dashboard Link](https://public.tableau.com/app/profile/jarred.robidoux4256/viz/NFLReceiverClusters/ClusterDash)

**How It Works**
1. **Feature Selection**: The following variables are used to define player characteristics:
   - Height
   - Weight
   - aDOT (Average Depth of Target)
   - YAC per Reception (Yards After Catch per reception)
   - Yards per Reception
   - Drop Percentage (Drop %)
   - Target Share
2. **K-Means Clustering**: The K-Means algorithm groups receievers into 10 clusters based on these attributes, highlighting patterns and similarities among players. 

**Repository Structure**
- **wr_clustering.R**: Main file containing the K-means clustering model.
- **download_logos.R**: Secondary file that downloads all 32 NFL team logos for the Tableau Dashboard shown above.
- **pc2_csv**: Data that feeds the Tableau Dashboard.

**Future Improvements**

- Revisit analysis once the 2024 NFL season is completed to see how receiver clusters changed.
- Create a more interactive visualization that allows users to see the distinctions between clusters instead of just a scatterplot.

**Contributing**
If you're interested in contributing, please submit a pull request or open an issue with ideas for improving the model. 
