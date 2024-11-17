# load libraries
library(nflreadr)
library(cfbfastR)
library(tidyverse)
library(gsisdecoder)
library(rvest)
library(janitor)
library(gghighlight)
library(ggrepel)
library(extrafont)
library(gt)
library(ggimage)

# set personal theme for rules you want every graph to have (this saves time / space)
theme_jelly <- function(){
  theme(
    text = element_text(family = "Segoe UI", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  )
}


# load snap count data 
snap_counts24 <- nflreadr::load_snap_counts(seasons = 2024)
# load roster data 
rosters <- nflreadr::load_rosters(seasons = 2024)
# load combine data to include physical attributes we need
combine <- nflreadr::load_combine()
# load teams 
teams <- nflreadr::load_teams(current = T)

# collect only receiver data from the snap_counts24 object 
wr_snaps24 <- nflreadr::load_snap_counts(seasons = 2024) %>% 
  filter(position == "WR") %>% 
  group_by(pfr_player_id, player) %>% 
  summarise(
    total_snaps = sum(offense_snaps)
  )

# create a histogram to understand where snap counts for WRs drops off this year
wr_snaps24 %>% 
  ggplot(aes(x=total_snaps))+
  geom_histogram(aes(y=..density..), position = 'identity', 
                 fill = 'navy', alpha = 0.9, bins = 30)+
  geom_density(alpha = 0.2, fill = "orange")+
  scale_x_continuous(
    breaks = seq(-600, 600, 100)
  )

# there seems to be a drop-off so lets only keep players' seasons in which they played at least 350 snaps 
wrs <- wr_snaps24 %>% 
  filter(total_snaps > 350)


# first we need to join to the rosters data frame in order to join to the pbp data down the line 
wrs_combine <- wrs %>% 
  inner_join(rosters, join_by("pfr_player_id" == "pfr_id")) %>% 
  left_join(combine, join_by("pfr_player_id" == "pfr_id")) %>% 
  select(
    pfr_player_id, player, total_snaps, team, height, weight, headshot_url, draft_round, draft_ovr, forty, gsis_id
  )

# next step is to try and find the performance metrics that we want to be included in our model: 
# target share
# yards after catch (YAC) per reception
# average depth of target
# slot rate 
# contested catches
# drop rate

# most of these other stats will come from a csv that i will find on the web, but we can gather each players aDOT (Avg. Depth of Target)
# from the load_pbp()
pbp24 <- nflreadr::load_pbp(seasons = 2024)

# clean_name function
clean_name <- function(name){
  name %>% 
    str_remove_all("\\s*(Jr\\.|Sr\\.|II|III)\\s*") %>% 
    str_trim() %>% 
    str_to_lower()
}

# gathering the aDot for each receiver in the NFL in 2024. 
aDot_df <- pbp24 %>% 
  filter(season_type == "REG" & play_type == "pass") %>% 
  group_by(receiver_player_id, receiver_player_name) %>% 
  summarize(
    air_yards = sum(air_yards, na.rm = T),
    targets = n(), 
    aDOT = air_yards / targets
  ) %>% 
  ungroup() %>% 
  select(receiver_player_id, receiver_player_name, aDOT)

# create a new object where we join the aDot to each WR
wrs_cluster <- wrs_combine %>% 
  inner_join(aDot_df, join_by("gsis_id" == "receiver_player_id")) %>% 
  left_join(teams, join_by("team" == "team_abbr")) %>% 
  mutate(
    receiver_player_name = clean_name(receiver_player_name)
  )


# now it's time to gather the fantasy pros information. to do this, we will need to use the rvest package. 
# create url object
url <- "https://www.fantasypros.com/nfl/advanced-stats-wr.php"

# read the webpage 
webpage <- read_html(url)

# extract the table 
fantasy_pros <- webpage %>% 
  # use the html_nodes function to pull in the table 
  html_nodes("table") %>% 
  # pull the first table 
  .[[1]] %>% 
  # use the html_table function to convert to a table 
  html_table(fill = TRUE)

# now that we have our data in, we can use the janitor package to clean up our column names 
fantasy_pros_clean <- fantasy_pros %>% 
  # convert the 2nd row to your column names
  janitor::row_to_names(row_number = 2) %>% 
  # clean names
  janitor::clean_names() %>% 
  # rename columns for better understanding
  rename(
    target_share = percent_tm,
  ) %>% 
  mutate(
    # converting all of these columns to numeric values and subsituting any values that may be (e.g., 1,000) so that we don't end up with NAs
    across(c(g, rec, yds, ybc, air, yac, yacon, brktkl, tgt, catchable, drop, rz_tgt, x10_yds, x20_yds, x30_yds, x40_yds, x50_yds, lng), ~as.numeric(gsub(",", "", .))),
    # converting all these columns to doubles because they include a decimal
    across(c(y_r, ybc_r, air_r, yac_r, yacon_r), as.double),
    # convert target_share to decimal 
    target_share = as.numeric(gsub("%", "", target_share)) / 100, 
    # remove the parentheses and their contents from the player column so we can join to wrs_cluster
    player = gsub("\\s*\\(.*?\\)", "", player), 
    # apply clean_name function to player
    player = clean_name(player), 
    # create receiver player name 
    receiver_player_name = str_replace(player, "^(\\w).*?\\s(.*)$", "\\1.\\2"),
    # creating a new calculated column that returns a receivers drop percentage
    drop_pct = (drop / tgt) * 100
  )

# now that we've cleaned our fantasy_pros data, we can join it back to our wrs_cluster data frame 
wrs_cluster_full <- wrs_cluster %>% 
  # left join the fantasy_pros_clean data to get other variables for our model 
  left_join(fantasy_pros_clean, join_by("receiver_player_name" == "receiver_player_name")) %>% 
  # filter out anyone who has na for g variable 
  filter(!is.na(g)) %>% 
  ungroup()

# it's time to select our variables for the cluster model along with scale them 
X <- wrs_cluster_full %>% 
  # convert variables to numerics
  mutate(
    across(c(height, weight, aDOT, yac_r, y_r, drop_pct, target_share), as.numeric)
  ) %>% 
  # select only predictor variables 
  select(
    height, weight, aDOT, yac_r, y_r, drop_pct, target_share
  ) %>% 
  # scale data
  scale()

# Since the k means algorithm fits only the number of clusters specified, some testing must be done to find the optimal k 
# the optimal k will do the best job at ensuring observations within the same cluster are as similar as possible and observations 
# in different clusters are as different as possible. We are going to fit the algorithm for 1 to 20 clusters and save the total within 
# sum of squares value from the model. 
set.seed(333) # set seed to ensure reproduceability b/c k-means relies on random states for initialization
MAX_K <- 20
sse <- c()

for (k in 1:MAX_K){
  algo_k <- kmeans(X, centers=k, nstart=22, iter.max = 20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
}

# plotting the regular sse
tibble(k = 1:MAX_K, SSE = sse) %>%
  ggplot(aes(x=k, y=SSE)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE", title = "Where does this level off?") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# plotting the sse-1
tibble(k=1:MAX_K, SSE_difference = sse-lead(sse)) %>% 
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE", title = "Where does this level off?") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# plotting the sse -2
tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# for this model we will go with 10 clusters 
set.seed(13)
# set k-means = 10 
k = 10
kmeans10 <- kmeans(X, centers = k, nstart = 22, iter.max = 20)
km_centers <- as.data.frame(kmeans10$centers) 

# name clusters 
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4', 
                        'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8', 
                        'Cluster 9', 'Cluster 10')

# massage data 
km_centers <- km_centers %>% 
  # give predictors a shorter name for plotting
  rename(
    ypr = y_r, 
    tgt_share = target_share
  ) %>% 
  # pivot data to make plotting easier 
  pivot_longer(!Cluster, names_to = "feature", values_to = "z_val")

# reset the order of predictor variables for plotting
km_centers$feature <- factor(km_centers$feature, levels = c("height", "weight", "aDOT", 
                                                            "yac_r", "ypr", "drop_pct", "tgt_share"))

# reset the clusters for plotting 
km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8',
                                                          'Cluster 9', 'Cluster 10'))

# here is a look at the first cluster compared to the other nine 
km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster))+
  geom_point(color = "navy")+
  gghighlight(Cluster == "Cluster 1", use_direct_label = F)+
  labs(
    x = "Predictor", y="Cluster Center", 
    title = "Visualizing K-Means Cluster Makeups", 
    subtitle = "Cluster 10"
  )+
  theme_minimal()+
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10))


# Principal Component Analysis 
pca <- prcomp(X) # perform Principle Component Analysis
pca_summary <- summary(pca) # summary of PCA model 

# plot % of variance between players explained by each subsequent PC
tibble(imp = pca_summary$importance[2,], n = 1:length(imp)) %>% # get importance scores for PCA summary
  ggplot(aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Combining PCA with K-Means allows you to visually examine the differences between clusters in the two dimensions that 
# account for the majority of the variance between observations. 
pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans10$cluster) # add player clusters
pc2$player <- as.character(wrs_cluster_full$player.x) # add player id 
pc2$team <- as.character(wrs_cluster_full$team) # add team abbreviation for plotting
pc2$team_logo <- as.character(wrs_cluster_full$team_logo_espn) # add team logo for plotting 
cluster1_var <- round(pca_summary$importance[2,1], 4)* 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4)* 100 # get variance explained by cluster 2

# write pc2 to csv 
# write_csv(pc2, "pc2.csv", col_names = T)


# here's how each of the K-Means clusters compare: 
clusters_compare <- km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster))+
  geom_point()+
  scale_color_brewer(palette = "Paired")+
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(color = "gray", alpha = 0.15))+
  facet_wrap(~ Cluster, ncol = 3)+
  labs(
    x="Predictor", y="Cluster Center", 
    title = "Visualizing K-Means Cluster Makeups", 
    caption = "By Jarred Robidoux | @JarredRobidoux | Data: FantasyPros"
  )+
  theme_minimal()+
  theme_jelly()+
  theme(
    legend.position = "none", strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle=90, size=8), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

clusters_compare

# save this plot 
# ggsave("cluster_comparison.jpeg", plot = clusters_compare, dpi = 600, width = 7, height = 7)


# where does every receiver land 
cluster_pyramid <- pc2 %>% 
  group_by(Cluster) %>% 
  mutate(
    Order = row_number()
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x=Cluster, y=Order))+
  geom_image(aes(image = team_logo), size = 0.035)+
  geom_text(aes(label = player), vjust = -1.5, size = 2)+
  theme_jelly()+
  labs(
    title = "Which cluster does every receiver fall under?", 
    subtitle = "Min. 350 snaps",
    captions = "By Jarred Robidoux | @JarredRobidoux | Data: FantasyPros",
    y="", 
  )

cluster_pyramid

# save the cluster pyramid 
# ggsave("cluster_pyramid.jpeg", plot = cluster_pyramid, dpi = 600, width = 7, height = 7)
