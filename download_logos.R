# gather url list of all images 
team_urls <- nflreadr::load_teams(current = T) %>% 
  select(team_logo_espn)

url_list <- as.vector(team_urls$team_logo_espn)

# create the name of each file 
file_names <- c("team1.png", "team2.png", "team3.png", "team4.png",
                "team5.png", "team6.png", "team7.png", "team8.png", 
                "team9.png", "team10.png", "team11.png", "team12.png",
                "team13.png", "team14.png", "team15.png", "team16.png",
                "team17.png", "team18.png", "team19.png", "team20.png", 
                "team21.png", "team22.png", "team23.png", "team24.png", 
                "team25.png", "team26.png", "team27.png", "team28.png",
                "team29.png", "team30.png", "team31.png", "team32.png")

# directory to save the images 
output_dir <- "C:/Users/Jrobidoux/Documents/nfl_knn/nfl_logos"

# ensure the directory exists 
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

# download the images 
# for(i in seq_along(url_list)){
  # download.file(
    # url = url_list[i], 
    # destfile = file.path(output_dir, file_names[i]), 
    # mode = "wb"
  # )
# }
