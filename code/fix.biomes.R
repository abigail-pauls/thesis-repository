#library(data.table)
#library(tidyverse)
#library(sf)

#biomes.2<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.2.csv")

# Extract file names (row 2)
#filenames <- as.character(biomes.2[2, ])

# Remove the first two rows (headers + metadata)
#coords <- biomes.2[-c(1, 2), ]

# Convert to numeric
#coords <- as.data.frame(lapply(coords, as.numeric))

#coords_long <- pivot_longer(coords,
#                            cols = everything(),
#                            names_to = "point_id",
#                            values_to = "value")

file_map <- data.frame(
  point_id = names(filenames),
  filename = filenames
)

coords_clean <- left_join(coords_clean, file_map, by = "point_id")
