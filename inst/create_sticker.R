# Code for creating package sticker

# Load libraries
library(hexSticker)
library(magick)

# Read in first image
limeaid1_raw <- image_read("./inst/limeaid1.png")

# Sticker with first image
sticker1 <- sticker(
  
  # dress image
  subplot = limeaid1_raw,
  s_x = 1,
  s_y = 1.02,
  s_width = 1.4,
  s_height = 1.4,
  
  # package name
  package = "",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",
  
  # background format
  h_fill = "white",
  h_color = "#00ABCA",
  h_size = 2,
  
  # url
  url = "https://goodekat.github.io/limeaid/",
  u_size = 1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.09,
  
  # save sticker
  filename = "./inst/limeaid-sticker1.png"
  
)

# Print the sticker
print(sticker1)

# Read in second image
limeaid2_raw <- image_read("./inst/limeaid2.png")

# Sticker with first image
sticker2 <- sticker(
  
  # dress image
  subplot = limeaid2_raw,
  s_x = 1,
  s_y = 1.02,
  s_width = 1.4,
  s_height = 1.4,
  
  # package name
  package = "",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",
  
  # background format
  h_fill = "white",
  h_color = "#00ABCA",
  h_size = 2,
  
  # url
  url = "https://goodekat.github.io/limeaid/",
  u_size = 1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.09,
  
  # save sticker
  filename = "./inst/limeaid-sticker2.png"
  
)

print(sticker2)

ggplot2::ggsave(filename = "./README_files/limeaid-sticker2.png", 
                plot = sticker2, width = 4, height = 4)
