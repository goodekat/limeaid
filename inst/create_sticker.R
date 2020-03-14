# Code for creating package sticker

# Load libraries
library(hexSticker)
library(magick)

# Read in image
limeaid_raw <- image_read("./inst/limeaid.png")

# Print the image
print(limeaid_raw)

# Sticker with green text
sticker1 <- sticker(
  
  # dress image
  subplot = limeaid_raw,
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
  filename = "./inst/limeaid-sticker.png"
  
)

print(sticker1)

