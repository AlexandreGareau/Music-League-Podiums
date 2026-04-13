library(magick)

imgs <- list.files("images/river", full.names = T)

gif <- image_read(imgs) |>
  image_animate(fps = .75)

image_write(gif, "images/animation.gif")
