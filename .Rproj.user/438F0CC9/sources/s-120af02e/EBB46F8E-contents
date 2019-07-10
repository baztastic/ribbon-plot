source("data.R")

# Sys.setenv("plotly_username" = "baztastic")
# Sys.setenv("plotly_api_key" = "VOxQztJe6nHzPq3TgzkA")

spiral_moon <- function(z, moonName="Io", moonsDf=moons) {
  moon <- moonsDf[which(moonsDf$name == moonName),]
  rad <- moon$mean_orbital_radius_km
  period <- moon$orbital_period_days
  x <- rad * cos(z / period)
  y <- rad * sin(z / period)
  z <- z
  return(list(x=x, y=y, z=z))
}
z <- seq(0,150, by=0.5)

Io <- data.frame(spiral_moon(z, moonName = "Io"))
Europa <- data.frame(spiral_moon(z, moonName = "Europa"))
Ganymede <- data.frame(spiral_moon(z, moonName = "Ganymede"))
Callisto <- data.frame(spiral_moon(z, moonName = "Callisto"))
Jupiter <- spiral_moon(z <- seq(0,150, by=0.07), moonName="Jupiter", moonsDf = data.frame(name="Jupiter", orbital_period_days=0.01, mean_orbital_radius_km=jupiterRad, eccentricity=0.0))

bl <- list(color = 'rgb(0, 0, 0)', width = 2)

py <- plot_ly(x=Io$x, y=Io$y, z=Io$z, name = "Io", type="scatter3d", mode="lines", line = bl, projection = "ortho") %>%
  add_trace(x=Europa$x, y=Europa$y, z=Europa$z, name = "Europa", mode = 'lines', line = bl) %>%
  add_trace(x=Ganymede$x, y=Ganymede$y, z=Ganymede$z, name = "Ganymede", mode = 'lines', line = bl) %>%
  add_trace(x=Callisto$x, y=Callisto$y, z=Callisto$z, name = "Callisto", mode = 'lines', line = bl) %>%
  add_trace(x=Jupiter$x, y=Jupiter$y, z=Jupiter$z, name = "Jupiter", mode = 'lines', line = list(color = 'rgb(0,0,0)', width = 10)) #%>%

py

# api_create(py, filename = "Galilean Moons of Jupiter", fileopt = "overwrite")
