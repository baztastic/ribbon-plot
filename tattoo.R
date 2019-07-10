library(ggplot2)
library(plotly)
# library(rgl) # doesn't work with rstudio server
### this file is mainly for experiments - see the individual .R files for working versions

jupiterRad <- 71492  # radius of Jupiter (km): 71492
moons <- read.csv('galilean_moons.csv')
planets <- read.csv('planets.csv')

## first ggplot attempt
sin_moon <- function(x, moonName="Io", moonsDf=planets, logY=F) {
  moon <- moonsDf[which(moonsDf$name == moonName),]
  rad <- moon$mean_orbital_radius_km
  period <- moon$orbital_period_days
  y <- sign(runif(1)-0.5) * rad * sin(x / period + runif(1))
  # print(y)
  if(logY) {
    if(abs(y) < 1){
      return(NA)
    }
    y <- sign(y) * log(abs(y))
  }
  return(y)
}
sunRad <- planets[planets$name=="Sun","radius"]
# p <- 
growSize <- 2
p <- ggplot(data = data.frame(x=seq(0,400000)), mapping = aes(x = x)) +
  # geom_vline(xintercept =  seq(5,150, by = 5*pi)) +
  stat_function(fun = sin_moon, n = 5000, args = c(moonName="Mercury"), size=growSize*0.1, color="peachpuff") +
  # geom_ribbon(min=-(sunRad), max=(sunRad), fill='white', alpha=0.3) +
  stat_function(fun = sin_moon, n = 5000, args = c(moonName="Venus"), size=growSize*0.1, color="wheat1") +
  stat_function(fun = sin_moon, n = 5000, args = c(moonName="Earth"), size=growSize*0.1, color="dodgerblue") +
  stat_function(fun = sin_moon, n = 5000, args = c(moonName="Mars"), size=growSize*0.2, color="firebrick1") +
  stat_function(fun = sin_moon, n = 1000, args = c(moonName="Jupiter"), size=growSize*0.3, color="orange1") +
  stat_function(fun = sin_moon, n = 1000, args = c(moonName="Saturn"), size=growSize*0.3, color="lightgoldenrod1") +
  stat_function(fun = sin_moon, n = 1000, args = c(moonName="Uranus"), size=growSize*0.4, color="plum1") +
  stat_function(fun = sin_moon, n = 1000, args = c(moonName="Neptune"), size=growSize*0.4, color="skyblue") +
  
  stat_function(fun = sin_moon, n = 1000, args = c(moonName="Pluto"), size=growSize*0.5, color="darkorange") +
  geom_ribbon(min=-sunRad, max=sunRad, fill='black') +
  geom_hline(yintercept=-sunRad, color='black', size=0.3) +
  geom_hline(yintercept=sunRad, color='black', size=0.3) +
  theme_void() + 
  # coord_flip() +
  theme(
    plot.background = element_rect(fill="black"),    # Background of the entire plot
    panel.background = element_rect(fill="black")   # Background of plotting area
  )
  
ggsave(plot = p, filename='planets_ls.png', width = 2*177*2, height=200, units='mm', dpi=300)
# system('convert planets.png -channel RGB -negate planets_inverted.png')
system('./glow -a 2.5 -s 50 planets_ls.png planets_ls_glow.png')
print(p)
ggplotly(p)
## plotly attempt
spiral_moon <- function(z, moonName="Io", moonsDf=moons) {
  moon <- moonsDf[which(moonsDf$name == moonName),]
  rad <- moon$mean_orbital_radius_km
  period <- moon$orbital_period_days
  x <- rad * cos(z / period)
  y <- rad * sin(z / period)
  z <- z
  return(list(x=x, y=y, z=z))
}
z <- seq(0,150, by=0.01)

Io <- data.frame(spiral_moon(z, moonName = "Io"))
Europa <- data.frame(spiral_moon(z, moonName = "Europa"))
Ganymede <- data.frame(spiral_moon(z, moonName = "Ganymede"))
Callisto <- data.frame(spiral_moon(z, moonName = "Callisto"))
Jupiter <- spiral_moon(z <- seq(0,150, by=0.001), moonName="Jupiter", moonsDf = data.frame(name="Jupiter", orbital_period_days=0.01, mean_orbital_radius_km=jupiterRad, eccentricity=0.0))

bl <- list(color = 'rgb(0, 0, 0)', width = 2)

plot_ly(x=Io$x, y=Io$y, z=Io$z, name = "Io", type="scatter3d", mode="lines", line = bl, projection = "ortho") %>%
  add_trace(x=Europa$x, y=Europa$y, z=Europa$z, name = "Europa", mode = 'lines', line = bl) %>%
  add_trace(x=Ganymede$x, y=Ganymede$y, z=Ganymede$z, name = "Ganymede", mode = 'lines', line = bl) %>%
  add_trace(x=Callisto$x, y=Callisto$y, z=Callisto$z, name = "Callisto", mode = 'lines', line = bl) %>%
  add_trace(x=Jupiter$x, y=Jupiter$y, z=Jupiter$z, name = "Jupiter", mode = 'lines', line = list(color = 'rgb(0,0,0)', width = 1)) #%>%

## second ggplot attempt
sin_moon_infront <- function(x, moonName="Ganymede", moonsDf=moons) {
  moon <- moonsDf[which(moonsDf$name == moonName),]
  rad <- moon$mean_orbital_radius_km
  period <- moon$orbital_period_days
  initOffset <- offSets[moonName,]$initPhase
  flip <- offSets[moonName,]$flip
  # sign(runif(1)-0.5) * rad * sin(x / period + runif(1))
  y <- flip * rad * sin(x / period + initOffset)
  phase <- (-0.25 + ((x + initOffset)/period) / (2*pi)) %% 1
  depth <- sign(phase-0.5)
  if( depth >= 0 ) {
      return(y)
    } else {
      return(NA)
    }
}

sin_moon_behind <- function(x, moonName="Ganymede", moonsDf=moons) {
  moon <- moonsDf[which(moonsDf$name == moonName),]
  rad <- moon$mean_orbital_radius_km
  period <- moon$orbital_period_days
  initOffset <- offSets[moonName,]$initPhase
  flip <- offSets[moonName,]$flip
  # sign(runif(1)-0.5) * rad * sin(x / period + runif(1))
  y <- flip * rad * sin(x / period + initOffset)
  phase <- (-0.25 + ((x + initOffset)/period) / (2*pi)) %% 1
  depth <- sign(phase-0.5)
  if( depth <= 0 ) {
      return(y)
    } else {
      return(NA)
    }
}

offSets <- setNames(data.frame(
    c(10, 20, 30, 100), 
    c(-1, 1, 1, 1),
    c(0.6, 0.6, 0.6, 0.6)
  ), 
  c("initPhase", "flip", "linewidth")
  )
row.names(offSets) <- c("Io", "Europa", "Ganymede", "Callisto")

planetPos <- list(
  "Io" = 24.02,
  "Europa" = -12.60,
  "Ganymede" = 66.32,
  "Callisto" = -87.50
)
posI <- data.frame(x=abs(planetPos$Io), y=sign(planetPos$Io) * moons[which(moons$name=="Io"),"mean_orbital_radius_km"])
posE <- data.frame(x=abs(planetPos$Europa), y=sign(planetPos$Europa) * moons[which(moons$name=="Europa"),"mean_orbital_radius_km"])
posG <- data.frame(x=abs(planetPos$Ganymede), y=sign(planetPos$Ganymede) * moons[which(moons$name=="Ganymede"),"mean_orbital_radius_km"])
posC <- data.frame(x=abs(planetPos$Callisto), y=sign(planetPos$Callisto) * moons[which(moons$name=="Callisto"),"mean_orbital_radius_km"])

z <- seq(0,175, by=0.01)
IoF <- c()
IoB <- c()
EuropaF <- c()
EuropaB <- c()
GanymedeF <- c()
GanymedeB <- c()
CallistoF <- c()
CallistoB <- c()
for(x in z) {
  IoF <- c(IoF, sin_moon_infront(x, moonName = "Io"))
  IoB <- c(IoB,  sin_moon_behind(x, moonName = "Io"))
  EuropaF <- c(EuropaF, sin_moon_infront(x, moonName = "Europa"))
  EuropaB <- c(EuropaB,  sin_moon_behind(x, moonName = "Europa"))
  GanymedeF <- c(GanymedeF, sin_moon_infront(x, moonName = "Ganymede"))
  GanymedeB <- c(GanymedeB, sin_moon_behind(x, moonName = "Ganymede"))
  CallistoF <- c(CallistoF, sin_moon_infront(x, moonName = "Callisto"))
  CallistoB <- c(CallistoB,  sin_moon_behind(x, moonName = "Callisto"))
}


p <- ggplot(data = data.frame(x=z), mapping = aes(x = x))

p <- 
p + 
  geom_path(aes(z, IoB),       size=offSets["Io", "linewidth"]) +
  geom_path(aes(z, EuropaB),   size=offSets["Europa", "linewidth"]) +
  geom_path(aes(z, GanymedeB), size=offSets["Ganymede", "linewidth"]) +
  geom_path(aes(z, CallistoB), size=offSets["Callisto", "linewidth"]) +

  geom_ribbon(min=-jupiterRad, max=jupiterRad, fill='white') +
  geom_hline(yintercept=-jupiterRad, color='black', size=offSets["Io", "linewidth"]) +
  geom_hline(yintercept=jupiterRad, color='black', size=offSets["Io", "linewidth"]) +
  
  geom_path(aes(z, IoF),       size=offSets["Io", "linewidth"]) +
  geom_path(aes(z, EuropaF),   size=offSets["Europa", "linewidth"]) +
  geom_path(aes(z, GanymedeF), size=offSets["Ganymede", "linewidth"]) +
  geom_path(aes(z, CallistoF), size=offSets["Callisto", "linewidth"]) +
  
  geom_point(data=posI, aes(x, y), size=2*moons$radius[1]/min(moons$radius), fill="white", color="black", shape = 21, stroke = 0.8) +
  geom_point(data=posE, aes(x, y), size=2*moons$radius[2]/min(moons$radius), fill="white", color="black", shape = 21, stroke = 0.8) +
  geom_point(data=posG, aes(x, y), size=2*moons$radius[3]/min(moons$radius), fill="white", color="black", shape = 21, stroke = 0.8) +
  geom_point(data=posC, aes(x, y), size=2*moons$radius[4]/min(moons$radius), fill="white", color="black", shape = 21, stroke = 0.8) +
  theme_void() + ylim(-2E6, 2E6) + coord_flip()
print(p)

# ggplotly(p)

# p <- p + 
#   # geom_vline(xintercept =  seq(5,150, by = 5*pi)) +
#   stat_function(fun = sin_moon_behind, n = 1000, args = c(moonName="Io"), size=1.0) + 
#   geom_line(data = Io)
#   # geom_ribbon(min=-jupiterRad, max=jupiterRad, fill='white') +
#   # geom_hline(yintercept=-jupiterRad, color='black', size=1.2) +
#   # geom_hline(yintercept=jupiterRad, color='black', size=1.2) +
#   # stat_function(fun = sin_moon, n = 1000, args = c(moonName="Europa"), size=1.4) + 
#   # stat_function(fun = sin_moon, n = 1000, args = c(moonName="Ganymede"), size=1.6) + 
#   # stat_function(fun = sin_moon, n = 1000, args = c(moonName="Callisto"), size=1.2) + 
#   # xlim(1, 150) + 
#   theme_void() + coord_flip()
# print(p)

