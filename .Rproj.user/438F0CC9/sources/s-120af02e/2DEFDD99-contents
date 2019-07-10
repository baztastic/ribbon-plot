source("data.R")

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