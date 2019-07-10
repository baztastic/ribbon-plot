source("data.R")

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

circle <- function(theta, bodyName="Earth", bodyDf=planets) {
  body <- bodyDf[which(bodyDf$name == bodyName),]
  rad <- body$mean_orbital_radius_km
  period <- body$orbital_period_days
  x <- rad * cos(theta / period)
  y <- rad * sin(theta / period)
  return(list(x=x, y=y, theta=theta))
}
thetas <- seq(1, 2400, by=10)
MercuryC <- circle(thetas, bodyName = "Mercury")
VenusC   <- circle(thetas, bodyName = "Venus")
EarthC   <- circle(thetas, bodyName = "Earth")
MarsC    <- circle(10*thetas, bodyName = "Mars")
JupiterC <- circle(100*thetas, bodyName = "Jupiter")
SaturnC  <- circle(100*thetas, bodyName = "Saturn")
UranusC  <- circle(100*thetas, bodyName = "Uranus")
NeptuneC <- circle(500*thetas, bodyName = "Neptune")
PlutoC   <- circle(500*thetas, bodyName = "Pluto")
pc <- ggplot(data=data.frame(seq_along(thetas))) + 
  geom_path(aes(x=MercuryC$x, y=MercuryC$y), size=growSize*0.1, color="peachpuff") + 
  geom_path(aes(x=VenusC$x, y=VenusC$y), size=growSize*0.1, color="wheat1") + 
  geom_path(aes(x=EarthC$x, y=EarthC$y), size=growSize*0.1, color="dodgerblue") + 
  geom_path(aes(x=MarsC$x, y=MarsC$y), size=growSize*0.2, color="firebrick1") + 
  geom_path(aes(x=JupiterC$x, y=JupiterC$y), size=growSize*0.3, color="orange1") + 
  geom_path(aes(x=SaturnC$x, y=SaturnC$y), size=growSize*0.3, color="lightgoldenrod1") + 
  geom_path(aes(x=UranusC$x, y=UranusC$y), size=growSize*0.4, color="plum1") + 
  geom_path(aes(x=NeptuneC$x, y=NeptuneC$y), size=growSize*0.4, color="skyblue") + 
  geom_path(aes(x=PlutoC$x, y=PlutoC$y), size=growSize*0.5, color="darkorange") + 
  theme_void() + 
  # coord_flip() +
  theme(
    plot.background = element_rect(fill="black"),    # Background of the entire plot
    panel.background = element_rect(fill="black"),   # Background of plotting area
    aspect.ratio = 1
  )


sunRad <- planets[planets$name=="Sun","radius"]
# p <- 
growSize <- 2
p <- ggplot(data = data.frame(x=seq(0,400000)), mapping = aes(x = x)) +
  # geom_vline(xintercept =  seq(5,150, by = 5*pi)) +
  stat_function(fun = sin_moon, n = 5000, args = c(moonName="Mercury"), size=growSize*0.1, color="peachpuff") +
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

# ggsave(plot = p, filename='planets_ls.png', width = 2*177*2, height=200, units='mm', dpi=300)
# system('./glow -a 2.5 -s 50 planets_ls.png planets_ls_glow.png')

ggsave(plot = pc, filename='circles.png', width = 100, height=100, units='mm', dpi=300)
system('./glow -a 2.5 -s 50 circles.png circles_glow.png')

print(p)
# ggplotly(p)
