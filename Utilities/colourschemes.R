#########################################################
###Define Colors
#########################################################

BlauUB <- "#07579C"
LightBlauUB <- "#07579C80"
TransparentBlauUB <- alpha(BlauUB,0.5)
Yellow <- "#ffa007"
LightYellow <- "#ffeb0f"
TransparentYellow <- alpha(Yellow,0.5)
Red <- "#ff2407"
LightRed <- "#ff9b7f"
TransparentRed <- alpha(Red,0.5)
Turquoise <-"#0cf5c6"
LightTurquoise <- "#bde0d3"
TransparentTurquoise <- alpha(Turquoise,0.5)
Lila <- "#ff02e6"

ColorScheme4 <- c(BlauUB,Yellow,Red,Turquoise)
ColorScheme3 <- c(BlauUB,Yellow,Red)
ColorScheme2 <- c(BlauUB,Yellow)
ColorScheme2.2 <- c("#07579c","#ff2407")
ColorScheme2.3 <- c("#07579c","#f22fd5")


palette7ColorsBluetoYellow <- colorRampPalette(c(BlauUB,Yellow))(7)
palette7ColorsBluetoRed <- colorRampPalette(c(BlauUB,Red))(7)
palette7ColorsYellowtoRed <- colorRampPalette(c(Yellow,Red))(7)

palette5ColorsBluetoYellow <- colorRampPalette(c(BlauUB,Yellow))(5)
palette5ColorsBluetoRed <- colorRampPalette(c(BlauUB,Red))(5)
palette5ColorsYellowtoRed <- colorRampPalette(c(Yellow,Red))(5)

palette7ColorsBluetoRedTransparent <- alpha(palette7ColorsBluetoRed,0.5)