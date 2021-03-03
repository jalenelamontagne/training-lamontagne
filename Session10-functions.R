library(ggplot2)

airtemps <- c(212, 30.3, 78,32)
celsius1 <- (airtemps[1]-32)*5/9
celsius2 <- (airtemps[2]-32)*5/9
celsius3 <- (airtemps[3]-32)*5/9


#' Convert temps # this is like how help files are made
#' @parm airtemps temperatures in fahrenheit
#' 
#' @return temperature in celsius
#' @export
#' 
#' @examples 


#original code from extract function - note, this will only use the third value
#fahr_to_celsius <- function(airtemps) {
#  celsius3 <- (airtemps[3]-32)*5/9
#}

#edited version to make it more general, and get it to return the value
fahr_to_celsius <- function(airtemps) {
  celsius <- (airtemps-32)*5/9
  
  return(celsius)
}

celsius4<- fahr_to_celsius(airtemps[1])

celsius_conv<-fahr_to_celsius(airtemps)

#create a function to go into reverse - from C to F
celsius_to_fahr <-function(celsius_conv){
    fahr <- (celsius_conv*9/5) +32
    
    return(fahr)
}

(fahr_test<-celsius_to_fahr(celsius_conv))

#can also use the function on a single value
celsius_to_fahr(100)

convert_temps <-function(fahr) {
  celsius <- (fahr-32) * 5/9
  kelvin <- celsius + 275.15
  
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10))) # create a dataframe
View(temps_df)

#creating a custom theme for ggplot
library(ggplot2)
custom_theme <- function(base_size = 9) {      
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}

ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(10)