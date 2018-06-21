#Empirical cumulative distribution function

MDepths <- read.csv("Depths.csv", header=TRUE)
X07<- MDepths[,07]
X07<-X07[!is.na(X07)]
as.data.frame(X07)
dat07<- data.frame(X07)
ecdf07<-ggplot(dat07, aes(X07, colour = "red")) + 
	stat_ecdf(geom = "point", show.legend = TRUE) +
	labs(title="Empirical Cumulative \n Density Function, Profile Nr.07", y = "Density", x="Depths in m") +
	theme_minimal()
	
X03<- MDepths[,03]
X03<-X03[!is.na(X03)]
as.data.frame(X03)
dat03<- data.frame(X03)
ecdf03<-ggplot(dat03, aes(x=X03, colour = "green")) + 
	stat_ecdf(geom = "point", show.legend = TRUE) +
	labs(title="Empirical Cumulative \n Density Function, Profile Nr.03", y = "Density", x="Depths in m") +
	theme_minimal()
	
	
X02<- MDepths[,02]
X02<-X02[!is.na(X02)]
as.data.frame(X02)
dat02<- data.frame(X02)
ecdf02<-ggplot(dat02, aes(x=X02, colour = "green")) + 
	stat_ecdf(geom = "step", show.legend = FALSE) +
	labs(title="Empirical Cumulative \n Density Function, Profile Nr.02", y = "Density", x="Depths in m") +
	theme_minimal()	

X04<- MDepths[,04]
X04<-X04[!is.na(X04)]
as.data.frame(X04)
dat04<- data.frame(x=X04, y = seq(1, 518))

ecdf04<-ggplot(dat04, aes(X04, y, colour = X04)) + 
	stat_ecdf(geom = "point", show.legend = FALSE) +
	geom_point(shape = 16, size = 1, show.legend = TRUE) + 
	theme_minimal() + 
	scale_color_gradient(low = "#0091ff", high = "#f0650e") +   
	scale_alpha(range = c(.05, .25)) +
	labs(title="Empirical Cumulative \n Density Function, Profile Nr.03", y = "Density", x="Depths in m")
	
# муляж данных если нет по оси y
dummy <- dat04 %>% count(X04)

# вчитать доп. ось У с номерами рядов
dat04<- data.frame(x = X04, y = seq(0:518))

ggplot(dat04, aes(x=x, y = y, color = x, alpha = 1/density)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  scale_alpha(range = c(.05, .25))
