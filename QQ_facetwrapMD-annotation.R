# QQ t.test
# шаг-1. вчитываем таблицу. делаем из нее датафрейм. говорим сколько колонок и строк.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
MDepth<- data.frame(MDepths, nrow = 1008, ncol = 25)

# шаг-2. чистим датафрейм от NA значений
df<- na.omit(MDepth) 
row.has.na <- apply(df, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
df # смотрим очищенный датафрейм. теперь с ним работаем.

# шаг-3. задаем чистый датафрейм MDepths_df. говорим что по X (profiles), что по Y(depths).
MDepths_df <- data.frame(profiles = 1:25, depths = c(df$profile1, df$profile2, df$profile3, df$profile4, df$profile5, df$profile6, df$profile7, df$profile8, df$profile9, df$profile10, df$profile11, df$profile12, df$profile13, df$profile14, df$profile15, df$profile16, df$profile17, df$profile18, df$profile19, df$profile20, df$profile21, df$profile22, df$profile23, df$profile24, df$profile25)) 
summary(MDepths_df) # смотрим основную статистику датафрейма MDepths_df. теперь работаем с MDepths_df

# шаг-4. #задаем номера профилей как факторное значение
MDepths_df$profiles<- as.factor(MDepths_df$profiles) 
head(MDepths_df) 

# одиночный график 1 вариант, через qplot
p<- qplot(data = MDepths_df, sample = MDepths_df$depths, color=MDepths_df$profiles) 
# одиночный график 2 вариант через ggplot плохой: не контролирует размер точек ! но работает.
p <- ggplot(MDepths_df, aes(sample= MDepths_df$depths, color=MDepths_df$profiles)) + 
	stat_qq() +
	qplot(x, y, data=df, colour=factor(type), size=I(1)) +
	theme(legend.position = "top") + 
	labs(title="Mariana Trench, Profiles Nr.1-25, QQ Statistics (Quantile-Quantile)") 
p + scale_color_brewer(palette="Spectral") + # красим разноцветной палитрой
	theme_classic()

# множественный фасетный график по статистике QQ из 25 маленьких 	
QQ_facetwrapMD <- ggplot(MDepths_df, aes(sample= MDepths_df$depths, color=profiles, size=profiles)) + 
	stat_qq() +  
	facet_wrap( ~ profiles) +
	xlab("Theoretical Quantiles") +   
	ylab("Sample Quantiles (Depths, m)") +
	labs(title="Mariana Trench, Profiles Nr.1-25, Normal QQ Statistics (Quantile-Quantile)") +
	scale_color_grey() 	+
	scale_size_manual(values= c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)) +
	scale_x_continuous(breaks = c(seq(-3, 3, by = 1))) + # насечки оси X через 1, от -3 до 3.
  	scale_y_continuous(breaks = c(seq(-10000, 0, by = 2500))) +
  	geom_abline(intercept = mean(MDepths_df$depths), slope = sd(MDepths_df$depths), colour = "red", size = .3) +
  	geom_segment(aes(x = 1, y = -7500, xend = 0, yend = -5000), size = .1, arrow = arrow(length = unit(0.2, "cm"))) +
  	p_ann2 <- p_ann1 + annotate("text", label = "Polina", size = 4, x = 1, y = -1000) +
  	theme(
	plot.margin = margin(5, 10, 20, 5),
    plot.title = element_text(family = "Times New Roman", face = 2, size = 10),
    panel.background=ggplot2::element_rect(fill = "white"),
    legend.justification = "bottom", 
    legend.position = "right",
    legend.box.just = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_rect(fill = "white"),
    legend.key.width = unit(.3,"cm"),
    legend.key.height = unit(.3,"cm"),
    legend.spacing.x = unit(.2,"cm"),
    legend.spacing.y = unit(.3,"cm"),
    legend.box.background = element_rect(colour = "honeydew4",size=0.2),
    legend.text = element_text(family = "Arial", colour="black", size=6, face=1),
    legend.title = element_text(family = "Arial", colour="black", size=6, face=1),
    strip.text.x = element_text(colour = "white", family = "Arial", size=6, face=1),
    panel.grid.major = element_line("gray24", size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line("gray24", size = 0.1, linetype = "dotted"),
    axis.text.x = element_text(family = "Arial", face = 3, color = "gray24",size = 5, angle = 15),
    axis.text.y = element_text(family = "Arial", face = 3, color = "gray24",size = 4, angle = 15),
    axis.ticks.length=unit(.1,"cm"),
    axis.line = element_line(size = .3, colour = "grey80"),
    axis.title.y = element_text(margin = margin(t = 20, r = .3), family = "Times New Roman", face = 2, size = 8),
    axis.title.x = element_text(family = "Times New Roman", face = 2, size = 8, margin = margin(t = .2))) +
  	guides(col = guide_legend(nrow = 26, ncol = 1, byrow = TRUE))	# вытягиваем легенду вниз по вертикали.

#подписываем текст-аннотации на графики
QQ_facetwrap_ann <- QQ_facetwrapMD + annotate("text", label = "Polina", family = "Times New Roman", size = 2, x = 1, y = -8000) 
ggsave("QQ_facetwrapMD.pdf", device = cairo_pdf, fallback_resolution = 300)
