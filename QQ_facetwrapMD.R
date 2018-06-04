# QQ t.test facetwrap // QQ- статистика (квантили)
# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDF) # смотрим очищенный датафрейм. теперь с ним работаем.

# ЧАСТЬ 2: сшиваем все колонки типа "профиль№" в единую группу "профили". 
	# шаг-3. Значение id - номера измерений (1:518). При этом вообще не трогаем группу колонок "тектоника" (здесь они не нужны, поэтому игнорируем, и он их не будет учитывать)
MarDF_NEW<- melt(MDF, id.vars = c('observ'), measure.vars = c("profile1", "profile2", "profile3", "profile4", "profile5", "profile6", "profile7", "profile8", "profile9", "profile10","profile11", "profile12", "profile13", "profile14", "profile15", "profile16", "profile17", "profile18", "profile19", "profile20","profile21", "profile22", "profile23", "profile24", "profile25"), variable.name ='profiles')
head(MarDF_NEW)
 
# ЧАСТЬ 3: делаем множественный фасетный график из 25 профилей по QQ-статистике, рисуем график.
	# шаг-4. 
QQ_facetwrapMD<- ggplot(MarDF_NEW, aes(sample = value, color = "profiles", size = "profiles"), show.legend=FALSE) + 
	stat_qq() + 
	facet_wrap( ~ profiles, labeller = label_both) +
	xlab("Theoretical Quantiles") + 
	ylab("Sample Quantiles (Depths, m)") +  	
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。分位数统计。Normal QQ Statistics (Quantile-Quantile))",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	geom_abline(aes(intercept = mean(value), slope = sd(value), color = "mean"), size = .3) + # рисуем линию средних значений mean с наклоном по станд. девиации sd
	geom_segment(aes(x = 1, y = -7500, xend = 0, yend = -5000, color = "nr_of_samples"), size = .1, arrow = arrow(length = unit(0.1, "cm"))) + # рисуем стрелку для аннотации на каждом графике
	annotate("text", label = "n = 518", family = "Times New Roman", size = 2, color = "blue", x = 1, y = -8000) + # подписываем текст-аннотацию у стрелки (здесь: число измерений)
	scale_color_manual(name = "Legend:", values = c(mean = "red", text = "blue", nr_of_samples = "blue", profiles = "green")) +
	scale_size_manual(values= c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)) +
	scale_x_continuous(breaks = c(seq(-3, 3, by = 1))) + # насечки оси X через 1, от -3 до 3.
	scale_y_continuous(breaks = c(seq(-10000, 0, by = 2500))) +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = "bold", size = 10),
		plot.subtitle = element_text(family = "Hei", face = "bold", size = 8),
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(.1,"cm"),
		legend.key.height = unit(.1,"cm"),
		legend.spacing.x = unit(.1,"cm"),
		legend.spacing.y = unit(.1,"cm"),
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
	guides(col = guide_legend(nrow = 2, ncol = 3, byrow = TRUE)) # вытягиваем легенду вниз по вертикали.
QQ_facetwrapMD  