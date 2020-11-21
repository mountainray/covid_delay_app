shinyServer(function(input, output) {
# dfx<-data.frame(read_csv("https://opendata.arcgis.com/datasets/1456d8d43486449292e5784dcd9ce4a7_0.csv"),
# 		stringsAsFactors = F)

	output$plotmapx<-renderPlot({
	jj<-filter(dfx2,
		   Desc_ %in% input$measure_selector &
		   	County %in% input$county_selector &
		   	newdate>=as.Date("2020-01-01"))%>%
		mutate(color="red")

counties_included<-filter(counties, unique(jj$County) %in% input$county_selector
			  & ID %in% as.list(str_c("colorado,",tolower(unique(jj$County)))))
ggplot(data = world) +
    geom_sf(data = counties, fill = "blue", color = gray(.5)) +
    geom_sf(data = counties_included, fill = "red2", color = gray(.5)) +
    geom_sf_text(data = county_names, aes(label=word(ID,2,sep=',')), color="yellow") +
	xlab(label = "longitude")+ylab(label = "latitude")+
    coord_sf(xlim = c(-109.2, -102), ylim = c(36.9, 41.2), expand = FALSE)
})

# output$plotx<-renderPlot({
# 	jj<-filter(dfx2,
# 		   Desc_ %in% input$measure_selector &
# 		   	County %in% input$county_selector &
# 		   	newdate>=as.Date("2020-01-01"))
#
# 	ggplot(data=jj, mapping=aes(x=newdate, y=newvalue, color=County), size=2)+
# 	       	geom_line()+
# 		geom_smooth(data=all, mapping = aes(x=datex, y=hospitality_2020), color="orange")+
# 		ggtitle(input$measure_selector)
# })

output$plotx<-renderPlot({
hospitality_2020<-c(4.8,6.0,7.2,7.4,8.0)*100
dates<-c("2020-04-20","2020-05-20","2020-06-20",
	 "2020-07-20","2020-08-20")
####
xxx<-function(weeks){
	(all<-data.frame(
	#construction_2020,
	hospitality_2020,
	#trade_2020,
	datex=as.Date(dates))%>%filter(datex>="2020-04-01"))

allnew<-all
allnew$datex<-allnew$datex+(weeks)

dfx3<-inner_join(dfx2, allnew, by = c("newdate"="datex"))%>%
	filter(Desc_==input$measure_selector & County %in% c(input$county_selector))%>%
	group_by(newdate)%>%
	summarise(newvalue=round(mean(newvalue)))%>%ungroup()
(dfx4<-data.frame(dfx3, hospitality_2020=allnew[1:nrow(dfx3),1]))
allnew<-allnew[1:nrow(dfx3),]-1+(weeks+1)/(weeks+1)

fit<-glm(newvalue~hospitality_2020, dfx4, family = "poisson")

round(sum((dfx4$newvalue-fit$fitted.values)^2))
}
# x0<-xxx(0)
# x1<-xxx(1)
# x2<-xxx(2)
# x3<-xxx(3)
# x4<-xxx(4)
# x5<-xxx(5)
# x6<-xxx(6)
# x7<-xxx(7)
# x8<-xxx(8)
# x9<-xxx(9)
# x10<-xxx(10)
x0<-xxx(0)
x1<-xxx(7)
x2<-xxx(14)
x3<-xxx(21)
x4<-xxx(28)
x5<-xxx(35)
x6<-xxx(42)
x7<-xxx(49)
x8<-xxx(56)
x9<-xxx(63)
x10<-xxx(70)
#x<-data.frame(delay=c(x0,x1,x2,x3,x4,x5,x6,x7))
#x<-data.frame(delay=c(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10), weeks=c(0:10))
x<-data.frame(delay=c(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10), weeks=c(str_c(" ",as.character(c(0:9))),"10"), stringsAsFactors = F)
labelsx<-as.character(c(0:10))
theselection<-x[which(as.numeric(x$weeks)==input$delay_selector),]
theminimum<-x[which(as.numeric(x$delay)==min(x$delay)),]
#thecolor<-ifelse(theselection==theminimum, "green", "yellow")
thecolor<<-ifelse(theselection$delay==theminimum$delay, "green", "yellow")
###
#	ggplot(data=x, mapping=aes(x=x))+
	ggplot(data=x, mapping=aes(x=weeks,y=delay))+
	       	geom_bar(stat = "identity", fill="blue")+
	       	geom_bar(data=theminimum, mapping=aes(x=weeks, y=delay),stat = "identity", fill="green")+
		xlab("Weeks Delay")+ylab("Total SE")+
	       	geom_bar(data=theselection, mapping=aes(x=weeks, y=delay),stat = "identity", fill=thecolor)+
		xlab("Weeks Delay")+ylab("Total SE")+
		theme_dark() + theme(panel.background=element_rect("black")) +theme(panel.grid=element_line("black"))+
	# 	theme(axis.ticks=element_line(F))+theme(axis.text=element_text(""))+theme(axis.title=element_text(""))
#		ggtitle(input$measure_selector)
		ggtitle("Total SE")
})

#reactive(captionx<<-str_c("Raw county data (",input$measure_selector,")"))
output$tablex<-renderTable({
	head(filter(dfx2,
		   Desc_==input$measure_selector &
		   	County %in% input$county_selector &
		   	as.Date(Date, format = "%m/%d/%Y")>=as.Date("2020-01-01"))%>%
		select(County, Date, newvalue)%>%arrange(desc(Date), County)%>%
			pivot_wider(names_from = County, values_from = newvalue)
		,10)
	# captionx<-str_c("Raw county data (",input$measure_selector,")")
	},caption="Raw county data, previous 10 days")
	#},caption=captionx)
	#},caption=str_c("Raw county data (",input$measure_selector,")"))

output$plotx2<-renderPlot({

# hospitality_2020<-c(7.5,7.6,7.9,4.8,6.0,7.2,7.4,8.0,7.9,7.9)*100
# dates<-c("2020-01-20","2020-02-20","2020-03-20","2020-04-20","2020-05-20","2020-06-20",
# 	 "2020-07-20","2020-08-20","2020-09-20", "2020-10-20")
hospitality_2020<-c(4.8,6.0,7.2,7.4,8.0)*100
dates<-c("2020-04-20","2020-05-20","2020-06-20",
	 "2020-07-20","2020-08-20")

(all<-data.frame(
	#construction_2020,
	hospitality_2020,
	#trade_2020,
	datex=as.Date(dates))%>%filter(datex>="2020-04-01"))

allnew<-all
allnew$datex<-allnew$datex+input$delay_selector*7

dfx3<-inner_join(dfx2, allnew, by = c("newdate"="datex"))%>%
	filter(Desc_==input$measure_selector & County %in% c(input$county_selector))%>%
	group_by(newdate)%>%
	summarise(newvalue=round(mean(newvalue)))%>%ungroup()
(dfx4<-data.frame(dfx3, hospitality_2020=allnew[1:nrow(dfx3),1]))
allnew<-allnew[1:nrow(dfx3),]-1+(input$delay_selector+1)/(input$delay_selector+1)

fit<-glm(newvalue~hospitality_2020, dfx4, family = "poisson")

sum(exp(fit$residuals)^2)

exp(predict(fit, data.frame(hospitality_2020=790)))

box<-str_c("Total SE\n",round(sum((dfx4$newvalue-fit$fitted.values)^2)))

ggplot()+
	geom_smooth(mapping = aes(x=allnew$datex, y=fit$fitted.values), color=thecolor, se=F, size=4)+
#	geom_point(mapping = aes(x=allnew$datex, y=fit$fitted.values), color="blue", pch=19, size=4)+
	geom_smooth(data=filter(dfx2, Desc_==input$measure_selector & County %in% c(input$county_selector)),
		    mapping = aes(x=newdate, y=newvalue, color=County), se=F, alpha=.3)+
	geom_point(data=filter(dfx2, Desc_==input$measure_selector & County %in% c(input$county_selector)),
		   mapping = aes(x=newdate, y=newvalue, color=County), pch=21, size=4, alpha=.3, stroke=2)+
	# geom_smooth(mapping = aes(x=allnew$datex, y=fit$fitted.values), color="yellow", se=F, size=4, alpha=.4)+
	# geom_point(mapping = aes(x=allnew$datex, y=fit$fitted.values), color="blue", pch=21, size=4)+
	xlab(label = "Date")+ylab(label = input$measure_selector)+
		theme_dark() + theme(panel.background=element_rect("black")) +theme(panel.grid=element_line("black"))+
#	geom_label(mapping = aes(x=as.Date("2020-04-15"), y=1500), label=box, fill="yellow")
	geom_label(mapping = aes(x=as.Date("2020-04-15"), y=1500), label=box, fill=thecolor, size=7)+
	ggtitle(input$measure_selector)
})
})
