setwd("H:/service/2017/12 7 蝈蝈和小熊")
# 1.做热力图，要求底图是北京地图，能知道热力值最高的前几或前十几个值的具体地点（根据生成的热力图来确定最终地点个数）；


library(data.table)

data=read.csv("train_1.csv")

data=as.data.table(data)

data

library("leaflet")
# pal <- colorNumeric(
#   palette = "Reds",
#   domain = data$startloc_latitude_zscore)

data=data[sample(1:nrow(data),30000),]
df = data.table(Lat = data$startloc_latitude_zscore, Lon = data$startloc_longitude_zscore)

m <-leaflet(df)
# addCircles(m, 
#            color = "pink" )



df[1:100,]%>%leaflet()%>%addTiles()%>%
  addCircleMarkers(lng=~Lon,lat=~Lat , fill = TRUE, 
                   color = "pink" )


library(magrittr)
library(leaflet.extras)
df%>%leaflet()%>%addTiles() %>% 
  addHeatmap(lng=~Lon, lat=~Lat , minOpacity = 0.1, radius=6 )





 


data[,6:9]=round(data[,6:9],5)


 



# 2.计算经纬度之间的距离，即每个终点与起点的距离，画出距离的直方图分布（比如大部分距离是三公里以内）；


library(geosphere)
for(i in 1:nrow(data))data$dist[i]= distGeo(c(data$startloc_longitude_zscore[i],data$startloc_latitude_zscore[i] ),
                                                                        c(data$endloc_longitude_zscore[i],data$ endloc_latitude_zscore[i] ) )

 
x=hist(data$dist,xlim=c(0,3000),breaks = 200,ylim=c(0,5000) )


library(plotly)
trace1 <- list(
x = x$breaks[1:50], 
  y = x$counts[1:50], 
  connectgaps = FALSE, 
  line = list(
    color = "rgb(190, 207, 182)", 
    shape = "spline", 
    width = 4
  ), 
  marker = list(
    color = "rgb(222, 183, 175)", 
    line = list(
      color = "rgb(127, 127, 127)", 
      width = 2.5
    ), 
    size = 7, 
    symbol = "diamond"
  ), 
  mode = "lines+markers", 
  name = "数量", 
  showlegend = TRUE, 
  type = "scatter", 
  uid = "337af6", 
  visible = TRUE, 
  xsrc = "glttom:41:5b5e26", 
  ysrc = "glttom:41:62cafd"
)
trace2 <- list(
  x =   x$breaks[1:50],y = round(x$density[1:50]*1000,2), 
        marker = list(color = "rgb(222, 183, 175)"), 
        name = "比率", 
        opacity = 0.6, 
        type = "bar", 
        uid = "cee4ea", 
        xsrc = "glttom:41:a330b5", 
        yaxis = "y2", 
        ysrc = "glttom:41:a38cfc"
  )
  data <- list(trace1, trace2)
  layout <- list(
    bargap = 0.3, 
    font = list(
      color = "rgb(127, 127, 127)", 
      size = 18
    ), 
    legend = list(
      x = 0.35, 
      y = -0.171333579049, 
      orientation = "h"
    ), 
    margin = list(b = 40,t=40,r=70,l=80), 
    title = "骑行距离分布", 
    titlefont = list(
      color = "rgb(127, 127, 127)", 
      size = 23
    ), 
    xaxis = list(
      autorange = TRUE, 
      range = c(0, 2000),
      ticksuffix = "", 
      title = " ", 
      type = "category"
    ), 
    yaxis = list(
      autorange = TRUE, 
      # range = c(3.06061181835, 628.939388182), 
      ticksuffix = "", 
      title = "频数", 
      type = "linear"
    ), 
    yaxis2 = list(
      autorange = FALSE, 
      overlaying = "y", 
      range = c(0, 1.5),
      side = "right", 
      tickfont = list(color = "rgb(148, 103, 189)"), 
      ticksuffix = "%", 
      title = "比率", 
      titlefont = list(color = "rgb(148, 103, 189)"), 
      type = "linear"
    )
  )
  p <- plot_ly()
  p <- add_trace(p, x=trace1$x, y=trace1$y, 
                 connectgaps=trace1$connectgaps, 
                 line=trace1$line, marker=trace1$marker, 
                 mode=trace1$mode, name=trace1$name, 
                 showlegend=trace1$showlegend, type=trace1$type, uid=trace1$uid, visible=trace1$visible, xsrc=trace1$xsrc, ysrc=trace1$ysrc)
  p <- add_trace(p, x=trace2$x, y=trace2$y,
                 marker=trace2$marker, name=trace2$name,
                 opacity=trace2$opacity, type=trace2$type,
                 uid=trace2$uid, xsrc=trace2$xsrc, 
                 yaxis=trace2$yaxis, ysrc=trace2$ysrc)
  p <- layout(p, bargap=layout$bargap, font=layout$font, 
              legend=layout$legend, margin=layout$margin, 
              title=layout$title, titlefont=layout$titlefont,
              xaxis=layout$xaxis, yaxis=layout$yaxis, 
              yaxis2=layout$yaxis2)


  p



# 3.高频用户出行轨迹（在地点上，高频用户都是从哪里去哪里；在时间上，按时间分布做频次，高频用户一般在哪个时间点上用车）


 
#  route


data$pos=paste(data$startloc_latitude_zscore,data$startloc_longitude_zscore,data$endloc_latitude_zscore,data$endloc_longitude_zscore)

postab=table(data$pos)

 
#rank
 
postab=postab[order(postab,decreasing = T)]



#first 5000


postab50=postab[1:5000 ]

postab50=data.table::as.data.table(postab50)


#extract lat long
postab50$startloc_latitude_zscore=as.numeric(lapply(strsplit(postab50$V1," "),function(x)x[1]))
postab50$startloc_longitude_zscore=as.numeric(lapply(strsplit(postab50$V1," "),function(x)x[2]))
postab50$endloc_latitude_zscore=as.numeric(lapply(strsplit(postab50$V1," "),function(x)x[3]))
postab50$endloc_longitude_zscore=as.numeric(lapply(strsplit(postab50$V1," "),function(x)x[4]))


route=postab50


route%>%leaflet()%>%addTiles()%>%addProviderTiles(providers$CartoDB.Positron)%>% 
  addHeatmap(lng=~startloc_longitude_zscore, lat=~startloc_latitude_zscore , minOpacity = 0.9, radius=7)



#结束

 

route%>%leaflet()%>%addTiles()%>%addProviderTiles(providers$CartoDB.Positron)%>% 
  addHeatmap(lng=~startloc_longitude_zscore, lat=~startloc_latitude_zscore , minOpacity = 0.9, radius=7)




library(leaflet)
library(geosphere)


# route
gcIntermediate(route[1:1000,c("startloc_longitude_zscore",  "startloc_latitude_zscore" )],route[1:1000,c("endloc_longitude_zscore" ,"endloc_latitude_zscore" )],
               n=10  , breakAtDateLine=T,
               addStartEnd=T,
               sp=TRUE) %>%leaflet() %>% 
  addTiles() %>%  addProviderTiles(providers$CartoDB.Positron)%>% 
  addPolylines(fill = TRUE, 
                color = "pink" ,fillColor = "pink",
                stroke = TRUE  ,smoothFactor=99  )

 
# Plot flight routes
library(ggplot2)
library(ggrepel)
library(ggmap)
# worldmap <- borders("world", xlim = c(115,117), ylim = c(39, 41), colour="#efede1", fill="#efede1") # create a layer of borders
worldmap <- get_map(location = "Beijing", zoom = 11,color="bw")

 
 

ggmap(worldmap) +
  geom_point(data = route[1:400 ,],
             aes(x = startloc_longitude_zscore,
                 y = startloc_latitude_zscore, size = N  ),
             colour = "red", alpha =0.4)+
  geom_point(data = route[1:400 ,],
             aes(x = endloc_longitude_zscore,
                 y = endloc_latitude_zscore, size = N,stroke=N ),
             colour = "red", alpha =0.4) +
  geom_curve(data=route[1:400 ,], aes(x = startloc_longitude_zscore, 
                             y = startloc_latitude_zscore,
                             xend = endloc_longitude_zscore , 
                             yend = endloc_latitude_zscore ) , size = N, curvature = .2,
             colour = "red", alpha=0.4)+  coord_cartesian() + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

 
 # 出发点
route%>%leaflet()%>%addTiles()%>% addProviderTiles(providers$CartoDB.Positron)%>%
  addCircleMarkers(lng=~startloc_longitude_zscore,lat=~startloc_latitude_zscore , fill = TRUE, 
                   color = "pink" ,fillColor =~"red"    ,fillOpacity =~N/50 ,
                   radius =~N/10    )


# 结束点
route%>%leaflet()%>%addTiles()%>% addProviderTiles(providers$CartoDB.Positron)%>%
  addCircleMarkers(lng=~endloc_longitude_zscore,lat=~endloc_latitude_zscore , fill = TRUE, 
                   color = "pink" ,fillColor =~"red"    ,fillOpacity =~N/50 ,
                   radius =~N*2   )




 


 
# 时间

hist(data$starttime)

plot(density(data$starttime),main="时间")
 

tab=table(round(data$starttime))

library(plotly)
trace1 <- list(x=tab,y = as.numeric(names(tab)) , 
        hoverinfo = "x+y+name", 
        marker = list(
          color = "rgba(238, 76, 77, 0.5)", 
          line = list(width = 0)
        ), 
        name = "y", 
        opacity = 1, 
        orientation = "h", 
        showlegend = TRUE, 
        type = "bar", 
        uid = "2f399e", 
        visible = TRUE, 
        xsrc = "glttom:45:132231", 
        ysrc = "glttom:45:785e47"
  )
  data <- list(trace1)
  layout <- list(
    autosize = TRUE, 
    bargap = 0.26, 
    barmode = "group", 
    barnorm = "", 
    dragmode = "pan", 
    font = list(size = 18), 
    height = 742, 
    margin = list(
      r = 0, 
      t = 130, 
      b = 190, 
      l = 290, 
      pad = 5
    ), 
    showlegend = FALSE, 
    title = "<b>各时间段单车使用情况</b>", 
    titlefont = list(size = 25), 
    width = 1151, 
    xaxis = list(
      autorange = TRUE, 
      domain = c(0, 0.75), 
      range = c(0, 35.5104629053), 
      showgrid = FALSE, 
      side = "bottom", 
      tickfont = list(size = 13), 
      ticksuffix = " ", 
      title = "", 
      titlefont = list(size = 25), 
      type = "linear", 
      zeroline = FALSE
    ), 
    yaxis = list(
      autorange = TRUE, 
      range = c(-0.5, 5.5), 
      ticksuffix = "", 
      title = "", 
      type = "category"
    )
  )
  p <- plot_ly()
  p <- add_trace(p, x=trace1$x, y=trace1$y, hoverinfo=trace1$hoverinfo, marker=trace1$marker, name=trace1$name, opacity=trace1$opacity, orientation=trace1$orientation, showlegend=trace1$showlegend, type=trace1$type, uid=trace1$uid, visible=trace1$visible, xsrc=trace1$xsrc, ysrc=trace1$ysrc)
  p <- layout(p, autosize=layout$autosize, 
              bargap=layout$bargap,
              barmode=layout$barmode, 
              barnorm=layout$barnorm, dragmode=layout$dragmode,
              font=layout$font, height=layout$height, 
              margin=layout$margin, showlegend=layout$showlegend,
              title=layout$title, titlefont=layout$titlefont, 
              width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis)
  p
  
  
  
  
  #sankey 
  
  
  library(plotly)
  
  # setwd("H:/service/Toolkit/桑吉图")
  colfunc1 <- colorRampPalette(c("red", "grey"))
  colfunc1(10)
  colfunc2 <- colorRampPalette(c("lightgrey", "darkgrey"))
  colfunc2(10)
  plot(rep(1,10),col=colfunc2(10),pch=19,cex=3)
  
  
  
  
  links=read.csv( "links.csv")
  nodes=read.csv( "nodes.csv")
  
  
  
  trace1 <- list(
    domain = list(
      x = c(0, 1), 
      y = c(0, 1)
    ), 
    link = list(
      color = c(colfunc1(36)), 
      source = links$source, 
      target = links$target, 
      value = links$value
    ), 
    node = list(
      color = colfunc2(17), 
      label =  nodes$name, 
      line = list(
        color = "black", 
        width = 0
      ), 
      pad = 10, 
      thickness = 30
    ), 
    orientation = "h", 
    type = "sankey", 
    valueformat = ".0f"
  )
  data <- list(trace1)
  layout <- list(
    font = list(size = 20, family = "微软雅黑"), 
    height = 700, 
    title = "骑行地区路径分布"  , margin = list(
      r = 60, 
      t = 120, 
      b = 60, 
      l = 60
    ), titlefont = list(size = 21)
  )
  p <- plot_ly()
  p <- add_trace(p, domain=trace1$domain, link=trace1$link, node=trace1$node, orientation=trace1$orientation, type=trace1$type, valueformat=trace1$valueformat)
  p <- layout(p, font=layout$font, height=layout$height, title=layout$title)
  p
  
  
  
