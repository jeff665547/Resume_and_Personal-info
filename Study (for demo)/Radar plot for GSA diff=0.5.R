#install.packages("plotly")
#install.packages("dplyr")
library(plotly)
library(dplyr)
#Data Input
setwd("C://Users//jeff//Desktop//study")

AA = "<b><i>Mix of MVN</i></b>
<b>�g: 0, 0</b>
�l<sub>ij</sub>: 0.1, 0.9"
AA.data = read.csv("GSA tools power simulation(MON with same Mean).csv",
                   header = TRUE)[5,-c(1, 3)]

BB = "<b><i>MVN</i></b>
�l<sub>ij</sub>: 0.1"
BB.data = read.csv("GSA tools power simulation(multivariate normal distribution(rho=0.1 and 0.9)).csv",
                   header = TRUE)[5,-c(1, 3)]

CC = "<b><i>MVN</i></b> 
�l<sub>ij</sub>: 0.9"
CC.data = read.csv("GSA tools power simulation(multivariate normal distribution(rho=0.1 and 0.9)).csv",
                   header = TRUE)[14,-c(1, 3)]

DD = "<b><i>MVT</i></b> 
�l<sub>ij</sub>: 0.9"
DD.data = read.csv("GSA tools power simulation(multivariate t).csv",
                   header = TRUE)[5,-c(1, 3)]

EE = "<b><i>Mix of MVN </i></b>
<b>�g: 0, 2</b>
�l<sub>ij</sub>: 0.1, 0.9"
EE.data = read.csv("GSA tools power simulation(MON with Different Mean(0, 2)).csv",
                   header = TRUE)[14 ,-c(1, 3)]


FF = "<b><i>Mix of MVN</i></b>
<b>�g: 0, 1</b>
�l<sub>ij</sub>: 0.1, 0.9"
FF.data = read.csv("GSA tools power simulation(MON with Different Mean(0, 1)).csv",
                   header = TRUE)[14 ,-c(1, 3)]


data = rbind(AA.data, BB.data, CC.data, DD.data, EE.data, FF.data)
colnames(data) = c("<b>Hotelling's T<sup>2</sup></b>",
                   "<b>Global Test</b>",
                   "<b>GlobalANCOVA</b>",
                   "<b>Energy Test(N-statistic)</b>",
                   "<b>GSEA(Category)</b>")
data = as.data.frame(data)

`<b>Hotelling's T<sup>2</sup></b>` = data$`<b>Hotelling's T<sup>2</sup></b>`
data = cbind(data[,-1], `<b>Hotelling's T<sup>2</sup></b>`)
########################################################################################
#Parameter Input
#radar plot �C��individual�n��R����m
fill.colors = c("rgba(0, 207, 213, 0.15)",
                "rgba(255, 153, 201, 0.15)", "rgba(27, 44, 193, 0.15)",
                "rgba(87, 184, 255, 0.15)", "rgba(249, 133, 32, 0.15)")
#radar plot �C��individual�n�e��ɩҨϥΪ���m
border.colors = c("rgba(0, 207, 213, 1)",
                  "rgba(255, 153, 201, 1)", "rgba(27, 44, 193, 1)",
                  "rgba(87, 184, 255, 1)", "rgba(249, 133, 32, 1)")
#radar plot �C��individual�n�e���(�I)�ҨϥΪ��Ÿ�(symbols)
dot.symbols = c("circle", "square", "diamond", "triangle-up", "star")
#radar plot �C��individual�n�e���(�I)�ҨϥΪ��j�p
dot.size = 13


#Radar Chart Program Start
#data polar coordinate transformation
#program
getPolarCoord <- function(r, matrix = F, na = F){
  # Get starting angle and angle increments
  theta <- 0
  dtheta <- 360 / length(r)
  dtheta <- (pi / 180) * dtheta  # in radians
  # Get polar coordinates
  x <- c()
  y <- c()
  for(i in 1:length(r)){
    x <- c(x, r[i] * cos(theta))
    y <- c(y, r[i] * sin(theta))
    theta <- theta + dtheta
  }
  x[length(x) + 1] <- x[1]
  y[length(y) + 1] <- y[1]
  if(na == T){
    x[length(x) + 1] <- NA
    y[length(y) + 1] <- NA
  }
  if(matrix == T){
    return(cbind(x, y))
  }else{
    return(list(x = x, 
                y = y))
  }
}
#Transform data
draw.data = c()
individual = c()
for(i in c(1:ncol(data))) {
  temp = data[,i]
  for(j in c(1:(nrow(data)+1))){
    j = j%%nrow(data)
    if(j == 0){j = 6}
    draw.data = rbind(draw.data, c(sapply(getPolarCoord(rep(temp[j], nrow(data))), '[[', j), 
                                   "text" = temp[j]))
    individual = rbind(individual, names(data)[i])
  }
}
draw.data = as.data.frame(draw.data)
draw.data[,4] = as.factor(individual)
names(draw.data)[4] = "individual"

# Plot
# Add grids ø�s���A��w�n�X��(���@��plot����y�b��label�B�e������)
#�i�հѼ�
#�`�N: getPolarCoord(rep(���b�|, �b�@�Ӷ�P�W�����I�Ӽ�))
grid <- rbind(getPolarCoord(rep(0.10, 80), matrix = T, na = T),
              getPolarCoord(rep(0.20, 170), matrix = T, na = T),
              getPolarCoord(rep(0.30, 250), matrix = T, na = T),
              getPolarCoord(rep(0.40, 320), matrix = T, na = T),
              getPolarCoord(rep(0.50, 400), matrix = T, na = T),
              getPolarCoord(rep(0.60, 490), matrix = T, na = T),
              getPolarCoord(rep(0.70, 590), matrix = T, na = T),
              getPolarCoord(rep(0.80, 700), matrix = T, na = T),
              getPolarCoord(rep(0.90, 790), matrix = T, na = T),
              getPolarCoord(rep(1.00, 900), matrix = T, na = T))


#program
grid <- as.data.frame(grid)

#�ХX�n�j�ժ������
#�i�հѼ�
grid.marknumber = 2
grid.mark = c(0.5, 0.8)
grid.mark.color = c(rgb(212/255, 54/255, 55/255, 0.5), rgb(50/255, 158/255, 83/255, 0.7))
grid.mark.width = c(2.5, 2.8)
grid.mark.legend = c("<b>Power: 0.5</b>", "<b>Power: 0.8</b>")
show.grid.mark.legend = c(TRUE, TRUE)

#program
grid.marks = list()
for(i in 1:grid.marknumber){
  grid.marks[[i]] = as.data.frame(getPolarCoord(rep(grid.mark[i], 900), matrix = T, na = T))
}

#��w�b�u(�C�@��Y�b)���_�l�b�|�ζb���Ӽ�(�X���) (�e�U�ӫ��������ɪ��b�u)
#�i�հѼ�
inner = getPolarCoord(rep(0.1, nrow(data)))   #�U�Ӷb���_�l��m�Z��������h��(�b�|)
outer = getPolarCoord(rep(1.025, nrow(data)))   #�U�Ӷb�����I��m�Z��������h��(�b�|)


#program
x = t(cbind(inner$x, outer$x))
y = t(cbind(inner$y, outer$y))
x <- as.numeric(apply(x, 2, function(vec){
  return(c(vec, NA))
}))
y <- as.numeric(apply(y, 2, function(vec){
  return(c(vec, NA))
}))
linegrid <- data.frame(x = x, y = y)

#ø�s�U�Ӷb���W�٥H���\���m�A�`�N�[�@�ӪŮ���̫�@�Ӷb���W��(�]���|��Ĥ@�Ӷb���|)
#�i�հѼ�
axis.name = c(AA,
              BB,
              CC,
              DD,
              EE,
              FF,"")  #�U�Ӷb���W��(e.g.: ���ҦW�١A��ئW��)
axis.name.radius = 1.18  #�b�W�٦�m�Z��������h��(�b�|)
axis.name.size = 12 #�b�W�٦r��j�p
axis.name.color = rgb(36/255,41/255,46/255,0.8)

#program
axis.labels <- paste0("", axis.name, "")
axis.pos = getPolarCoord(rep(axis.name.radius, nrow(data)))

#�ХX�C�Ӷ��ҥN�������
#�i�հѼ�
Y.label = seq(0.1, 1.1, 0.1) #�n�ФW�h����� �n�w�d�@��Хܳs���ܶq���N�[(e.g."power, score") �ҥH�~�|�O��1.1
Y.label.mean = "    <b>Power</b>" #�s���ܶq���N�[(e.g."power, score")
Y.label.pos = 4    #��ܨ�׭n�񪺷��y�Ш���(1~36) �C�W�[�@�ӼƭȥN�����F10��
Y.label.margin = 0.04    #�񪺦�m�Z���C�Ӽнu�h�ֶZ��
Y.label.size = 9
Y.label.color = rgb(0,0,0)

#program
Y.labels = paste0("<b>", Y.label, "</b>")
Y.labels = c(Y.labels, "")
Y.labels = paste0("", Y.labels, "")
Y.labels[length(Y.label)] = Y.label.mean
Y.label.position = c()
for (i in Y.label) {
  Y.label.position = rbind(Y.label.position, round(sapply(getPolarCoord(rep(i-Y.label.margin, 36)), 
                                                          '[[', Y.label.pos), digits = 3))
}
Y.label.position = rbind(Y.label.position, NA)

# Add a gray circle#�e��餺���Ƕ�(���ggplot2���ĪG)
#�i�հѼ�
biggraycircle = getPolarCoord(rep(0.98, 200)) #getPolarCoord(rep(�Ƕ�b�|, �b�@�Ӷ�P�W�����I�Ӽ�))

p = plot_ly()
#PLOT PROGRAM START
p = plot_ly(autosize = FALSE, width = 1250, height = 690) %>% #�e������
  add_trace(x= grid$x, y = grid$y, mode = "lines",
            line = list(color = rgb(0.6, 0.6, 0.6), dash = "4px", width = 1),
            showlegend = F,
            hoverinfo = "none") %>% #�e�U�ӫ��������ɪ��b�u
  add_trace(x = linegrid$x, y = linegrid$y, mode = "lines",
            line = list(color = "#57788e", dash = "4px", width = 1),
            showlegend = F,
            hoverinfo = "none") %>% #�e��餺���Ƕ�(���ggplot2���ĪG)
  add_trace(p, x = biggraycircle$x, y = biggraycircle$y, 
            fill = "toself",
            fillcolor = "rgba(200, 200, 200, 0.3)",
            line = list(color = "transparent"),
            mode = "lines",
            hoverinfo = "none",
            showlegend = F) %>% #�ХX�U�Ӷb�u���W��(e.g.:���ҡA���)
  add_trace(x = axis.pos$x, y = axis.pos$y, 
            mode = "text", text = axis.labels,
            showlegend = F,
            hoverinfo = "none",
            textfont = list(family = "MS Reference Sans Serif", size = axis.name.size, color = axis.name.color)) %>% #�ХX�C�Ӷ��ҥN�������
  add_trace(x = Y.label.position[,"x"], y = Y.label.position[,"y"], 
            mode = "text", text = Y.labels,
            showlegend = F,
            hoverinfo = "none",
            textfont = list(family = "MS Reference Sans Serif", size = Y.label.size, color = Y.label.color))
#�ե����ܾ��]���b�|��U�[�F�@�ӳz���I
p = add_trace(p, x = c(1.3, -1.3) , y = c(0, 0), 
              mode = "markers", 
              marker = list(color = "rgba(242.25, 242.25, 242.25, 0)"), 
              showlegend = F, 
              hoverinfo = "none")

p = add_trace(p, x = 0, y = -1.015, 
              mode = "markers", 
              marker = list(color = "rgba(242.25, 242.25, 242.25, 0)"), 
              showlegend = F, 
              hoverinfo = "none") 
#�ХX�n�j�ժ������
for(i in 1:grid.marknumber){
  p = add_trace(p, x= grid.marks[[i]]$x, y = grid.marks[[i]]$y, mode = "lines",
                line = list(color = grid.mark.color[i], dash = "solid", 
                            width = grid.mark.width[i]), 
                showlegend = TRUE,
                name = grid.mark.legend[i],
                hoverinfo = "none")
}
#�e�U��individual��radar��
for(i in 1: ncol(data)){
  p = add_trace(p,
                x = draw.data[draw.data$individual == names(data)[i],]$x, 
                y = draw.data[draw.data$individual == names(data)[i],]$y, 
                mode = "lines+markers",
                fill = "toself",
                fillcolor = fill.colors[i],
                marker = list(symbol = dot.symbols[i], size = dot.size, color = border.colors[i]),
                line = list(smoothing = 0.5, shape = "spline", color = border.colors[i]),
                hoverinfo = "name+text",
                name = names(data)[i],
                text = paste("Power:", draw.data[draw.data$individual == names(data)[i],]$text),
                hoverlabel = list(font = list(family = "MS Reference Sans Serif"), 
                                  namelength = -1))
}
#�M�w��ӳ̤j�ϼh�Ȫ��I������, �Ϫ�size �H��X�b�MY�b���Φ�
LL = layout(p, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                            domain = c(0, 0.6)), #radar chart �Ϯؾ�V���
            yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                         domain = c(0, 0.93)), #radar chart �Ϯت��V���
            font = list(family = "serif", size = 15),
            legend = list(x = 0.6, y = 0.93, bgcolor = "transparent", 
                          font = list(family = "MS Reference Sans Serif")),
            plot_bgcolor = toRGB(rgb(0.95, 0.95, 0.95)), #radar chart �Ϯؤ����ϼh����
            paper_bgcolor = toRGB(rgb(0.95, 0.95, 0.95))) #�̤j�ϼh�Ȫ��C��
# Add titles, description etc
#�i�հѼ�
final = layout(LL, annotations = list(
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.02, y = 1.04, 
       showarrow = F, 
       text = "<b>The Power Performance of Five GSA Tools on Six Different Scenarios</b>",
       font = list(family = "MS Reference Sans Serif",
                   size = 27, 
                   color = "#4080bf")),
  
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.02, y = 0.98, 
       showarrow = F, 
       align = "left",
       text = '<em>Remark: The performance results are based on a difference of 0.5 on the mean of the six distributions between case and control group on every dimension.</em>',
       font = list(family = "MS Reference Sans Serif",
                   size = 13, 
                   color = "#679bcb")),
  #�r���Y��(&nbsp;  �N���ť���)
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.65, y = 0.58, 
       showarrow = F, 
       align = "left",
       text = '�� <b><i>MVN</i></b>: Multivariate normal distribution
&nbsp;&nbsp; <b><i>MVT</i></b>: Multivariate t-distribution(df=3)

�� The performance of Hotelling\'s T<sup>2</sup> is the
&nbsp;&nbsp; worst among all the five GSA Tools in all                     
&nbsp;&nbsp; scenarios.
�� The performance of energy test outperforms
&nbsp;&nbsp; than all the other tests in the multivariate                    
&nbsp;&nbsp; t-distribution scenario.
�� In the multivariate normal distribution 
&nbsp;&nbsp; scenario, the correlation among variables
&nbsp;&nbsp; significantly influences the performance of 
&nbsp;&nbsp; every test.
�� The more different the scenario is from a  
&nbsp;&nbsp; multivariate standard normal distribution, 
&nbsp;&nbsp; the worse the performance of GSA tools is.',
       font = list(family = "arial",
                   size = 18))
),

#���j�ϼh�Ȫ��d��H���C��
shapes = list(
  list(
    xref = "paper", yref = "paper",
    x0 = 0, x1 = 0.98,
    y0 = 0, y1 = 1,
    type = "rect",
    layer = "above",
    fillcolor = "rgba(242, 242, 242, 0.1)",
    line = list(color = "transparent"))                  
))

print(final)