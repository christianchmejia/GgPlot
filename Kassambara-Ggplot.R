#install.packages("devtools")
#devtools::install_github("kassambara/survminer")
# Seleccionar mediante click
my_data <- read.delim(file.choose())

data("mtcars")
head(mtcars,3)
mtcars[,"mpg"]
#qplot(x = mpg, y = wt, data = mtcars, geom = "point")
qplot(x=mpg,y=wt, data = mtcars,geom = "point")
qplot(x=mpg,y=wt, data = mtcars,geom = c("point","smooth"))

## color depende del valor de cyl no de factor
qplot(mpg,wt,data=mtcars,color=cyl)

## color y tamaño depende de cyl como factor
mtcars$cyl <- as.factor(mtcars$cyl)
qplot(mpg,wt,data=mtcars,color=cyl,shape=cyl)

## tamaño dependera del mpg
qplot(mpg, wt, data=mtcars,size=mpg)


####### Box plot

set.seed(1234)
wdata=data.frame(sex=factor(rep(c("F","M"),each=200)),
                 weight = c(rnorm(200,55),rnorm(200,58)))

head(wdata)
## diagrama de cajas 
qplot(sex, weight, data = wdata,geom = "boxplot")

## Histograma Basico
qplot(weight, data=wdata, geom = "histogram")

# Grafico densidad
qplot(weight, data=wdata, geom = "density",
      xlab = "Weight(Kg)", ylab="Density",
      main = "Density Plot")
## guardar grafico como imagen
ggsave("densityplot.jpg",width = 5,height = 5)


ggplot(data = mtcars,aes(x=wt,y=mpg))+
  geom_point()

ggplot(data = mtcars,aes(x=wt,y=mpg))+
  geom_point(size=3,shape=18)

ggplot(data = mtcars,aes(x=wt,y=mpg))+
  geom_point(size=2,shape=23)

## grafico densidad
ggplot(wdata,aes(x=weight))+geom_density()
## g. de densidad rellenado
ggplot(wdata,aes(x=weight))+stat_density()

## grafico lineal con puntos
ggplot(data=mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_line()

## una linea para un subset
ggplot(data=mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  geom_line(data = head(mtcars),color="red")
## grafico de log x y log y
ggplot(data=mtcars,aes(x=log2(wt),y = log2(mpg)))+
  geom_point()

ggplot(data = mtcars, aes_string(x = "wt", y = "mpg")) +
  geom_point(color = "red") +
  geom_smooth()
### Uso de aes_string
ggpoints <- function(data,xName,yName){
  p <- ggplot(data=data,aes_string(xName,yName))+
    geom_point(color="red")+
    geom_smooth()
  return(p)
}

ggpoints(mtcars,xName = "wt",yName = "mpg")


## guardar graficos

### ggplot en pdf 
pdf("myplot.pdf")
myplot <- ggplot(mtcars,aes(wt,mpg))+geom_point()
print(myplot)
dev.off()
### ggplot en png
png("myplot.png")
print(myplot)
dev.off()


### data continua y discreta ###

mu <- wdata %>%
  group_by(sex) %>%
    summarise(grp.mean=mean(weight))

head(mu)

#Para una variable continua:
#- geom_area () para el diagrama de área
#- geom_density () para el gráfico de densidad
#- geom_dotplot () para diagrama de puntos
#- geom_freqpoly () para el polígono de frecuencia
#- geom_histogram () para el diagrama de histograma
#- stat_ecdf () para la función empírica de densidad acumulativa
#- stat_qq () para cuantil - gráfico de cuantiles
#Para una variable discreta:
#  - geom_bar () para diagrama de barras

a <- ggplot(wdata,aes(x=weight))
a+geom_area()
a+geom_density()
a+geom_dotplot()
a+geom_freqpoly()
a+geom_histogram()
a+stat_ecdf() #densidad acumulativa 
a+stat_qq()


#### Grafico de area
a+geom_area(stat="bin",color="black",
            fill = "#00AFBB")
#Tenga en cuenta que, por defecto, el eje y 
#corresponde al recuento de valores de peso. 
#Si desea cambiar la gráfica para tener la 
#densidad en el eje y, el código R sería el siguiente.


#Grafico de area de densidad
a+geom_area(aes(y=..density..),stat = "bin")

data("diamonds")
head(diamonds)
p <- ggplot(diamonds,aes(x=price,fill=cut))
## bar plot
p+geom_bar(stat = "bin")
#area plot
p+geom_area(stat = "bin")

### grafico de densidad
a+geom_density()

a+geom_density(color="black",fill="gray")+
  geom_vline(aes(xintercept=mean(weight)),
             color="#FC4E07",linetype="dashed",size=1)

## Colores segun sexo
#Cambiar colores de línea por sexo
a+geom_density(aes(color=sex))

## Cambiar color de relleno por sexo
# Use relleno semitransparente: alfa = 0.4
a+geom_density(aes(fill=sex),alpha=0.4)

# Añadir las líneas medias de color y por sexo

a+geom_density(aes(color=sex),alpha=0.4)+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),
             linetype="dashed")
#scale_color_manual (), scale_fill_manual (): para usar colores personalizados
#scale_color_brewer (), scale_fill_brewer (): para usar paletas de colores del paquete RColor-Brewer
#scale_color_grey (), scale_fill_grey (): para usar paletas de colores grises

a2 <- a+geom_density(aes(color=sex),alpha=0.4)+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),
             linetype="dashed")+theme_minimal()
a3 <- a+geom_density(aes(fill=sex),alpha=0.4)+theme_bw()

a2+scale_color_manual(values=c("#999999","#E69F00"))
a3+scale_fill_manual(values=c("#999999","#E69F00"))

a2+scale_color_brewer(palette = "Dark2")
a3+scale_fill_brewer(palette = "Dark2")

a2+scale_color_grey()
a3+scale_fill_grey

## Histogramas

a+geom_histogram()
a+geom_histogram(bins = 30)
a+geom_histogram(color="black",fill="gray")+
  geom_vline(aes(xintercept=mean(weight)),color = "#FC4E07"
             ,linetype="dashed",size=1)+
  theme_bw()

# ancho o bindwith = 0.5
# numero de contonedores = 50

## densidad
a+geom_histogram(aes(y=..density..))



### Histograma segun color

a+geom_histogram(aes(color=sex),fill="white")

## Histograma superpuesto, si hay mayores varones con peso 56 kg
## que mujeres con 56 este sera mostrado

a+geom_histogram(aes(color=sex),fill="white",alpha=0.6,
                 position = "identity")

a+geom_histogram(aes(color=sex),fill="white",position = "dodge")+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),linetype="dashed")

a+geom_histogram(aes(color=sex,fill=sex),position = "identity")+
  geom_vline(data=mu,aes(xintercept=grp.mean,color=sex),linetype="dashed")+    
  scale_fill_brewer(palette = "Dark2")

a + geom_histogram(aes(color = sex), fill = "white",
                   alpha = 0.4, position = "identity") +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))

a + geom_histogram(aes(color = sex, fill = sex),
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))

# Combinar el histograma con el grafico de desnidad

a + 
  geom_histogram(aes(y=..density..),
                 colour="black",fill="white")+
  geom_density(alpha=0.2,colour="blue",fill="#FF6666")
  
a + 
  geom_histogram(aes(y=..density..,fill=sex,colour=sex),
                alpha=0.5)+
  geom_density(aes(colour=sex),alpha=0.2,fill="#FF6666",size=1.1)

#Poligono de frecuencias
a+geom_freqpoly(bins=30)+theme_minimal()

a+geom_freqpoly(aes(color=sex,linetype=sex),bins=30)+
  scale_colour_brewer(palette="Set1")+theme_minimal()

#Poligono de la densidad
a+geom_freqpoly(aes(y=..density..,color=sex,linetype=sex),
                bins=30)+scale_colour_manual()

# Grafico de puntos para una variable:
a+geom_dotplot(aes(fill=sex))

#El ECDF (Función de densidad acumulativa empírica) 
#informa para cualquier número dado el porcentaje 
#de individuos que están por debajo de ese umbral

a+stat_ecdf(geom = "point")
a+stat_ecdf(geom = "step")

####### QQ PLOTS ##########
## Para ver si sigue una distribución normal

head(mtcars[,c("mpg","cyl")])

p <- ggplot(mtcars,aes(sample=mpg))
p+stat_qq()

p+stat_qq(aes(shape=cyl,color=cyl))+
  scale_color_brewer(palette = "Set1")+theme_bw()

#### Grafico de barras ####
data("mpg")
head(mpg,4)

ggplot(mpg,aes(fl))+
  geom_bar(fill="steelblue")+theme_minimal()

### Gráfico de dispersión ####

b <- ggplot(mtcars,aes(x=wt,y=mpg))
#geom_point () para el diagrama de dispersión
#geom_smooth () para agregar una línea suavizada como una línea de regresión
#geom_quantile () para agregar líneas cuantiles
#geom_rug () para agregar una alfombra marginal
#geom_jitter () para evitar sobreplotting
#geom_text () para agregar anotaciones textuales

b+geom_point()
b+geom_point()+geom_smooth(method = "lm")
b+geom_point()+geom_quantile()
b+geom_point()+geom_rug()
b+geom_point()+geom_jitter()
b+geom_text(aes(label=mpg))

b+geom_point(aes(color="#CC79A7"),show.legend = F)
b+geom_point(aes(color=cyl))
b+geom_point(aes(shape=cyl),size=3)
# Tamaño del punto de control por valores variables continuos
b+geom_point(aes(size=qsec),color="#00AFBB")

b+geom_point()+
  geom_text(label=row.names(mtcars),nudge_x = 0.5,nudge_y = 0.5)

#Se utilizan las siguientes funciones:
#-scale_shape_manual () para formas de puntos
#-scale_color_manual () para colores de puntos
#-scale_size_manual () para tamaños de puntos

b+geom_point(aes(color=cyl,shape=cyl,size=cyl))+
  scale_size_manual(values=c(2,3,4))

b+geom_point(aes(color=cyl,shape=cyl))+
  scale_shape_manual(values=c(3,16,17))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))

#Agregar línea de regresión o condicional suavizado media
#geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
#method = "lm", "glm","gam","loess","rlm".
#Calcula una regresión local suave = loess

#Tenga en cuenta que también es posible 
#indicar la fórmula como formula = y ~ poly (x, 3) 
#para especificar un grado 3

#se: valor lógico. 
#Si es VERDADERO, el intervalo de confianza se muestra suave

#fullrange: rango completo: valor lógico. Si es VERDADERO, 
#el ajuste abarca el rango completo de la trama

#level:nivel de intervalo de confianza a utilizar. 
#El valor predeterminado es 0.95

b+geom_point()+geom_smooth(method="lm")
#Sin intervalo de confianza
b+geom_point()+geom_smooth(method = "lm",se=F)

# loess method: local regression fitting
b + geom_point() + geom_smooth()

# Change color and shape by groups (cyl)
b+geom_point(aes(color=cyl,shape=cyl))+
  geom_smooth(aes(color=cyl,shape=cyl),method = lm)

b+geom_point(aes(color=cyl,shape=cyl))+
  geom_smooth(aes(color=cyl,shape=cyl),method = lm,se=F,
              fullrange=T)
#Agregar líneas cuantiles de una regresión cuantil
ggplot(mpg,aes(cty,hwy))+
  geom_point()+geom_quantile(color="blue")+
  theme_minimal()

#Puntos de fluctuación para reducir la sobreplotación
p <- ggplot(mpg, aes(displ, hwy))
p + geom_point()
# aumentar o reducir los valores
p + geom_jitter(position = position_jitter(width = 1, height = 1))
p + geom_jitter(position = position_jitter(width = 3, height = 3))

### Anotaciones de texto 

b+geom_text(aes(label=rownames(mtcars)),size=3)


## Distribución bivariada continua

data("diamonds")
head(diamonds[,c("carat","price")])

c <- ggplot(diamonds,aes(carat,price))

263/5000
# Las capas posibles incluyen:

# -geom_bin2d () para agregar un mapa de calor de 2d 
#bin recuentos. Rectángulo rectangular.

# -geom_hex () para agregar una unión hexagonal. 
#Se requiere el paquete R hexbin para esta funcionalidad

# -geom_density_2d () para agregar contornos a 
#partir de una estimación de densidad 2d

c+geom_bin2d()
c+geom_hex()
c+geom_density_2d()

#Agregar mapa de calor de recuentos de contenedores 2D
c+geom_bin2d()
c+geom_bin2d(bins=15)
c+geom_bin2d(binwidth=c(1,1000))

c+stat_bin_2d()
c+stat_summary_2d(aes(z=depth))

#Agregar hexagonal
require(hexbin)
c+geom_hex()
c+geom_hex(bins=10)
c+stat_bin_hex()
c+stat_summary_hex(aes(z=depth))
#Gráficos de dispersión con estimación de densidad 2D

data("faithful")
head(faithful)
sp <- ggplot(faithful,aes(eruptions,waiting))
sp+geom_hex()
sp+geom_density_2d(color="#E7B800")

  sp+geom_point(color="#00AFBB")+
    geom_density_2d(color="#E7B800")
#Use stat_density_2d con geom = "polygon" 
sp+geom_point()+
  stat_density_2d(aes(fill=..level..),geom = "polygon")
# Change the gradient color
sp+geom_point()+
  stat_density_2d(aes(fill=..level..),geom = "polygon")+
  scale_fill_gradient(low = "#00AFBB",high = "#FC4E07")


### Funcions continuas 

library(tidyverse)
data("economics")
head(economics)
d <- ggplot(economics,aes(x=date,y=unemploy))
#
#- geom_area () para el diagrama de área
#- geom_line () para el gráfico de línea que conecta observaciones, ordenado por x
#- geom_step () para conectar observaciones por escaleras
#

d+geom_area(fill="#00AFBB",color="black")

d+geom_line(color="#E7B800")+theme_bw()

set.seed(1234)

## extraer una muestra simple
ss <- economics[sample(1:nrow(economics),15),]
ggplot(ss,aes(x=date,y=unemploy))+
  geom_step(color="#FC4E07")

######## Grafico de discreto X y disceto Y ##########

data("diamonds")
ggplot(diamonds,aes(cut,color))+
  geom_jitter(aes(color=cut),size=0.5)

####### Grafico de variable discreta X y 
####### variable continua Y

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

#Las posibles capas incluyen:
#-geom_boxplot () para el diagrama de caja
#- geom_violin () para violín plot
#- geom_dotplot () para diagrama de puntos
#- geom_jitter () para stripchart
#- geom_line () para trazado de línea
#- geom_bar () para diagrama de barras

e <- ggplot(ToothGrowth,aes(x=dose,y=len))
e + geom_boxplot()
e + geom_violin()+theme_light()
e+geom_dotplot()
e+geom_jitter()
e+geom_line()
e+geom_bar()

### Diagrama de Cajas######

#geom_boxplot(outlier.colour = "black", outlier.shape = 16,
#             outlier.size = 2, notch = FALSE)

#outlier.colour, outlier.shape, outlier.size:
#el color, la forma y el tamaño de los puntos periféricos

e+geom_boxplot()
e+geom_boxplot()+coord_flip()
e+geom_boxplot(notch = T)

## Mostrar la media encima del grafico
e+geom_boxplot()+
  stat_summary(fun.y = mean,geom = "point",
               shape=18,size=4,color="blue")+
  theme_bw()

#Es posible usar la función scale_x_discrete () para:
  
#  elegir qué elementos mostrar: 

# por ejemplo c ("0.5", "2"), 
# por ejemplo de c ("0.5", "1", "2") a c ("2", "0.5", "1")

e+geom_boxplot()+
  scale_x_discrete(limits=c("0.5","2"))

# Orden cambiar:

e+geom_boxplot()+
  scale_x_discrete(limits=c("2","0.5","1"))

e+stat_boxplot(coef = 1.5)

#### Por grupooo

e+geom_boxplot(color="black",fill="steelblue")
e+geom_boxplot(aes(color=dose))
e+geom_boxplot(aes(fill=dose))

e2 <- e + geom_boxplot(aes(color = dose)) + theme_minimal()

e2 + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
e2 + scale_color_brewer(palette="Dark2")
e2 + scale_color_grey()



e3 <- e + geom_boxplot(aes(fill = dose)) + theme_minimal()

e3+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
e3 + scale_fill_brewer(palette="Dark2")
e3 + scale_fill_grey()

e+geom_boxplot(aes(fill=supp))
e+geom_boxplot(aes(fill=supp),position = position_dodge(1))
## position dodge: que tan separados
e+geom_boxplot(aes(fill=supp),position = position_dodge(0.9))

e + geom_boxplot(aes(fill = supp), position = position_dodge(1)) +
  scale_fill_manual(values=c("#999999", "#E69F00"))

## Violin plots #####
e+geom_violin()

### Rotar los graficos de violin 
e+geom_violin()+coord_flip()

##Tenga en cuenta que por defecto trim = TRUE. 
#En este caso, se recortan las colas de los violines.
#Si es FALSO, las colas no se recortan.
e+geom_violin(trim = F,fill="steelblue")
### Agregar un resumen de estadisticos:
e+geom_violin(trim = F)+
  stat_summary(fun.y = mean,geom = "point",
               shape=23,size=4,color="red")+
  stat_summary(fun.y = median,geom = "point",
               shape=17,size=4,color="blue")
# INNTERVALO DE CONFIANZA +DESVIACIÓN ESTANDAR  
# Añadir puntos medios +/- SD
# Use geom = "pointrange" o geom = "crossbar"

#La función mean_sdl se usa para agregar la media y 
#la desviación estándar. Calcula la media más o 
#menos una constante por la desviación estándar. 
#En el código R anterior, la constante se especifica 
#utilizando el argumento mult (mult = 1). 
#Por defecto mult = 2. La media +/- SD se puede 
#agregar como --crossbar o -pointrange.

e+geom_violin(trim = F)+
  stat_summary(fun.data = "mean_sdl",fun.args = list(mult=1.6),
               geom = "crossbar",color="red")

e+geom_violin(trim = F)+
  stat_summary(fun.data = "mean_sdl",fun.args = list(mult=1.6),
               geom = "pointrange",color="red")

## Width es el tamño de las cajas
e+geom_violin(trim = FALSE)+
  geom_boxplot(width=0.1)

e+geom_violin(trim = FALSE)+
  geom_boxplot(width=0.4)

### Cambiar el respecto de cada grafico de violin respecto a 
### la variable X

e+geom_violin(aes(color=dose),trim = FALSE)
e+geom_violin(aes(fill=dose),trim = FALSE)

e+geom_violin(aes(color=dose),trim=FALSE)+
  scale_color_brewer(palette = "Dark2")

## El eje X respecto a cada escala de supp
e+geom_violin(aes(color=supp),trim = F)

e+geom_violin(aes(color=supp),trim=F)+
  scale_color_grey()

e+geom_violin(aes(fill=supp),trim=FALSE)+
  scale_fill_brewer(palette = "Dark2")

########## Dot Plots #####
e+geom_dotplot(binaxis="y",stackdir = "center")
e + geom_dotplot(binaxis = "y", stackdir = "center",
                 stackratio = 1.5, dotsize = 1.1)

## Linea de puntos ####
df <- data.frame(dose=c("D0.5","D1","D2"),
                 len=c(4.2,10,29.5))
## each: repite cada valor 3 veces, primero 3 veces, la segunda 3 veces.
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))

p <- ggplot(data=df,aes(x=dose,y=len,group=1))
p+geom_line()+geom_point()

p+geom_line(linetype="dashed",color="blue")+
  geom_point(color="blue")

#-geom_line (): conecta observaciones, ordenadas por valor x
#-geom_path (): las observaciones están conectadas en orden original
#-geom_step (): conecta observaciones por escaleras

#### grafico escalado
p+geom_point()+geom_line()+geom_step()

#Trazado lineal con múltiples grupos

p <- ggplot(df2,aes(x=dose,y=len,group=supp))
## tipo de linea segun supp
p+geom_line(aes(linetype=supp))+
  geom_point(aes(shape=supp))

p+geom_line(aes(linetype=supp,color=supp))+
  geom_point(aes(shape=supp,color=supp))

p+geom_line(aes(linetype=supp,color=supp))+
  geom_point(aes(shape=supp,color=supp))+
  scale_color_brewer(palette = "Dark2")

df3 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("0.5", "1", "2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))

head(df3)


df3$dose <- as.numeric(as.vector(df3$dose))


###### tomar al eje x como valores numericos 
ggplot(data = df3,aes(x=dose,y=len,group=supp,color=supp))+
  geom_line()+geom_point()

## como factor
df3$dose <- as.factor(df3$dose)
ggplot(data = df3,aes(x=dose,y=len,group=supp,color=supp))+
  geom_line()+geom_point()

########## Series de tiempo ###########

head(economics)

ggplot(data = economics,aes(x=date,y=unemploy))+
  geom_line()
### Fechas mayores a 01 de enero de 2006
ss <- subset(economics,date>as.Date("2006-1-1"))

ggplot(data=ss,aes(x=date,y=unemploy))+geom_line()


ggplot(data=economics,aes(x=date,y=pop,
                          size=unemploy/pop))+
  geom_line()+theme_minimal()

######## Varias de series de tiempo superpuestos #######
head(economics)

ggplot(economics,aes(x=date))+
  geom_line(aes(y=psavert),color="darkred")+
    geom_line(aes(y=uempmed),color="steelblue",linetype="dashed")+
      theme_minimal()

##Solution 2: se funden por fecha.
library(tidyr)
require(reshape2)
## melt genera una columna con nombre variable, y el valor sera el valor de cada variable
df <- melt(economics[,c("date","psavert","uempmed")],id="date")


ggplot(df,aes(x=date,y=value))+
  geom_line(aes(color=variable))+
    theme_minimal()
###########################
iris.m <- melt(iris)
iris.m

iris.m <- melt(iris,id.vars = "Species")
iris.m <- melt(iris,id.vars = 5)
######################### Grafico de area###########
ggplot(economics,aes(x=date))+
  geom_area(aes(y=psavert),fill="#999999",color="#999999",
            alpha=0.5)+
  geom_area(aes(y=uempmed),fill="#E69F00",color="#999998",
            alpha=0.5)+
  theme_minimal()
    
######### Grafico de barras #########

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))

df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
f <- ggplot(df,aes(x=dose,y=len))
###
f+geom_bar(stat="identity")
##Cambie el color de relleno y agregue etiquetas 
#en la parte superior (vjust = -0.3)
f+geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=len),vjust=-0.3,size=4)+
    theme_minimal()

#Etiqueta dentro de las barras, vjust = 1.6
f+geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=len),vjust=1.6,size=4,color="white")+
  theme_minimal()

#Es posible cambiar el ancho de las barras 
#usando el ancho del argumento (por ejemplo: width = 0.5)
f+geom_bar(aes(color=dose),fill="white",stat="identity")

f+geom_bar(aes(fill=dose),stat="identity")

f+geom_bar(aes(color=dose),stat="identity",fill="white")+
  scale_color_brewer(palette = "Dark2")

f+geom_bar(aes(fill=dose),stat="identity")+
  scale_fill_brewer(palette = "Dark2")


g <- ggplot(data=df2,aes(x=dose,y=len,fill=supp))
# Gráfico de barras apiladas
g+geom_bar(stat="identity")
# Use position = position_dodge ()
# Barras no apiladas
g+geom_bar(stat="identity",position = position_dodge())

#Agregue etiquetas a un diagrama de barra esquivado:
g+geom_bar(stat="identity",position = position_dodge())+
  geom_text(aes(label=len),vjust=1.6,color="white",
            position = position_dodge(0.8),size=6)


library(plyr)
# Clasificar por dose y supp
df_sorted <- arrange(df2,dose,supp)
head(df_sorted)
# Calcular la suma acumulativa de len para cada dosis

df_cumsum <- ddply(df_sorted,"dose",transform,
                   label_ypos=cumsum(len))
## label_ypos: la posicion donde se pondra la etiqueta y el
## valor del acumulado

head(df_cumsum)

ggplot(df_cumsum,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=label_ypos,label=len),vjust=1.6,size=3.5,
            color="white")

#Si desea colocar las etiquetas en el medio de las barras, 
#debe modificar la suma acumulativa de la siguiente manera:

df_cumsum <- ddply(df_sorted,"dose",transform,
                   label_ypos=cumsum(len)-0.5*len)

ggplot(df_cumsum,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=label_ypos,label=len),vjust=-0.5,size=5,
            color="white")+theme_light()
############## Visualizing Error ###############
df <- ToothGrowth
df$dose <- as.factor(df$dose)
head(df,3)

library(dplyr)
df2 <- df %>% 
        group_by(dose) %>%
          summarise(sd=sd(len),len=mean(len))

head(df2)
library(ggplot2)
f <- ggplot(df2,aes(x=dose,y=len,
            ymin=len-sd,ymax=len+sd))

# Las capas posibles incluyen:
# -geom_crossbar () para barra hueca con el 
#centro indicado por una línea horizontal

# -geom_errorbar () para barras de error

# -geom_errorbarh () para barras de error horizontales

# -geom_linerange () para dibujar un intervalo 
#representado por una línea vertical

# -geom_pointrange () para crear un intervalo 
#representado por una línea vertical, 
#con un punto en el medio.
f+geom_crossbar()
f+geom_errorbar()
f+geom_linerange()
f+geom_pointrange()

## Cross Bar
f+geom_crossbar()
f+geom_crossbar(aes(color=dose))

f+geom_crossbar(aes(color=dose))+
  scale_color_brewer(palette = "Dark2")+
    theme_minimal()

f+geom_crossbar(aes(fill=dose))

df3 <- df %>% 
      group_by(supp,dose) %>%
        summarise(sd=sd(len),len=mean(len))
head(df3)

f <- ggplot(df3,aes(x=dose,y=len,ymin=len-sd,ymax=len+sd))
f+geom_crossbar(aes(color=supp))

f+geom_crossbar(aes(color=supp),
                position = position_dodge(1))+
                scale_color_brewer(palette = "Dark2")
#Una alternativa simple a geom_crossbar () 
#es usar la función stat_summary () de la siguiente manera. 
#En este caso, la media y
#el -SD se pueden calcular automáticamente.

ggplot(df,aes(x=dose,y=len,color=supp))+
  stat_summary(fun.data = "mean_sdl",geom = "crossbar",
               fun.args = list(mult=1),
               width=0.6,position = position_dodge(0.8))


f <- ggplot(df2, aes(x = dose, y = len,
                     ymin = len-sd, ymax = len+sd))

f+geom_errorbar(aes(color=dose),width=0.2)
##Combinar con grafico de linea
f+geom_line(aes(group=1))+
  geom_errorbar(width=0.2)

## Combinar con bar plot por color y grupo
f+geom_bar(aes(color=dose),stat="identity",fill="white")+
  geom_errorbar(aes(color=dose),width=0.2)
## Mantener solo barras de error superiores
## Width: ancho de la barra
f+geom_bar(aes(color=dose),stat="identity",fill="white")+
  geom_errorbar(aes(color=dose,ymin=len),width=0.2)

f <- ggplot(df3,aes(x=dose,y=len,
                    ymin=len-sd,ymax=len+sd))
## Grafico de barras junto a grafico de barras de errores
f + geom_bar(aes(fill = supp), stat = "identity",
             position = position_dodge(1))+
  geom_errorbar(aes(color=supp),position =position_dodge(1))
## Grafico de lineas junto a grafico de barras de errores
f+geom_line(aes(group=supp,color=supp))+
  geom_point(aes(color=supp))+
  geom_errorbar(aes(color=supp),width=0.2,
                position = position_dodge(0.05))

### Grafico de pie 

df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50))
head(df)

library(ggplot2)
p <- ggplot(df,aes(x="",y=value,fill=group))+
  geom_bar(width = 1,stat = "identity")+
  coord_polar("y",start = 0)

p+scale_fill_brewer(palette = "Dark2")

#Gráficos circulares personalizados
# Crear tema negro
blank_theme <- theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14,face="bold")
        )
## blank_theme, genera un fondo blanco solo con el grafico
require(scales)
library(scales)
p+scale_fill_brewer("Blues")+blank_theme+
  geom_text(aes(y=value/3+c(0,cumsum(value)[-length(value)]),
                label=percent(value/100)),size=5)

#### Parametros Graficos

#Esta sección describe cómo agregar 
#elementos gráficos (polígono, trayectoria, cinta, 
#segmento y rectángulo) a un gráfico.

#Se utilizarán las siguientes funciones:
# -geom_polygon (): agrega polígono, una ruta rellena
# -geom_path (): conecta las observaciones en el orden original
# -geom_ribbon (): agrega cintas, rango y con valores de x continuos.
# -geom_segment (): agrega segmentos de una sola línea
# -geom_curve (): Agregar curvas
# -geom_rect (): Agrega un rectángulo 2D.

require(maps)

peru=map_data('world',region = "Peru")
france=map_data('world',region = 'France')
ggplot(france,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="white",colour="black")

#Usa datos de economía [en ggplot2] 
#y produce ruta, cinta y rectángulos.

h <- ggplot(economics,aes(date,unemploy))

h+geom_path(size=0.3,color="#E46726")+
  theme_minimal()

# Combinar ruta, cinta y rectángulo

h+geom_rect(aes(xmin=as.Date('1980-01-01'),ymin=-Inf,
                xmax=as.Date('1985-01-01'),ymax=Inf),
            fill='#A29B32',color='#D8DA9E',size=1.5)+
#  geom_ribbon(aes(ymin=unemploy-900,ymax=unemploy+900),
#              fill='#F3BF94')+
  geom_path(size=0.8,color="#E46726")+
  theme_minimal()
## geom_ribbon: le aumenta una cinta alrededor de la serie

# Create a scatter plot
i <- ggplot(mtcars,aes(wt,mpg))+geom_point()
# Add segment
i+geom_segment(aes(x=2,y=15,xend=3,yend=15))
# Add arrow
require(grid)
i+geom_segment(aes(x=4.3,y=28,xend=3.5,yend=25),
               arrow = arrow(length = unit(0.2,"cm")))
## arrow(lenght = unit(0.2,"cm")) : grosor de la flecha
## xend: el fin de la flecha coordenada x
## y end: el fin de la flecha coordenada y
## x=el punto a señalar coordenada x
## y = el punto a señalar coordenada y

i+geom_curve(aes(x=2,y=15,xend=3,yend=15),
             arrow=arrow(length = unit(0.2,"cm")))
## arrow : sera para indicar que es una flecha tamebien

## titulos, titulos de eje, leyendas

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
p <- ggplot(ToothGrowth, aes(x=dose, y=len, fill = dose)) + geom_boxplot()

#p + ggtitle ("Título principal"): agrega un título principal sobre la trama
#p + xlab ("etiqueta del eje X"): cambia la etiqueta del eje X
#p + ylab ("etiqueta del eje Y"): cambia la etiqueta del eje Y
#p + labs (title = "Título principal", x = "Etiqueta del eje X", 
#y = "Etiqueta del eje Y"): cambia las etiquetas del título principal y del eje

#La función labs () también se puede usar 
#para cambiar el título de la leyenda.

print(p)
p <- p+labs(title = "Plot of length /nby dose",
            x="Dose(mg)",y="Teeth length")
p+
  theme(plot.title = element_text(color="green",size = 12,face = "bold.italic"),
        axis.title.x = element_text(color="blue",size=7,face="bold"),
        axis.title.y = element_text(color="red",size=9,face="italic"))
## sin etiquetas   
p+
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
p+
  labs(fill="Dose (KG)")
####### Leyendas #################
## Posición de la leyenda:
# "left","top", "right", "bottom", "none"
# arriba
p+theme(legend.position = "top")
# abajo
p+theme(legend.position = "bottom")
# derecha
p+theme(legend.position = "right")
# izquierda
p+theme(legend.position = "left")
# sin leyenda
p+theme(legend.position = "none")

# Posición de la leyenda como vector
p+theme(legend.position = c(0.8,0.2))
## Parte inferior izquierda
p+theme(legend.position = c(0,0))
## Parte superior derecha
p+theme(legend.position = c(1,1))

# Cambiar la apariencia del título y 
#las etiquetas de la leyenda

p+theme(legend.text = element_text(colour="blue"),
        legend.title = element_text(colour="green",size = 21))

## Color de la caja de leyendas 

p+theme(legend.background = element_rect(fill="green"))

p+scale_x_discrete(limits=c("2","0.5","1"))
p+scale_fill_discrete(name="Dose",labels=c("A","B","C"))
#Cambie el orden de los elementos de la leyenda: scale_x_discrete ()
# Establecer el título y las etiquetas de la leyenda: scale_fill_discrete ()

#-guides (): establece o elimina la 
#leyenda para un determinado
#estético

mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)

#La función guide_legend () 
#se usa para cambiar el orden de las guías en la leyenda.
p <- ggplot(data=mtcars,
            aes(x=mpg,y=wt,color=cyl,
                size=qsec,shape=gear))+
                  geom_point()
p
p+guides(color=guide_legend(order = 1),
         size=guide_legend(order = 2),
         shape=guide_legend(order=3))
# Eliminar una leyenda para una estética 
#particular (color y tamaño)

p+guides(color=F,size=F)

#Tenga en cuenta que, en el caso del color continuo, 
#la función guide_colourbar () debe usarse para 
#cambiar el orden de la guía de color.

# en el caso distingamos al color por alguna variable

qplot(data = mpg, x = displ, y = cty, size = hwy,
      colour = cyl, shape = drv) +
  guides(colour = guide_colourbar(order = 1),
         alpha = guide_legend(order = 2),
         size = guide_legend(order = 3))

#La eliminación de una leyenda particular se 
#puede hacer también cuando se usan las funciones 
#scale_xx. En este caso, la guía de argumentos 
#se usa de la siguiente manera.
## La guia de shape
p+scale_shape(guide=F)

#Remove leyenda de size:
p+scale_size(guide=F)

## La leyenda de cyl
p+scale_color_manual(values=c('#999999','#E69F00','#56B4E9')
                     ,guide=F)

## codigo hexadecimal: "#FF1234"
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)

#Diagrama de Cajas
bp <- ggplot(ToothGrowth,aes(x=dose,y=len))

#Grafico de Dispersión
sp <- ggplot(mtcars,aes(x=wt,y=mpg))

bp+geom_boxplot(color="red",fill="blue")


bp <- bp+geom_boxplot(aes(fill=dose))
bp

sp <- sp+geom_point(aes(color=cyl))
sp

## claridad del color

bp+scale_fill_hue(l=40,c=35)
bp+scale_fill_hue(l=30,c=85)

sp+scale_color_hue(l=40,c=30)
sp+scale_color_hue(l=50,c=35)
## mas claro
sp+scale_color_hue(l=65,c=100)

## Ver la paleta de colores
RColorBrewer::brewer.pal.info

## Paleta Wes Anderson
install.packages("wesanderson")
library(wesanderson)
##Paleta
wesanderson::wes_palettes

bp+scale_fill_manual(values=wes_palette(n=3,name="GrandBudapest1"))
sp+scale_color_manual(values=wes_palette(n=3,name = "Moonrise3"))

#Gradiente o colores continuos
#scale_color_gradient (), scale_fill_gradient () para gradientes secuenciales entre dos colores
#scale_color_gradient2 (), scale_fill_gradient2 () para gradientes divergentes
#scale_color_gradientn (), scale_fill_gradientn () para gradiente entre n colores

sp2 <- ggplot(mtcars,aes(x=wt,y=mpg))+
        geom_point(aes(color=qsec))

sp2

# Cambiar los colores bajos y altos
# Esquema de color secuencial

sp2+scale_color_gradient(low="blue",high="red")

# Esquema de color divergente
mid <- mean(mtcars$qsec)
sp2+scale_color_gradient2(midpoint = mid,low = "blue",
                          mid="purple",high="red",
                          space="Lab")

sp3 <- ggplot(mtcars,aes(x=wt,y=mpg))+
        geom_point(aes(color=mpg))
sp3
# Gradient between n colors
sp3+scale_color_gradientn(colours = rainbow(5))


##### Limite de ejes #######
### Sin recorte: cambiar coordenadas de los ejes
### coord_cartesian(xlim=c(x1,x2),ylim=c(y1,y2))
data(cars)
library(ggplot2)
p <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()
p
p+coord_cartesian(xlim = c(5,20),ylim = c(0,20))

p+expand_limits(x=0,y=0)

#Las funciones scale_x_continuous () 
#y scale_y_continuous () se pueden usar para 
#cambiar los límites / etiquetas de los ejes x e y, 
#respectivamente:

p+scale_x_continuous(name="Speed of Cars",limits=c(0,30))+
  scale_y_continuous(name = "Stopping distance",limits = c(0,150))

### Transformación de ejes:
data(cars)
p <- ggplot(cars,aes(x=speed,y=dist))+geom_point()
### transformaciónlog al eje x y al eje y
p+scale_x_log10()+scale_y_log10()
### transformación con raiz
p+scale_x_sqrt()+scale_y_sqrt()
### girar los ejes y sean decrecientes
p+scale_x_reverse()+scale_y_reverse()
p+coord_trans(x="log2",y="log2")


require(scales)

p+geom_text(aes(label=speed))

p+geom_text(aes(label=speed))+
### Solo cambia las etiquetas de las escalas sin transformar los datos
    scale_x_continuous(trans = log2_trans(),
#### ejes de 2 a la x
              breaks=trans_breaks("log2",function(x) 2^x),
#### ejes en notación 2 a la x
### # Formatee etiquetas de marca de marca 
########## de eje para mostrar exponentes
                       labels=trans_format("log2",math_format(2^.x)))

require(scales)
### etiqueta formato porcentaje
p+scale_y_continuous(labels=percent)
### etiqueta formato dolar
p+scale_y_continuous(labels = dollar)
## Formato cientifico

p+scale_y_continuous(labels=scientific)




## Eje en formato DATE #####
set.seed(1234)
df <- data.frame(
  date=seq(Sys.Date(),len=100,by="1 day")[sample(100,50)],
  price=runif(50)
)
## Fecha actual
Sys.Date()
## Secuencia desde la fecha de hoy, cada 2 dias 
seq(Sys.Date(),len=100,by="2 day")

## ordenar por date o fecha
df <- df[order(df$date),]

head(df)

#Formato de etiquetas 
#de marca de verificación del eje: días, semanas, meses

p <- ggplot(data = df,aes(x=date,y=price))+geom_line()
print(p)

# Formato de etiquetas de marca de verificación de eje
# Formato: mes / día

require(scales)
## Etiquetas de ejee formato dia/mes
p+scale_x_date(labels = date_format("%m/%d"))+
  ## cambiar el angulo del eje x, para que se en diagonal
  theme(axis.text.x = element_text(angle = 45))

#Formato Mes
p+scale_x_date(labels = date_format("%W"))

## solamente el mes
p+scale_x_date(breaks = date_breaks("month"),
### solo se repetira una vez, si es day, se repetira 30 veces el mes 
               labels = date_format("%b"))+
  theme(axis.text.x = element_text(angle = 45))


data("economics")
head(economics)
p <- ggplot(economics,aes(x=date,y=psavert))+
  geom_line(color="steelblue")

p
min <- as.Date("2002-1-1")
max <- max(economics$date)
## cambiar el limite de los ejes
p+scale_x_date(limits = c(min,max))


#Marcadores de eje: personalice marcas ####
#y etiquetas de marca, ####
#reordenar y seleccionar elementos####

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()
p

## cambiar color, y el angulo del eje
p + theme(axis.text.x = element_text(face="italic", color="#993333",
                                    size=12, angle=45),
          axis.text.y = element_text(face="bold", color="red",
                                     size=12, angle=45))


### Remover todo:
p+theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) ## remueve las grillas de los ejes


### Cambiar el color a las rectas de los ejes y tipo de lineas
p+theme(axis.line = element_line(colour="blue",linetype = "dashed",
                                 size=1))


#- scale_x_discrete(name, breaks, labels, limits): for X axis
#- scale_y_discrete(name, breaks, labels, limits): for y axis

#- scale_x_continuous(name, breaks, labels, limits, trans): for X axis
#- scale_y_continuous(name, breaks, labels, limits, trans): for y axis

# . nombre: etiquetas del eje xo y
# . descansos: vector que especifica qué descansos mostrar
# . etiquetas: etiquetas de marcas de eje
# . límites: vector que indica el rango de datos

# scale_x_discrete o xlim()

p + scale_x_discrete(name ="Dose (mg)",
                     limits=c("2","1","0.5"))

#Cambiar etiquetas de marca de verificación
p+scale_x_discrete(breaks=c("0.5","1","2"),
                   labels=c("D0.5","D1","D2"))
## escoge que valores de x mostrar:
p + scale_x_discrete(limits=c("0.5", "2"))

p+xlim("0.5","2")

sp <- ggplot(cars,aes(x=speed,y=dist))+geom_point()
sp

sp+scale_x_continuous(name="Speed of Speed",limits = c(10,20))+
  scale_y_continuous(name="Distance",limits = c(25,50))

### Separado por 50, del 0 a 150, las lineas
sp+scale_y_continuous(breaks = seq(0,150,50))
#Que ejes apareceran en y 0,50,65,75,150
sp+scale_y_continuous(breaks = c(0,50,65,75,150))

# REMOVER LAS ETIQUETAS DE Y ,LAS LINEAS DE CUADRICULA DE Y
sp + scale_y_continuous(breaks=NULL)

# Possible values for labels = percent, scientific,
sp + scale_y_continuous(labels = percent)

######## THEME AND BACKGROUND COLORS #########
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot()

### Cambiar temas 

#-theme_gray (): color de fondo gris y líneas de cuadrícula blancas
#-theme_bw (): fondo blanco y líneas grises
#-theme_linedraw (): líneas negras alrededor de la trama
#-theme_light (): líneas y ejes grises claros (más atención hacia los datos)

p+theme_gray(base_size = 14) ## base_size = tamaño de los ejes y lineas
p+theme_bw()
p+theme_linedraw()
p+theme_light()

#-theme_minimal (): sin anotaciones de fondo
#-theme_classic (): tema con líneas de eje y sin líneas de cuadrícula (diagrama estándar)
#-theme_void (): tema vacío, útil para gráficos con coordenadas no estándar o para dibujos
#-theme_dark (): fondo oscuro diseñado para resaltar los colores
## sin linea de ejes
p+theme_minimal()
## sin lineas de fondo, solo con lineas de ejes
p+theme_classic()
### para imagen, sin ejes ni fondo
p+theme_void()
## fondo oscuro
p+theme_dark()

# -base_size: tamaño de fuente base (para cambiar el tamaño de todos los elementos de texto de trazado)
# -base_family: familia de fuentes base

#El tamaño de todos los elementos de texto 
#de la trama se puede cambiar fácilmente a la vez

p+theme_gray(base_size = 10)
p+theme_gray(base_size = 20)

#Tenga en cuenta que la función theme_set () se puede usar para 
#cambiar el tema para toda la sesión.
theme_set(theme_gray(base_size = 20))
theme_set(theme_gray(base_size = 15))

#La función theme () se usa para controlar partes 
#del gráfico que no son de datos, que incluyen:

#-Line elements: axis lines, minor and 
#major grid lines, plot panel border, 
#axis ticks background color, etc.

# -Elementos de línea: líneas de eje, 
#líneas de cuadrícula menores y mayores, 
#borde del panel de trazado, 
#color de fondo de marcas de eje, etc.

#-Text elements: plot title, axis titles, 
#legend title and text, axis tick mark labels, etc.

# -Elementos de texto: título de la trama, 
#títulos de eje, título de leyenda y texto, 
#etiquetas de marca de marca de eje, etc.

#-Rectangle elements: plot background, 
#panel background, legend background, etc.

# -Elementos rectangulares: fondo de trama, 
#fondo de panel, fondo de leyenda, etc.

#Hay una función específica para modificar cada uno de estos tres elementos:
#-element_line () para modificar los elementos de línea del tema
#-element_text () para modificar los elementos de texto
#-element_rect () para cambiar la apariencia de los elementos rectangulares

# Cambiar colores

# En esta sección, cambiaremos el color del 
#fondo del panel de trazado y las líneas de la cuadrícula.

# - Las funciones theme () y 
#element_rect (#fill, #colour, #size, #linetype) 
#se utilizan para cambiar el color de fondo del 
#panel de trazado.

# - La apariencia de las líneas de cuadrícula 
#se puede cambiar usando la 
#función element_line (#colour, #size, #linetype).

## fill: fondo del grafico

## panel.background = color de panel
## fill = color del fondo
## colour = color de la linea del panel
## size = tamaño de la linea 
## linetype = tipo de linea en el caso solido

### panel.grid.major = lineas de cuadriculas grandes, las 
### que intersectan en los ejes x y ejes y
p+theme(
  panel.background = element_rect(fill="#BFD5E3",colour="#6D9EC1",
                                  size=3,linetype = "solid"),
  panel.grid.major = element_line(size=0.5,linetype = "solid",
                                  colour="green"),
  panel.grid.minor = element_line(size=0.25,linetype = "solid",
                                  colour="red"))
### el fondo del panel donde esta la grafica, no el fondo del grafico
p+theme(plot.background = element_rect(fill="lightblue"))


## remover panel borders y grid lines
## Solo grafico
p+theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

### GG-Themes ######

install.packages("ggthemes")
library(ggthemes)

p <- ggplot(ToothGrowth,aes(x=dose,y=len))+
  geom_boxplot(aes(fill=dose))
p
p+theme_economist()+scale_fill_economist()
### theme stata

p+theme_stata()+scale_fill_stata()

#### Crear el propio tema
theme_gray
function(base_size=11,base_family=""){
  half_line <- base_size/2
  theme(
    ...axis.text = element_text(size=rel(0.8),colour="grey"),
    ...
  )
}
#Tenga en cuenta que la función rel () 
#modifica el tamaño en relación con el tamaño base


# Anotaciones de texto:

# -geom_text (): agrega texto directamente a la trama
# -geom_label (): dibuja un rectángulo debajo del texto, lo que facilita su lectura.
# -annotate (): útil para agregar pequeñas anotaciones de texto en una ubicación particular en la trama
# -annotation_custom (): agrega anotaciones estáticas que son las mismas en cada panel

set.seed(1234)

df <- mtcars[sample(1:nrow(mtcars),10),]
df$cyl <- as.factor(df$cyl)

##Anotaciones de texto usando geom_text y
#etiqueta_geom

sp <- ggplot(df,aes(x=wt,y=mpg))+
  geom_point()

# Agregar texto, cambiar colores por grupos
# Cambiar justificación vertical, un poco mas abajo del punto
sp+geom_text(aes(label=rownames(df),color=cyl),
             vjust=2,size=3)
# Agregar texto en una coordenada particular en el lugar x=3,y=28
sp+geom_text(x=5, y=24,
             label="Scatter Plot",color="red",
             fontface=2)
  
#geom_label () funciona como 
#geom_text () pero dibuja un rectángulo 
#redondeado debajo de cada etiqueta. Esto es útil cuando 
#desea etiquetar diagramas que son densos con datos.

## mostrar como etiqueta en un rectangulo el nombre que se
## desea poner
sp+geom_label()

#nudge_x y nudge_y: le permiten compensar las etiquetas 
#de sus puntos correspondientes.
#La función position_nudge () también se puede utilizar.

#check_overlap = TRUE: 
#para evitar la superplotación de etiquetas
#--------------------------------------------------
#hjust y vjust ahora pueden ser vectores 
#de caracteres (ggplot2 v> = 2.0.0): 
#"left", "center", "right", "bottom", "middle", "top". 
#Las nuevas opciones incluyen "hacia adentro" 
#y "hacia afuera" 
#que alinean el texto hacia y 
#lejos del centro de la trama, respectivamente.
#--------------------------------------------------
#fontface: Cambiar fontface. Valores permitidos: 
#1 (normal), 
#2 (bold=negrita), 
#3 (italic=cursiva) y 
#4 (bold.italic=negrita cursiva).
  
#annotation_custom: agrega una anotación de texto estático

#Las funciones annotation_custom () y textGrob () 
#se utilizan para agregar anotaciones estáticas 
#que son las mismas en todos los paneles. 

#Se requiere el paquete -grid

sp2 <- ggplot(mtcars,aes(x=wt,y=mpg,label=rownames(mtcars)))+geom_point()
sp2+geom_text()
library(grid)

## textGrob(lo que se dea poner, 
#x sera el lugar que se pondra los margenes van de 0 a 1
# y igualmente va de 0 a 1, sin importar las escalas,
# x es donde comienza)

grob <- grobTree(
  textGrob("Scatter plot",x=0.95,y=0.95,hjust = 0,
            gp=gpar(col="red",fontsize=13,fontface="italic")))

sp2+annotation_custom(grob)+
      facet_wrap(~cyl,scales = "free")

#ggrepel: evite la superposición de etiquetas de texto

install.packages("ggrepel")

## Preparar la data
set.seed(1234)
ss <- sample(1:32,15)
df <- mtcars[ss,]
p <- ggplot(df,aes(x=wt,y=mpg))+
  geom_point(color='red')+
  theme_minimal(base_size = 10)
p
p+geom_text(aes(label=rownames(df)),size=3.5)
require(ggrepel)

set.seed(42)
### es casi lo mismo que geom_text incluye etiqueta a los puntos
p+geom_text_repel(aes(label=rownames(df)),size=3.5)

# Use ggrepel :: geom_label_repel y
# Cambiar color por grupos

p+geom_label_repel(aes(label=rownames(df),fill=factor(cyl)),
                   color="white",size=5)+
  theme(legend.position = "bottom")

#################### facetas ##########
#-Facets: dividir una trama en una matriz de paneles

data("ToothGrowth")

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

head(ToothGrowth)

p <- ggplot(ToothGrowth,aes(x=dose,y=len))+
  geom_boxplot(aes(color=dose),fill="white")
p+theme_light()

#p + facet_grid (supp ~.): faceta en dirección vertical basada en los niveles de la variable supp.
#p + facet_grid (. ~ supp): faceta en dirección horizontal basada en los niveles de la variable supp.
#p + facet_grid (dosis ~ supp) Faceta en dirección horizontal y vertical basada en dos variables: dosis y supp.
#p + facet_wrap (~ fl) Coloque la faceta una al lado de la otra en un diseño rectangular

p+facet_grid(supp~.)
p+facet_grid(.~supp)

p+facet_grid(supp~dose)
p+facet_grid(dose~supp)
## lo mismo que la faceta en dirección horizontal
p+facet_wrap(~supp)

### generaran una linea mas 
### con un grafico con todas las escalas en conjunto
### para para cada escala de supp

p+facet_grid(dose~supp,margins = T)

## En la etiqueta de las facetas, incluira las variables con la escala 
p + facet_grid(dose ~ supp, labeller=label_both)

# Cambiar la fuente del texto de faceta. 
#Posibles valores para el estilo de fuente:
# 'plain', 'italic', 'bold', 'bold.italic'.

p+facet_grid(dose~supp)+
  theme(strip.text.x = element_text(face = "bold.italic",color="blue",size = 16),
        strip.text.y = element_text(face="bold",color="red",size=20))

# Cambiar la apariencia del rectángulo alrededor de la etiqueta de faceta

p+facet_grid(dose~supp)+
  theme(strip.background = element_rect(fill="lightblue",colour = "red",
                                        linetype = "solid"))

p + facet_grid(dose ~ supp)+
  theme(strip.background = element_rect(colour="black", fill="white",
                                        size=2, linetype="solid"),
        strip.text = element_text(colour = "red",face = "bold",size = 17))

bp+facet_wrap(~dose,ncol = 2)
bp+facet_wrap(~dose,nrow = 3)

################## gganimate #############
install.packages("gganimate")
library(gganimate)

####### libreria para crear el gif ######
install.packages("gifski")
library(gifski)

####### cargar datos #### 
library(gapminder)

data("gapminder")

head(gapminder)

gapminder %>% 
  group_by(year,continent)%>%
  summarize(mean.life=mean(lifeExp)) %>% 
  ggplot(aes(x=year,y=mean.life,color=continent))+
  geom_line()+
## animación grafica
  ## por que variable quiero que cambie
  transition_reveal(year)

### mejorar la animación
gapminder %>% 
  group_by(year,continent)%>%
  summarize(mean.life=mean(lifeExp)) %>% 
  ggplot(aes(x=year,y=mean.life,color=continent))+
### poner {frame_along} hara que en el titulo varie el año  
  labs(title = "Evolución de la Esperanza de Vida en {frame_along}",
       x="Fecha",y="Años de Vida")+
  
  geom_line(size=1.5)+
  geom_point(size=4)+
  theme_minimal()+
  ## animación grafica
  ## por que variable quiero que cambie
  transition_reveal(year)


library(ggplot2)
p <- ggplot(mpg,aes(fl,fill=drv))

# Organizar elementos uno al lado del otro
# para cada escala de fl, habra tres barras de las de drv
# barras agrupadas
p+geom_bar(position = "dodge")

# Apilar objetos uno encima del otro,
# y normalizar para tener la misma altura
# Barras apiladas agrupadas
library(scales)
p+geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)

# Elementos de pila uno encima del otro, solo conteo
p+geom_bar(position = "stack")


# Agregue ruido aleatorio a la posición X e Y
# de cada elemento para evitar la sobreplotación

ggplot(mpg,aes(cty,hwy))+
  geom_point(position = "jitter")

#position_dodge(width, height)
#position_fill(width, height)
#position_stack(width, height)
#position_jitter(width, height)


p+geom_bar(position = position_dodge(width = 1))
p+geom_bar(position = position_dodge(width = 0.5))#no tan separados

#######Coordinate Systems#########

p <- ggplot(mpg,aes(x=fl))+geom_bar()
p

#p + coord_cartesian (xlim = NULL, ylim = NULL): 
#sistema de coordenadas cartesianas (predeterminado). 
#Es el tipo de sistema de coordenadas más familiar y común.

#p + coord_fixed (ratio = 1, xlim = NULL, ylim = NULL): 
#coordenadas cartesianas con relación fija entre las escalas x e y. 
#La relación representa el número de unidades en el eje y 
#equivalente a una unidad en el eje x.

#El valor predeterminado, ratio = 1, asegura que una unidad 
#en el eje x tenga la misma longitud que una unidad en el eje y.

#p + coord_flip (...): coordenadas cartesianas invertidas. 
#Útil para crear trama horizontal mediante rotación.

p+coord_flip()

##coordenadas polares.####
#p + coord_polar (theta = "x", inicio = 0, dirección = 1): 

#El sistema de coordenadas polares se usa más comúnmente para 
#gráficos circulares, 
#que son gráficos de barras apiladas en coordenadas polares.

#p + coord_trans (x, y, limx, limy): 
#sistema de coordenadas cartesianas transformado.

#coord_map (): proyecciones de mapas. 
#Proporciona la gama completa de proyecciones 
#de mapas disponibles en el paquete mapproj.

## Parametros coord_polar
# -theta: variable para asignar ángulo a (x o y)
# -start: desplazamiento del punto de partida desde las 12 en radianes
# -dirección: 1, en sentido horario; -1, en sentido antihorario

p+coord_cartesian(ylim = c(0,200))
# disminuir tamaño
p+coord_fixed(ratio = 1/50)

## Grafico circular 
p+coord_polar(theta="x",direction = 1)
p+coord_trans(y="sqrt")

###### extensiones ggplot #######

#Para organizar múltiples gráficos ggplot2 
#en la misma página, las funciones R estándar 
#- par () y layout () - no pueden usarse.

#Funciones clave:
# -grid.arrange () [en el paquete gridExtra]
# -plot_grid () y draw_plot () [en el paquete cowplot]

#install.packages("gridExtra")
#install.packages("cowplot")

library(gridExtra)
library(cowplot)
library(ggplot2)


data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
data("economics") # Load economics
data("diamonds")

my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

p <- ggplot(ToothGrowth,aes(dose,len))
bxp <- p+geom_boxplot(aes(color=dose))+
  scale_color_manual(values = my3cols)

bxp

dp <- p+geom_dotplot(aes(color=dose,fill=dose),
                     binaxis = "y",stackdir = "center")+
  scale_color_manual(values = my3cols)+
  scale_fill_manual(values=my3cols)
dp

lp <- ggplot(economics,aes(x=date,y=psavert))+
  geom_line(color="#E46726")

lp

#Si desea agregar líneas de cuadrícula o usar el tema ggplot2 
#predeterminado, siga el código R a continuación:

# Agregar líneas de cuadrícula, esta funcion es de cowplot
bxp+background_grid(major = "xy",minor = "none")

bxp+theme_gray()

###Combine multiple plots####

#Funciones clave:
# -plot_grid (): combina fácilmente múltiples parcelas
# -ggdraw () + draw_plot () + draw_plot_label (): 

#Coloque gráficos en ubicaciones particulares 
#con un tamaño particular.

#En el siguiente código R, la función plot_grid () 
#se usa para combinar un diagrama de caja (bxp), 
#un diagrama de puntos (dp) y un diagrama de línea (lp) 
#en una cuadrícula de 2 columnas y 2 filas:

plot_grid(bxp,dp,lp,ncol = 2,nrow = 2)

#La combinación de las funciones ggdraw (), draw_plot () y 
#draw_plot_label () se puede utilizar para colocar gráficos y 
#etiquetas en ubicaciones particulares con un tamaño particular.

#-ggdraw (): inicializa un lienzo de dibujo vacío
#-draw_plot (): coloca un diagrama en algún lugar del lienzo de dibujo.
#-draw_plot_label (): agrega una etiqueta de trazado a la esquina superior izquierda de un gráfico.
#     Puede manejar vectores de etiquetas con coordenadas asociadas.

#draw_plot(plot, x = 0, y = 0, width = 1, height = 1)

# -plot: el diagrama a colocar (ggplot2 o un gtable)
# -x, y: la ubicación x / y de la esquina inferior izquierda de la gráfica.
# width, height: el ancho y la altura de la trama


#draw_plot_label(label, x = 0, y = 1, size = 16, ...)
#label: un vector de etiquetas para dibujar
#x, y: Vector que contiene la posición x e y de las etiquetas, respectivamente.
#size: tamaño de fuente de la etiqueta a dibujar

#Tenga en cuenta que, por defecto, las coordenadas van de 0 a 1, y 
#el punto (0, 0) está en la esquina inferior izquierda del lienzo 
#(vea la figura a continuación).

plot_combined<- ggdraw()+
  draw_plot(bxp,x=0,y=.5,width = 0.5,height = 0.5)+
  draw_plot(dp,x=.5,y=.5,width = 0.5,height = 0.5)+
  draw_plot(lp,x=0,y=0,width = 1,height = 0.5)
#x: donde comienza de la parte inferior izquierda, por ejemplo el primer
# grafico comienza desde la cordenada (0,0.5), para arriba la altura y para
# la derecha el ancho


save_plot("mpg.jpg",plot_combined,
          base_aspect_ratio = 1.3)

plot2by2 <- plot_grid(bxp, dp, lp, labels = c("A", "B", "C"),
                      ncol = 2, nrow = 2)
plot2by2
### guardar como pdf
save_plot("plot2by2.pdf", plot2by2,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)

## paquetet: gridExtra########

# Combinaremos las siguientes parcelas
# la gráfica de caja, la gráfica de puntos y la 
#gráfica de línea creada en las secciones anteriores


# un diagrama de barras creado usando los 
#conjuntos de datos de diamantes de la siguiente manera

my5cols <- c("#6D9EC1", "#646567", "#A29B32", "#E46726", "#F3BF94")
data("diamonds")

brp <- ggplot(diamonds,aes(x=clarity))+
        geom_bar(aes(fill=cut),position = "dodge")+
            scale_fill_manual(values = my5cols)
brp
require(gridExtra)
grid.arrange(bxp,dp,lp,brp,ncol=2,nrow=2)

## grid.arrange () y organizarGrob (): 
#Cambiar el intervalo de columna / fila de un gráfico

#La función arangeGrop () ayuda a cambiar el rango de fila / 
#columna de un gráfico.

## arrangeGrob, hara que lo tome solo como un grafico, es decir 
## sera un panel que tendra dos o mas graficos dentro
grid.arrange(bxp,arrangeGrob(bxp,brp),ncol=2)
theme_set(theme_bw())
grid.arrange(arrangeGrob(bxp,brp,ncol = 2),lp,nrow=2)

#En el código R a continuación, 
#layout_matrix es una matriz de 2x2 (2 columnas y 2 filas). 
#La primera fila son todos 1s, ahí es donde vive la primera trama, 
#que abarca las tres columnas; la segunda fila contiene las 
#parcelas 2, 3, 4, cada una ocupando una fila.

## layout_matrix=indicara que en la primera fila: ira solo el grafico 1
## en la segunda grafico 2 y 3
grid.arrange(brp, bxp, dp, ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,1), c(2,3)))

#Usa leyendas comunes para múltiples gráficos####


#Esto se puede hacer en cuatro simples pasos:
#1. Cree los gráficos: p1, p2,. . . .
#2. Guarde la leyenda del diagrama p1 como un elemento 
#gráfico externo (denominado "grob" en la terminología de cuadrícula)
#3. Eliminar las leyendas de todas las parcelas
#4. Dibuje todas las parcelas con solo una leyenda en el panel derecho

get_legend <- function(myggplot){
  require(gridExtra)
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs,function(x) x$name)=="guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
## Obtener las leyendas
legend<- get_legend(dp)
## Remover las leyendas
bxp2 <- bxp+theme(legend.position = "none")
dp2 <- dp+theme(legend.position = "none")

## ponerlo en la tercera columna la leyenda, con ancho de 0.8
grid.arrange(bxp2,dp2,legend,ncol=3,widths=c(2.3,2.3,0.8))

legend2 <- get_legend(dp+theme(legend.position = "top"))

grid.arrange(arrangeGrob(bxp2,dp2,ncol = 2),
             legend2,nrow=2,heights=c(6,0.8))

#grid.arrange(bxp2, dp2, legend2, ncol=2, nrow = 2,
#             layout_matrix = rbind(c(1,2), c(3,3)),
#             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

grid.arrange(bxp2,dp2,legend2,nrow=2,ncol=2,
             layout_matrix=rbind(c(3,3),c(1,2)),
             heights=c(0.2,2.5))

grid.arrange(legend2,bxp2,dp2,nrow=2,ncol=2,
             layout_matrix=rbind(c(1,1),c(2,3)),
             heights=c(0.2,2.5))
###Diagrama de dispersión con diagramas de densidad marginal#####
my3cols <- c("#6D9EC1", "#646567", "#A29B32")
set.seed(1234)

x <- c(rnorm(350,mean=-1),rnorm(350,mean=1.5),rnorm(350,mean=4))

y <- c(rnorm(350,mean=-0.5),rnorm(350,mean=1.7),rnorm(350,mean=2.5))

group <- as.factor(rep(c(1,2,3),each=350)) ## repite 1 350, luego 2 350 veces...

df2 <- data.frame(x,y,group)
head(df2)

scatter_plot <- ggplot(df2,aes(x,y,color=group))+geom_point()
scatter_plot <- scatter_plot+scale_color_brewer(palette = "Dark2")
scatter_plot <- scatter_plot+theme(legend.position = c(0.1,0.8))
scatter_plot

xdensity <- ggplot(df2,aes(x))+
            geom_density(aes(fill=group),alpha=0.7)+
            scale_fill_brewer(palette = "Dark2")+
            theme(legend.position = "none")

ydensity <- ggplot(df2,aes(y))+
  geom_density(aes(fill=group),alpha=0.7)+
  scale_fill_manual(values = my3cols)+
  theme(legend.position = "none")+coord_flip()

### Grafico en blanco 
blank_plot <- ggplot()+geom_blank(aes(1,1))+theme_void()

require(gridExtra)

grid.arrange(xdensity,blank_plot,scatter_plot,ydensity,nrow=2,ncol=2,
             layout_matrix=rbind(c(1,2),c(3,4)),
             widths=c(5,1.3),heights=c(1.3,5))


# Crear un diseño complejo utilizando la función viewport ()######
# 1.Crear plots
# 2.Mover a una nueva página en un dispositivo de cuadrícula 
# utilizando la función grid.newpage ()
# 3.Cree un diseño 2X2: número de columnas = 2; número de filas = 2
# 4.Definir una vista de cuadrícula: 
# una región rectangular en un dispositivo gráfico
# 5.Imprima un diagrama en la ventana gráfica


require(grid)
## Mover a una nueva pagina 
grid.newpage()
# Crear diseño
pushViewport(viewport(layout=grid.layout(2,2)))
# Una función auxiliar para definir una región en el diseño

define_region <- function(row,col){
  viewport(layout.pos.row = row,layout.pos.col = col)
}

# Organizar las parcelas
## agregas en la primera fila scatter plot
print(scatter_plot,vp=define_region(1,1:2))
print(xdensity,vp=define_region(2,1))
print(ydensity,vp=define_region(2,2))
############ gg Extra #########3
#ggExtra: Agregar gráficos de distribuciones marginales
library(ggExtra)
## generara un grafico de densidad de cada variable
ggMarginal(scatter_plot)
## un histograma
ggMarginal(scatter_plot,type="histogram",
           fill = "#6D9EC1", color = "#BFD5E3")


## Insertar un elemento gráfico externo dentro de un ggplot#####
##puede usar para agregar tablas, 
##gráficos u otros elementos basados en cuadrículas. 
##El formato simplificado es:

#annotation_custom(grob, xmin, xmax, ymin, ymax)

# -grob: el elemento gráfico externo para mostrar
# -xmin, xmax: x ubicación en coordenadas de datos (ubicación horizontal)
# -ymin, ymax: y ubicación en coordenadas de datos (ubicación vertical)

#Los diferentes pasos son:
#  1. Cree un diagrama de dispersión de y = f (x)
#  2. Agregue, por ejemplo, el diagrama de caja de las 
#  variables x e y dentro del diagrama de dispersión 
#  usando la función annotation_custom ()

## tema transparente
library(tidyverse)
transparent_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(fill = "transparent",colour=NA),
  plot.background = element_rect(fill="transparent",colour = NA)
)

p1 <- scatter_plot
p2 <- ggplot(df2,aes(factor(1),x))+geom_boxplot(width=0.3)+coord_flip()+
  transparent_theme

p3 <- ggplot(df2,aes(factor(1),y))+geom_boxplot(width=0.3)+
  transparent_theme

# Crear los elementos gráficos externos
# llamado "grop" en terminología de cuadrícula
p2_grob = ggplotGrob(p2)
p3_grob = ggplotGrob(p3)

# Insertar p2_grob dentro del diagrama de dispersión

xmin <- min(x);xmax <- max(x)
ymin <- min(y);ymax <- max(y)

scatter_plot+annotation_custom(grob = p2_grob,
                     xmin = xmin,
                     xmax=xmax,
                     ymin = -4,
                     ymax=-2)+
    annotation_custom(grob = p3_grob,
                      xmin = 6.5,
                      xmax=7.5,
                      ymin=ymin,
                      ymax = ymax)

##Mezcle tablas, texto y gráficos ggplot2

ggplot(df2,aes(x))+geom_histogram(fill="white",colour="blue",
                                  aes(y=..density..))+geom_density()

# Se requieren las siguientes funciones:

# -tableGrob () [en el paquete gridExtra]: 
#para agregar una tabla de datos a un dispositivo gráfico

#-splitTextGrob () [en el paquete RGraphics]: 
#para agregar un texto a un gráfico

#install.packages("RGraphics")
library(RGraphics)
library(gridExtra)

p1 <- tableGrob(head(ToothGrowth,3))
text <- paste0("ToothGrowth data describes the effect",
               "of Vitamin C on tooth growth in Guinea pigs.")

p2 <- splitTextGrob(text)

p3 <- ggplot(ToothGrowth,aes(x=dose,y=len))+
        geom_boxplot(aes(color=dose))+
          scale_color_brewer(palette = "Dark2")


grid.arrange(p1,p2,p3,ncol=1,heights=c(0.25,0.2,0.55))


######## Correlation ##########

library(GGally)
mydata <- mtcars[,c(1,3,4,5,6,7)]

## Matriz de correlación
ggcorr(mydata,palette = "RdBu",label = T)

# Matriz de diagrama de dispersión
ggpairs(mydata)

### ggcorrplot ####
install.packages("ggcorrplot")

### instalar la ultima versión 

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")

library(ggcorrplot)

corr <- round(cor(mydata),3)

head(corr[,1:6],3)

### Calcular una matriz de valores p de correlación

p.mat <- ggcorrplot::cor_pmat(mydata)

round(p.mat,3)

head(p.mat[,1:4],3)

# Visualiza la matriz de correlación
# --------------------------------
# método = "cuadrado" (predeterminado)
library(ggcorrplot)
ggcorrplot(corr)

# Reordenando la matriz de correlación
# --------------------------------
# usando agrupamiento jerárquico

### ordenando por colores
ggcorrplot(corr,hc.order = T,outline.color = "white")

# Tipos de diseño y personalización de correlogramas
# --------------------------------
# Agregar coeficientes de correlación

ggcorrplot(corr,hc.order = T,
           type="lower", #### obtener el triángulo inferior de la matriz
           outline.color = "white",
           ggtheme = theme_bw,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab=TRUE)

# Agregar nivel de significancia de correlación
# --------------------------------

# Argumento p.mat
# Salvo el coeficiente no significativo
### marcara los no significativos a un 5%
ggcorrplot(corr,hc.order = T,type = "lower",p.mat = p.mat)

#Trazado de curvas de supervivencia####

#El análisis de supervivencia se centra en la 
#duración esperada del tiempo hasta la 
#ocurrencia de un evento de interés. 
#Sin embargo, este tiempo de falla puede no 
#observarse dentro del período de tiempo del estudio, 
#produciendo las llamadas observaciones censuradas.

#El paquete de supervivencia R se ajusta y 
#traza curvas de supervivencia utilizando gráficos de base R.

library(survival)
data("lung")

head(lung)

#Ajuste de curvas de supervivencia
fit <- survfit(Surv(time,status)~sex, data = lung)

#Dibujando curvas de supervivencia con survminer



#El paquete R survminer contiene la función ggsurvplot () 
#para dibujar fácilmente curvas de supervivencia hermosas y 
#listas para publicar usando ggplot2. ggsurvplot () 
#incluye también algunas opciones para mostrar el valor p 
#y la tabla "número en riesgo", debajo de las curvas de supervivencia. 
#survminer se puede instalar desde CRAN o GitHub.
library(survminer)

ggsurvplot(fit)

ggsurvplot(fit, size = 1, # change line size
           palette = c("#E7B800", "#2E9FDF"), # custom color palette
           conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Risk table color by groups
           ggtheme = theme_bw() # Change ggplot2 theme
)

############################################################

### Coronavirus #########
Covid_19 <- readxl::read_excel(
  path = "C:/Users/X541UA-0635T/Downloads/COVID-19-geographic-disbtribution-worldwide.xlsx")

View(Covid_19)

Covid_19 %>% 
  group_by(countriesAndTerritories)%>%
  summarize(Contagiados = sum(cases))%>%
  filter(countriesAndTerritories=='Peru')

