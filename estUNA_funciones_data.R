#libreria estUNA - revision 19/07/2016
#-----------------------------------------------------------------------
#Definicion de funciones
#-----------------------------------------------------------------------

#Funcion para corregir la clausura de los interv. de clase
#NO EXPORTAR EN PAQUETE - solo para uso interno
.corregir.clausura <- function(x) {
	#funcion utilizada en:
	#  factor(x="agrupado")  
	#  agrupar(x="numeric")
	x <- sub("\\[-Inf,","\\(-Inf,",x)
	x <- sub(", *Inf\\]",", Inf\\)",x)
	return(x) }
	
#Función para acomodar las lineas de salida para que no sean demasiado
#largas.  Se usa en el método "ajuste".
#NO EXPORTAR EN PAQUETE - solo para uso interno
.formatear_texto <- function(texto, ancho=66) {
	#funcion utilizada en:
	#  ajustar(x="numeric",...)
	#  imprimir(x="independencia")
	#  resumen(x="ajuste")
	for (i in 1:length(texto)) {
		s <- texto[i]
		if (nchar(s)>0) { 
		   s <- gsub(paste("(.{1,",ancho,"})(\\s|$)",sep=""),
		             "\\1\n", s)
		   s <- gsub("\n$","",s)
		   #coloca espacio al principio de cada linea
		   s <- gsub("\n","\n    ",s)
		   texto[i] <- s}
	}
	return(texto)
}

#Función para estimar los parametros de una distribución según la
#muestra. Agrega los grados de libertad a deducir (k)
#NO EXPORTAR EN PAQUETE - solo para uso interno
.ajuste.param <- function(x,distr,param) {
	#se invoca desde:
	#  ajustar(x="numeric",...)	
	k <- 1
	#obtener los parametros a partir del argumento "param"
	#o estimar los que faltan y "perder" grados de libertad
	if (missing(param)) param <- numeric(0)
	if (!is.numeric(param)) param <- numeric(0)
	switch(distr,
		normal = {if (is.na(param["media"])) {
					param["media"] <- mean(x); k <- k+1}
				  if (is.na(param["desv"])) {
					param["desv"] <- sd(x); k <- k+1} },
		expo   =  if (is.na(param["tasa"])) {
					param["tasa"] <- 1/mean(x); k <- k+1},
		unifc  = {if (is.na(param["min"])) {
					param["min"] <- min(x); k <- k+1}
				  if (is.na(param["max"])) {
					param["max"] <- max(x); k <- k+1} },
		gamma  = {i <- 0
				  if (is.na(param["s"])) i <- i+1
				  if (is.na(param["a"])) i <- i+2
				  switch(i,
					#falta solo el parametro "s"
					{fvs <- function(p,muestra) 
						return(sum(log(
							dgamma(muestra,scale=p,shape=param["a"]))))
					 param["s"] <- optimize(fvs,lower=0,
						upper=2*mean(x)/param["a"],muestra=x,
						maximum=TRUE)$maximum
					 k <- k+1 },			
					#falta solo el parametro "a"
					{fvs <- function(p,muestra) 
						return(sum(log(
							dgamma(muestra,scale=param["s"],shape=p))))
					param["a"] <- optimize(fvs,lower=0,
						upper=2*mean(x)/param["s"],muestra=x,
						maximum=TRUE)$maximum
					k <- k+1 },
					#faltan ambos parametros
					{fvs <- function(p,muestra) 
						return(sum(log(
							dgamma(muestra,scale=p[1],shape=p[2]))))
					param <- optim(par=c(var(x)/mean(x),
						(mean(x)^2)/var(x)),fn=fvs,muestra=x,
						control=list("fnscale"=-1))$par
					names(param) <- c("s","a")
					k <- k+1 } ) } ,
		geom   = if (is.na(param["p"])) {
					param["p"] <- 1/(1+mean(x)); k <- k+1} ,
		binom  = {if (is.na(param["n"])) {
					param["n"] <- round(mean(x)^2/(mean(x)-var(x)))
					k <- k+1}
				  if (is.na(param["p"])) {
				    param["p"] <- mean(x)/param["n"]
				    k <- k+1} },
		pois   = if (is.na(param["tasa"])) {
					param["tasa"] <- mean(x); k <- k+1} ,
		unifd  = {if (is.na(param["min"])) {
					param["min"] <- min(x); k <- k+1}
				  if (is.na(param["max"])) {
					param["max"] <- mean(x); k <- k+1}
				 } )
	param["k"] <- k
	return(param)
}

#Ajuste a distribucion continua
#  toma como argumento un vector x de muestra
#  y devuelve un objeto de clase "ajuste"
#NO EXPORTAR EN PAQUETE - solo para uso interno
.ajuste.cont <- function(x,distr="normal",param) {
	#se invoca desde:
	#  ajustar(x="numeric",...)
	#generar los intervalos de corte según la distribución
	y <- switch(distr,
		normal    = rnorm(n=length(x),mean=param["media"],
						  sd=param["desv"]),
		expo	  = rexp(n=length(x),rate=param["tasa"]),
		unifc	  = runif(n=length(x),min=param["min"],
						max=param["max"]),
		gamma     = rgamma(n=length(x),scale=param["s"],
						shape=param["a"]) )
	h <- hist(y,plot=FALSE)
	clases <- length(h$counts)
	inf <- h$breaks[1:clases]
	sup <- h$breaks[2:(clases+1)]			
	#ajustar los limites inferiores o superiores según la
	#distribución teorica:
	switch(distr,
		normal	  = {inf[1] <- -Inf; sup[clases] <- Inf},
		expo	  = {inf[1] <- 0; sup[clases] <- Inf},
		unifc	  = {inf[1] <- param["min"]
					 sup[clases] <- param["max"]},
		gamma	  = {inf[1] <- 0; sup[clases] <- Inf})
	#clasificar las observaciones en estos intervalos
	intervalos <- sort(unique(c(inf,sup)))
	frec <- as.numeric(table(cut(x,intervalos,include.lowest=TRUE,
		ordered_result=TRUE)))
	#calcular las frecuencias esperadas y reajustar los parametros
	switch(distr,
		normal = frec.esp <- (pnorm(sup,mean=param["media"]
					,sd=param["desv"]) - pnorm(inf,mean=param["media"],
					sd=param["desv"]))*sum(frec) ,
		expo   = frec.esp <- (pexp(sup,rate=param["tasa"]) - 
					pexp(inf,rate=param["tasa"]))*sum(frec),
		unifc  = frec.esp <- (punif(sup,min=param["min"],
					max=param["max"])-punif(inf,min=param["min"],
					max=param["max"]))*sum(frec),
		gamma  = frec.esp <- (pgamma(sup,scale=param["s"],
					shape=param["a"])-pgamma(inf,scale=param["s"],
					shape=param["a"]))*sum(frec) )					
	#unir clases hasta que frec.esp > 5 para todas
	#o por lo menos, mientras haya suficientes clases
	while (any(frec.esp<5) & (clases-param["k"])>1) {
		ofende <- which.min(frec.esp) #se toma uno solo
		if (ofende==1) otro <- 2
		else
		  if (ofende==clases) otro <- (clases-1)
		  else {
			otro <- c(ofende-1,ofende+1)
			i <- which.min(frec.esp[otro]) #se toma uno solo
			otro <- otro[i]}
		frec.esp[otro] <- frec.esp[otro]+frec.esp[ofende]
		frec[otro] <- frec[otro]+frec[ofende]
		if (otro > ofende)
			inf[otro] <- inf[ofende]
		else 
			sup[otro] <- sup[ofende]
		#elimina el intervalo "ofensivo"
		frec.esp <- frec.esp[-ofende]
		frec <- frec[-ofende]
		inf <- inf[-ofende]
		sup <- sup[-ofende]
		clases <- clases - 1
	}	
	estadistico <- sum((frec-frec.esp)^2/frec.esp)
	grados <- clases-param["k"]
	p.valor <- pchisq(q=estadistico,df=grados,lower.tail=FALSE)
	param <- head(param,-1)  #quitale el "k"
	return(new("ajuste",
		clase = c(paste("(",inf[1:(clases-1)],",",
						sup[1:(clases-1)],"]",sep=""),
				  paste("(",inf[clases],",",sup[clases],")",sep="")),
		distr=distr,frec = frec, param = param, frec.esp = frec.esp,
		estadistico = estadistico, grados = grados, pvalor = p.valor))
}

#Ajuste a distribucion discreta
#NO EXPORTAR EN PAQUETE - solo para uso interno
.ajuste.discreta <- function(x,distr="normal",param=NULL) {
	#se invoca desde:
	#  ajustar(x="numeric",...)
	#generar los intervalos de corte según la distribución
	y <- switch(distr,
		binom	= seq(from=0,to=param["n"]),
		geom	= c(seq(from=0,to=max(x)),Inf),
		pois	= c(seq(from=0,to=max(x)),Inf),
		unifd	= seq(from=param["min"],to=param["max"]) )
	clases <- length(y)
	inf <- y; sup <- y			
	#clasificar las observaciones en estos intervalos
	frec <- as.numeric(table(factor(x,levels=y,ordered=TRUE)))
	#calcular las frecuencias esperadas
	switch(distr,
		binom = frec.esp <- dbinom(y,size=param["n"],prob=param["p"])
							*sum(frec) ,
		geom  = frec.esp <- dgeom(y,prob=param["p"])*sum(frec),
		pois  = frec.esp <- dpois(y,lambda=param["tasa"])*sum(frec),
		unifd = frec.esp <- rep(x=sum(frec)/clases,times=clases) )					
	#unir clases hasta que frec.esp > 5 para todas
	#o por lo menos, mientras haya suficientes clases
	while (any(frec.esp<5) & (clases-param["k"])>1) {
		ofende <- which.min(frec.esp) #se toma uno solo
		if (ofende==1) otro <- 2
		else
		  if (ofende==clases) otro <- (clases-1)
		  else {
			otro <- c(ofende-1,ofende+1)
			i <- which.min(frec.esp[otro]) #se toma uno solo
			otro <- otro[i]}
		frec.esp[otro] <- frec.esp[otro]+frec.esp[ofende]
		frec[otro] <- frec[otro]+frec[ofende]
		if (otro > ofende)
			inf[otro] <- inf[ofende]
		else 
			sup[otro] <- sup[ofende]
		#elimina el intervalo "ofensivo"
		frec.esp <- frec.esp[-ofende]
		frec <- frec[-ofende]
		inf <- inf[-ofende]
		sup <- sup[-ofende]
		clases <- clases - 1
	}	
	estadistico <- sum((frec-frec.esp)^2/frec.esp)
	grados <- clases-param["k"]
	p.valor <- pchisq(q=estadistico,df=grados,lower.tail=FALSE)
	param <- head(param,-1)  #quitale el "k"
	return(new("ajuste",
		clase = ifelse(inf==sup,paste(inf),
				  ifelse(sup==Inf,paste("[",inf,",Inf)",sep=""),
								  paste("[",inf,",",sup,"]",sep=""))),
		distr=distr,frec = frec, param = param, frec.esp = frec.esp,
		estadistico = estadistico, grados = grados, pvalor = p.valor))
}
#----------------------------------------------------------------------------------
#Funciones exportables
	
#FUNCION para leer datos "amistosamente" en un data.frame 
leer.datos <- function() {
	ruta <- switch(.Platform$OS.type,
		unix	={	require(tcltk)
					tk_choose.files()},
		windows	=choose.files()
		)
	datos <- read.table(ruta,header=TRUE)
	attach(datos)
	return(datos)
}

#FUNCION para generar graficas de dispersion con líneas de tendencia
#25/3/13 : se incluyo el match.call, do.call para pasar argumentos 
#          adicionales a la función plot.  Por ejemplo:
#          graficar.dispersion(Ir,Fi,cex=0.2,pch=20)
graficar.dispersion <- function(x,y,color_lowess="dark green", ...) {
	#obtener los nombres de las variables
	nomx <- deparse(substitute(x))
	nomy <- deparse(substitute(y))
	#obtener argumentos adicionales
	extras <- match.call(expand.dots = FALSE)$...
	#eliminar los valores NA en ambos vectores
	#(la funcion lowess no puede trabajar con valores NA)
	i <- !(is.na(x)|is.na(y))
	x <- x[i]
	y <- y[i]
	titulo <- paste("Grafica de dispersion de",nomx,"y",nomy)
	do.call(plot,c(list(x=x,y=y,main=titulo,xlab=nomx,ylab=nomy),extras))
	lines(lowess(x,y),col=color_lowess)
} #graficar.dispersion

ojiva <- function(x,intervalos="sturges.r") {
	#igual que para el método agrupar, los posibles valores de intervalo
	#son:
	#* un número indicando el número de clases,
	#* una palabra, que a su vez indica el número de clases
	#* o un vector numérico indicando los límites de clase (breakpoints)
	if (mode(intervalos)=="character") {
		clase <- tolower(intervalos)
		if (length(clase)>1) 
			stop("Agrupamiento de clase ambiguo")
		i <- switch(clase,
			sturges 	= nclass.Sturges(x),
			sturges.r   = {h <- hist(x,breaks="Sturges",
									plot=FALSE)
						   h$breaks},
			fd 			= {h <- hist(x,breaks="FD",plot=FALSE)
						   h$breaks},
			scott		= {h <- hist(x,breaks="Scott",
									plot=FALSE)
						   h$breaks},
			stop("Especificacion de clase invalida"))
	} else 
			i <- sort(intervalos)
	corte <- cut(x,breaks=i,include.lowest=TRUE,
		dig.lab=5,ordered_result=TRUE)
	levels(corte) <- .corregir.clausura(levels(corte))
	niveles <- levels(corte)
	n <- length(niveles)
	frec <- as.numeric(table(corte))
	inf <- sort(as.numeric(
		sub("(\\(|\\[)(.+),.*", "\\2", niveles)))
	sup <- sort(as.numeric(
		sub("(.+), *(.+)(\\)|\\])", "\\2",niveles)))
	xcoord <- c(inf[1],sup)
	ycoord <- c(0,cumsum(frec))
	#ahora la grafica de ojiva
	nomx <- deparse(substitute(x))
	plot(xcoord, ycoord, main="Ojiva de frecuencias", 
		xlab=nomx,ylab="Frecuencia acumulada")
	lines(xcoord, ycoord, col="darkblue")
}


#tipos de distribuciones
#Nota- aparecen en las siguientes metodos
#  ajustar(x="numeric")
#  graficar(x="ajuste")
#En las siguientes funciones se consideran explicitamente:
#  ajuste.cont --> solo a las distribuciones en "continuas"
#  ajuste.discreta --> sólo a las distribuciones en "discretas"
#Las distribuciones continuas son
#  continuas <- c("normal","expo","gamma","unifc")
#Las distribuciones discretas son
#  discretas <- c("binom","geom","pois","unifd")


#-----------------------------------------------------------------------
# Carga de datasets
#-----------------------------------------------------------------------

#Area de trabajo:
#------------ 2009-2 --------------
d20092 <- read.table("datos_est_2009-2.dat",header=TRUE)
d20092$X8 <- as.factor(d20092$X8)
d20092$X10 <- as.factor(d20092$X10)
#------------ 2010-2 --------------
d20102 <- read.table("datos_est_2010-2.dat",header=TRUE)
d20102$X10 <- as.factor(d20102$X10)
d20102$X11 <- as.factor(d20102$X11)
#------------ 2011-1 --------------
d20111 <- read.table("datos_est_2011-1.dat",header=TRUE)
d20111$X1 <- as.factor(d20111$X1)
d20111$X3 <- as.factor(d20111$X3)
d20111$X5 <- as.factor(d20111$X5)
d20111$X6 <- as.factor(d20111$X6)
#------------ 2011-1b--------------
d20111b <- read.table("datos_est_2011-1_b.dat",header=TRUE)
d20111b$X3 <- as.factor(d20111b$X3)
d20111b$X5 <- as.factor(d20111b$X5)
d20111b$X6 <- as.factor(d20111b$X6)
#------------ 2011-2 --------------
d20112 <- read.table("datos_est_2011-2.dat",header=TRUE)
d20112$region <- as.factor(d20112$region)
#------------ 2011-2b--------------
d20112b <- read.table("datos_est_2011-2_b.dat",header=TRUE)
d20112b$X1 <- as.factor(d20112b$X1)
d20112b$X2 <- as.factor(d20112b$X2)
d20112b$X4 <- as.factor(d20112b$X4)
d20112b$X7 <- as.factor(d20112b$X7)
d20112b$X8 <- as.factor(d20112b$X8)
#------------ 2012-1 --------------
d20121 <- read.table("datos_est_2012-1.dat",header=TRUE,dec=",")
d20121$X4 <- as.factor(d20121$X4)
d20121$X7 <- as.ordered(d20121$X7)
#------------ 2013-1 --------------
d20131 <- read.table("datos_est_2013-1.dat",header=TRUE,dec=",")
d20131$X5 <- as.factor(d20131$X5)
d20131$X10 <- as.factor(d20131$X10)
#------------ 2013-2 --------------
d20132 <- read.table("datos_est_2013-2.dat",header=TRUE,dec=",")
d20132$X2 <- as.factor(d20132$X2)
#------------ 2014-1 --------------
d20141 <- read.table("datos_est_2014-1.dat",header=TRUE,dec=",")
d20141$X4 <- as.factor(d20141$X4)
#------------ 2015-1 --------------
d20151e1 <- read.table("datos_est_2015-1_e1.dat",header=TRUE,dec=",")
d20151e1$X3 <- as.factor(d20151e1$X3)
d20151e2 <- read.table("datos_est_2015-1_e2.dat",header=TRUE,dec=",")
d20151e2$X1 <- as.factor(d20151e2$X1)
d20151e2$X2 <- as.factor(d20151e2$X2)
d20151e2$X22 <- as.factor(d20151e2$X22)
d20151e2$X23 <- as.factor(d20151e2$X23)
#------------ 2015-2 --------------
d20152 <- read.table("datos_est_2015-2.dat",sep="\t",
	header=TRUE,as.is=TRUE)
#------------ 2016-1 --------------
d20161 <- read.table("datos_est_2016-1.dat",header=TRUE,dec=",")
d20161$X1 <- as.factor(d20161$X1)
d20161$X2 <- as.factor(d20161$X2)
d20161$X3 <- as.factor(d20161$X3)
d20161$X4 <- as.factor(d20161$X4)
d20161$X5 <- as.factor(d20161$X5)
#------------ 2017-1 -------------
d20171 <- d20121

