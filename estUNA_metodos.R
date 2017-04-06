#libreria estUNA - revision 20/4/2011

#-----------------------------------------------------------------------
#Definicion de metodos
#-----------------------------------------------------------------------

#METODOS BASICOS DE ACCESO A LOS ATRIBUTOS DE CLASES

setMethod("nombre",signature(x="agrupado"),
	function(x) return(x@nombre)
)

setMethod("nombre",signature(x="ajuste"),
	function(x) return(x@nombre)
)

setMethod("nombre",signature(x="independencia"),
	function(x,variable) return(x@nombres[variable])
)

setMethod("frec",signature(x="agrupado"),
	function(x) return(x@frec)
)

setMethod("frec",signature(x="ajuste"),
	function(x) return(x@frec)
)

setMethod("frec",signature(x="independencia"),
	function(x) return(x@fobs)
)

setMethod("frec.esp",signature(x="ajuste"),
	function(x) return(x@frec.esp)
)

setMethod("frec.esp",signature(x="independencia"),
	function(x) return(x@fesp)
)

setMethod("tipo",signature(x="agrupado"),
	function(x) return(x@tipo)
)

setMethod("variable",signature(x="agrupado"),
	function(x) 
		return(eval(as.name(x@nombre),envir=.GlobalEnv))
)

setMethod("variable",signature(x="ajuste"),
	function(x) 
		return(eval(as.name(x@nombre),envir=.GlobalEnv))
)

#El siguiente método ya tiene una función genérica asociada.
#Devuelve el factor asociado al agrupamiento de la variable x.
#(es un vector de tipo "factor" de igual longitud que x)
setMethod("factor",signature(x="agrupado", 
							 levels = "missing", labels = "missing",
							 exclude = "missing", ordered = "missing"),
	function(x) {
		tmp <- variable(x)
 		if (x@tipo %in% c("continua","discreta"))
			if (all(x@inf!=x@sup)) {
				intervalos <- sort(unique(c(x@inf,x@sup)))
				tmp <- cut(tmp,breaks=intervalos,include.lowest=TRUE,
						ordered_result=TRUE) 
				levels(tmp) <- .corregir.clausura(levels(tmp)) }
			else 
				tmp <- ordered(tmp)
		if (x@tipo=="ordinal") 
			tmp <- ordered(tmp,levels=x@clase)
		return(as.factor(tmp)) }
)

setMethod("frec.rel",signature(x="agrupado"),
	function(x) return(x@frec/sum(x@frec))
)

setMethod("intervalo.clase",signature(x="agrupado"),
	function(x) return(x@clase)
)

setMethod("marca.clase",signature(x="agrupado"),
	function(x) return(x@clase)
)

setMethod("marca.clase",signature(x="cuantitativo.agrupado"),
	function(x) return(x@medio)
)

setMethod("lim.inferior",signature(x="cuantitativo.agrupado"),
	function(x) return(x@inf)
)

setMethod("lim.superior",signature(x="cuantitativo.agrupado"),
	function(x) return(x@sup)
)

setMethod("distr",signature(x="ajuste"),
	function(x) return(x@distr)
)

setMethod("distrl",signature(x="ajuste"),
	function(x) return(x@distrl)
)

setMethod("param",signature(x="ajuste"),
	function(x) return(x@param)
)

setMethod("grados",signature(x="ajuste"),
	function(x) return(x@grados)
)

setMethod("grados",signature(x="independencia"),
	function(x) return(x@grados)
)

setMethod("estadistico",signature(x="ajuste"),
	function(x) return(x@estadistico)
)

setMethod("estadistico",signature(x="independencia"),
	function(x) return(x@estchi)
)

setMethod("pvalor",signature(x="ajuste"),
	function(x) return(x@pvalor)
)

setMethod("pvalor",signature(x="independencia"),
	function(x) return(x@pvalor)
)

setMethod("notas",signature(x="ajuste"),
	function(x) return(x@notas)
)

setMethod("fallo",signature(x="ajuste"),
	function(x) return(x@fallo)
)

#Metodos compuestos para clases "agrupado" o "ajuste"
#Los métodos "tabla" extraen el data.frame
setMethod("tabla",signature(x="agrupado"),
	function(x) {
		a <- data.frame(clase=marca.clase(x),
						frec.abs=frec(x),
						frec.rel=frec.rel(x))
		return(a)
	}
)

setMethod("tabla",signature(x="cuantitativo.agrupado"),
	function(x) {
		a <- data.frame(clase=intervalo.clase(x),
						medio=marca.clase(x),
						frec.abs=frec(x),
						frec.rel=frec.rel(x))
		return(a)	
	}
)

setMethod("tabla",signature(x="ajuste"),
	function(x) {
		a <- data.frame(clase=x@clase,
						frec=x@frec,
						frec.esp=x@frec.esp)
		return(a)
	}
)				

#METODOS DE MEDIDAS DESCRIPTIVAS

setMethod("moda",signature(x="agrupado"),
	function(x) {
		xf <- frec(x)
		i <- which(xf==max(xf))
		return(marca.clase(x)[i])
	}
)

setMethod("moda",signature(x="cuantitativo.agrupado"),
	function(x) {
		xf <- frec(x)
		i <- which(xf==max(xf))
		n <- length(xf)
		moda <- NULL
		for (clase in i) {
			if (clase!=1) d1 <- xf[clase] - xf[(clase-1)] 
			else d1 <- 0
			if (clase!=n) d2 <- xf[clase] - xf[(clase+1)]
			else d2 <- 0
			if (d1+d2==0) moda <- c(moda,marca.clase(x)[clase])
			else {
				linf <- lim.inferior(x)[clase]
				lsup <- lim.superior(x)[clase]
				moda <- c(moda,linf + 
						   (d1/(d1+d2))*(lsup-linf))
			}
		}
		return(moda)
	}
)

setMethod("moda",signature(x="numeric"),
	function(x) {
		kd <- density(x)
		return(kd$x[which.max(kd$y)])
	}
)

setMethod("moda",signature(x="factor"),
	function(x) {
		a <- table(x)
		return(names(a)[which.max(as.numeric(a))])
	}
)

setMethod("moda",signature(x="character"),
	function(x) {
		a <- table(x)
		return(names(a)[which.max(as.numeric(a))])
	}
)

setMethod("media", signature(x="cuantitativo.agrupado"),
	function(x) sum(marca.clase(x)*frec.rel(x))
)

setMethod("media",signature(x="numeric"),
	function(x) mean(x)
)

setMethod("cuartil",signature(x="cuantitativo.agrupado",cual="numeric"),
	function(x,cual) {
		if (cual %in% c(1,2,3)) {
			if (tipo(x)=="continua") {
				m <- sum(frec(x))*cual/4
				f <- cumsum(frec(x))
				cl <- min(which(f>=m))
				if (cl==1) falta <- m else falta <- m - f[(cl-1)]
				ancho <- lim.superior(x)[cl] - lim.inferior(x)[cl]
				return(lim.inferior(x)[cl]+falta/frec(x)[cl]*ancho) }
			else {
				tmp <- variable(x)
				return(quantile(tmp,probs=cual/4,type=2)) }
			}
		else
			stop("Argumento 'cual' de cuartil no valido.")
	}
)

setMethod("cuartil",signature(x="numeric",cual="numeric"),
	function(x,cual) {
		if (cual %in% c(1,2,3))
			return(quantile(x,probs=cual/4,type=2))
		else
			stop("Argumento 'cual' de cuartil no valido.")}
)

setMethod("desv",signature(x="cuantitativo.agrupado"),
	function(x) {
		n <- sum(frec(x))
		return(sqrt(
		(sum(marca.clase(x)^2*frec(x))-n*media(x)^2)/(n-1) ) )
	}
)

setMethod("desv",signature(x="numeric"),
	function(x) return(sd(x))
)

setMethod("curtosis",signature(x="numeric"),
	#Esta función retorna la curtosis tipo 2 como en la función
	#kurtosis del paquete e1071:
	#  D. N. Joanes and C. A. Gill (1998), Comparing measures of sample
	#  skewness and kurtosis. The Statistician, 47, 183–189.
	function(x) {
		n <- length(x)
		m <- media(x)
		m4 <- sum((x-m)^4)/n
		m2 <- sum((x-m)^2)/n
		g2 <- m4/(m2^2) - 3
		return(((n+1)*g2 + 6) * (n-1) / ((n-2)*(n-3)))
	}
)

setMethod("asimetria",signature(x="numeric"),
	#Esta función utiliza la fórmula del coeficiente de asimetria
	#de fischer tipo 2 utilizada por la función "skewness" en el
	#paquete e1071:
	#  D. N. Joanes and C. A. Gill (1998), Comparing measures of sample
	#  skewness and kurtosis. The Statistician, 47, 183–189.
	function(x) {
		n <- length(x)
		m <- media(x)
		m3 <- sum((x-m)^3)/n
		m2 <- sum((x-m)^2)/n
		g1 <- m3/(m2^1.5)
		return(g1 * sqrt(n*(n-1)) / (n-2))
	}
)

setMethod("atipicos",signature(x="numeric"),
	function(x,tipo="todos") {
	#tipo="moderado", "extremo", o "todos".  Por defecto,
	#devuelve todos los datos atipicos (moderados O extremos)
		q1 <- cuartil(x,1); q3 <- cuartil(x,3); riq <- q3-q1
		switch(tipo,
			moderado = a <- x[((x>q1-riq*3)&(x<=q1-riq*1.5)) | 
							((x<q3+riq*3)&(x>=q3+riq*1.5))],
			extremo  = a <- x[(x<=q1-riq*3)|(x>=q3+riq*3)],
			todos    = a <- x[(x<=q1-riq*1.5)|(x>=q3+riq*1.5)],
			stop(paste("Tipo",tipo,"no valido.")))
		if (length(a)>0) {
			obs <- which(x %in% a)
			names(a) <- paste("#",obs,sep="")
			a <- sort(a)}
		return(a)
	}
)

#METODOS PARA CREACION DE OBJETOS

#Método para crear objetos de clase "agrupado"
#Devuelve un objeto de clase "categorico.agrupado" o
#"cuantitativo.agrupado" correspondiente
setMethod("agrupar",signature(x="factor"),
	function(x,nombre=deparse(substitute(x,env=parent.frame()))) {
		a <- table(x)
		clase <- levels(x)
		frec <- as.numeric(a)
		if (is.ordered(x))
			return(new("agrupado",nombre=nombre, tipo="ordinal",
						clase=clase, frec=frec))
		else
			return(new("agrupado",nombre=nombre, tipo="categorica",
					clase=clase, frec=frec))	
	}	
)


setMethod("agrupar",signature(x="character"),
	function(x,orden,nombre=deparse(substitute(x,env=parent.frame()))) {
		if (!missing(orden)) {
			if (mode(orden)=="character") {
				x <- ordered(x,levels=orden)
				a <- table(x)
				clase <- levels(x)
				frec <- as.numeric(a)
				return(new("agrupado",nombre=nombre, tipo="ordinal",
						clase=clase, frec=frec)) } }
		else {
			x <- as.factor(x)
			a <- table(x)
			clase <- levels(x)
			frec <- as.numeric(a)
			return(new("agrupado",nombre=nombre, tipo="categorica",
					clase=clase, frec=frec)) }
	}	
)	
	
setMethod("agrupar",signature(x="numeric"),
	function(x,intervalos="sturges.r",tipo="continua",
			 nombre=deparse(substitute(x,env=parent.frame()))) {
		#Verifica el tipo indicado por el usuario para vectores
		#numericos.
		if (!(tipo %in% c("continua","discreta")))
			stop("Tipo de variable inválido.")
		#Verifica si el tipo es discreto pero no todos los valores son
		#enteros.  En este caso, modificar tipo a "continuo".
		if (tipo=="discreta" & is.character(all.equal(x,as.integer(x))))
			tipo <- "continuo"
		#De modo que si el tipo todavia es "discreta", podemos
		#asumir que todos los valores de x son enteros.
		#Verificar el argumento intervalos, traduce todo a
		#unos puntos de corte o a una cantidad de clases.
		#El resultado se coloca en i.
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
		}
		else 
			i <- sort(intervalos)
		#si x es discreta, verifica el asunto de los intervalos
		por_intervalos <- TRUE  #valor por defecto
		if (tipo=="discreta" & !missing(intervalos)) { 
			if (length(i)==1 & (diff(range(x))+1)<i) {
				por_intervalos <- FALSE
				warning(paste("Número de intervalos excede",
					"rango de valores.\n",
					"Revirtiendo a agrupamiento por valores.")) }
			if (length(i)>1 & is.character(all.equal(i,as.integer(i)))) {
				por_intervalos <- FALSE
				warning(paste("Limites de intervalos no son valores",
					"enteros.\nRevirtiendo a agrupamiento por",
					"valores.")) }
		}		
		if (tipo=="discreta" & missing(intervalos))
			por_intervalos <- FALSE
		#NOTA: las variables tipo continua siempre se agrupan por
		#intervalos y para cada intervalo- inf < medio < sup
		#Algunas variables discretas se agrupan por intervalos,
		#otras no, pero para las que se agrupan por valores, se tiene
		#que inf=medio=sup.
		#La idea es que en todo caso, cuando x es numerico, siempre se
		#le pueda calcular la media, la desviación estándard y todo lo
		#demás.
		#-------------------------------------------------------------		
		#Agrupar por intervalos o por valores.
		if (por_intervalos) {
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
			medio <- (inf + sup)/2
			#tomar en cuenta que si algún límite es infinito,
			#el punto medio de ese intervalo lo será tambien
			mi <- min(x[!is.na(x)]); ma <- max(x[!is.na(x)])
			if (medio[1]==-Inf) medio[1] <- (mi+sup[1])/2
			if (medio[n]==Inf) medio[n] <- (inf[n]+ma)/2
			return(new("cuantitativo.agrupado",nombre=nombre,
						tipo=tipo, clase=niveles, inf=inf, 
						medio=medio, sup=sup,frec=frec))
		}
		else {
			tmp <- ordered(x)
			niveles <- levels(tmp)
			medio <- as.numeric(niveles); inf <- medio; sup <- medio
			frec <- as.numeric(table(tmp))
			return(new("cuantitativo.agrupado",nombre=nombre,
						tipo=tipo, clase=niveles, inf=inf,medio=medio,
						sup=sup,frec=frec))
		}
	}
)

#Método para crear objetos de contraste de ajuste
setMethod("ajustar",signature(x="numeric",distr="character"),
	function(x,distr,param,
			 nombre=deparse(substitute(x,env=parent.frame()))) {
		#define las posibles distribuciones continuas y discretas
		continuas <- c("normal","expo","gamma","unifc")
		discretas <- c("binom","geom","pois","unifd")
		#elimina los missing values
		x <- x[!is.na(x)]
		#"adivina" que tipo de distribución es x
		#regla 1 : si x contiene numeros con decimales, es continua.
		#		   si x no tiene numeros decimales, ?
		#regla 2 : si x tiene números negativos, es continua,
		#		   pero no es gamma (incluye exponencial)
		decimales <- is.character(all.equal(x,as.integer(x)))
		negativos <- any(x<0)
		#verificamos los parametros primeros
		param <- .ajuste.param(x,distr,param)
		n <- ""
		switch(distr,
		normal	= {if (is.infinite(param["desv"]))
				     n <- c(n,"La desv. estándar no puede ser Inf.")
				   if (param["desv"]<0)
				     n <- c(n,"La desv. estándar no puede ser <0.")
				   },
		expo	= {if (param["tasa"]<=0)
				     n <- c(n,"La tasa debe ser >0.") },
		gamma   = {if (param["s"]<=0)
					 n <- c(n,"El parametro s debe ser >0.")
				   if (param["a"]<=0)
				     n <- c(n,"El parámetro a debe ser >0.") },
		unifc   = {if (param["min"]>param["max"])
					 n <- c(n,"Min. no puede ser mayor que Max.")},
		binom   = {if (param["p"]<0|param["p"]>1)
					 n <- c(n,"p debe estar entre 0 y 1")
					 if (param["n"]<0|!all.equal(param["n"],
						trunc(param["n"])))
						n <- c(n,"Valor de n no entero o negativo.")
					 if (param["n"]<max(x))
					    n <- c(n,paste("El valor máximo de",nombre,
					         "no puede exceder el valor máximo de n."))
				   },
		geom    = {if (param["p"]<0|param["p"]>1)
					  n <- c(n,"p debe estar entre 0 y 1") },
		pois    = {if (param["tasa"]<0)
					  n <- c(n,"tasa no puede ser negativa")},
		unifd   = {if (param["min"]>param["max"])
					  n <- c(n,"minimo no puede ser mayor que maximo") }				  ,
		stop(paste("Distribución",distr,"no reconocida.")) )
		#crear el objeto de ajuste en sí, solo si paramok
		#(esto es sentido común)
		paramok <- (length(n)==1)
		if (paramok)
			if (distr %in% continuas) 
				a <- .ajuste.cont(x,distr,param)
			else
				a <- .ajuste.discreta(x,distr,param)
		else {
			param <- head(param,-1)
			a <- new("ajuste",distr=distr,param=param)
		}
		#avisar si se está ajustando x a una distribución discreta
		#cuando algunos valores no son enteros.
		if (decimales & distr %in% discretas) 
			n <- c(n,paste("Algunos valores de",nombre,
						   "no son enteros, pero se esta ajustando",
						   nombre,"a una variable discreta."))
		#avisar si hay valores negativos y distr no lo permite
		if (negativos & (distr %in% c("expo","gamma","binom","geom",
			"pois")|(distr %in% c("unifc","unifd") & param["min"]>=0))) 	
			n <- c(n,paste("Algunos valores de",nombre,
						   "son negativos,pero la distribución",
						   "indicada no asume valores negativos."))
		#avisar si distr es binomial y hay valores fuera de rango
		if (distr=="binom")
			if (any(x>param["n"]))
				n <- c(n,paste("Algunos valores de",nombre,
							   "son mayores que el n de la",
							   "distribución binomial"))
		#avisar si distr es una de las uniformes y hay valores fuera
		#de rango
		if (distr %in% c("unifc","unifd"))
			if (any(x<param["min"])|any(x>param["max"]))
				n <- c(n,paste("Algunos valores de",nombre,
							   "están fuera del rango de la",
							   "distribución uniforme"))
		#avisar si se esta ajustando x a una distribución continua
		#y todos los valores de x son enteros.
		if (!decimales & distr %in% continuas)
			n <- c(n,paste("Aviso: Todos los valores de",nombre,
						   "son enteros y se esta ajustando",
						   nombre,"a una variable continua."))		
		#avisar si hay frecuencias esperadas <5 
		if (any(frec.esp(a)<5)) 
			n <- c(n,"Hay frecuencias esperadas <5.")
		#avisar si no se pudo efectuar el cotraste
		if (!paramok) 
			n <- c(n,paste("No se pudo efectuar el contraste por",
				 "algunas de las razones mencionadas arriba."))
		#ampliar el nombre de la distribución
		distrl <- switch(distr,
			normal = "normal",
			expo   = "exponencial",
			gamma  = "gamma",
			unifc  = "uniforme continua",
			binom  = "binomial",
			geom   = "gemetrica",
			pois   = "poisson",
			unifd  = "uniforme discreta")
		#borrar el primer elemento de n porque siempre es vacio ""
		#y acomoda las lineas muy largas.
		n <- n[-1]
		if (length(n)>0)
			n <- .formatear_texto(n,ancho=62)
		#agregar algunas cosillas al objeto a
		a@fallo <- !paramok
		a@distrl <- distrl
		a@notas <- n
		a@nombre <- nombre
		return(a)
	}
)
	
#Métodos para crear objetos de contraste de independencia
#(fue ampliado para incluir los signatures de factor: 27/4/13)

setMethod("test.independencia",signature(v1="agrupado",
	v2="agrupado"),
	function(v1,v2) {
		nombres <- c(nombre(v1),nombre(v2))
		grados <- (length(frec(v1))-1)*(length(frec(v2))-1)
		if (grados < 1)
			stop("Cantidad de categorías insuficiente.")
		fobs <- as.matrix(table(factor(v1),factor(v2)))
		v1.mar <- frec(v1); v2.mar <- t(frec(v2))
		fesp <- (v1.mar %*% v2.mar)/sum(v1.mar)
		dimnames(fesp) <- list(intervalo.clase(v1),intervalo.clase(v2))
		esta <- (fobs-fesp)^2/fesp
		if (any(fesp<5))
			notas <- "Hay frecuencias esperadas menores que 5."
		else
			notas <- ""
		pv <- pchisq(q=sum(esta),df=grados,lower.tail=FALSE)
		return(new("independencia",nombres=nombres,fobs=fobs,fesp=fesp,
				estchi=sum(esta),grados=grados,pvalor=pv,notas=notas))
	}
)

setMethod("test.independencia",signature(v1="factor",
	v2="factor"),
	function(v1,v2) {
		nombres <- c(deparse(substitute(v1)),
					 deparse(substitute(v2)))
		grados <- (length(levels(v1))-1)*(length(levels(v2))-1)
		if (grados < 1)
			stop("Cantidad de categorías insuficiente.")
		fobs <- as.matrix(table(v1,v2))
		v1.mar <- as.numeric(table(v1)); v2.mar <- t(as.numeric(table(v2)))
		fesp <- (v1.mar %*% v2.mar)/sum(v1.mar)
		dimnames(fesp) <- list(levels(v1),levels(v2))
		esta <- (fobs-fesp)^2/fesp
		if (any(fesp<5))
			notas <- "Hay frecuencias esperadas menores que 5."
		else
			notas <- ""
		pv <- pchisq(q=sum(esta),df=grados,lower.tail=FALSE)
		return(new("independencia",nombres=nombres,fobs=fobs,fesp=fesp,
				estchi=sum(esta),grados=grados,pvalor=pv,notas=notas))
	}
)

setMethod("test.independencia",signature(v1="factor",
	v2="agrupado"),
	function(v1,v2) {
		nombres <- c(deparse(substitute(v1)),nombre(v2))
		grados <- (length(levels(v1))-1)*(length(frec(v2))-1)
		if (grados < 1)
			stop("Cantidad de categorías insuficiente.")
		fobs <- as.matrix(table(v1,factor(v2)))
		v1.mar <- as.numeric(table(v1)); v2.mar <- t(frec(v2))
		fesp <- (v1.mar %*% v2.mar)/sum(v1.mar)
		dimnames(fesp) <- list(levels(v1),intervalo.clase(v2))
		esta <- (fobs-fesp)^2/fesp
		if (any(fesp<5))
			notas <- "Hay frecuencias esperadas menores que 5."
		else
			notas <- ""
		pv <- pchisq(q=sum(esta),df=grados,lower.tail=FALSE)
		return(new("independencia",nombres=nombres,fobs=fobs,fesp=fesp,
				estchi=sum(esta),grados=grados,pvalor=pv,notas=notas))
	}
)

setMethod("test.independencia",signature(v1="agrupado",
	v2="factor"),
	function(v1,v2) {
		nombres <- c(nombre(v1),deparse(substitute(v2)))
		grados <- (length(frec(v1))-1)*(length(levels(v2))-1)
		if (grados < 1)
			stop("Cantidad de categorías insuficiente.")
		fobs <- as.matrix(table(factor(v1),v2))
		v1.mar <- frec(v1); v2.mar <- t(as.numeric(table(v2)))
		fesp <- (v1.mar %*% v2.mar)/sum(v1.mar)
		dimnames(fesp) <- list(intervalo.clase(v1),levels(v2))
		esta <- (fobs-fesp)^2/fesp
		if (any(fesp<5))
			notas <- "Hay frecuencias esperadas menores que 5."
		else
			notas <- ""
		pv <- pchisq(q=sum(esta),df=grados,lower.tail=FALSE)
		return(new("independencia",nombres=nombres,fobs=fobs,fesp=fesp,
				estchi=sum(esta),grados=grados,pvalor=pv,notas=notas))
	}
)

#Metodo para crear objeto tipo regresion
setMethod("regresion.lineal",signature(modelo="formula",x="data.frame"),
	function(modelo,x,nombre=deparse(substitute(x,env=parent.frame())),
			 otras="todas")
	{
		# * x es el dataframe con los datos de la regresion
		# * otras es un argumento opcional para especificar 
		#   las variables externas al modelo
		#   Por defecto, es igual a 
		ml <- as.character(modelo)
		ml <- paste(ml[2],ml[1],ml[3])
		ans <- lm(modelo,data=x)
		if (otras=="todas") {
			#extrae todas las variables del marco
			otras <- colnames(x)
			#quitale las variables en el modelo ...
			otras <- setdiff(otras,colnames(ans$model) )
			#esas son las otras variables.  cuales son numericas?
			if (length(otras)>0)
			   otras <- otras[sapply(1:length(otras),
			                  function(i) 
			                  is.numeric(eval(as.name(otras[i]),
			                  envir=x)))]
		}						  
		return(new("regresion",marco=nombre,formulamodelo=ml,
			   resultado=ans,otras=otras))
	}
)

setMethod("regresion.lineal",signature(modelo="formula",x="missing"),
	function(modelo,otras="ninguna")
	{
		# * x es el dataframe con los datos de la regresion
		# * otras es el conjunto de las otras variables de interes.
		#   Por defecto, es "ninguna".
		ml <- as.character(modelo)
		ml <- paste(ml[2],ml[1],ml[3])
		ans <- lm(modelo)
		if (otras=="ninguna") otras <- character(0)
		return(new("regresion",marco="variables globales",
		           formulamodelo=ml,resultado=ans,otras=otras))
	}
)

#Metodo para devolver los numeros de renglon correspondientes
#a aquellos datos con residuos atipicos en la regresion
setMethod("residuos.atipicos",signature(x="regresion"),
	function(x) {
		  z <- x@resultado
		  r <- residuals(z)
	      atip <- r[r %in% atipicos(r)]
	      return(as.numeric(names(atip)))
	}
)

setMethod("coeficientes",signature(x="regresion"),
	function(x) {
		ans <- coefficients(x@resultado)
		return(ans)
	}
)

#Metodos para imprimir data.frames o vectores según "castellanizados",
#es decir, con la "," como punto decimal.

setMethod("imprimir",signature(x="data.frame"),
	function(x) {
		print(format(x,justify="centre",decimal.mark=","),quote=FALSE)
	}
)

setMethod("imprimir",signature(x="matrix"),
	function(x) {
		print(format(x,justify="centre",decimal.mark=","),quote=FALSE)
	}
)
setMethod("imprimir",signature(x="table"),
	function(x) {
		print(format(x,justify="centre",decimal.mark=","),quote=FALSE)
	}
)

setMethod("imprimir",signature(x="numeric"),
	function(x) {
		if (length(x)>0)
			print(format(x,justify="right",decimal.mark=","),
				quote=FALSE)
		else
			cat("Ninguno\n")
	}
)

setMethod("imprimir",signature(x="ajuste"),
	function(x) {
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Contraste de",nombre(x),"a una distribucion",distrl(x),
			"\n\n")
		cat("Tabla de frecuencias observadas y esperadas:\n")
		imprimir(tabla(x))
		cat("\nParametros:\n")
		print(param(x))
		cat("\nEstadistico de prueba : ",estadistico(x),"\n")
		cat("Grados de libertad    : ",grados(x),"\n")
		cat("p-valor               : ",pvalor(x),"\n")
		cat("---------------------------------------------------------",
			"-----\n",sep="")		
	}
)

setMethod("imprimir",signature(x="independencia"),
	function(x) {
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Contraste de independencia chi-cuadrado\n\n")
		nota <- paste("NOTA: En las tablas, las filas representan",
					  "niveles de la variable",x@nombres[1],
					  "y las columnas representan los niveles de la",
					  "variable ",x@nombres[2],".\n")
		cat(.formatear_texto(nota,ancho=55),"\n")			  
		cat("Tabla de frecuencias observadas:\n")
		imprimir(frec(x))
		cat("\nTabla de frecuencias esperadas:\n")
		imprimir(frec.esp(x))
		cat("\nEstadistico de prueba : ",estadistico(x),"\n")
		cat("Grados de libertad    : ",grados(x),"\n")
		cat("p-valor               : ",pvalor(x),"\n")
		cat("---------------------------------------------------------",
			"-----\n",sep="")
	}
)

#Métodos "resumen" para: categoricos agrupados, cuantitativos agrupados,
#datos sin agrupar, contrastes de ajuste e independencia, regresion.

setMethod("resumen",signature(x="agrupado"),
	function(x) {
		if (tipo(x)=="ordinal") {
			ordenado <- TRUE
			vari <- as.numeric(factor(x)) }
		else
			ordenado <- FALSE
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Resumen de la Variable",nombre(x),
			"para datos agrupados.\n")
		cat("Tabla de frecuencias:\n")
		imprimir(tabla(x))
		cat("\n","Medidas de Tendencia Central","\n",sep="")
		cat("  Moda    : ",moda(x),"\n")
		if (ordenado)
			cat("  Mediana : ",levels(factor(x))[cuartil(vari,2)],"\n")
		cat("\n","Medidas de Dispersion","\n",sep="")
		cat("  Categorias :",length(frec(x)),"\n")
		if (ordenado) {
			cat("\n","Medidas de Posición\n")
			cat("  Cuartil 1 :",levels(factor(x))[cuartil(vari,1)],"\n")
			cat("  Cuartil 3 :",levels(factor(x))[cuartil(vari,3)],"\n")
		}
		cat("---------------------------------------------------------",
			"-----\n",sep="")	
	}
)

setMethod("resumen",signature(x="cuantitativo.agrupado"),
	function(x) {
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Resumen de la Variable",nombre(x),"para datos agrupados.\n")
		cat("Tabla de frecuencias:\n")
		imprimir(tabla(x))
		cat("\n","Medidas de Tendencia Central","\n")
		cat("  Moda        :",moda(x),"\n")
		cat("  Mediana     :",cuartil(x,2),"\n")
		cat("  Media       :",media(x),"\n")
		cat("\n","Medidas de Dispersion","\n")
		cat("  Rango Inter-cuartil  :",cuartil(x,3)-cuartil(x,1),"\n")
		cat("  Desviacion Estandard :",desv(x),"\n")
		cat("\n","Medidas de Posición","\n")
	    cat("   Cuartil 1 :",cuartil(x,1),"\n")
	    cat("   Cuartil 3 :",cuartil(x,3),"\n")
		cat("---------------------------------------------------------",
			"-----\n",sep="")
	}
)

setMethod("resumen",signature(x="numeric"),
	function(x,nombre=deparse(substitute(x,env=parent.frame()))) {
		variable <- nombre
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Resumen de la Variable",variable," para datos sin agrupar\n")
		#elimina los NA y da un aviso si hay
		if (any(is.na(x))) {
			warning("La muestra contiene NA's. Han sido eliminados")
			x <- x[!is.na(x)]
		}	
		cat("\n","Medidas de Tendencia Central","\n")
		cat("  Moda    : ",moda(x),"\n")
		cat("  Mediana : ",	cuartil(x,2),"\n")
		cat("  Media   : ",media(x),"\n")
		cat("\n","Medidas de Dispersion","\n")
		cat("  Desv. Mediana Absoluta : ",mad(x),"\n")
		cat("  Rango Inter Cuartil    : ",cuartil(x,3)-cuartil(x,1),
			"\n")
		cat("  Desviacion Estandard   : ",desv(x),"\n")
		cat("\n","Medidas de Posicion","\n")
		cat("  Minimo     : ",min(x),"\n")
		cat("  Cuartil 1  : ",cuartil(x,1),"\n")
		cat("  Mediana    : ",cuartil(x,2),"\n")
		cat("  Cuartil 3  : ",cuartil(x,3),"\n")
		cat("  Maximo     : ",max(x),"\n")
		cat("\n","Valores Atipicos","\n")
		am <- atipicos(x,"moderado"); ae <- atipicos(x,"extremo")
		cat("  Moderados : \n")
		imprimir(am)
		cat("  Extremos  : \n")
		imprimir(ae)
		cat("\n","Medidas de Forma","\n")
		cat("  Coeficiente de asimetria  : ",asimetria(x),"\n")
		cat("  Curtosis                  : ",curtosis(x),"\n")
		cat("\n")
		cat("---------------------------------------------------------",
			"-----\n",sep="")
	}
)

setMethod("resumen",signature(x="factor"),
	function(x) {
		a <- agrupar(x,nombre=deparse(substitute(x,env=parent.frame())))
		resumen(a)
	}
)

setMethod("resumen",signature(x="character"),
	function(x) {
		a <- agrupar(x,nombre=deparse(substitute(x,env=parent.frame())))	
		resumen(a)
	}
)

setMethod("resumen",signature(x="ajuste"),
	function(x) {
		s <- paste("Contraste de bondad de ajuste de",nombre(x),
				"a una distribucion",distrl(x),"\n\n")
		s <- .formatear_texto(s)
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat(s)
		cat("H_0: ",nombre(x),"~",distr(x),"\n")
		cat("H_1: ",nombre(x),"!~",distr(x),"\n\n")
		cat("Estdst. chi-cuadrado : ",estadistico(x),"\n")
		cat("Grados de libertad   : ",grados(x),"\n")
		cat("p-valor              : ",ifelse(pvalor(x)<10^-3,"< 0.001",pvalor(x)),"\n")
		if (length(notas(x))>0) {
			cat("Observacion(es):\n")
			for (i in 1:length(notas(x)))
				cat("(",i,") ",notas(x)[i],"\n",sep="") }
		cat("---------------------------------------------------------",
			"-----\n",sep="")
	}
)

setMethod("resumen",signature(x="independencia"),
	function(x) {
		cat("---------------------------------------------------------",
			"-----\n",sep="")
		cat("Contraste de independencia chi-cuadrado\n\n")
		cat("H_0:",nombre(x,1),"y",nombre(x,2),"son independientes.\n")
		cat("H_1:",nombre(x,1),"y",nombre(x,2),
			"no son independientes.\n")
		cat("Estdst. chi-cuadrado :",estadistico(x),"\n")
		cat("Grados de libertad   :",grados(x),"\n")
		cat("p-valor              :",pvalor(x),"\n")
		if (all(x@notas!="")) {
			cat("Observaciones:\n")
			for (i in 1:length(x@notas))
				cat("(",i,") ",x@notas[i],"\n",sep="") }
		cat("---------------------------------------------------------",
			"-----\n",sep="")
	}
)

setMethod("resumen",signature(x="regresion"),
    function (x,nombre=deparse(substitute(x,env=parent.frame())),
			  atipicos=FALSE)  {
    #* x es el objeto tipo regresion
    #* nombre el el identificador del objeto tipo regresion
    #  (tipo caracter)
    #* atipicos es TRUE si se desea imprimir el detalle de los
    #  residuos atipicos.
      z <- x@resultado
      p <- z$rank
      if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        }
        else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/(n - p)
        ans <- z[c("call", "terms")]
        class(ans) <- "summary.lm"
        ans$aliased <- is.na(coef(z))
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA, 0L, 4L)
        dimnames(ans$coefficients) <- list(NULL, c("Estimacion", 
            "Error Est.", "Estadistico T", "p-valor"))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        return(ans)
      }
      Qr <- z$qr
      if (is.null(z$terms) || is.null(Qr)) 
        stop("objeto 'lm' invalido:  no hay componente 'terms' o 'qr'")
      n <- NROW(Qr$qr)
      rdf <- n - p
      if (is.na(z$df.residual) || rdf != z$df.residual) 
          warning("los grados de libertad de los residuos sugieren que este no es un ajuste \"lm\"")
      p1 <- 1L:p
      r <- z$residuals
      f <- z$fitted.values
      w <- z$weights
      if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept")) 
                 sum((f - mean(f))^2)
               else
                 sum(f^2)
        rss <- sum(r^2)
      }
      else {
        mss <- if (attr(z$terms, "intercept")) {
                 m <- sum(w * f/sum(w))
                 sum(w * (f - m)^2)
               }
               else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
      }
      resvar <- rss/rdf
      R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
      se <- sqrt(diag(R) * resvar)
      est <- z$coefficients[Qr$pivot[p1]]
      tval <- est/se
      ans <- z[c("call", "terms")]
      ans$residuals <- r
      ans$coefficients <- cbind(est, se, tval, 2 * pt(abs(tval), 
        rdf, lower.tail = FALSE))
      loscoef <- names(z$coefficients)
      loscoef <- gsub("\\(Intercept\\)","[Intercepto]",loscoef)
      loscoef <- gsub("^I\\(","",loscoef)
      loscoef <- gsub("\\)$","",loscoef)
      dimnames(ans$coefficients) <- list(loscoef[Qr$pivot[p1]], 
        c("Estimacion", "Error Est.", "Estadistico T", "p-valor"))
      ans$coefficients <- as.data.frame(ans$coefficients)
      ans$coefficients[,4] <- format.pval(ans$coefficients[,4])
      ans$aliased <- is.na(coef(z))
      ans$sigma <- sqrt(resvar)
      ans$df <- c(p, rdf, NCOL(Qr$qr))
      if (p != attr(z$terms, "intercept")) {
        df.int <- if (attr(z$terms, "intercept")) 
                   1L
                  else
                   0L
        ans$r.squared <- mss/(mss + rss)
        ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
            df.int)/rdf)
        ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
            numdf = p - df.int, dendf = rdf)
       }
       else 
         ans$r.squared <- ans$adj.r.squared <- 0
       ans$cov.unscaled <- R
       dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 
        1)]
       if (!is.null(z$na.action)) 
         ans$na.action <- z$na.action
	   cat("---------------------------------------------------------",
	       "-----\n",sep="")
	   cat("Resumen de regresion lineal\n\n")
	   cat("  MODELO         :",nombre,"\n")
	   cat("  Marco de datos :",x@marco,"\n")
	   cat("  Formula        :",x@formulamodelo,"\n\n")
	   cat("Estimacion de los coeficientes poblacionales\n\n")
	   print(ans$coefficients)
	   cat("\nPrueba F global\n\n")
	   f <- ans$fstatistic["value"]
	   d1 <- ans$fstatistic["numdf"]
	   d2 <- ans$fstatistic["dendf"]
	   cat("  Valor F :",f,"  gl. num:",d1,"  gl. den :",d2,
	       "\n  p-valor :",format.pval(pf(q=f,df1=d1,df2=d2,
	       lower.tail=FALSE)),"\n\n")
	   cat("Coeficientes de determinacion\n\n")
	   cat("   R^2 :",ans$r.squared,"  R^2 ajustado :",
	       ans$adj.r.squared,"\n\n")
	   cat("Residuos\n\n")
	   cat("  Minimo  :",min(r),"\n")
	   cat("  Mediana :",cuartil(r,2),"\n")
	   cat("  Maximo  :",max(r),"\n")
	   cat("\n  Desv. estandar residual: ",ans$sigma,"\n\n")
	   if (atipicos) {	
		  cat("Residuos atipicos\n\n")
		  #si no hay marco de datos, sacarlo de las variables de
		  #la regresion	

	      iatip <- which(r %in% atipicos(r))
	      fv <- fitted(z)
	      datf <- z$model
		  if (x@marco=="variables globales") {
			vari <- names(attr(z$terms,"dataClasses"))
		    marco <- cbind(sapply(1:length(vari),
		                   function (i) eval(as.name(vari[i])) ) )
		    marco <- as.data.frame(marco)
		    colnames(marco) <- vari }
		  else               
		    marco <- eval(as.name(x@marco),envir=.GlobalEnv)
	      y <- colnames(datf)[1]
	      nombres <- c(if (!(y %in% colnames(marco))) y,
	                   paste(y,"_pred",sep=""),
	                   "resid")
	      datfr <- cbind(if (!(y %in% colnames(marco))) z$model[,1],
	                     fv,r)
	      colnames(datfr) <- nombres
	      atip <- datfr[iatip,]
	      datfr <- cbind(marco[as.numeric(rownames(atip)),],atip)
	      print(datfr)
	   } 
	   cat("---------------------------------------------------------",
	       "-----\n",sep="")		
    }
)


#METODOS DE GRAFICACION

setMethod("graficar",signature(x="factor"),
	function(x,nombre=deparse(substitute(x,env=parent.frame())),
			 tipo="torta") {
		#elimina los NA y da un aviso si hay
		if (any(is.na(x))) {
			warning("La muestra contiene NA's. Han sido eliminados")
			x <- x[!is.na(x)] }
		a <- agrupar(x,nombre=nombre)
		if (missing(tipo) & tipo(a)=="ordinal") 
			tipo <- "barras"
		switch(tipo,
		torta 	= {
			rotulo <- paste("Grafica de Torta de ",nombre(a),sep="")
			pie(frec(a),labels=paste(marca.clase(a),"\n",frec(a)," (",
				round(frec.rel(a)*100,2),"%)",sep=""),
				col=rainbow(length(marca.clase(a))),
				main=rotulo) },
		barras = {
			rotulo <- paste("Grafica de Barras de ",nombre(a),sep="")
			barplot(frec.rel(a), names.arg=marca.clase(a),main=rotulo,
					 xlab=nombre(a)) },
		stop(paste("Tipo de grafica invalido:",tipo)) )
	}
)

setMethod("graficar",signature(x="character"),
	function(x,nombre=deparse(substitute(x,env=parent.frame())),
			 tipo="torta") {
		#elimina los NA y da un aviso si hay
		if (any(is.na(x))) {
			warning("La muestra contiene NA's. Han sido eliminados")
			x <- x[!is.na(x)] }
		a <- agrupar(x,nombre=nombre)
		switch(tipo,
		torta 	= {
			rotulo <- paste("Grafica de Torta de ",nombre(a),sep="")
			pie(frec(a),labels=paste(marca.clase(a),"\n",frec(a),
				" (",round(frec.rel(a)*100,2),"%)",sep=""),
				col=rainbow(length(marca.clase(a))),
				main=rotulo) },
		barras = {
			rotulo <- paste("Grafica de Barras de ",nombre(a),sep="")
			barplot(frec.rel(a), names.arg=marca.clase(a),main=rotulo,
					 xlab=nombre(a)) },
		stop(paste("Tipo de grafica invalido:",tipo)) )		
	}
)

setMethod("graficar",signature(x="numeric"),
	function(x,intervalos="sturges.r",tipo="histograma",
			 nombre=deparse(substitute(x,env=parent.frame()))) {
		#elimina los NA y da un aviso si hay
		if (any(is.na(x))) {
			warning("La muestra contiene NA's. Han sido eliminados")
			x <- x[!is.na(x)] }
		switch(tipo,
		histograma = {
			#agrupa los datos primero
			a <- agrupar(x,intervalos=intervalos,tipo="continua",
						 nombre=nombre)
			#Obtiene el rotulo principal de la gráfica
			rotulo <- paste("Histograma de ",nombre(a),sep="")
			ejex <- sort(unique(c(lim.inferior(a),lim.superior(a))))
			#Obtiene la altura máxima (eje y)
			h <- hist(x,breaks=ejex,plot=FALSE)
			lim_y <- max(h$density)*1.1
			par(mar=c(5,5,4,2))
			h <- hist(x,breaks=ejex,freq=FALSE,col=gray(0.80),
						ylim=c(0,lim_y),main=rotulo,xlab=nombre(a),
						ylab="Densidad",axes=F)
			rug(x)
			n <- length(h$density)
			ejey <- sort(c(0,h$density))
			axis(2, at=ejey, 
				labels=c("0",rep("",n-1),sprintf("%1.2e",
				ejey[n+1])), las=2)
			axis(1, at=ejex,labels=sprintf("%1.3g",ejex), las=0)
			kd <- density(x)
			lines(kd,col="red",lty=2)
			abline(v=moda(x),col="red")
			abline(v=cuartil(x,2),col="green")
			abline(v=media(x),col="blue")
			legend(x="top",
					legend=c("Densidad","moda","mediana","media"),
					text.col=c("red","red","green","blue"),
					col=c("red","red","green","blue"),box.col="white",
					lty=c(2,1,1,1),bg="white",horiz=T) },
		barras = {
			#agrupa los datos primero
			a <- agrupar(x,tipo="discreta")
			#Obtiene el rotulo principal de la gráfica
			rotulo <- paste("Grafica de barras de ",nombre,sep="")
			ejex <- as.numeric(marca.clase(a))
			#Obtiene la altura máxima (eje y)
			lim_y <- max(frec.rel(a))*1.1
			par(mar=c(5,5,4,2))
			barplot(frec.rel(a), names.arg=ejex,main=rotulo,
					 xlab=nombre) },
		torta = {
			#agrupa los datos primero
			a <- agrupar(x)
			#Obtiene el rotulo principal de la gráfica
			rotulo <- paste("Grafica de torta de ",nombre,sep="")
			pie(frec(a),labels=paste(intervalo.clase(a),"\n",frec(a),
				" (",round(frec.rel(a)*100,2),"%)",sep=""),
				col=rainbow(length(frec(a))),
				main=rotulo) },		
		stop(paste("Tipo de grafica (",tipo,
					") invalido para la variable ",nombre,".\n")) )	
	}
)

setMethod("graficar",signature(x="ajuste"),
	function(x) {
		#primero lo primero: verifica si se hizo el ajuste
		if (fallo(x)) {
			cat("No se puede graficar porque 'ajustar' fallo.\n")
			return(NULL) }
		#define los posibles tipos de distribuciones continuas y discretas
		continuas <- c("normal","expo","gamma","unifc")
		discretas <- c("binom","geom","pois","unifd")
		#continua extrayendo la data necesaria...
		vx <- variable(x)
		vx <- vx[!is.na(vx)]
		prm <- param(x)
		if (distr(x) %in% continuas) {
			h <- hist(vx,plot=FALSE)
			decimales <- round(log(max(vx),base=10))
			lmy <-round(max(c(density(vx)$y,h$density))*1.2,decimales)
			hist(vx,freq=FALSE,xlab=nombre(x),ylab="Densidad",
				 main=paste("Histograma de",nombre(x),
							"\nAjuste a distribucion",distrl(x),
							"\np-valor :",
							ifelse(pvalor(x)<10^-3,"< 0.001",pvalor(x))),
				 ylim=c(0,lmy))
			switch(distr(x),
				normal = {curve(dnorm(x,mean=prm["media"],
								sd=prm["desv"]),from=min(vx),to=max(vx),
								col="red",add=TRUE)
						  legend(x="top",text.col="black",
								legend=c(distrl(x),
								sprintf("media = %.4f",prm["media"]),
								sprintf("desv. = %.4f",prm["desv"])),
								fill=c("red","white","white"),
								border="white",bg="white",horiz=TRUE)
						 },
				expo   = {curve(dexp(x,rate=prm["tasa"]),from=0,
								to=max(vx), col="red",add=TRUE)
						  legend(x="top",text.col="black",
								legend=c(distrl(x),
								sprintf("tasa= %.4f",prm["tasa"])),
								fill=c("red","white"),
								border="white",bg="white",horiz=TRUE)
						 },
				gamma  = {curve(dgamma(x,scale=prm["s"],shape=prm["a"]),
								from=0,to=max(vx), col="red",add=TRUE)
						  legend(x="top",text.col="black",
								legend=c(distrl(x),
								sprintf("escala= %.4f",prm["s"]),
								sprintf("forma = %.4f",prm["a"])),
								fill=c("red","white","white"),
								border="white",bg="white",horiz=TRUE)
						 },
				unifc  = {curve(dunif(x,min=prm["min"],max=prm["max"]),
								from=prm["min"],to=prm["max"],
								col="red",add=TRUE)
						  legend(x="top",text.col="black",
								legend=c(distrl(x),
								sprintf("min. = %.4f",prm["min"]),
								sprintf("max. = %.4f",prm["max"])),
								fill=c("red","white","white"),
								border="white",bg="white",horiz=TRUE) }
			)
			
		}
		else {
			a <- min(vx); b <- max(vx); rango <- b-a
			if (rango>7) {#agrupa los valores en intervalos
				h <- hist(vx,plot=FALSE)
				print(h)
				inf <- head(h$breaks,-1) -1
				sup <- tail(h$breaks,-1) -1
				switch(distr(x),
					binom = {inf[1] <- -1
							 sup[length(sup)] <- prm["n"] },
					geom  = {inf[1] <- -1
							 sup[length(sup)] <- Inf } ,
					pois  = {inf[1] <- -1
							 sup[length(sup)] <- Inf } ,
					unif  = {inf[1] <- prm["min"]-1
							 sup[length(sup)] <- prm["max"] } )
				switch(distr(x),
					binom = {n <-  prm["n"]; pr <- prm["p"]
							 frec.esp <- (pbinom(sup,size=n,prob=pr)-
								pbinom(inf,size=n,prob=pr))*length(vx)} ,
					geom  = {pr <- prm["p"]
						     frec.esp <- (pgeom(sup,prob=pr) - 
								pgeom(inf,prob=pr))*length(vx) },
					pois  = {ta <- prm["tasa"]
							 frec.esp <- (ppois(sup,lambda=ta) - 
								ppois(inf,lambda=ta))*length(vx)} ,
					unif  = {min <- prm["min"]; max <- prm["max"]
							 frec.esp <- (punif(sup,min=min,max=max) - 
								punif(inf,min=min,max=max))*length(vx) } )
				lmy <-round(max(frec.esp)*1.2,2)
				plot(h,xlab=nombre(x),ylab="Densidad",
					main=paste("Histograma de",nombre(x),
					"\nAjuste a distribucion",distrl(x),
					"\np-valor : ",ifelse(pvalor(x)<10^-3,"< 0.001",pvalor(x))),
					ylim=c(0,lmy))
				for (i in 1:length(h$mids))	
					lines(x=rep(h$mids[i],2),y=c(0,frec.esp[i]),
						  col="red")
				points(x=h$mids,y=frec.esp,col="red")
				}
			else {
				}
				
		}
	}
)

setMethod("graficar",signature(x="regresion"),
	function(x,nombre=deparse(substitute(x,env=parent.frame()))) {
    #* x es el objeto tipo regresion
        #extract model formula for title
        #fs means "formula string"
		fs <- x@formulamodelo
		fs <- strsplit(fs," + ",fixed=TRUE)[[1]]
		fs_b <- strsplit(fs[1]," ~ ",fixed=TRUE)[[1]]
		fs <- fs[-1]
		if (fs_b[2]=="1") {
			fs_b[2] <- "beta[0]"
		}
		fs2 <- ""
		for (i in (1:length(fs))) {
			tempx <- gsub("I\\((.*)\\)$","\\1",fs[i],perl=FALSE)
			#this eliminates the I(.) stuff
			tempx <- paste("beta[",i,"]%.%",tempx,sep="")
			#this pre-pends the beta_i stuff
			fs2 <- paste(fs2," + ",tempx,sep="")
		}
		fs <- paste(fs_b[1]," == ",fs_b[2],fs2," + epsilon",sep="")
		#extraer los resultados del modelo
		elmodelo <- x@resultado
		titulo <- paste("Analisis de Residuos para",nombre)	
		#extrae los residuos
		res <- residuals(elmodelo)
		if (any(is.na(res)))
			res <- subset(res,subset=!is.na(res))
		margenes_anteriores <- par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,4,0))
		#grafica de ajuste de residuos a la normal
		p.aj <- shapiro.test(res)$p.value
		hist(res,freq=FALSE,
			main=paste("Verificacion gráfica de normalidad de los residuos",
			   "\np-valor del contraste de ajuste :",
			   ifelse(p.aj<10^-3,"< 0.001",p.aj) ) )
		curve(dnorm(x,mean=0,sd=sd(res)),from=min(res),
			to=max(res), col="red",add=TRUE)
		#imprime el titulo para el histograma de normalidad
		mtext("Análisis de residuos para el modelo",side=3, line=2, outer=TRUE, cex=1.5)
		mtext(parse(text=fs), side=3, line=0, outer=TRUE, cex=1)
		#grafica q-q plot para los residuos estudentizados
		qqnorm(rstudent(elmodelo),ylab="Residuos estudentizados",
			xlab="Cuantiles teoricos",
			main=paste("Verificacion grafica de normalidad de los residuos",
			   "\n(Grafica cuantil-cuantil)") )
		abline(0,1,col="red")
		#imprime el titulo para el plot cuantil-cuantil
		mtext("Análisis de residuos para el modelo",side=3, line=2, outer=TRUE, cex=1.5)
		mtext(parse(text=fs), side=3, line=0, outer=TRUE, cex=1)
		terminos <- terms(elmodelo)
		#now do the residual versus other variable plots
		#letter index
		lindex <- 2 #start with b cos a is fitted values
		#selecciona aquellas variables independientes que sean numericas
		vp <- attr(terminos,"term.labels")
		vp <- vp[sapply(vp,function(argu) 
			is.numeric(eval(as.name(argu),envir=elmodelo$model)))]
		reticulado_anterior <- par(mfrow=c(2,2),mar=c(5,2,1,2),oma=c(2,2,6,2))$mfrow
		#selecciona los valores ajustados no NA y graficalos contra los residuos
		fv <- as.numeric(fitted.values(elmodelo))
		if (any(is.na(fv)))
			fv <- subset(fv,subset=!is.na(fv))
		plot(fv,res,xlab="(a) Predicciones",ylab="residuos")
		mtext("Diagramas de dispersión de residuos para el modelo",side=3, line=4, outer=TRUE, cex=1.5)
		mtext(parse(text=fs), side=3, line=1, outer=TRUE, cex=1)
		#could use lowess instead of smooth.spline?
		lines(lowess(fv,res,iter=6),col="green")
		abline(h=0,col="green",lty=2)
		iqr <- cuartil(res,3)-cuartil(res,1)
		#dibuja los limites de atipicidad moderada
		abline(h=cuartil(res,3)+iqr*1.5,col="orange",lty=2)
		abline(h=cuartil(res,1)-iqr*1.5,col="orange",lty=2)
		#dibuja los limites de atipicidad extrema
		abline(h=cuartil(res,3)+iqr*3,col="red",lty=2)
		abline(h=cuartil(res,1)-iqr*3,col="red",lty=2)
		#grafica las variables independientes contra residuos
		for (i in 1:length(vp)) {
			lv <- eval(as.name(vp[i]),envir=elmodelo$model)
			vn <- gsub("^I\\((.*)\\)","\\1",vp,perl=FALSE)
			if (length(unique(lv))>2) {
				plot(lv,res,xlab=paste("(",letters[lindex],") ",parse(text=vn[i]),sep=""),ylab="residuos")
				if ((lindex-1) %% 4 ==0) {
					mtext("Diagramas de dispersión de residuos para el modelo",side=3, line=4, outer=TRUE, cex=1.5)
					mtext(parse(text=fs), side=3, line=1, outer=TRUE, cex=1)
				}
				lindex <- lindex+1
				lines(lowess(lv,res,iter=6),col="green")
				abline(h=0,col="green",lty=2)
				#dibuja los limites de atipicidad moderada
				abline(h=cuartil(res,3)+iqr*1.5,col="orange",lty=2)
				abline(h=cuartil(res,1)-iqr*1.5,col="orange",lty=2)
				#dibuja los limites de atipicidad extrema
				abline(h=cuartil(res,3)+iqr*3,col="red",lty=2)
				abline(h=cuartil(res,1)-iqr*3,col="red",lty=2) 
			}
			else {
				caja(res,agrupar(lv))
				if (i %% 4 ==0) {
					title(main=paste(titulo,pagina),outer=TRUE)
					pagina <- pagina +1
				}		    
			}
	    }
		#encuentra otras variables cuantitativas en el marco de datos
		#del modelo. SOLO APLICA SI EXISTE UN MARCO DE DATOS.
		if (x@marco!="variables globales") 
			marco <- eval(as.name(x@marco),envir=.GlobalEnv)
		op <- x@otras
		if (length(op)>0)
			for (i in 1:length(op)) {
				lv <- if (x@marco!="variables globales")
						eval(as.name(op[i]),envir=marco)
					  else
						eval(as.name(op[i]),envir=.GlobalEnv)  
				#hay que truncar lv solamente a aquellos elementos de
				#la regresion no NA
				#primero, captura los mismos de la regresion
				lv <- lv[as.numeric(rownames(elmodelo$model))]
				#pero puede haber NA's en lv
				ind <- !is.na(lv)
				lv <- lv[ind]
				#elimina los indices de NA en lv en los residuos tambien
				resv <- res[ind]
				if (length(unique(lv))>2) {
					plot(lv,resv,xlab=paste("(",letters[lindex],") ",op[i]," (extra)",sep=""),
						ylab="residuos")
					lindex <- lindex+1
					if ((lindex-1) %% 4 ==0) {
						mtext("Diagramas de dispersión de residuos para el modelo",side=3, line=4, outer=TRUE, cex=1.5)
						mtext(parse(text=fs), side=3, line=1, outer=TRUE, cex=1)
					}
					lines(lowess(lv,resv,iter=6),col="green")
					abline(h=0,col="green",lty=2)
					#dibuja los limites de atipicidad moderada
					abline(h=cuartil(resv,3)+iqr*1.5,col="orange",lty=2)
					abline(h=cuartil(resv,1)-iqr*1.5,col="orange",lty=2)
					#dibuja los limites de atipicidad extrema
					abline(h=cuartil(resv,3)+iqr*3,col="red",lty=2)
					abline(h=cuartil(resv,1)-iqr*3,col="red",lty=2)
				}
				else {
					caja(resv,agrupar(lv))
					if ((i+length(vp)) %% 4 ==0) {
						title(main=paste(titulo,pagina),outer=TRUE)
						pagina <- pagina +1
					}		    
				}
			}	
		par(margenes_anteriores)
		}
	)
#Got the following error on 9/10/2012 (caja methods)
#Error en text.default(x = ati_x, y = ati_y, labels = cuales_l, pos = posi,  : 
#  no se especificó ninguna coordenada
#Calls: caja -> caja -> .local -> text -> text.default
#Ejecución interrumpida
#Corrected bug the same day: added if (length(ati_y)>0) ...)

#On 14/3/2013 I incorporated a better graphics output based on what I did 
#for the coursera assignments. The model formula is printed better.


setMethod("caja",signature(x="numeric",
						   a="agrupado"),
	function(x,a,titulox=deparse(substitute(x,env=parent.frame())),
			 tituloa=deparse(substitute(a,env=parent.frame())),
			 titulo=paste(titulox,"segun",tituloa)) {
		b <- factor(a)
		plotcaja <- boxplot(x~b,main=titulo,ylab=titulox,
							xlab=tituloa,notch=TRUE)
		ati_y <- plotcaja$out; ati_x <- plotcaja$group
		if (length(ati_x)>0) {
			bodf <- unique(data.frame(g=ati_x,y=ati_y))
			ati_x <- bodf$g; ati_y <- bodf$y
			ind <- order(ati_x,ati_y)
			ati_x <- ati_x[ind]; ati_y <- ati_y[ind]
			cuales_l <- paste("(",
						sapply(1:length(ati_y),
							function(i) 
								paste(as.character(which(x==ati_y[i]
								& as.numeric(b)==ati_x[i])),collapse=",")),
								")",sep="")
			posi <- rep(c(4,2),length.out=length(ati_y))
			text(x=ati_x,y=ati_y,labels=cuales_l,pos=posi,offset=0.4,
				cex=0.6,col="red")
		}
	}
)

setMethod("caja",signature(x="numeric",a="factor"),
	function(x,a,titulox=deparse(substitute(x,env=parent.frame())),
			 tituloa=deparse(substitute(a,env=parent.frame())),
			 titulo=paste(titulox,"segun",tituloa)) {
		plotcaja <- boxplot(x~a,main=titulo,ylab=titulox,
							xlab=tituloa,notch=TRUE)
		ati_y <- plotcaja$out; ati_x <- plotcaja$group
		if (length(ati_x)>0) {
			bodf <- unique(data.frame(g=ati_x,y=ati_y))
			ati_x <- bodf$g; ati_y <- bodf$y
			ind <- order(ati_x,ati_y)
			ati_x <- ati_x[ind]; ati_y <- ati_y[ind]
			cuales_l <- paste("(",
						sapply(1:length(ati_y),
							function(i) 
								paste(as.character(which(x==ati_y[i]
								& as.numeric(a)==ati_x[i])),collapse=",")),
								")",sep="")
			posi <- rep(c(4,2),length.out=length(ati_y))
			text(x=ati_x,y=ati_y,labels=cuales_l,pos=posi,offset=0.4,
				cex=0.6,col="red")	
			}
	}
)

setMethod("caja",signature(x="numeric",a="missing"),
	function(x,titulox=deparse(substitute(x,env=parent.frame())),
			 titulo=deparse(substitute(x,env=parent.frame()))) {
			plotcaja <- boxplot(x,main=titulo,ylab=titulox)
			ati_y <- sort(unique(plotcaja$out))
			if (length(ati_x)>0) {
				cuales_l <- paste("(",
							sapply(1:length(ati_y),
							function(i) 
								paste(as.character(which(x==ati_y[i])),
								collapse=",")),")",sep="")
				posi <- rep(c(4,2),length.out=length(ati_y))
				text(x=1,y=ati_y,labels=cuales_l,pos=posi,offset=0.4,
					cex=0.6,col="red")
			}
	}
)
