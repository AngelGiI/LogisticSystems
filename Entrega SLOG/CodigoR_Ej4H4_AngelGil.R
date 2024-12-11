##### SLOG H4 - EJ 4 #####

##### Notación #####
# N = Número de máquinas en cola.
# TM = Tiempo de simulación.
# TMAX = Tiempo máximo de simulación.
# TANT = Instante anterior.
# TL = Próximo instante de llegada.
# TS = Próximo instante de servicio.
# TFR = Próximo instante de avería del robot.
# TFAR = Próximo instante de fin de avería del robot.
# R = 1 si el robot está averiado, 0 en caso contrario.
# A = Tiempo que el robot no ha estado trabajando.
# SUMA = Suma del tiempo total en cola.

##### Funcion de avería robot #####
vida_robot=function(x){
  # Seguimos la distribucion de avería de los robots.
  y = 10000
  f = 0
  while (y > f){
    u1 = runif(1)
    u2 = runif(1)
    y = 2.67*u2
    f = (u1<=1/3)*(8*u1) + (u1>1/3)*(4-4*u1)
    x1 = 0.75+0.75*u1
    x2 = 60*x1
  }
  return (x2)
}

##### Subrutinas #####
Llegada = function(N,TANT,TM,TS,A,SUMA,R){
  N = N+1
  # Llegadas: P(5) --> ?? = 5 (h) --> ?? = 1/12 (min)
  u = runif(1)
  DL = -12*log(u)
  TL = TM+DL
  # Si no había máquinas en cola, comienza a repararse directamente.
  if (N == 1){
    u2 = runif(1)
    DS = -velocidad_servicio[i]*log(u2)
    TS = TM+DS
    A = A + (TM-TANT)
  }
  # Si ya había, se suma el tiempo de cola a "SUMA".
  else{
    A = A + (TM-TANT)*R
    SUMA = SUMA + (N-1)*(TM-TANT)
  }
  return(c(N,TL,TS,A,SUMA))
}

Servicio = function(N,TANT,TM,TS,SUMA){
  N = N-1
  # Si no hay máquinas siendo reparadas, el tiempo de servicio es infinito.
  if (N == 0){
    TS = Inf
  # En caso contrario, se reparan según la capacidad de la máquina empleada.
  }else{
    u = runif(1)
    DS = -velocidad_servicio[i]*log(u)
    TS = TM+DS
  }
  SUMA = SUMA + (N+1)*(TM-TANT)
  return(c(N,TS,SUMA))
}

Averia = function(N,TANT,TM,TS,SUMA,R){
  # Si el robot se avería, sumamos 15 minutos al tiempo de reparacion.
  R = 1
  TFAR = TM + 15
  TAR = Inf
  if (N>0){
    TS = TS + 15
    SUMA = SUMA + N*(TM-TANT)
  }
  return(c(TS,TAR,TFAR,SUMA,R))
}

Fin_Averia = function(N,TANT,TM,A,SUMA,R){
  R = 0
  x = runif(1)
  DAR = vida_robot(x)
  TAR = TM + DAR
  TFAR = Inf
  A = A + (TM-TANT)
  SUMA = SUMA + N*(TM-TANT)
  return(c(TAR,TFAR,A,SUMA,R))
}

##### Vectores de costes #####
costes1 = c()
costes2 = c()

##### 1000 simulaciones para cada robot #####

for (i in 1:2){
  for (j in 1:100000){
    ##### Inicialización #####
    N = 0
    TM=0
    TANT=0
    TMAX=120
    TS = Inf
    TAR = Inf
    TFAR = Inf
    R=0
    A=0
    SUMA=0
    
    u1=runif(1)
    DL=-12*log(u1)
    TL=DL
    
    velocidad_servicio = c(10,60/7)
    
    x = runif(1)
    DAR=vida_robot(x)
    TAR=DAR
    
    #### PROGRAMA PRINCIPAL ####
    while (TM<TMAX){
      
      TANT = TM
      TM=min(TL,TS,TAR,TFAR,TMAX)
      
      if (TM==TL){
        k = Llegada(N,TANT,TM,TS,A,SUMA,R)
        N = k[1]
        TL = k[2]
        TS = k[3]
        A = k[4]
        SUMA = k[5]
      }
      
      if (TM==TS){
        k = Servicio(N,TANT,TM,TS,SUMA)
        N = k[1]
        TS = k[2]
        SUMA = k[3]
      }
      
      if (TM==TAR){
        k = Averia(N,TANT,TM,TS,SUMA,R)
        TS=k[1]
        TAR = k[2]
        TFAR = k[3]
        SUMA = k[4]
        R = k[5]
      }
      
      if (TM==TFAR){
        k = Fin_Averia(N,TANT,TM,A,SUMA,R)
        TAR = k[1]
        TFAR = k[2]
        A = k[3]
        SUMA = k[4]
        R = k[5]
      }
      
      if (TM==TMAX){
        if (N == 0){
          A = A + (TM-TANT)
        }
        else{
          A = A + (TM-TANT) * R
          SUMA = SUMA + (1-R) * N * (TM-TANT)
        }
      }
    }
    
    if (i == 1){
      coste = (1000/60)*TMAX + (2000/60)*(TMAX-A) + (8000/60)*SUMA
      costes1 = c(costes1, coste)
    }else{
      coste = (1500/60)*TMAX + (4000/60)*(TMAX-A) + (8000/60)*SUMA
      costes2 = c(costes2, coste)
    }
    
  }
}

##### Análisis de resultados #####

Analisis = data.frame(Robot = character(), `Media Muestra` = numeric(), 
                      `Cuasivarianza Muestral` = numeric(), `LimInf IC 95%` = numeric(),
                      `LimSup IC 95%` = numeric())

for (i in 1:2){
  media = mean(get(paste0("costes",i)))
  cuasivar = sd(get(paste0("costes",i)))
  intervalo = t.test(x=get(paste0("costes",i)), conf.level=0.95)$conf.int
  liminf = intervalo[1]
  limsup = intervalo[2]
  Analisis = rbind(Analisis, c(i, media, cuasivar, liminf, limsup))
}

colnames(Analisis) = c("Robot", "Media Muestral", "Cuasidesviacion tipica", 
                       "LimInf","LimSup")