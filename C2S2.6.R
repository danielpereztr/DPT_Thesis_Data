library(idefix)
library(survival)
#Usar versión antigua de mlogit, la nueva no funciona bien
install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)
library(gmnl)
library(foreign)
library(RStata)

# Generamos un diseño experimental óptimo (priors fijos = 0)
niveles <- c(2,2,3)
codif <- c("D", "D", "D")
priors <- c(0,0,0,0)

diseno <- CEA(niveles, codif, n.sets = 8, n.alts = 2, par.draws = priors,
              no.choice = FALSE, parallel = TRUE)

dis <- diseno$design

# Simulamos las preferencias
preferencias <- c(0.54, -0.14, -0.23, -0.45)

set.seed(1234)

des <- dis
resp <- c()
for (x in 1:100){
  resp <- append(resp, RespondMNL(preferencias, dis, 2))
  des <- rbind(des, dis)
}

dis <- cbind(des[1:1600,], resp)
dis <- cbind(dis, "gid" = rep(1:800, each = 2))
dis <- cbind(dis, "pid" = rep(1:100, each = 16))
dis <- cbind(dis, "alt" = rep(c(1,2)))

#Añadimos algunas características demográficas inventadas
edad <- rnorm(100, 50, 20)
genero <- rbinom(100, 1, 0.5)

# Las añadimos a la base de datos
dis <- cbind(dis, "edad" = rep(edad, each = 16), "genero" = rep(genero, each = 16))
dis <- as.data.frame(dis)
dis$precio <- ifelse(dis$Var32 == 1, 60, ifelse(
  dis$Var33 == 1, 90, 30))
dis$edad2 <- dis$edad^2

# Calculamos el logit condicional
modeloCL <- clogit(resp ~ Var12 + Var22 + Var32 + Var33 + strata(gid), data = dis)
summary(modeloCL)
modeloCL <- clogit(resp ~ Var12 + Var22 + precio + strata(gid), data = dis)
summary(modeloCL)
pseudoR2_CL <- 1-modeloCL[["loglik"]][2]/modeloCL[["loglik"]][1]

# Calculamos el logit de parámetros aleatorios
disMIX <- mlogit.data(dis, choice = "resp", shape = "long", alt.var = "alt", chid.var = "gid",
            id.var = "pid")
modeloMIX <- mlogit(resp ~ Var12 + Var22 + precio | 0, disMIX, panel = TRUE,
                    rpar = c(Var12 = 'n', Var22 = 'n', precio = 'n'), R = 100)
summary(modeloMIX)


# Calculamos el logit de clases latentes usando Stata
options("RStata.StataVersion" = 16)

stata(src = "C2S2.6.b.do", data.in = dis)



