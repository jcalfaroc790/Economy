//////////////////////////////////////
////////////// KENYI /////////////////
//////////////////////////////////////

*definir la ruta de trabajo
cd "C:\Users\"

*importar excel 
import excel "C:\Users\") firstrow
save dkenyi
use dkenyi
*crear archivo log para guardar los resultados de Stata
log using kenyi2.log

*PRUEBA DE NORMALIDAD 
*Pruebas de normalidad
sum 

*histograma
replace IDH = . if IDH == 0 
hist IDH , norm 
replace HA = . if HA == 0   
hist HA        , norm 
replace CIAP = . if CIAP == 0
hist CIAP      , norm 
hist HaB       , norm
hist Tran      , norm


*prueba graficas de dispersion
stem IDH 
replace IDH = . if IDH == 0        
stem HA  
replace HA = . if HA == 0   
stem CIAP    
replace CIAP = . if CIAP == 0    
stem HaB 
replace HaB = . if HaB == 0      
stem Tran
replace Tran = . if Tran == 0       

*grafico densidad 
kdensity IDH 
kdensity HA , norm 
kdensity lnHA      , norm 
kdensity HaB      , norm
kdensity Tran      , norm


*Grafico de distrinucion multiple
histogram IDH, nodraw normal bin(9) xlabel(4(2)16) ylabel(0(.1).3) saving(f1_15a,replace)
graph box IDH, nodraw ylabel(4(2)16) saving(f1_15b,replace)
symplot IDH, nodraw xlabel(0(1)6) ylabel(0(1)6) saving(f1_15c,replace)
qnorm IDH, nodraw xlabel(4(2)16) ylabel(4(2)16) saving(f1_15d,replace)
////conbinar los graficos generados 
graph combine f1_15a.gph f1_15b.gph f1_15c.gph f1_15d.gph, saving(f1_15, replace)

*Test de normalidad
sktest IDH Tran [fweight=CIAP]
sktest IDH Tran HaB HA, noadjust

sktest IDH Tran HaB HA CIAP 
sktest lnIDH lnTran lnHaB lnHA lnCIAP 

swilk lnIDH lnTran lnHaB lnHA lnCIAP
sfrancia lnIDH lnTran lnHaB lnHA lnCIAP

*test de normalidad
sktest IDH        
sktest HA
sktest Tran     
sktest CIAP     
    
swilk IDH
sktest HA        
swilk Tran     
swilk CIAP     
  
sfrancia IDH
sfrancia HA        
sfrancia Tran     
sfrancia CIAP     

expand 5

////////////////////////////////////////////////////////////////////////////////
////  ANÁLISIS POR REGIONES ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
cls
clear
use dkenyi, clear 


destring CIAPhogaresafiliados, generate(CIAP)
rename HogaresAfiliados HA
rename Hogaresabonados HaB
rename Transferencias Tran

generate lnIDH = ln(IDH)
generate lnTran = ln(Tran)
generate lnHA = ln(HA)
generate lnCIAP = ln(CIAP)
generate lnHaB= ln(HaB)

save dkenyi, replace


**************************
***Modelo MCO por regiones
**************************
regress lnIDH lnTran if Región== "Amazonas"
regress lnIDH lnTran if Región== "Ancash"
regress lnIDH lnTran if Región== "Ancash"
regress lnIDH lnTran if Región== "Apurimac"
regress lnIDH lnTran if Región== "Arequipa"
regress lnIDH lnTran if Región== "Ayacucho"
regress lnIDH lnTran if Región== "Cajamarca"
*regress lnIDH lnTran if Región== "Callao" /// No implementado
regress lnIDH lnTran if Región== "Cusco"
regress lnIDH lnTran if Región== "Huancavelica"
regress lnIDH lnTran if Región== "Huánuco"
*regress lnIDH lnTran if Región== "Ica"  /// No implementado 
regress lnIDH lnTran if Región== "Junín"
regress lnIDH lnTran if Región== "La Libertad"
regress lnIDH lnTran if Región== "Lambayeque"
regress lnIDH lnTran if Región== "Lima "
regress lnIDH lnTran if Región== "Loreto"
regress lnIDH lnTran if Región== "Madre de Dios"
*regress lnIDH lnTran if Región== "Moquegua" /// No implementado
regress lnIDH lnTran if Región== "Pasco"
regress lnIDH lnTran lnHA if Región== "Piura"
regress lnIDH lnTran if Región== "Puno"
regress lnIDH lnTran if Región== "Piura"
*regress lnIDH lnTran if Región== "San Martin" /// No implementado
regress lnIDH lnTran if Región== "Tacna"
*regress lnIDH lnTran if Región== "Tumbes"  /// No implementado
regress lnIDH lnTran if Región== "Ucayali"

*******************************************
******* Correlación según regiones
*******************************************
graph pwcorr lnIDH lnTran if Región== "Amazonas"
pwcorr lnIDH lnTran if Región== "Ancash"
pwcorr lnIDH lnTran if Región== "Apurimac"
pwcorr lnIDH lnTran if Región== "Arequipa"
pwcorr lnIDH lnTran if Región== "Ayacucho"
pwcorr lnIDH lnTran if Región== "Cajamarca"
*pwcorr lnIDH lnTran if Región== "Callao" /// No implementado
pwcorr lnIDH lnTran if Región== "Cusco"
pwcorr lnIDH lnTran if Región== "Huancavelica"
pwcorr lnIDH lnTran if Región== "Huánuco"
pwcorr lnIDH lnTran if Región== "Ica"
pwcorr lnIDH lnTran if Región== "Junín"
pwcorr lnIDH lnTran if Región== "La Libertad"
pwcorr lnIDH lnTran if Región== "Lambayeque"
pwcorr lnIDH lnTran if Región== "Lima "
pwcorr lnIDH lnTran if Región== "Loreto"
pwcorr lnIDH lnTran if Región== "Madre de Dios"
*pwcorr lnIDH lnTran if Región== "Moquegua" /// No implementado
pwcorr lnIDH lnTran if Región== "Pasco"
pwcorr lnIDH lnTran if Región== "Piura"
pwcorr lnIDH lnTran if Región== "Puno"
*pwcorr lnIDH lnTran if Región== "San Martin" /// No implementado
pwcorr lnIDH lnTran if Región== "Tacna"
*pwcorr lnIDH lnTran if Región== "Tumbes" /// No implementado
pwcorr lnIDH lnTran if Región== "Ucayali"



////////////////////////////////////////////////////////////////////////////////
////////ANALISIS POR MACROREGIONES//////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

*Definimos las Dimenciones de Espacio y Tiempo en el modelo
xtset Reg Año
*DEFINICION DE LAS MACROREGIONES
*Norte: Tumbes, Piura, Lambayeque, Cajamarca, La Libertad. 
*Centro: Ancash, Junín, Cerro de Pasco, Huánuco, Huancavelica, Ayacucho, Ica. 
*Sur: Arequipa, Moquegua, Tacna, Cusco, Madre de Dios, Apurímac.


**************************
*** MACROREGION NORTE ****
**************************
clear
use dkenyi
xtset Reg Año
*Definir variables
keep if (Región =="Lambayeque" | Región =="Cajamarca" | Región =="La Libertad" | Región =="Tumbes")
*Revisar la base de datos
tab Región /// Si se considero las regiones anteriores
*Gráficos
graph matrix lnIDH lnHA lnCIAP lnHaB lnTran
pwcorr lnIDH lnHA lnCIAP lnHaB lnTran
*DETERMINAR SI USAR EL METODO DE MCO O DATA PANEL
*************************************************
*Instalar paquete 
findit xtcsd

*PRUEBA PARA DATA PANEL LARGA
*PRUEBA BREUSCH AND PAGAN LAGRANGIAN MULTIPLIER 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba Breusch Pagan Lagrangian
xttest0
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR DATOS PANEL 
*Esta prueba solo se hace con efectos aleatorios


*PRUEBA PARA DATA PANEL CORTA 
*PESARAN'S TEST OF CROSS SECTIONAL INDEPENDENCE 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba
xtcsd, pesaran abs
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: NO CONVIENE USAR PANEL DATA


* DETERMINAR EL TIPO DE MODELO USAR
***************************************

*MODELO DE EFECTOS FIJOS
xtreg lnIDH lnHA lnTran, re
estimates store re1

*MODELO DE EFECTOS ALEATORIOS
xtreg lnIDH lnHA lnTran, fe
estimates store fe1

*TEST DE HAUSMAN TEST, para elegir que tipo de modelo usar. 
hausman fe1 re1
h hausman 
*Ho: Usar EFECTOS ALEATORIOS ( > .05). el efecto inobservable no esta correlacionado con las variables explicativas
*H1: Usar EFECTOS FIJOS ( < .05)
*Conclusión: USAR MODELO DE DATA PANEL DE EFECTOS ALEATORIOS

  
*AUTOCORRELACION
****************
*instalar
findit xtserial
*Regresión
xtreg lnIDH lnHA lnTran, re
*Test de Woldridge para determinar la autocorrelación
xtserial lnIDH lnHA lnTran, output
*Ho: No Existe autocorrelacion de primer orden (>.05)
*H1: Existe autocorrelacion (< .05)
*CONCLUSIÓN: No existe autocorrelación

*HETEROCEDASTICIDAD
*instalar 
findit xttest3
*Regresión
xtreg lnIDH lnHA lnTran, fe
*Test de Wald para determinar la heterocedasticidad
xttest3
h xttest3
*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*CONCLUSIÓN: Si existe heterocedasticidad

*CORRECCIÓN DEL MODELO 
//de un modelo con solo heteroscedasticidad//
xtpcse lnIDH lnHA lnTran , het
xtpcse lnIDH lnTran, het
xtgls lnIDH lnHA lnTran, p(h)
regress lnIDH lnHA lnTran 

*CORRECCIÓN DEL MODELO 
//de un modelo con autocorrelación y heterocedasticidad//
xtpcse lnIDH lnHA lnTran, het c(ar1)
*xtgls lnIDH lnHA lnTran, p(h) c(ar1) ///la variable años no cuenta con suficientes datos en todas las variables
*xtgls lnIDH lnHA lnTran,  c(ar1) ///la variable años no cuenta con sufucientes datos en todas las variables


********************************
**** MACROREGIÓN CENTRO ********
********************************

clear all 

use dkenyi
destring CIAPhogaresafiliados, generate(CIAP)
rename HogaresAfiliados HA
rename Hogaresabonados HaB
rename Transferencias Tran

generate lnIDH = ln(IDH)
generate lnTran = ln(Tran)
generate lnHA = ln(HA)
generate lnCIAP = ln(CIAP)
generate lnHaB= ln(HaB)

*Definir las dimensions de Espacio y Tiempo del modelo DATA PANEL 
xtset Reg Año
*Especificar las variables
keep if (Región =="Ancash" | Región =="Junín" | Región =="Pasco" | Región =="Huánuco" | Región =="Huancavelica" | Región =="Ayacucho"  | Región =="Ica")
*Lima (excluye provincia de LIma- Se esta tomando en cuenta la clasificación del Ministerio de Produccion del 2017)

*Correlación
graph matrix lnIDH lnHA lnCIAP lnHaB lnTran
graph matrix lnIDH lnTran
pwcorr lnIDH lnHA lnCIAP lnHaB lnTran

*Estimación Data Panel Región Centro 
xtreg lnIDH lnHA lnTran , fe
xtreg lnIDH lnHA lnTran , re
xtreg lnIDH lnTran , re

*DETERMINAR SI USAR EL METODO DE MCO O DATA PANEL
*************************************************

*Instalar paquete 
findit xtcsd

*PRUEBA PARA DATA PANEL LARGA

*PRUEBA BREUSCH AND PAGAN LAGRANGIAN MULTIPLIER 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba Breusch Pagan Lagrangian
xttest0
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR DATOS PANEL 
*Esta prueba solo se hace con efectos aleatorios


*PRUEBA PARA DATA PANEL CORTA 

*PESARAN'S TEST OF CROSS SECTIONAL INDEPENDENCE 
*Estimar la regresión
xtreg lnIDH lnTran, re
*Utilizar la Prueba
xtcsd, pesaran abs
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR PANEL DATA


* DETERMINAR EL QUE TIPO DE MODELO USAR
***************************************

*MODELO DE EFECTOS FIJOS
xtreg lnIDH lnHA lnTran, re
estimates store re1

*MODELO DE EFECTOS ALEATORIOS
xtreg lnIDH lnHA lnTran, fe
estimates store fe1

*TEST DE HAUSMAN TEST, para elegir que tipo de modelo usar. 
hausman fe1 re1
suest fe1 re1
h suest	
*Ho: Usar EFECTOS ALEATORIOS ( > .05). el efecto inobservable no esta correlacionado con las variables explicativas
*H1: Usar EFECTOS FIJOS ( < .05)
*Conclusión: USAR MODELO DE DATA PANEL DE EFECTOS ALEATORIOS

  
*AUTOCORRELACION
****************
*instalar
findit xtserial
*Regresión
xtreg lnIDH lnHA lnTran, re
*Test de Woldridge para determinar la autocorrelación
xtserial lnIDH lnTran, output
*Ho: No Existe autocorrelacion de primer orden (>.05)
*H1: Existe autocorrelacion (< .05)
*CONCLUSIÓN: NO EXISTE AUTOCORRELACION

*HETEROCEDASTICIDAD

*******Prueba de heterocedasticidad (Test modificado de Wald solo con efectos fijos*******************************

*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*instalar 
findit xttest3
*Regresión
xtreg lnIDH lnHA lnTran, fe
*Test de Wald para determinar la heterocedasticidad
xttest3
*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*CONCLUSIÓN: Si existe heterocedasticidad

*CORRECCIÓN DEL MODELO 
//de un modelo con solo heteroscedasticidad//

xtpcse lnIDH lnHA lnTran, het
xtgls lnIDH lnHA lnTran, p(h)

*CORRECCIÓN DEL MODELO 
//de un modelo con autocorrelación y heterocedasticidad//
xtpcse lnIDH lnTran, het c(ar1)
xtgls lnIDH lnHA lnTran, p(h) c(ar1)
xtgls lnIDH lnHA lnTran,  c(ar1)



*****************************
***** MACROREGIÓN SUR *******
*****************************

clear all 

use dkenyi
destring CIAPhogaresafiliados, generate(CIAP)
rename HogaresAfiliados HA
rename Hogaresabonados HaB
rename Transferencias Tran

generate lnIDH = ln(IDH)
generate lnTran = ln(Tran)
generate lnHA = ln(HA)
generate lnCIAP = ln(CIAP)
generate lnHaB= ln(HaB)

xtset Reg Año
keep if (Región == "Arequipa" | Región =="Tacna" | Región =="Cusco" | Región =="Puno" | Región =="Apurimac")

* Moquegua aun no se implementado
*Correlación

graph matrix lnIDH lnHA lnCIAP lnHaB lnTran
pwcorr lnIDH lnHA lnCIAP lnHaB lnTran

*Estimación 
xtreg lnIDH lnHA lnTran , fe
xtreg lnIDH lnHA lnTran , re
xtreg lnIDH lnTran , re

*DETERMINAR SI USAR EL METODO DE MCO O DATA PANEL
*************************************************
*Instalar paquete 
findit xtcsd

*PRUEBA PARA DATA PANEL LARGA
*PRUEBA BREUSCH AND PAGAN LAGRANGIAN MULTIPLIER 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba Breusch Pagan Lagrangian
xttest0
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR DATOS PANEL 
*Esta prueba solo se hace con efectos aleatorios


*PRUEBA PARA DATA PANEL CORTA 
*PESARAN'S TEST OF CROSS SECTIONAL INDEPENDENCE 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba
xtcsd, pesaran abs
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: NO CONVIENE USAR PANEL DATA


* DETERMINAR EL TIPO DE MODELO USAR
***************************************

*MODELO DE EFECTOS FIJOS
xtreg lnIDH lnHA lnTran, re
estimates store re1

*MODELO DE EFECTOS ALEATORIOS
xtreg lnIDH lnHA lnTran, fe
estimates store fe1

*TEST DE HAUSMAN TEST, para elegir que tipo de modelo usar. 
hausman fe1 re1
*Ho: Usar EFECTOS ALEATORIOS ( > .05). el efecto inobservable no esta correlacionado con las variables explicativas
*H1: Usar EFECTOS FIJOS ( < .05)
*Conclusión: USAR MODELO DE DATA PANEL DE EFECTOS FIJOS

  
*AUTOCORRELACION
****************
*instalar
findit xtserial
*Regresión
xtreg lnIDH lnHA lnTran, fe
*Test de Woldridge para determinar la autocorrelación
xtserial lnIDH lnHA lnTran, output
*Ho: No Existe autocorrelacion de primer orden (>.05)
*H1: Existe autocorrelacion (< .05)
*CONCLUSIÓN: NO EXISTE AUTOCORRELACION

*HETEROCEDASTICIDAD
*instalar 
findit xttest3
*Regresión
xtreg lnIDH lnHA lnTran, fe
*Test de Wald para determinar la heterocedasticidad
xttest3
*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*CONCLUSIÓN: SI HAY HETEROCEDASTICIDAD

*CORRECCIÓN DEL MODELO 
//de un modelo con solo heteroscedasticidad//
xtpcse lnIDH lnHA lnTran, het
xtgls lnIDH lnHA lnTran, p(h)

*CORRECCIÓN DEL MODELO 
//de un modelo con autocorrelación y heterocedasticidad//
xtpcse lnIDH lnHA lnTran, het c(ar1)
*xtgls lnIDH lnHA lnTran, p(h) c(ar1) ///la variable años no cuenta con suficientes datos en todas las variables
*xtgls lnIDH lnHA lnTran,  c(ar1) ///la variable años no cuenta con sufucientes datos en todas las variables


*************************
**** MACROREGIÓN SELVA***
*************************

clear all 

use dkenyi
destring CIAPhogaresafiliados, generate(CIAP)
rename HogaresAfiliados HA
rename Hogaresabonados HaB
rename Transferencias Tran

generate lnIDH = ln(IDH)
generate lnTran = ln(Tran)
generate lnHA = ln(HA)
generate lnCIAP = ln(CIAP)
generate lnHaB= ln(HaB)

keep if (Región == "Loreto"  | Región =="San Martín" | Región =="Amazonas" | Región =="Ucayali" | Región =="Madre de Dios")
*Especificar las dimensiones 
xtset Reg Año
*Correlación
graph matrix lnIDH lnHA lnCIAP lnHaB lnTran
pwcorr lnIDH lnHA lnCIAP lnHaB lnTran
*Estimación Data Panel Región Sur 
xtreg lnIDH lnTran , fe
xtreg lnIDH lnHA lnTran , fe

xtreg lnIDH lnTran , re
xtreg lnIDH lnHA lnTran , re

*DETERMINAR SI USAR EL METODO DE MCO O DATA PANEL
*************************************************

*Instalar paquete 
findit xtcsd

*PRUEBA PARA DATA PANEL LARGA

*PRUEBA BREUSCH AND PAGAN LAGRANGIAN MULTIPLIER 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba Breusch Pagan Lagrangian
xttest0
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR DATOS PANEL 
*Esta prueba solo se hace con efectos aleatorios


*PRUEBA PARA DATA PANEL CORTA 

*PESARAN'S TEST OF CROSS SECTIONAL INDEPENDENCE 
*Estimar la regresión
xtreg lnIDH lnHA lnTran, re
*Utilizar la Prueba
xtcsd, pesaran abs
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR PANEL DATA


* DETERMINAR EL QUE TIPO DE MODELO USAR
***************************************

*MODELO DE EFECTOS FIJOS
xtreg lnIDH lnHA lnTran, re
xtreg lnIDH lnTran, re
estimates store re1

*MODELO DE EFECTOS ALEATORIOS
xtreg lnIDH lnHA lnTran, fe
xtreg lnIDH lnTran, fe
estimates store fe1

*TEST DE HAUSMAN TEST, para elegir que tipo de modelo usar. 
hausman fe1 re1
*Ho: Usar EFECTOS ALEATORIOS ( > .05). el efecto inobservable no esta correlacionado con las variables explicativas
*H1: Usar EFECTOS FIJOS ( < .05)
*Conclusión: USAR MODELO DE DATA PANEL DE EFECTOS ALEATORIOS

  
*AUTOCORRELACION
****************
*instalar
findit xtserial
*Regresión
xtreg lnIDH lnTran, re
*Test de Woldridge para determinar la autocorrelación
xtserial lnIDH lnTran, output
*Ho: No Existe autocorrelacion de primer orden (>.05)
*H1: Existe autocorrelacion (< .05)
*CONCLUSIÓN: Existe autocorrelación

*HETEROCEDASTICIDAD
*instalar 
findit xttest3
*Regresión
xtreg lnIDH lnHA lnTran, fe
*Test de Wald para determinar la heterocedasticidad
xttest3
*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*CONCLUSIÓN: Si existe heterocedasticidad
*****HETEROCEDASTICIDAD PRUEBAS ADICIONALES 


*CORRECCIÓN DEL MODELO 
//de un modelo con solo heteroscedasticidad//

xtpcse lnIDH lnHA lnTran, het
regress lnIDH lnHA lnTran
xtgls lnIDH lnTran, p(h)


*CORRECCIÓN DEL MODELO 
//de un modelo con autocorrelación y heterocedasticidad//
xtpcse lnIDH lnTran, het c(ar1)
xtgls lnIDH lnHA lnTran, p(h) c(ar1)
xtgls lnIDH lnHA lnTran,  c(ar1)


////////////////////////////////////////////////////////////////////////////////
////// ANALISIS GENERAL /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


**************************
***** MODELO GLOBAL ******
**************************

clear all
use dkenyi

cls
clear
use dkenyi

destring CIAPhogaresafiliados, generate(CIAP)
rename HogaresAfiliados HA
rename Hogaresabonados HaB
rename Transferencias Tran

generate lnIDH = ln(IDH)
generate lnTran = ln(Tran)
generate lnHA = ln(HA)
generate lnCIAP = ln(CIAP)
generate lnHaB= ln(HaB)

xtset Reg Año
*DETERMINAR SI USAR EL METODO DE MCO O DATA PANEL
*************************************************

*Instalar paquete 
findit xtcsd

*PRUEBA PARA DATA PANEL LARGA

*PRUEBA BREUSCH AND PAGAN LAGRANGIAN MULTIPLIER 
*Estimar la regresión
xtreg lnIDH lnHA lnCIAP lnHaB lnTran, re
*Utilizar la Prueba Breusch Pagan Lagrangian
xttest0
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR DATOS PANEL 
*Esta prueba solo se hace con efectos aleatorios


*PRUEBA PARA DATA PANEL CORTA 

*PESARAN'S TEST OF CROSS SECTIONAL INDEPENDENCE 
*Estimar la regresión
xtreg lnIDH lnHA lnCIAP lnTran, re
*Utilizar la Prueba
xtcsd, pesaran abs
*Ho: Usar MCO ( > .05)
*H1: Usar Panel de Datos ( < .05) Existe heterogeneidad no observada
*Conclusión: CONVIENE USAR PANEL DATA


* DETERMINAR EL QUE TIPO DE MODELO USAR
***************************************

*MODELO DE EFECTOS FIJOS
xtreg lnIDH lnHA lnCIAP lnTran, re
xtreg lnIDH lnTran, re
estimates store re1

*MODELO DE EFECTOS ALEATORIOS
xtreg lnIDH lnHA lnCIAP lnTran, fe
xtreg lnIDH lnTran, fe
estimates store fe1

*TEST DE HAUSMAN TEST, para elegir que tipo de modelo usar. 
hausman fe1 re1
*Ho: Usar EFECTOS ALEATORIOS ( > .05). el efecto inobservable no esta correlacionado con las variables explicativas
*H1: Usar EFECTOS FIJOS ( < .05)
*Conclusión: USAR MODELO DE DATA PANEL DE EFECTOS FIJOS

*AUTOCORRELACION
****************
*instalar
findit xtserial
*Regresión
xtreg lnIDH lnHA lnCIAP lnTran, fe
*Test de Woldridge para determinar la autocorrelación
xtserial lnIDH lnHA  lnCIAP lnTran, output
*Ho: No Existe autocorrelacion de primer orden (>.05)
*H1: Existe autocorrelacion (< .05)
*CONCLUSIÓN: Existe autocorrelación

*HETEROCEDASTICIDAD
*instalar 
findit xttest3
*Regresión
xtreg lnIDH lnHA lnCIAP lnTran, fe
*Test de Wald para determinar la heterocedasticidad
xttest3
*Ho: No Existe heterocedasticidad (>.05)
*H1: Existe heterocedasticidad (< .05)
*CONCLUSIÓN: Si existe heterocedasticidad
*****HETEROCEDASTICIDAD PRUEBAS ADICIONALES 


*CORRECCIÓN DEL MODELO 
//de un modelo con solo heteroscedasticidad//

xtpcse lnIDH lnHA lnHaB lnCIAP lnTran, het
xtgls lnIDH lnHA lnHaB lnCIAP lnTran, p(h)


*CORRECCIÓN DEL MODELO 
//de un modelo con autocorrelación y heterocedasticidad//
xtpcse lnIDH lnHA lnHaB lnCIAP lnTran, het c(ar1)
xtpcse lnIDH lnHA lnCIAP lnTran, het c(ar1)
xtgls lnIDH lnHA lnHaB lnCIAP lnTran, p(h) c(ar1)
xtgls lnIDH lnHA lnHaB lnCIAP lnTran,  c(ar1)

/////////////////////////////////////////////////////////////////
/// MAPAS POR REGIONES //////////////////////////////////////////
/////////////////////////////////////////////////////////////////

*Instalando paquetes 
ssc install spmap
ssc install shp2dta
ssc install mif2dta

*Generando gráficos
cls
use dkenyi, clear
*gen _ID= Reg

keep if Año == 2010 //// Se puede considerar otro año: 2008, 2013, 2018, 2019

save dkeyis, replace

use departamento, clear
use departamento_shp, clear

tw (sc _Y _X)

*Unir las bases de datos
*________________________
*savea gasto_dbr_dto

use departamento, clear //base departamentales
spset

*la unidad: Departamento
**# Bookmark #1

merge 1:1 _ID using dkeyis

spmap IDH using departamento_shp, id(_ID)  ///
clnumber(4) fcolor(BuRd) ocolor(none ..) ///
label(xcoord(_CX) ycoord(_CY) label(Región) size(1.5)) ///
title("IDH por regiones en el Perú", size(4)) ///
legstyle(3) ///
legend(ring(1) position(3)) ///
note("Fuente:  Portal Informativo Porgrama Juntos", size(2) position(7)) clbreaks (-3 -2 -1 0 1 2 3)






