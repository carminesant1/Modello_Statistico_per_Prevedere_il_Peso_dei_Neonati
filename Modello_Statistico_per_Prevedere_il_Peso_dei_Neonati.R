#1) Importa il dataset "neonati.csv" e controlla che sia stato letto correttamente dal software
setwd("C:/Users/csant/Desktop/ProfessionAI/Statistica Inferenziale")
df <- read.csv("neonati.csv")
View(df)

###EDA###

#2) Descrivi il dataset, la sua composizione, il tipo di variabili e l'obbiettivo dello studio
sum(is.na.data.frame(df))
  #Non ci sono NA
str(df)
  #il dataset è composto da 2500 osservazioni e 10 variabili. Ci sono 6 variabili quantitative... 
  #...(Anni.madre, N.gravidanze, Gestazione, Peso, Lunghezza e Cranio) e 4 variabili qualitative...
  #...di cui 1 già codificata (Fumatrici) e altre sotto forma di stringa (Tipo.parto, Ospedale, Sesso)....
  #...L 'obiettivo dello studio è prevedere il peso del nascituro, isolando l'effetto di alcuni comportamenti assunti dalla madre durante il periodo di gravidanza.

#3) Indaga le variabili effettuando una breve analisi descrittiva, utilizzando indici e strumenti grafici che conosci
#Grafici
library(ggplot2)

cont_var <- c("Peso", "Lunghezza", "Cranio", "Anni.madre")
lista_grafici_cont <- list()
for (colonna in cont_var){
  grafico <- ggplot(data=df)+
    geom_histogram(aes(x=.data[[colonna]]), #non utilizzare stats="count"
                   fill = "white", 
                   color = "black",
                   bins = 20) +
    labs(title= paste("Istogramma di", colonna),
         x=colonna,
         y="Freq. Ass.")+
    theme_light()+
    geom_vline(xintercept=mean(df[[colonna]]),color="red")+
    annotate("text",label=paste("Media", colonna, "=", round(mean(df[[colonna]]),2)), x = min(df[[colonna]]), y = -10, color="red")
  lista_grafici_cont[[colonna]] <- grafico
}
for (grafico in lista_grafici_cont) {
  print(grafico)
}
  #ci sono valori anomali in Anni.madre
del_oss <- which(df$Anni.madre<10)
del_oss
df<-df[-del_oss,]

#df[["Anni.madre"]]==df$Anni.madre#
dis_var <- c("N.gravidanze", "Gestazione")
lista_grafici_dis <- list()
for (colonna in dis_var) {
  tab_freq <- table(df[[colonna]])
  modalità <- as.numeric(rownames(tab_freq))
  frq <- c()
  for (i in seq(length(tab_freq))){ 
    frq <- c(frq, tab_freq[[i]])
  }
  prc <- round(frq/sum(frq),3)
  df_bar = data.frame(modalità, prc)
  grafico <- ggplot(data = df_bar) +
    geom_bar(aes(x = modalità, y=prc),
             stat="identity",
             fill="white",
             color="black") +
    labs(title = paste("Barplot di", colonna),
         x = colonna,
         y = "Freq. Rel.") +
    theme_light()+
    annotate("text", x = min(modalità)+1, label = paste("Media di", colonna, "=", round(mean(df[[colonna]]),2)), y=-0.01, color="red")
  
  lista_grafici_dis[[colonna]] <- grafico
}
for (grafico in lista_grafici_dis) {
  print(grafico)
}
  
var_qual <- c("Tipo.parto", "Ospedale", "Sesso", "Fumatrici")
df$Fumatrici <- ifelse(df$Fumatrici==0, "No Fum.", "Fum")
lista_grafici_qual <-list()
for (colonna in var_qual) {
  tab_freq <- table(df[[colonna]])
  modalità <- rownames(tab_freq)
  frq <- c()
  for (i in seq(length(tab_freq))){ 
    frq <- c(frq, tab_freq[[i]])
  }
  prc <- round(frq/sum(frq),3)
  df_pie = data.frame(modalità, prc)
  grafico <- ggplot(df_pie, aes(x="", y=prc, fill=modalità))+
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    geom_text(aes(label = paste0(prc, "%")), position = position_stack(vjust=0.5))+
    labs(title=paste("Pie chart di", colonna))+
    theme_void()
  lista_grafici_qual[[colonna]] <- grafico
}

for (grafico in lista_grafici_qual) {
  print(grafico)
}


#Relazioni tra variabili
#1) Ospedale e tipo di parto
ggplot(data=df)+
  geom_bar(aes(x=Ospedale,
           fill=Tipo.parto),
           position = "fill")+
  labs(y="Percentuali")+
  theme_light()

cont_tabOT <-table(df$Tipo.parto, df$Ospedale)
chisq.test(cont_tabOT)
  #indipendenza, significa che non ci sono ospedali "specializzati" in un tipo di parto

#2.1) Tipo parto e peso
ggplot(data=df)+
  geom_boxplot(aes(x=Tipo.parto,
                   y=Peso))+
  theme_light()
t.test(Peso~Tipo.parto, data=df)

#2.2) Tipo parto e lunghezza 
ggplot(data=df)+
  geom_boxplot(aes(x=Tipo.parto,
                   y=Lunghezza))+
  theme_light()
t.test(Lunghezza~Tipo.parto, data=df)

#2.3) Tipo parto e diametro cranio
ggplot(data=df)+
  geom_boxplot(aes(x=Tipo.parto,
                   y=Cranio))+
  theme_light()
t.test(Cranio~Tipo.parto, data=df)

#2.4) Tipo parto e Fumatrici
ggplot(data=df)+
  geom_bar(aes(x=Tipo.parto,
               fill=Fumatrici),
           position = "fill")+
  labs(y="Percentuali")+
  theme_light()

cont_tabPF <-table(df$Tipo.parto, df$Fumatrici)
chisq.test(cont_tabPF)
  #indipendenza

#2.5) Tipo parto e gestazione
ggplot(data=df)+
  geom_boxplot(aes(x=Tipo.parto,
                   y=Gestazione))+
  theme_light()
t.test(Gestazione~Tipo.parto, data=df)
#Non c'è una differenza significativa

#3.1) Gestazione e anni madre
ggplot(data=df)+
  geom_point(aes(x=Gestazione,
                 y=Anni.madre))+
  theme_light()
cor(df$Anni.madre, df$Gestazione)

#3.2) Gestazione e fumo
ggplot(data=df)+
  geom_boxplot(aes(x=Fumatrici,
                   y=Gestazione))+
  theme_light()
t.test(Gestazione~Fumatrici, data=df)

#4.1) Peso e sesso
ggplot(data=df)+
  geom_boxplot(aes(x=Sesso,
                   y=Peso))+
  theme_light()
t.test(Peso~Sesso, data=df)
#differenza in media di 300 grammi

#4.2) Peso e fumo
ggplot(data=df)+
  geom_boxplot(aes(x=Fumatrici,
                   y=Peso))+
  theme_light()
t.test(Peso~Fumatrici, data=df)

#4.3) Peso e gravidanze
ggplot(data=df)+
  geom_point(aes(x=N.gravidanze,
                 y=Peso))+
  theme_light()
cor(df$Peso, df$N.gravidanze)

#5.1) Sesso e lunghezza
ggplot(data = df)+
  geom_boxplot(aes(x=Sesso,
                   y=Lunghezza))+
  theme_light()
t.test(Lunghezza~Sesso, data=df)

#5.2) Sesso e cranio
ggplot(data = df)+
  geom_boxplot(aes(x=Sesso,
                   y=Cranio))+
  theme_light()
t.test(Cranio~Sesso, data=df)

#Peso e lunghezza, confronto media popolazione
t.test(df$Peso, mu=3300)
  #non si rigetta l'ipotesi nulla
t.test(df$Lunghezza, mu=500)
  #sulla base delle evidenze empiriche si rigetta l'ipotesi nulla di uguaglianza della media...
  #...del campione con quella della popolazione
#fonte: https://www.ospedalebambinogesu.it/da-0-a-30-giorni-come-si-presenta-e-come-cresce-80012/#:~:text=In%20media%20il%20peso%20nascita,pari%20mediamente%20a%2050%20centimetri.

###REGRESSIONE###
#Encoding
df$Tipo.parto_ces <- ifelse(df$Tipo.parto=="Ces", 1, 0)
df$Sesso_m <- ifelse(df$Sesso=="M",1, 0)
df$Ospedale1 <- ifelse(df$Ospedale=="osp1", 1, 0)
df$Ospedale2 <- ifelse(df$Ospedale=="osp2", 1 ,0)
df$Fumatrici <- ifelse(df$Fumatrici=="Fum",1,0)
df$Tipo.parto<-NULL
df$Ospedale<-NULL
df$Sesso<-NULL

#Effetto del fumo
mod_fum <- lm(Peso~Fumatrici, data = df)
summary(mod_fum)
  #distorsione da variabile omessa, in questo caso ritengo che il coefficeinte sia sottostimato...
  #...(sovrastimato in valore assoluto), in quanto il coefficiente potrebbe includere la...
  #...variabilità di altri fattori
cor(df, df$Fumatrici) 
  #non ci sono variabili nel dataset correlate con "Fumatrici", pensiamo ad eventuali effetti interazione
  #penso che l'effetto del fumo sulla salute del nascituro possa dipendere dall'età della madre
mod_fum2 <- lm(Peso~Fumatrici*Anni.madre, data=df)
summary(mod_fum2)
  #modello non significativo, proviamo ad utilizzare S.E. robusti all'eteroschedasticità
library(estimatr)
mod_fum2_rob <- lm_robust(Peso~Fumatrici*Anni.madre, data=df)
summary(mod_fum2_rob)
  #possiamo concludere che, date le variabili contenute del dataset, non ci sono evidenze statistiche...
  #...sull'influenza del fumo sul peso del neonato. In un futuro lavoro potrei pensare di raccogliere...
  #...altre variabili relative al consumo di alcool e alimentazione per utilizzarle come variabili...
  #...di controllo

#Previsione peso neonato
mod_gen <- lm(Peso~., data=df)
summary(mod_gen)
  #consideriamo effetti non lineari:
library(gridExtra)
grafici <-c()
for (i in colnames(df)) {
  grafico<-ggplot(data=df)+
    geom_point(aes(x=.data[[i]],
                   y=.data[["Peso"]]))+
    theme_light()
  grafici[[i]] <- grafico
}
grid.arrange(grobs = grafici, ncol = 3)

mod_gen2 <- update(mod_gen,~.+Fumatrici:Anni.madre+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2))
summary(mod_gen2)
mod_gen3 <- update(mod_gen,~.+Fumatrici:Anni.madre+poly(Gestazione,2)+poly(Lunghezza,2)+poly(Cranio,2))
summary(mod_gen3)

library(MASS)

mod_gen_upd <- stepAIC(mod_gen, direction = "both", k=log(nrow(df))) 
summary(mod_gen_upd)

mod_gen2_upd <- stepAIC(mod_gen2, direction = "both", k=log(nrow(df))) 
summary(mod_gen2_upd)

mod_gen3_upd <- stepAIC(mod_gen3, direction = "both", k=log(nrow(df)))
summary(mod_gen3_upd)

BIC(mod_gen_upd, mod_gen2_upd, mod_gen3_upd)
  #Utilizzo mod_gen3_upd come modello di riferimento, perchè, nonostante abbia un valore BIC più...
  #...elevato, inlude i predittori di grado 1
mod_gen_def <- mod_gen3_upd  
summary(mod_gen_def)
#analisi multicollinnearità
library(car)
vif(mod_gen_def, type="predictor")
  #utilizzo l'argomento type="predictor" per annullare l'effetto dei polinomi...
  #...(ovviamente correlati con la variabile di grado 1) sul calcolo dei vif...
  #...VIF superiore a 10 per tre predittori: Gestazione, Lunghezza e Cranio
mod_gen_def <- update(mod_gen_def,~.-poly(Cranio,2)+Cranio)
summary(mod_gen_def)
vif(mod_gen_def, type = "predictor")
#diagnostica residui
plot(mod_gen_def)
  #1) Assunzione degli errori a media 0 rispettata, non visualizzo dei pattern
  #2) Assunzione distribuzione normale errori non perfettamente rispettata, problema nelle code...
  #...che presentano osservazioni che si discostano dalla bisettrice
  #3) Assunzione omoschedasticità rispettata, visualizzo una nube casuale di punti
  #4) Osservazione 1551 da monitorare

cooksD <- cooks.distance(mod_gen_upd)
influential <- cooksD[(cooksD > 0.5)]
names_of_influential <- names(influential)
influential

library(dplyr)
dfo <- df[names_of_influential,]
df <- df %>% anti_join(dfo)

  #dopo aver rimosso l'osservazione ripetiamo la procedura stepAIC
mod_gen <- lm(Peso~., data=df)
summary(mod_gen)
mod_gen2 <- update(mod_gen,~.+Fumatrici:Anni.madre+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2)+I(N.gravidanze^2)+I(N.gravidanze^3)+I(N.gravidanze^4))
summary(mod_gen2)
mod_gen3 <- update(mod_gen,~.+Fumatrici:Anni.madre+poly(Gestazione,2)+poly(Lunghezza,2)+poly(N.gravidanze,4))
summary(mod_gen3)

mod_gen2_upd <- stepAIC(mod_gen2, direction = "both", k=log(nrow(df))) 
summary(mod_gen2_upd)

mod_gen3_upd <- stepAIC(mod_gen3, direction = "both", k=log(nrow(df)))
summary(mod_gen3_upd)

BIC(mod_gen2_upd, mod_gen3_upd)

mod_gen_def <- mod_gen3_upd  
summary(mod_gen_def)

vif(mod_gen_def, type = "predictor")

plot(mod_gen_def)
  #1) Assunzione degli errori a media 0 rispettata, non visualizzo dei pattern
  #2) Assunzione distribuzione normale errori non perfettamente rispettata, problema nelle code...
  #...che presentano osservazioni che si discostano dalla bisettrice
  #3) Assunzione omoschedasticità rispettata, visualizzo una nube casuale di punti
  #4) Non ci sono osservazioni che cadono all'interno dell'area di Cook

  #valutiamo le assunzioni del modello attraverso i principali indici
library(lmtest)
res_mod <- residuals(mod_gen_def)
shapiro.test(res_mod)
  #i residui non seguono una distribuzione normale, da indagare ulteriormente
plot(density(res_mod))
  #dopo aver visualizzato il grafico ritengo che la non normalità della distribuzione dei...
  #...residui, evidenziata dai risultati dello Shapiro test, non influenzi significativamente
  #...il potere predittivo del modello
bptest(mod_gen_def)
  #non si rigetta l'ipotesi nulla di omoschedasticità settando un livello di significatività del 1%...
  #...leggero problema di eteroschedasticità che non credo vada ad inficiare sul potere predittivo...
  #...del modello
dwtest(mod_gen_def)
  #non si rigetta l'ipotesi nulla di auto-correlazione dei residui pari a 0

#Previsione
prd <- data.frame(Cranio = mean(df$Cranio[df$Sesso_m==0]), Sesso_m = 0, Gestazione = 39, Lunghezza = mean(df$Lunghezza[df$Sesso_m==0]), N.gravidanze = 2)
predict(mod_gen_def, newdata=prd)

#Grafici
ggplot(data = df)+
  geom_point(aes(x = Peso, y = predict(mod_gen_def)))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  labs(title = "Peso osservato vs Peso previsto",
       x = "Peso Osservato",
       y = "Peso Previsto")+
  xlim(0,5000)+
  ylim(0,5000)+
  theme_light()

