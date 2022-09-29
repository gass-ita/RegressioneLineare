fasceEta <- c(35, 40, 50, 60, 65)                           #x
mediaFasceEta <- mean(fasceEta)                             #x medio     

#varianza in x
sum <- sum((fasceEta - mediaFasceEta) * (fasceEta - mediaFasceEta))
varianzaFasceEta <- sum / (length(fasceEta) - 1)


#---------------------LAVORATORI DIPENDENTI---------------------------#
stipendioDipendenteAnnuo <- c(29489, 35231, 36341, 40766, 44606) #y

plot(fasceEta, stipendioDipendenteAnnuo, main = "Stipendio per fasce di età di lavoratori dipendenti")

mediaDipendente  <- mean(stipendioDipendenteAnnuo)               #y medio

#varianza in y 
sum <- sum((stipendioDipendenteAnnuo - mediaDipendente) * (stipendioDipendenteAnnuo - mediaDipendente))
varianzaDipendente <- sum / (length(stipendioDipendenteAnnuo) - 1)

#covarianza in x, y
sum <- sum((fasceEta - mediaFasceEta) * (stipendioDipendenteAnnuo - mediaDipendente)) 
covarianzaFasciaDipendente <- sum / (length(stipendioDipendenteAnnuo) - 1)

#parametri regressione lineare
m <- covarianzaFasciaDipendente / varianzaFasceEta
q <- mediaDipendente - m * mediaFasceEta

#calcolo il valore previsto di y per i valori x
ycap <- m * fasceEta + q

lines(fasceEta, ycap, col = "red")
#------------------------------------------------------------------------#


#-------------------------LAVORATORI AUTONOMI----------------------------#
stipendioAutonomoAnnuo = c(27271, 40991, 40061, 46867, 59687)

plot(fasceEta, stipendioAutonomoAnnuo, main = "Stipendio per fasce di età di lavoratori autonomi")

mediaAutonomo <- mean(stipendioAutonomoAnnuo)

#varianza in y 
sum <- sum((stipendioAutonomoAnnuo - mediaAutonomo) * (stipendioAutonomoAnnuo - mediaAutonomo))
varianzaAutonomo <- sum / (length(stipendioAutonomoAnnuo) - 1)

#covarianza in x, y
sum <- sum((fasceEta - mediaFasceEta) * (stipendioAutonomoAnnuo - mediaAutonomo)) 
covarianzaFasciaAutonomo <- sum / (length(stipendioAutonomoAnnuo) - 1)

#parametri regressione lineare
m <- covarianzaFasciaAutonomo / varianzaFasceEta
q <- mediaAutonomo - m * mediaFasceEta

#calcolo il valore previsto di y per i valori x
ycap <- m * fasceEta + q

lines(fasceEta, ycap, col = "red")
