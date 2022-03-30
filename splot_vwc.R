library(agricolae)
attach(WettingAgentData)
data = WettingAgentData

data$TRAFFIC <- as.factor(data$TRAFFIC)
data$TREATMENT <- as.factor(data$TREATMENT)
data$REP<- as.factor(data$REP)

model <- sp.plot(block = REP, 
                 pplot = TREATMENT, 
                 splot = TRAFFIC, 
                 Y = VWC)

?sp.plot
# Get first error df
Edf_a <- model$gl.a
Edf_a
# Get second error df
Edf_b <- model$gl.b
Edf_b
# Get first error MS
EMS_a <- model$Ea
EMS_a
# Get second error MS
EMS_b <- model$Eb
EMSout1<- with(data, LSD.test(VWC,TREATMENT,Edf_a,EMS_a,console = TRUE))
EMOUT2 <- with(data, LSD.test(VWC,TRAFFIC,Edf_a,EMS_a,console = TRUE))
EMOUT3 <- with(data, LSD.test(VWC,TRAFFIC:TREATMENT,Edf_a,EMS_a,console = TRUE))


plot(EMSout1, 
     xlab = "TRET",
     ylab = "VWC",
     las = 1, 
     variation = "IQR")
plot(EMOUT2, 
     xlab = "TRAFFIC ",
     ylab = "VWC",
     las = 1, 
     variation = "IQR")
plot(EMOUT3, 
     xlab = "TREATMENT:TRAFFIC",
     ylab = "VWC",
     las = 1, 
     variation = "IQR")


library(ggplot2)
ggplot(data,aes(x=TREATMENT,y=VWC))+ geom_bar(stat = "identity",aes(fill = TRAFFIC),position = "dodge")
