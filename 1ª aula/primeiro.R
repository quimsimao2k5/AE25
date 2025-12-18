a<-100
b<-100
a+b

matriz<-matrix(c(22,20,20,20),2,2)
matriz

dados<-data.frame(nome=c("Quim","Vitor","Joao","Manel"),
                  signo=c("aquario","peixes","gemeos","carneiro"),
                  cidade=c("vieira","braga","fama","trofa"),
                  altura=c(1.84,1.72,1.80,1.80))
dados
str(dados)
summary(dados)
dim(dados)
dados[2,]
dados[,2]
dados[1,3]
dados[2:4,]
dados[2:3,]

write.csv(dados, "dadostp1.csv",row.names = FALSE)
saveRDS(dados,"dados.rds")
dados[dados$altura>1.80,]

library(dplyr)

dados %>% filter(altura>1.79)
dados %>% arrange(desc(altura))  

dados$pe<-c(44,42,40.5,44)

plot(dados$altura,dados$pe)

library(ggplot2)

ggplot(dados,aes(x=altura,y=pe))+geom_point()+labs(title="Dispersão altura vs tamanho do p",
                                                   x="Altura",y="Tamanho do p")
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_density_2d() +
  labs(title = "Dispersão wt vs mpg",
       x = "Peso (1000 lbs)", y = "Milhas/galão")
