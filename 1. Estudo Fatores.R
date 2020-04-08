
library(knitr)
library(ggplot2)


Dados = read.csv("C:/Users/Pedro/Downloads/Dados.csv")

# (Análise Descritiva do experimento)

summary(Dados$Altura)

plot(Dados$Altura, xlab="Ordem", ylab="Altura", main="Medições ao Longo do Tempo")
> abline(a=median(Dados$Altura), b=0, col="red")


boxFermento = ggplot(Dados, aes(x = Fermento, y = Altura, fill=Fermento)) +
  geom_boxplot() +
  guides(fill=FALSE)
boxFermento

boxOperador = ggplot(Dados, aes(x = factor(Operador), y = Altura, fill=Operador)) +
  geom_boxplot() +
  xlab("Operador") +
  guides(fill=FALSE)
boxOperador

boxFarinha = ggplot(Dados, aes(x = Farinha, y = Altura, fill=Farinha)) +
  geom_boxplot() +
  xlab("Farinha") +
  guides(fill=FALSE)
boxFarinha

boxFarinhaFermento = ggplot(Dados, aes(x = Farinha, y = Altura, fill=Farinha)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(. ~ Fermento)
boxFarinhaFermento

ggplot(Dados, aes(x = Farinha, y = Altura, fill=factor(Operador))) +
  geom_point(aes(color=factor(Operador))) +
  facet_grid(. ~ Operador,labeller=label_both) +
  guides(fill=FALSE) +
  geom_line(aes(group=factor(Operador)),size=1,stat="summary",fun.y=mean) +
  scale_color_discrete("Operadores", labels = c("Operador 1", "Operador 2",
                                                "Operador 3")) + scale_fill_discrete(labels = c("Operador 1", "Operador 2",
                                                                                                "Operador 3"))

# (Ajustando o modelo linear e construindo a tabela ANOVA)

fit = lm(Altura ~ Farinha * Fermento + Operador , data=Dados)
anova = anova(fit)

kable(anova)

# (Teste de homocestidade)

bartlett.test(Dados$Altura, Dados$Farinha)
bartlett.test(Dados$Altura, Dados$Fermento)

# (Teste de Normalidade)

shapiro.test(fit$residuals)

ggplot(Dados, aes(y=Altura, x=Farinha)) +
  geom_line(aes(group=Fermento, col=Fermento), stat = "summary", fun.y=mean)

# Análise de Resíduo
mse =anova[3][5,1]

st.residuals=fit$residuals/sqrt(mse)

# Resíduos Padronizados e Gráfico Quantil-Quantil
qqnorm(fit$residuals, datax=TRUE, main="Gráfico de Probabilidade Normal", ylab="Residuos", xlab="Quantis Teoricos",las=1)
qqline(fit$residuals, datax=TRUE, col="red", main="Gráfico de Probabilidade Normal",ylab="Residuos", xlab="Quantis Teoricos")

# Gráfico dos Resíduos vs Valores Ajustados
plot(residuals(fit) ~ fitted(fit), pch=16, cex=1.5, las=1, 
     cex.main=1.3, cex.lab=1.3, cex.axis=1.3, xlab="Valores Ajustados",
     ylab="Resíduos", main="Resíduos vs Valores Ajustados")
abline(h=0, lty=3, lwd=2, col="darkgrey")


# Gráfico dos Resíduos vs Níveis do Fator filtro
stripchart(st.residuals ~ Fermento + Operador, data=Dados, method="stack",
           vertical=TRUE, jitter=0, xlab="", ylab="Resíduos Padronizados", 
           pch=16, cex=1.5, las=1,
           main="Resíduos por Tratamentos")
abline(h=0, lty=3, lwd=2, col="darkgrey")

# Gráfico dos Resíduos vs Níveis do Fator sinal
stripchart(st.residuals ~ Farinha + Operador, data=Dados, method="stack",
           vertical=TRUE, jitter=0, xlab="", ylab="Resíduos Padronizados", 
           pch=16, cex=1.5, las=1,
           main="Resíduos por Tratamentos")
abline(h=0, lty=3, lwd=2, col="darkgrey")



