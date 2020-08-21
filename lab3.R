## Datos
d <- read.table(file = '~/R/ADII-3332/datasets/datalab2.txt', header = TRUE)
head(d)
str(d)

## Modelo RLM
modelo <- lm(y ~ x1 + x2, data = d)
summary(modelo)

## Intervalos de Confianza 95% para los betas
result <- summary(modelo)
all <- coefficients(result)
betahat <- all[,1]
Sbeta <- all[,2]
ll <- betahat - 1.95*Sbeta
ul <- betahat + 1.95*Sbeta
IC <- cbind(ll,betahat,ul)
IC

## Validación de supuestos
r <- rstudent(modelo)

# Media cero
t.test(r, mu = 0)

# Normalidad (Shapiro - Wilks) y Varianza Constante (Gráficas)

par(mfrow = c(2,2), mai = c(0.65,0.65,0.65,0.65)) #Establecer disposición de ventana de gráficos y márgenes

# Gráfica QQ y Shapiro - Wilks
qqnorm(r, las = 1, main = '')
mtext('QQ plot', side = 3, line = 0.2)
qqline(r, lty = 2, col = 2)
legend('topleft', paste0('p = ', round(shapiro.test(r)$p.value, 3)), bty = 'n')

# Varianza constante
plot(d$x1, r, las = 1, ylab = '', xlab = 'x1')
mtext('r', side = 2, line = 2.5, las = 1)
abline(h = 0, lty = 2, col = 2)
mtext('x1 vs. r', line = .2, las = 1, side = 3)

plot(d$x2, r, las = 1, ylab = '', xlab = 'x2')
mtext('r', side = 2, line = 2.5, las = 1)
abline(h = 0, lty = 2, col = 2)
mtext('x2 vs. r', line = .2, las = 1, side = 3)

plot(fitted(modelo), r, las = 1, ylab = '', xlab = expression(hat(y)))
mtext('r', side = 2, line = 2.5, las = 1)
abline(h = 0, lty = 2, col = 2)
mtext(expression(hat(y)*' vs. r'), line = .2, las = 1, side = 3)

# Varianza constante con Breusch - Pagan

car::ncvTest(modelo)

# Independencia usando ACF

par(mfrow = c(1,1))
acf(r, las = 1, lag.max = 15, main = '')
mtext('ACF', las = 1, side = 3, line = 0.2)

# Independencia usando Durbin - Watson

car::durbinWatsonTest(modelo)

# Global Significance Test usando gvlma

if(!require(gvlma)) install.packages('gvlma')
require('gvlma')
library(gvlma)

gvmodel <- gvlma(modelo)
summary(gvmodel)

## Identificación de observaciones influenciales
par(mai = c(1.2,1,0.5,0.5)) # Estableciendo los márgenes
plot(modelo, which = 4, las = 1)  # MUY IMPORTANTE: El 'which' determina el tipo de gráfica
                                  # 1: A plot of residuals against fitted values
                                  # 2: A normal Q-Q plot
                                  # 3: A Scale-Location plot of sqrt(| residuals |) against fitted values
                                  # 4: A plot of Cook's distances versus row labels
                                  # 5: A plot of residuals against leverages (distancia de una observación respecto a las demás)
                                  # 6: A plot of Cook's distances against leverage/(1-leverage)
                                  # By default, the first three and 5 are provided.
                                  # Check ?plot.lm in r for more details.

## Intervalos de confianza para E[Y|x1,x2] con x1 = 12 y x2 = 50

predict(modelo, newdata = data.frame(x1 = 12, x2 = 50), interval = 'confidence')

## Intervalos de predicción para E[Y|x1,x2] con x1 = 12 y x2 = 50

predict(modelo, newdata = data.frame(x1 = 12, x2 = 50), interval = 'prediction')
