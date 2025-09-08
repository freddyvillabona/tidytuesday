
library("dplyr")
library("ggplot2")
library("ggforce")

ancho <- 30
largo <- 100

capacidad=ancho * largo
radio=4
circulo=3.14*radio*radio
aforo=capacidad/circulo

aforo

h <- trunc((ancho/radio)/2)
h
l <- trunc((largo/radio)/2)   
l 

total = h*l
total

n <- seq(1:total)
df <- as.data.frame(n)

# Largo eje x
seq1 <- seq(1,largo, by=2)
seq1 <- seq1[1:l]
#x0 =as.numeric(rbind(seq1, seq1,seq1,seq1,seq1))
x0 <- as.numeric(mapply(rep, seq1, h))
x0


# Ancho eje y
seq2 <- seq(1,ancho, by=2)
seq2 <- seq2[1:h]
#y0 = c(seq2,seq2,seq2,seq2)
y0 = as.numeric(rep(seq2,l))


data <- df %>%
  mutate(r = radio) %>%
  mutate(x0 = x0*r) %>%
  mutate(y0 = y0*r)


ggplot() +
  geom_circle(aes(x0 = x0, 
                  y0 = y0, 
                  r = r, 
                  fill = r), 
              data = data) +
  scale_x_continuous(limits = c(0-radio,largo+radio)) +
  scale_y_continuous(limits = c(0-radio,ancho+radio)) +
  ggtitle("Plot (M)") +
  xlab("Large") +
  ylab("Width") +
  
  geom_hline(yintercept=ancho, linetype="dashed", 
             color = "red", size=2) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=2) +
  
  geom_vline(xintercept=largo, linetype="dashed", 
             color = "red", size=2) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "red", size=2)

#####



x <- list(foo = "", bar = 0)
x











A=rep(radio,radio)
B=rep(radio*3,radio)
C=rep(radio*5,radio)


  mutate(Y = c(rbind(A,B,C))) %>%
  mutate(x0 = c(A,B,C))



largo

####}






df <- data.frame(
  x0 = seq(1:9),
  y0 = 0,
  r = c(0)
)




data1 <- df %>%
  mutate(Y = c(rbind(A,B,C))) %>%
  mutate(X = c(A,B,C))

x0=c(A,B,C)

y0=c(rbind(A,B,C))

circles <- data.frame(
  x0 = x0,
  y0 = y0,
  r = c(radio)
)

ggplot() +
  geom_circle(aes(x0 = x0, 
                  y0 = y0, 
                  r = r, 
                  fill = r), 
              data = data1) +
  scale_x_continuous(limits = c(-3,21)) +
  scale_y_continuous(limits = c(-3,21))




