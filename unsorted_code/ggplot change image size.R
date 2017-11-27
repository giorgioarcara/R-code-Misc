library(ggplot2)

dat = data.frame(x=rnorm(10), y=rnorm(10))


prova = ggplot(dat, aes(x=x, y=y))+
  geom_point()

ggsave(filename="prova1.png", width = 1, height=1, scale=10, units = "cm", dpi=100)

ggsave(filename="prova2.png", width=1, height=1, scale=10, units="cm", dpi=200)
