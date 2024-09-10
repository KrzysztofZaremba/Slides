library(renderthis)
#remotes::install_github('rstudio/chromote')


#library(renderthis)
#to_pdf("C_2_slides_c.html")
pagedown::chrome_print(input = "C_3_slides_b.html", output = "C_3_slides_b.pdf", timeout = 300)

#probability that standard normal is larger than 1
pnorm(1, lower.tail = FALSE)


p=0.5
pnorm(0.51, mean=p, sd=sqrt(p*p/1000), lower.tail = FALSE)
