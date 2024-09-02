library(renderthis)
#remotes::install_github('rstudio/chromote')


#library(renderthis)
#to_pdf("C_2_slides_c.html")
pagedown::chrome_print(input = "C_3_slides_b.html", output = "C_3_slides_b.pdf", timeout = 300)
