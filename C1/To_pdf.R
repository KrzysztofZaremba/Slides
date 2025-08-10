library(renderthis)
#remotes::install_github('rstudio/chromote')
#to_pdf("C_1_slides.html")
pagedown::chrome_print(input = "C_1_slides.html", output = "C_1_slides.pdf", timeout = 9000)
