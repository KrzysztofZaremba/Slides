library(renderthis)
#remotes::install_github('rstudio/chromote')
to_pdf("C_3_slides_pdf.html")


#library(renderthis)
#to_pdf("C_2_slides_c.html")
pagedown::chrome_print(input = "C_3_slides_a.html", output = "C_3_slides_a.pdf", timeout = 300)
