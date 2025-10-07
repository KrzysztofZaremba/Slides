#library(renderthis)


#remotes::install_github("jhelvy/renderthis")
#remotes::install_github('rstudio/chromote')

#to_pdf("C_5_slides_a.html")

pagedown::chrome_print(input = "C_5_slides_a.html", output = "C_5_slides_a.pdf", timeout = 300)
