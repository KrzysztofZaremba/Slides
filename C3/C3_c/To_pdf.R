#library(renderthis)
#options("install.lock"=FALSE)
options("install.lock"=FALSE)
remotes::install_github("jhelvy/renderthis", force=TRUE)
#remotes::install_github('rstudio/chromote')
to_pdf("C_3_slides_c.html")
pagedown::chrome_print(input = "C_3_slides_c.html", output = "C_3_slides_c.pdf", timeout = 9000)
install.packages("cli", type="source", INSTALL_opts = '--no-lock') 

