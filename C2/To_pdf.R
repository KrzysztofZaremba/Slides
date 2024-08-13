remotes::install_github("jhelvy/renderthis", dependencies = TRUE)

install.packages("remotes")
library(renderthis)
remotes::install_github('rstudio/chromote')
to_pdf("C_2_slides_a.html")

?to_pdf
Sys.getenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_PAT")
