#remotes::install_github("jhelvy/renderthis", dependencies = TRUE)

#install.packages("chromote")
#install.packages("renderthis")

library(renderthis)
to_pdf("C_2_slides_c.html")
pagedown::chrome_print(input = "C_2_slides_c.html", output = "C_2_slides_c.pdf", timeout = 300)
# 
# usethis::git_sitrep()
# 
# 
# install.packages("usethis")
# library(usethis)
# usethis::git_sitrep()
# 
# usethis::browse_github_pat()
# 
# usethis::edit_r_environ()
