#remotes::install_github("jhelvy/renderthis", dependencies = TRUE)

#install.packages("chromote")
#install.packages("renderthis")
#remotes::install_github('yihui/servr')


pagedown::chrome_print(input = "C_2_slides_d.html", output = "C_2_slides_d.pdf", timeout = 300)
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
gitcreds::gitcreds_set()
