library(usethis)
system("git config --global user.name 'RobertMcGuinn'")
system("git config --global user.email 'robert.mcguinn@noaa.gov'")

use_git()
use_github()

usethis::github_token()

use_sitrep()
sitrep()
usethis::sitrep()
usethis::proj_sitrep()

