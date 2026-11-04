# --------------------------------------------------------------
# All credits for this functionality go to the Tidyverse package
# GitHub: https://github.com/tidyverse
# --------------------------------------------------------------

.onAttach <- function(...) {
  attached <- REDCapDM_attach()
  inform_startup(REDCapDM_attach_message(attached))
}
