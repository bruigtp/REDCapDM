.onAttach <- function(...) {
  attached <- REDCapDM_attach()
    inform_startup(REDCapDM_attach_message(attached))
}
