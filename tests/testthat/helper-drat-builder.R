git_setup_config <- function() {
  message("Setting config")
  call_git(c("config", "--global", "user.email", "rich.fitzjohn@gmail.com"))
  call_git(c("config", "--global", "user.name", "Rich FitzJohn"))
}

if (identical(Sys.getenv("TRAVIS"), "true")) {
  git_setup_config()
}
