.onAttach <- function(...) {
	needed <- pkgs[!is_attached(pkgs)]
	if (length(needed) == 0)
		return()

	attach_pkgs()
}

is_attached <- function(x) {
	paste0("package:", x) %in% search()
}
