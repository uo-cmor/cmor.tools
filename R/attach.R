pkgs <- c("formattr", "CMORprojects", "regtools", "CMORplots", "SF6Dvalues", "cea")

unloaded <- function() {
	search <- paste0("package:", pkgs)
	pkgs[!search %in% search()]
}

same_library <- function(pkg) {
	loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
	do.call(
		"library",
		list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
	)
}

attach_pkgs <- function() {
	to_load <- unloaded()
	if (length(to_load) != 0) {
		for (i in seq_along(to_load)) {
			conflicted::conflict_prefer_all(to_load[[i]], "cmor.tools", quiet = TRUE)
			same_library(to_load[[i]])
		}
	}

	invisible()
}
