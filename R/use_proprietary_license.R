#' Attach a 'proprietary' license to a package
#'
#' This function creates a LICENSE file indicating that the package is not open
#'     source and not for citation, and attaches it to the current package.
#'
#' @export
use_proprietary_license <- function() {
	fileConn <- file("LICENSE")
	writeLines(
		c("Proprietary", "", "Do not use or cite without the permission of the author."),
		fileConn
	)
	close(fileConn)

	desc::desc_set("License", "file LICENSE")

	invisible(TRUE)
}
