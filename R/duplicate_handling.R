#' Remove rows with identical entries
#'
#' There are some example instances of duplicated rows with
#' mysteriously identical entries, including strange replicated
#' typographical idiosyncracies. These can optionally be removed,
#' and their removal is reported to a logging file.
#'
#' @param df data.frame, raw data representation with no processing
#' @param output.prefix character vector, prefix of output logging file
#' @return data.frame, input df with duplicate rows removed (leaving first instance)
handle.duplicates <- function(df, output.prefix) {
	stopifnot(is.data.frame(df))
	stopifnot(is.vector(output.prefix, mode = "character"))
	res <- df
	output.filename <- paste(output.prefix, ".duplicate-log.tsv", sep = "")
	remove.rows <- duplicated(res[,-1], fromLast = FALSE)
	if (length(which(remove.rows)) > 0) {
		write.table(unique(res[remove.rows | duplicated(res[,-1], fromLast = TRUE), ]),
					output.filename,
					row.names = FALSE,
					col.names = TRUE,
					quote = FALSE,
					sep = "\t")
		res <- res[!remove.rows, ]
	}
	print(paste("    removed ", length(which(remove.rows)), " duplicate rows", sep = ""))
	if (length(which(remove.rows)) > 0) {
		print(paste("    (see file '", output.filename, "' for examples of the duplicated rows)", sep = ""))
	}
	res
}
