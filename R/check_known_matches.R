#' Take a file of known matches and use it to QC the
#' proposed results
#'
#' If the user has a curated set of results, specifically the
#' results of the initial training pass, this function will take
#' those curated data and use them to conduct regression testing,
#' more or less. This is complicated by the lack of deterministic
#' behavior in this package. Nevertheless, certainly any regression
#' is worth flagging, even if it's so that the relevant content can
#' be added to the postprocessing overrides.
#'
#' @param df data.frame, final results from a run of `parse_raw_data`
#' @param filename character vector, file of known matches between input and output, or NA
#' @export
check.known.matches <- function(df, filename) {
	if (is.na(filename)) return
	stopifnot(is.vector(filename, mode = "character"))
	stopifnot(file.exists(filename))
	stopifnot(is.data.frame(df))
	stopifnot(length(which(colnames(df) == "raw.string")) == 1)
	stopifnot(length(which(colnames(df) == "final.title")) == 1)
	stopifnot(length(which(colnames(df) == "final.author")) == 1)
	known.data <- read.table(filename, header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
	stopifnot(length(which(colnames(df) == "raw.string")) == 1)
	stopifnot(length(which(colnames(df) == "final.title")) == 1)
	stopifnot(length(which(colnames(df) == "final.author")) == 1)
	## extract out just the relevant columns, in case there are more, and fix their order
	known.data <- known.data[, c("raw.string", "final.title", "final.author")]
	## make sure that, while there can be duplicates, there should be no
	## instances of a single key having multiple values
	stopifnot(nrow(unique(known.data)) == nrow(unique(known.data[, "raw.string"])))
	known.data <- unique(known.data)
	found.errors <- FALSE
	for (i in seq_len(nrow(known.data))) {
		test.data <- df[df[, "raw.string"] == known.data[i, "raw.string"], ]
		if (nrow(test.data) > 0) {
			if (length(which(test.data[, "final.title"] == known.data[i, "final.title"])) != nrow(test.data)) {
				print(paste("known data error (title): mapping '", known.data[i, "raw.string"], "' -> '", known.data[i, "final.title"], "' fails: {'", paste(unique(test.data[, "final.title"]), collapse = "' '"), "'}", sep = ""))
				found.errors <- TRUE
			}
			if (length(which(test.data[, "final.author"] == known.data[i, "final.author"])) != nrow(test.data)) {
				print(paste("known data error (author): mapping '", known.data[i, "raw.string"], "' -> '", known.data[i, "final.author"], "' fails: {'", paste(unique(test.data[, "final.author"]), collapse = "' '"), "'}", sep = ""))
				found.errors <- TRUE
			}
		}
	}
	if (found.errors) {
		stop("check.known.matches: testing failed, terminating")
	} else {
		print("    all data match the known cases, you lucky goose!")
	}
}
