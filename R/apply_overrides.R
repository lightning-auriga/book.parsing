#' Apply manual reformatting
#'
#' This function accepts user-submitted input->output pairs
#' and uses pattern matching via `gsub` to convert instances of
#' 'input' to 'output' in all strings.
#'
#' @param data input character vector of data to be processed
#' @param filename name of file containing user-specified string replacements
#' @return character vector of the same length as input, with replacements applied
#' @export
apply.initial.overrides <- function(data, filename) {
	stopifnot(is.vector(data, mode = "character"))
	stopifnot(is.vector(filename, mode = "character"))
	stopifnot(file.exists(filename))
	replacements <- read.table(filename, header = FALSE, sep = "\t")
	result <- data
	for (i in seq_len(nrow(replacements))) {
		result <- gsub(replacements[i, 1], replacements[i, 2], result, perl = TRUE)
	}
	result
}

#' Apply arbitrary replacements overriding the results of NLP
#'
#' Especially when the total number of submissions is low or the number of unique
#' submissions is high, some things are just going to be incorrect. This method
#' allows arbitrary replacements to override the NLP corrections to the input.
#' The replacement file should be of format
#'
#' full.initial.entry	title	author	replacement.title	replacement.author
#'
#' Only fill in one of the three first columns; this will be the value that is
#' matched for replacement. Depending on which of the title and author (or both)
#' are specified, those same values will be replaced in the output.
#'
#' @param raw.input input character vector of initial submissions
#' @param title.data input character vector containing post-NLP assigned titles
#' @param author.data input character vector containing post-NLP assigned authors
#' @param filename input character vector containing filename of replacements
#' @return list with 'title' and 'author' entries with replacements applied
#' @export
apply.posthoc.overrides <- function(raw.input, title.data, author.data, filename) {
	stopifnot(is.vector(raw.input, mode = "character"))
	stopifnot(is.vector(title.data, mode = "character"))
	stopifnot(is.vector(author.data, mode = "character"))
	stopifnot(is.vector(filename, mode = "character") | is.na(filename))
	if (is.na(filename)) {
		list(title = title.data, author = author.data)
	} else {
		replacement.title <- title.data
		replacement.author <- author.data
		stopifnot(file.exists(filename))
		replacement.data <- read.table(filename, header = FALSE, sep = "\t")
		stopifnot(ncol(replacement.data) == 5)
		for (i in seq_len(nrow(replacement.data))) {
			if (!is.na(replacement.data[i, 1])) {
				replacement.title[raw.input == replacement.data[i, 1]] <- replacement.data[i, 4]
				replacement.author[raw.input == replacement.data[i, 1]] <- replacement.data[i, 5]
			} else if (!is.na(replacement.data[i, 2])) {
				replacement.title[replacement.title == replacement.data[i, 2]] <- replacement.data[i, 4]
			} else if (!is.na(replacement.data[i, 3])) {
				replacement.author[replacement.author == replacement.data[i, 3]] <- replacement.data[i, 5]
			}
		}
		list(title = replacement.title, author = replacement.author)
	}
}
