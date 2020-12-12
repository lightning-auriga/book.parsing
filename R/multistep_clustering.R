#' Utility function to adjust Levenshtein distance to
#' match affinity propagation's assumed similarity metric.
#'
#' 'apclust' assumes the provided distance metric will be a representation
#' of how similar pairs of values are, whereas 'adist' represents how different
#' two strings are. This is easily adjusted by negating the 'adist' results,
#' as 'apclust' is scale free. Note that this distance calculation ignores case
#' on input queries, which is not ideal but seems to be the most functional solution
#' for the types of input provided in the training data.
#'
#' Note also that this could easily be a traditional lambda function, but in this case
#' making the intention of the function clear is preferable.
#'
#' @param x character vector, input values to be clustered
#' @return Levenshtein distance matrix with negated values, ready for 'apclust'
wrap.dist.mat <- function(x) {
	-1 * adist(x, ignore.case = TRUE)
}

#' Get clusters and cluster labels for a given dataset
#'
#' This function runs a series of clustering passes to attempt to get clean consensus labels
#' for similar queries with minor differences. The process is pretty convoluted and may need to
#' be adapted for larger sample sizes when those become available. For the moment, the function
#' does an initial hierarchical clustering pass on case-insensitive Levenshtein distance,
#' and then does an affinity propagation and agglomeration pass to attempt to get consensus labels
#' for the clusters from the initial hierarchical pass. This is more complicated because the small
#' cluster sizes in training data cause convergence issues in affinity propagation clustering, so
#' multiple passes through other methods are used as a workaround. It's also pretty slow, all things
#' considered, given the small sample size in question.
#'
#' @param df data frame containing aggregated results, and to which clustering results will be appended
#' @param query character vector, data to be clustered, of length nrow(df) matching df contents
#' @param tag character vector, identifier for this run that will be used in column names
#'  in output
#' @param hclust.h parameter passed to 'cutree' representing height of tree at which
#'  to cut and call clusters
#' @return data frame, the input with appended columns 'tag'.search.input
#'  and 'tag'.consensus.label reflecting the input and output of this function execution
run.multistep.clustering <- function(df, query, tag, hclust.h) {
	out.df <- df
	out.df[,paste(tag, ".search.input", sep="")] <- query
	## perform distance matrix calculation and clustering
	dist.mat <- adist(query, ignore.case = TRUE)
	## assign rownames for pretty printing later
	rownames(dist.mat) <- query
	## perform hierarchical clustering
	hc <- hclust(as.dist(dist.mat))
	## estimate optimal clustering configuration
	res <- cutree(hc, h = hclust.h)
	## now for the slightly bananas part. pull in affinity propagation to get representative labels
	out.df[,paste(tag, ".consensus.label", sep="")] <- NA
	for (i in unique(res)) {
		ap.input <- names(res[res == i])
		names(ap.input) <- ap.input
		## for reasons, this library doesn't like small datasets, so... make input data proportionally larger lol
		ap.input <- rep(ap.input, 5)
		clust.name <- NA
		if (length(ap.input) > 1) {
			suppressWarnings(ap.res <- apclust::apcluster(book_parsing::wrap.dist.mat, ap.input, q = 0, details = FALSE))
			if (length(ap.res@clusters) == 1) {
				clust.name <- ap.input[ap.res@exemplars[1]]
			} else {
				agg.res <- apclust::aggExCluster(x = ap.res)
				cutree.res <- cutree(agg.res, k = 1)
				clust.name <- ap.input[cutree.res@exemplars[1]]
			}
			out.df[,paste(tag, ".consensus.label", sep="")][query %in% names(unlist(ap.res@clusters))] <- clust.name
		} else {
			clust.name <- ap.input[1]
			out.df[,paste(tag, ".consensus.label", sep="")][query %in% clust.name] <- clust.name
		}
	}
	out.df
}
