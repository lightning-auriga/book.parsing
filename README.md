# book.parsing
 
Read string variants of "book - author" combinations and parse them into their component parts.
Harmonize the values across multiple entries with the same value. Report the results in various
formats as needed.


## Installation

This repo is a formatted R library and can be installed with devtools::install_github:

`install.packages("devtools")`

`require(devtools)`

`devtools::install_github("cpalmer718/book.parsing")`

## Usage

First, load the relevant libary:

`require(book.parsing)`

The most basic usage is as follows:

`book.parsing::parse_raw_data("input.xlsx", "output_step1")

After the above step, please manually inspect the output `output_step1.tsv`
for any incorrect assignments or needed adjustments. All changes should be made to
the `final.title` and `final.author` columns. Any entries in the `final.title`
or `final.author` columns can be left as `NA`; such rows will be excluded
by `book.parsing::process.output`. When that is complete, use the following
command to postprocess and summarize the edited output file:

`book.parsing::process.output("output_step1.tsv", "output_step2", TRUE)`

The results will be stored in `output_step2.final-results.tsv`.

## Input Formats

### First round: `book.parsing::parse_raw_data`

For minimal usage, assume the user has a file `input.xlsx` structured as follows:
a first column that is ignored; then sets of three columns each of the header format
`Category name without dashes - Nomination #`. For rows, the header row is followed
by arbitrarily many rows with either valid entries or empty cells.

### Second round: `book.parsing::process.output`

This should generally be an output file from the above execution of `book.parsing::parse_raw_data`.
It is expected that there will need to be some user intervention to approach 100% accuracy; those
changes should be made to the `final.title` and `final.author` columns. `NA` entries in those
columns are treated as missing data and ignored.

## Output Formats

### Comment: Text Editing

For unfamiliar users, Excel spreadsheets can be rather cumbersome to work with for various reasons.
This code emits files in `.tsv` format: tab-separated values. This can be both read and emitted by various
programs. I highly recommend [Notepad++](https://notepad-plus-plus.org/) for Windows text editing; for Mac,
perhaps [Atom](https://atom.io/) would be a reasonable alternative. Linux users: what are you doing reading this,
[but also](https://www.gnu.org/software/emacs/).

### First round: `book.parsing::parse_raw_data`

The function `book.parsing::parse_raw_data` aggregates entries across categories
and reports them, along with their intermediate NLP results and final classification,
in a tabular format. That output format contains the following columns:

 - `raw.string`: the original entry encountered in the input file
 - `original.string`: the original entry, with any provided `preprocessing.overrides` replacements applied
 - `category`: which category the entry was initially found in
 - `submitter.comments`: in a few instances, freetext entry contained parenthetical additions, which are stored here, or `NA`
 - `predicted.title`: following expected `TITLE by AUTHOR` or `TITLE - AUTHOR` or `TITLE/AUTHOR` format: the predicted title
 - `predicted.author`: following expected `TITLE by AUTHOR` or `TITLE - AUTHOR` or `TITLE/AUTHOR` format: the predicted author
 - `combined.search.input`: the exact query used in combined Title/Author clustering; this is a mildly manipulated version of the original input
 - `combined.consensus.label`: the result of combined Title/Author clustering
 - `title.search.input`: the exact query used in Title clustering; when `predicted.title` is `NA`, this is the original query
 - `title.consensus.label`: the result of the Title clustering
 - `author.search.input`: the exact query used in Author clustering; this is just the predicted author, though clustering is run with `NA` entries removed
 - `author.consensus.label`: the result of the Author clustering
 - `final.title`: the final predicted title label for this entry, after NLP, standardized formatting, and `postprocessing.overrides` applied; or `NA`
 - `final.author`: the final predicted author label for this entry, after NLP, standardized formatting, and `postprocessing.overrides` applied; or `NA`
 - `final.message`: a `Success` or `Fail` message with some description of the result status; can be used to assess where the program is confident, or possibly needs improvement

### Second round: `book.parsing::process.output`

The function `book.parsing::process.output` aggregates harmonized data within categories
and reports them, such that the results within categories can be used for downstream purposes.
The tabular reporting format contains the following columns:
 - `Category`: the submission category in which the entry was found
 - `Vote`: the harmonized vote corresponding to the original free text entry or entries
 - `Title`: the Title component of the Vote; useful for sorting
 - `Author`: the Author component of the Vote; useful for sorting
 - `Count`: the number of times this Vote was encountered in this particular Category

## Version History

13 December 2020: v1.0.1, with improved userspace parameters in `parse_raw_data`, and a much-improved README.

12 December 2020: v1.0.0 release, with compatibility to training dataset.

12 December 2020: initial commit with minimal working solution.
