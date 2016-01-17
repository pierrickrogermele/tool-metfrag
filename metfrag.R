source(file.path('..', 'r-lib', 'htmlhlp.R'), chdir = TRUE)
source(file.path('..', 'r-biodb', 'ChemSpiderConn.R'), chdir = TRUE)
source(file.path('..', 'r-biodb', 'PubchemConn.R'), chdir = TRUE)

#############
# CONSTANTS #
#############

CURRENT.FILE.PATH <- parent.frame(2)$ofile

CHEMSPIDER <- 'chemspider'
PUBCHEM <- 'pubchem'
DBS <- c(CHEMSPIDER, PUBCHEM)

###############
# RUN METFRAG #
###############

run.metfrag <- function(db, ms.mode, precursor.ionized.mass, relative.mass.deviation, formula, spectrum.file, output.param.file, output.file, html.output.file) {
	
	# Run metfrag
	cmd  <- c(file.path(dirname(CURRENT.FILE.PATH), 'metfrag'),	'--spectrum', spectrum.file, '--mode', ms.mode, '--db', db, '-k', '744de16c-481a-489d-8803-48e9d35e7440', '--precursor-ionized-mass', precursor.ionized.mass, '-o', output.file, '--output-param-file', output.param.file, '--no-filter')
	cmd <- c(cmd, if (is.null(formula)) c('--relative-mass-deviation', relative.mass.deviation) else c('--precursor-formula', formula))
	system(paste(cmd, collapse = ' '))

	# Get scored candidates
	results <- read.table(output.file, sep='|', header = TRUE, comment.char = "", stringsAsFactors = FALSE, quote = "")

	return(results)
}

#############################
# WRITE METFRAG HTML OUTPUT #
#############################

write.metfrag.html.output <- function(results, db, file, useragent, title = NULL, highlighted.lines = NULL) {

	# Set molecule name when absent
	if (db == PUBCHEM) {
		conn <- pubchem = PubchemConn$new(useragent = useragent)
		noname <- is.na(results[['IUPACName']])
		results[noname, 'IUPACName'] <- vapply(results[noname, 'Identifier'], function(id) { e <- conn$getEntry(id) ; if (is.null(e)) NA_character_ else e$getName() }, FUN.VALUE = '')
	}

	# Add Structure column and set inside a link to molecule image
	results[['Structure']] <- vapply(results[['Identifier']], function(id) paste0('<img src="', switch(db, pubchem = get.pubchem.image.url(id), chemspider = get.chemspider.image.url(id)), '" width="150" height="150"/>'), FUN.VALUE = '')

	# Add links to db page
	results[['Identifier']] <- vapply(results[['Identifier']], function(id) paste0('<a href="', switch(db, pubchem = get.pubchem.entry.url(id), chemspider = get.chemspider.entry.url(id)), '">', id, '</a>'), FUN.VALUE = '')

	# Write HTML
	html <- HtmlWriter(file = file)
	html$writeBegTag('html')
	html$writeBegTag('header')
	html$writeTag('title', text = title)
	html$writeBegTag('style')
	html$write('table, th, td { border-collapse: collapse; }')
	html$write('table, th { border: 1px solid black; }')
	html$write('td { border-left: 1px solid black; border-right: 1px solid black; }')
	html$write('th, td { padding: 5px; }')
	html$write('th { background-color: LightBlue; }')
	html$write('tr:nth-child(even) { background-color: LemonChiffon; }')
	html$write('tr:nth-child(odd) { background-color: LightGreen; }')
	for (n in highlighted.lines)
		html$write(paste0('tr:nth-child(', n + 1, ') { background-color: Tomato; }'))
	html$writeEndTag('style')
	html$writeEndTag('header')
	html$writeBegTag('body')
	html$writeTag('h1', text = title)
	html$writeTable(results)
	html$writeEndTag('body')
	html$writeEndTag('html')
}
