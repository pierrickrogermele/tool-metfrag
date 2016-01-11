source(file.path(dirname(SCRIPT.PATH), '..', 'r-biodb', 'ChemSpiderConn.R'), chdir = TRUE)
source(file.path(dirname(SCRIPT.PATH), '..', 'r-biodb', 'PubchemConn.R'), chdir = TRUE)

#############
# CONSTANTS #
#############

CHEMSPIDER <- 'chemspider'
PUBCHEM <- 'pubchem'
DBS <- c(CHEMSPIDER, PUBCHEM)

###############
# RUN METFRAG #
###############

run.metfrag <- function(db, ms.mode, precursor.ionized.mass, relative.mass.deviation, formula, spectrum.file, output.param.file, output.file) {
	
	# Run metfrag
	cmd  <- c(file.path(dirname(SCRIPT.PATH), '..', 'tool-metfrag', 'metfrag'),	'--spectrum', spectrum.file, '--mode', ms.mode, '--db', db, '-k', '744de16c-481a-489d-8803-48e9d35e7440', '--precursor-ionized-mass', precursor.ionized.mass, '-o', output.file, '--output-param-file', output.param.file, '--no-filter')
	cmd <- c(cmd, if (is.null(formula)) c('--relative-mass-deviation', relative.mass.deviation) else c('--precursor-formula', formula))
	system(paste(cmd, collapse = ' '))

	# Get scored candidates
	results <- read.table(output.file, sep='|', header = TRUE, comment.char = "", stringsAsFactors = FALSE, quote = "")

	return(results)
}
