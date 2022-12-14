
mutation_types_mapping_so_to_maf <- function(){
  df <- utils::read.csv(
    file = system.file('so_2_maf_mapping.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
    )

  # Replace any blanks with NAs
  df[['MAF']] <- ifelse(nchar(df[['MAF']]) == 0, yes = NA, no = df[['MAF']])

  return(df)
}

# Exposed Functions -------------------------------------------------------

#' Dictionary of So terms
#'
#' @return valid SO terms (character)
#' @export
#'
#' @examples
#' mutation_types_so()
mutation_types_so <- function(){
  utils::read.csv(
    file = system.file('so_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )[['Terms']]
}

#' Dictionary of MAF terms
#'
#' @return valid MAF terms (character)
#' @export
#'
#' @examples
#' mutation_types_maf()
mutation_types_maf <- function(){
 utils::read.csv(
    file = system.file('maf_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )[['Terms']]
  #stats::na.omit(unique(mutation_types_mapping_so_to_maf()[['MAF']]))
}

#' Palettes: MAF
#'
#' @return named vector. Names are MAF terms. Values are colors
#' @export
#'
#' @examples
#' mutation_types_maf_palette()
mutation_types_maf_palette <- function(){
  df <- utils::read.csv(
    file = system.file('maf_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )
  pal <- df[['Palette']]
  names(pal) <- df[['Terms']]
  return(pal)
}

#' Palettes: SO
#'
#' @return named vector. Names are SO terms. Values are colors
#' @export
#'
#' @examples
#' mutation_types_so_palette()
mutation_types_so_palette <- function(){
   df <- utils::read.csv(
     file = system.file('so_terms.tsv', package = "mutationtypes"),
     header = TRUE,
     sep = "\t"
 )

 pal <- df[['Palette']]
 names(pal) <- df[['Terms']]
 return(pal)
}


so_terms_without_mapping <- function(){
  so2maf_df <- mutation_types_mapping_so_to_maf()
  stats::na.omit(unique(so2maf_df[['SO']][is.na(so2maf_df[['MAF']])]))
}

#' Convert SO Mutation Types to MAF
#'
#' @param so_mutation_types a vector of SO terms you want to convert to MAF variant classifications
#' @param verbose verbose (flag)
#' @return matched MAF variant classification terms (character)
#' @export
#'
#' @examples
#' mutation_types_convert_so_to_maf(c('INTRAGENIC', 'INTRAGENIC', 'intergenic_region'))
mutation_types_convert_so_to_maf <- function(so_mutation_types, verbose = TRUE){
  if(!is.character(so_mutation_types)) cli::cli_abort('so_mutation_types must be a character vector, not {class(so_mutation_types)}')

  so_mutation_types_uniq <- unique(so_mutation_types)

  if(verbose) cli::cli_h1('Validating Input')

  # Check input mutation types are valid so terms
  if(!all(so_mutation_types_uniq %in% mutation_types_so())){
    missing <- so_mutation_types_uniq[!so_mutation_types_uniq %in% mutation_types_so()]
    names(missing) <- rep('>', times = length(missing))
    cli::cli_abort(c(
      "The following are not valid so mutation classification terms: ",
      missing,'',
      '!'= "Please ensure all so_mutation_types are in {.code mutation_types_so()}"
      ))
  }
  if(verbose) cli::cli_alert_success('Supplied mutation types are valid so terms')

  so2maf_df <- mutation_types_mapping_so_to_maf()

  # Check all terms have a valid mapping partner
  terms_without_valid_mapping <- so_mutation_types_uniq[so_mutation_types_uniq %in% so_terms_without_mapping()]
  if(length(terms_without_valid_mapping) > 0){
   cli::cli_abort(
     "Could not find a valid mapping for term{?s}: [{.strong {terms_without_valid_mapping}}].
     This is typically because the term is too high-level to be unambiguously converted to a lower-level MAF term.
     Consider converting this term to a more specific mutation type before running it through this script.
     You will have to do this on your own")
  }
  if(verbose) cli::cli_alert_success('All input SO terms have valid mappings to MAF terms')


  # Map input across
  so2maf_df[['MAF']][match(so_mutation_types, so2maf_df[['SO']])]
}


#' Identify Mutation Dictionary Used
#'
#' @param mutation_types mutation types to test (character)
#' @param verbose verbose (flag)
#'
#' @return one of c('SO', 'MAF', 'UNKNOWN').
#' Will return 'UNKNOWN' unless ALL mutation types fit with one of the supported dictionaries
#' @export
#'
#' @examples
#' mutation_types_identify(c('bob', 'billy', 'missense_variant'))
mutation_types_identify <- function(mutation_types, verbose = TRUE){

  # assertions
  if(!is.character(mutation_types)) cli::cli_abort('mutation_types must be a character vector, not {class(mutation_types)}')
  if(!(is.logical(verbose) & length(verbose) == 1)) cli::cli_abort('{.arg verbose} must be a flag, not a {class(verbose)}')


  # Count unique mutation types
  uniq_mutation_types <- unique(mutation_types)
  n_uniq_mutation_types = length(uniq_mutation_types)

  so_classified_mutations <- uniq_mutation_types[uniq_mutation_types %in% mutation_types_so()]
  n_so_classified_mutations <- length(so_classified_mutations)

  maf_classified_mutations <- uniq_mutation_types[uniq_mutation_types %in% mutation_types_maf()]
  n_maf_classified_mutations <- length(maf_classified_mutations)

  unclassified_mutation_types <- uniq_mutation_types[! uniq_mutation_types %in% c(so_classified_mutations, maf_classified_mutations)]

  if(verbose){
    cli::cli_h1('Identify Class')
    cli::cli_alert_info('Found {n_uniq_mutation_types} unique mutation type{?s} in input set')
    cli::cli_alert_info('{n_so_classified_mutations}/{n_uniq_mutation_types} mutation types were valid {.strong SO} terms')
    cli::cli_alert_info('{n_maf_classified_mutations}/{n_uniq_mutation_types} mutation types were valid {.strong MAF} terms')

    if(n_maf_classified_mutations > 0 & n_maf_classified_mutations < n_uniq_mutation_types){
      cli::cli_h1('MAF Mutation Types')
      cli::cli_text("{maf_classified_mutations}")
    }

    if(n_so_classified_mutations > 0 & n_so_classified_mutations < n_uniq_mutation_types){
      cli::cli_h1('SO Mutation Types')
      cli::cli_alert("{so_classified_mutations}")
    }

    if(length(unclassified_mutation_types) > 0 & length(unclassified_mutation_types) < n_uniq_mutation_types){
      cli::cli_h1('Unknown Mutation Types')
      cli::cli_alert('{unclassified_mutation_types}')
    }


  }

  if(n_so_classified_mutations == n_uniq_mutation_types) return('SO')
  else if(n_maf_classified_mutations == n_uniq_mutation_types) return('MAF')
  else return('UNKNOWN')
}


