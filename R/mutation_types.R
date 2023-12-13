

# Unexposed Data Loading Functions ----------------------------------------
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

mutation_types_mapping_pave_to_maf <- function(){
  df <- utils::read.csv(
    file = system.file('pave_2_maf_mapping.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )

  # Replace any blanks with NAs
  df[['MAF']] <- ifelse(nchar(df[['MAF']]) == 0, yes = NA, no = df[['MAF']])

  return(df)
}



# Data Loading Functions -------------------------------------------------------

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

mutation_types_so_with_priority <- function(){
  mutation_types_so_wp <- utils::read.csv(
    file = system.file('so_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )[c('Terms', "EffectPriority")]

  mutation_types_so_wp[order(mutation_types_so_wp[["EffectPriority"]], decreasing = FALSE), , drop=FALSE]
}

mutation_types_pave_with_priority <- function(){
  mutation_types_so_wp <- utils::read.csv(
    file = system.file('pave_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )[c('Terms', "EffectPriority")]

  mutation_types_so_wp[order(mutation_types_so_wp[["EffectPriority"]], decreasing = FALSE), , drop=FALSE]
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

#' Dictionary of PAVE terms
#'
#' PAVE is a newer annotation software that supports annotaiton of mainly just a subset of SO terms,
#' but with a couple of important additions to indicate when a non-obvious consequence can be found thanks to phasing.
#'
#' @return valid PAVE terms (character)
#' @export
#'
#' @examples
#' mutation_types_pave()
mutation_types_pave <- function(){
  utils::read.csv(
    file = system.file('pave_terms.tsv', package = "mutationtypes"),
    header = TRUE,
    sep = "\t"
  )[['Terms']]
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

#' Palettes: PAVE
#'
#' @return named vector. Names are PAVE terms. Values are colors
#' @export
#'
#' @examples
#' mutation_types_pave_palette()
mutation_types_pave_palette <- function(){
  df <- utils::read.csv(
    file = system.file('pave_terms.tsv', package = "mutationtypes"),
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


# Conversions -------------------------------------------------------------

#' Convert SO Mutation Types to MAF
#'
#' @param so_mutation_types a vector of SO terms you want to convert to MAF variant classifications (character)
#' @param split_on_ampersand should '&' separated SO terms be automatically converted to single SO terms based on highest severity? (flag)
#' @param inframe is the mutation inframe? (logical). Used to map protein_altering_variant to valid MAF columns
#' @param variant_type a vector describing each mutations type. Valid elements include: "SNP", "DNP", "TNP", "ONP", "DEL", "INS". Used to map frameshift_variant to more specific MAF columns (character)
#' @param missing_to_silent should missing (NA) or empty ('') mutation types be converted to 'Silent' mutations?
#' @param verbose verbose (flag)
#' @return matched MAF variant classification terms (character)
#' @export
#'
#' @examples
#' mutation_types_convert_so_to_maf(c('INTRAGENIC', 'INTRAGENIC', 'intergenic_region'))
mutation_types_convert_so_to_maf <- function(so_mutation_types, variant_type = NULL, inframe = NULL, split_on_ampersand = TRUE,  missing_to_silent = FALSE, verbose = TRUE){

  # Assertions
  assertions::assert_character(so_mutation_types)
  if(!missing_to_silent) assertions::assert_no_missing(so_mutation_types)

  if(!missing_to_silent)
    assertions::assert(
      all(nchar(so_mutation_types) > 0),
      msg = "Found {sum(nchar(so_mutation_types) == 0)} variant{?s} with no mutation type value (empty string). There are 2 possible solutions\f
      1. Either ensure all variants have a consequence OR\f
      2. If appropriate set {.arg missing_to_silent = TRUE} to assume all variants without consequences have no effect (these will be set to SILENT in the MAF)"
    )

  if(split_on_ampersand)
    so_mutation_types <- select_most_severe_consequence_so(so_mutation_types, missing_is_valid = missing_to_silent)

  so_mutation_types_uniq <- unique(so_mutation_types)

  if(verbose) cli::cli_h2('Validating Input')

  # Check input mutation types are valid so terms
  assert_all_mutations_are_valid_so(so_mutation_types_uniq, missing_is_valid = missing_to_silent)

  if(verbose) cli::cli_alert_success('Supplied mutation types are valid so terms')
  so2maf_df <- mutation_types_mapping_so_to_maf()

  valid_variant_types = c("SNP", "DNP", "TNP", "ONP", "DEL", "INS")
  if("frameshift_variant" %in% so_mutation_types){
    # Require just variant_type and inframe to be supplied
    assertions::assert(
      !is.null(variant_type),
      msg = "To convert SO term {.strong frameshift_variant} to a MAF variant classification, you must supply argument {.arg variant_type}"
    )

    # assert variant_type
    assertions::assert_character(variant_type)
    assertions::assert_equal(length(variant_type), length(so_mutation_types))
    assertions::assert(all(variant_type %in% valid_variant_types), msg = "Invalid {.arg variant_type} values found ({unique(variant_type[!variant_type %in% valid_variant_types])}). Valid terms include {valid_variant_types}")

    if(!is.null(inframe)){
     inframe_status_of_frameshift_variants <- inframe[so_mutation_types == "frameshift_variant"]
     assertions::assert(all(inframe_status_of_frameshift_variants == FALSE), msg = "Cannot have a frameshift_variant with inframe status: [{.strong {unique(inframe[so_mutation_types == 'frameshift_variant' & inframe])}}]. Must be FALSE Either the variant classification or the inframe status must be incorrect")
    }
    incorrect_variant_types <- unique(variant_type[!variant_type[so_mutation_types=="frameshift_variant"] %in% c("INS", "DEL")])
    assertions::assert(
      length(incorrect_variant_types) == 0,
      msg = "Variant Type must be {.strong INS} or {.strong DEL} when so_mutation_type is 'frameshift_variant'. Not [{incorrect_variant_types}]"
    )

  }
  if("protein_altering_variant" %in% so_mutation_types){
    # Require both variant_type and inframe to be supplied
    assertions::assert(!is.null(variant_type) & !is.null(inframe), msg = "To convert SO term {.strong protein_altering_variant} to a MAF variant classification, you must supply arguments {.arg variant_type} and {.arg inframe}")

    # assert inframe
    assertions::assert_logical(inframe)
    assertions::assert_equal(length(inframe), length(so_mutation_types))
    assertions::assert(all(unique(inframe[so_mutation_types=="protein_altering_variant"]) %in% c(TRUE, FALSE)))

    # assert variant_type
    assertions::assert_character(variant_type)
    assertions::assert_equal(length(variant_type), length(so_mutation_types))
    assertions::assert(all(variant_type %in% valid_variant_types), msg = "Invalid {.arg variant_type} values found ({unique(variant_type[!variant_type %in% valid_variant_types])}). Valid terms include {valid_variant_types}")
    assertions::assert(
      all(unique(variant_type[so_mutation_types=="protein_altering_variant"]) %in% c("INS", "DEL")),
      msg = "Variant Type must be {.strong INS} or {.strong DEL} when so_mutation_type is 'protein_altering_variant'"
    )
  }

  # Whether missing (empty string or NA_character) values should be set to 'Silent' of 'NA_character' in fcase statement below
  if(missing_to_silent == TRUE)
    maf_value_for_missing = "Silent"
  else
    maf_value_for_missing = NA_character_

  maf_mutation_types <- data.table::fcase(
    # Replace missing or empty SO mutation typesto Silent or NA depending on missing_as_silent argument
    so_mutation_types == "" | is.na(so_mutation_types), maf_value_for_missing,
    # For all PAVE classifications except frameshift convert to the equivalent MAF terms
    !so_mutation_types %in% c("frameshift_variant", "protein_altering_variant"), so2maf_df[['MAF']][match(so_mutation_types, so2maf_df[['SO']])],
    so_mutation_types == "frameshift_variant" & variant_type == "DEL", "Frame_Shift_Del",
    so_mutation_types == "frameshift_variant" & variant_type == "INS", "Frame_Shift_Ins",
    so_mutation_types == "protein_altering_variant" & variant_type == "DEL" & inframe == TRUE, "In_Frame_Del",
    so_mutation_types == "protein_altering_variant" & variant_type == "INS" & inframe == TRUE, "In_Frame_Ins",
    so_mutation_types == "protein_altering_variant" & variant_type == "DEL" & inframe == FALSE, "Frame_Shift_Del",
    so_mutation_types == "protein_altering_variant" & variant_type == "INS" & inframe == FALSE, "Frame_Shift_Ins",

    default=NA_character_
    )

  assertions::assert_no_missing(maf_mutation_types)
  return(maf_mutation_types)
}

#' Convert PAVE Mutation Types to MAF
#'
#' @param pave_mutation_types a vector of PAVE terms you want to convert to MAF variant classifications (character)
#' @param split_on_ampersand should '&' separated PAVE terms be automatically converted to single PAVE terms based on highest severity? (flag)
#' @param variant_type a vector describing each mutations type. Valid elements include: "SNP", "DNP", "TNP", "ONP", "DEL", "INS". Used to map frameshift_variant to more specific MAF columns (character)
#' @inheritParams mutation_types_convert_so_to_maf
#' @param verbose verbose (flag)
#' @return matched MAF variant classification terms (character)
#' @export
#'
#' @examples
#' mutation_types_convert_pave_to_maf(
#'   c('upstream_gene_variant', 'stop_lost', 'splice_acceptor_variant')
#' )
mutation_types_convert_pave_to_maf <- function(pave_mutation_types, variant_type = NULL, split_on_ampersand = TRUE, missing_to_silent = FALSE, verbose = TRUE){

  # Assertions
  assertions::assert_character(pave_mutation_types)
  if(!missing_to_silent) assertions::assert_no_missing(pave_mutation_types)

  if(!missing_to_silent)
    assertions::assert(
      all(nchar(pave_mutation_types) > 0),
      msg = "Found {sum(nchar(pave_mutation_types) == 0)} variant{?s} with no mutation type value (empty string). There are 2 possible solutions\f
      1. Either ensure all variants have a consequence OR\f
      2. If appropriate set {.arg missing_to_silent = TRUE} to assume all variants without consequences have no effect (these will be set to SILENT in the MAF)"
    )


  if(split_on_ampersand)
    pave_mutation_types <- select_most_severe_consequence_pave(pave_mutation_types, missing_is_valid = missing_to_silent)

  pave_mutation_types_uniq <- unique(pave_mutation_types)

  if(verbose) cli::cli_h2('Validating Input')

  # Check input mutation types are valid pave terms
  assert_all_mutations_are_valid_pave(pave_mutation_types_uniq, missing_is_valid = missing_to_silent)

  if(verbose) cli::cli_alert_success('Supplied mutation types are valid pave terms')
  pave2maf_df <- mutation_types_mapping_pave_to_maf()

  valid_variant_types = c("SNP", "DNP", "TNP", "ONP", "DEL", "INS")
  if("frameshift_variant" %in% pave_mutation_types){
    # Require variant_type to be supplied if we have frameshift mutations we need to convert
    assertions::assert(
      !is.null(variant_type),
      msg = "To convert PAVE term {.strong frameshift} to a MAF variant classification, you must supply argument {.arg variant_type}"
    )

    # assert variant_type
    assertions::assert_character(variant_type)
    assertions::assert_equal(length(variant_type), length(pave_mutation_types))
    assertions::assert(all(variant_type %in% valid_variant_types), msg = "Invalid {.arg variant_type} values found ({unique(variant_type[!variant_type %in% valid_variant_types])}). Valid terms include {valid_variant_types}")

    incorrect_variant_types <- unique(variant_type[!variant_type[pave_mutation_types=="frameshift_variant"] %in% c("INS", "DEL")])
    assertions::assert(
      length(incorrect_variant_types) == 0,
      msg = "Variant Type must be {.strong INS} or {.strong DEL} when pave_mutation_type is 'frameshift_variant'. Not [{incorrect_variant_types}]"
    )
  }

  # Ensure variant_type of every inframe_insertion is INS and every inframe_deletion is DEL
  incorrect_variant_types_inframe_ins <- unique(variant_type[pave_mutation_types == "inframe_insertion" & variant_type != "INS"])
  assertions::assert(
    length(incorrect_variant_types_inframe_ins) == 0,
    msg = "Variant Type must be {.strong INS} when pave_mutation_type is 'inframe_insertion'. Not [{incorrect_variant_types_inframe_ins}]"
  )
  incorrect_variant_types_inframe_del <- unique(variant_type[pave_mutation_types == "inframe_deletion" & variant_type != "DEL"])
  assertions::assert(
    length(incorrect_variant_types_inframe_del) == 0,
    msg = "Variant Type must be {.strong DEL} when pave_mutation_type is 'inframe_deletion'. Not [{incorrect_variant_types_inframe_del}]"
  )

  # Ensure variant_type of frameshift is either an insertion/deletion
  incorrect_variant_types_frameshift <- unique(variant_type[pave_mutation_types == "frameshift_variant" & !variant_type %in% c("INS", "DEL")])
  assertions::assert(
    length(incorrect_variant_types_frameshift) == 0,
    msg = "Variant Type must be {.strong INS or DEL} when pave_mutation_type is {.strong frameshift}. Not [{incorrect_variant_types_frameshift}]"
  )

  # Whether missing (empty string or NA_character) values should be set to 'Silent' of 'NA_character' in fcase statement below
  if(missing_to_silent == TRUE)
    maf_value_for_missing = "Silent"
  else
    maf_value_for_missing = NA_character_


  maf_mutation_types <- data.table::fcase(
    pave_mutation_types == "" | is.na(pave_mutation_types), maf_value_for_missing,

    # For all PAVE classifications except frameshift convert to the equivalent MAF terms
    pave_mutation_types != "frameshift_variant", pave2maf_df[['MAF']][match(pave_mutation_types, pave2maf_df[['PAVE']])],
    # For frameshift mutations use variant_type to add the info we need for conversion
    pave_mutation_types == "frameshift_variant" & variant_type == "DEL", "Frame_Shift_Del",
    pave_mutation_types == "frameshift_variant" & variant_type == "INS", "Frame_Shift_Ins",
    default=NA_character_
  )

  assertions::assert_no_missing(maf_mutation_types, msg = "Should never see this. It means the fcase above is failing when mutation types are: {unique(maf_mutation_types[is.na(maf_mutation_types)])}")

  return(maf_mutation_types)
}


# Identification ----------------------------------------------------------


#' Identify Mutation Dictionary Used
#'
#' Looks at variant consequence terms and guesses what mutation dictionary was used.
#' SO and PAVE dictionaries overlap, meaning an observed set of terms can perfectly match both ontologies.
#' If this happens, we assume they are SO terms.
#'
#' @param mutation_types mutation types to test (character)
#' @param split_on_ampersand split mutation types in a single string separated by ampersand (&) into 2 distinct mutation type columns (flag)
#' @param ignore_missing should we ignore missing (NA) or empty ('') mutation_types when identifying a classification scheme (flag)
#' @param verbose verbose (flag)
#'
#' @return one of c('SO', 'MAF', 'UNKNOWN').
#' Will return 'UNKNOWN' unless ALL mutation types fit with one of the supported dictionaries
#' @export
#'
#' @examples
#' mutation_types_identify(c('bob', 'billy', 'missense_variant'))
mutation_types_identify <- function(mutation_types, split_on_ampersand = TRUE, verbose = TRUE, ignore_missing = FALSE){

  # assertions
  if(!is.character(mutation_types)) cli::cli_abort('mutation_types must be a character vector, not {class(mutation_types)}')
  if(!(is.logical(verbose) & length(verbose) == 1)) cli::cli_abort('{.arg verbose} must be a flag, not a {class(verbose)}')
  if(!(is.logical(split_on_ampersand) & length(verbose) == 1))  cli::cli_abort('{.arg split_on_ampersand} must be a flag, not a {class(split_on_ampersand)}')

  if(ignore_missing)
    mutation_types <- mutation_types[!is.na(mutation_types) & mutation_types != ""]

  if(length(mutation_types) == 0)
    return('UNKNOWN')

  # Split on ampersand so multi-consequence mutation_types
  # (e.g. splice_donor_variant&intron_variant) are still classified correctly)
  if(split_on_ampersand){
    mutation_types <- unlist(strsplit(paste0(mutation_types, "&"), split =  "&", fixed = TRUE))
  }
  # Count unique mutation types
  uniq_mutation_types <- unique(mutation_types)
  n_uniq_mutation_types = length(uniq_mutation_types)

  so_classified_mutations <- uniq_mutation_types[uniq_mutation_types %in% mutation_types_so()]
  n_so_classified_mutations <- length(so_classified_mutations)

  pave_classified_mutations <- uniq_mutation_types[uniq_mutation_types %in% mutation_types_pave()]
  n_pave_classified_mutations <- length(pave_classified_mutations)

  maf_classified_mutations <- uniq_mutation_types[uniq_mutation_types %in% mutation_types_maf()]
  n_maf_classified_mutations <- length(maf_classified_mutations)

  unclassified_mutation_types <- uniq_mutation_types[! uniq_mutation_types %in% c(so_classified_mutations, maf_classified_mutations, pave_classified_mutations)]

  if(verbose){
    cli::cli_h2('Identify Class')
    cli::cli_alert_info('Found {n_uniq_mutation_types} unique mutation type{?s} in input set')
    cli::cli_alert_info('{n_pave_classified_mutations}/{n_uniq_mutation_types} mutation types were valid {.strong PAVE} terms')
    cli::cli_alert_info('{n_so_classified_mutations}/{n_uniq_mutation_types} mutation types were valid {.strong SO} terms')
    cli::cli_alert_info('{n_maf_classified_mutations}/{n_uniq_mutation_types} mutation types were valid {.strong MAF} terms')

    if(n_maf_classified_mutations > 0 & n_maf_classified_mutations < n_uniq_mutation_types){
      cli::cli_h2('MAF Mutation Types')
      cli::cli_text("{maf_classified_mutations}")
    }

    if(n_so_classified_mutations > 0 & n_so_classified_mutations < n_uniq_mutation_types){
      cli::cli_h2('SO Mutation Types')
      cli::cli_alert("{so_classified_mutations}")
    }

    if(n_pave_classified_mutations > 0 & n_pave_classified_mutations < n_uniq_mutation_types){
      cli::cli_h2('PAVE Mutation Types')
      cli::cli_alert("{pave_classified_mutations}")
    }


    if(length(unclassified_mutation_types) > 0 & length(unclassified_mutation_types) < n_uniq_mutation_types){
      cli::cli_h2('Unknown Mutation Types')
      cli::cli_alert('{unclassified_mutation_types}')
    }


  }

  if(n_so_classified_mutations == n_uniq_mutation_types) return('SO')
  else if(n_pave_classified_mutations == n_uniq_mutation_types) return('PAVE')
  else if(n_maf_classified_mutations == n_uniq_mutation_types) return('MAF')
  else return('UNKNOWN')
}



# Consequence Ranking -----------------------------------------------------
#' Select the most severe consequence (SO)
#'
#' @noRd
#'
#' @param so_mutation_types_list a list, where each element is a vector containing the set of SO terms you want to choose the most severe consequence from.
#'
#' @return the most severe consequence for each vector in so_mutation_types_list (character)
#'
#' @examples
#' mutationtypes:::select_most_severe_consequence_so_list(
#'   list(
#'      c("intergenic_variant", "feature_truncation", "splice_acceptor_variant"),
#'      c("initiator_codon_variant", "inframe_insertion")
#'   )
#')
#' #> Result:
#' #> c("splice_acceptor_variant", "initiator_codon_variant")
select_most_severe_consequence_so_list <- function(so_mutation_types_list, missing_is_valid = FALSE){
  if(!is.list(so_mutation_types_list)) cli::cli_abort("{.strong {so_mutation_types_list}} must be a list, not a {.strong {class(so_mutation_types_list)}}")
  priority_mappings = mutation_types_so_with_priority()

  assert_all_mutations_are_valid_so(unlist(so_mutation_types_list), missing_is_valid = missing_is_valid)

  vapply(
    X=so_mutation_types_list,
    FUN = function(so_mutation_types){
      most_severe_term <- head(priority_mappings[priority_mappings[["Terms"]] %in% so_mutation_types,"Terms"], n=1)
      if(length(most_severe_term) == 0) return("")

      return(most_severe_term)
      # add test to make sure so_terms.tsv is sorted in order of EffectPriority since otherwise this doesn't work
    },
    FUN.VALUE = character(1))

}

#' Select the most severe consequence (PAVE)
#'
#' @noRd
#'
#' @param pave_mutation_types_list a list, where each element is a vector containing the set of PAVE terms you want to choose the most severe consequence from.
#'
#' @return the most severe consequence for each vector in pave_mutation_types_list (character)
#'
#' @examples
#' mutationtypes:::select_most_severe_consequence_pave_list(
#'   list(
#'      c("upstream_gene_variant", "phased_synonymous", "5_prime_UTR_variant"),
#'      c("missense_variant", "frameshift_variant")
#'   )
#')
#' #> Result:
#' #> c("phased_synonymous", "frameshift_variant")
select_most_severe_consequence_pave_list <- function(pave_mutation_types_list, missing_is_valid = FALSE){
  if(!is.list(pave_mutation_types_list)) cli::cli_abort("{.strong {pave_mutation_types_list}} must be a list, not a {.strong {class(pave_mutation_types_list)}}")
  priority_mappings = mutation_types_pave_with_priority()

  assert_all_mutations_are_valid_pave(unlist(pave_mutation_types_list), missing_is_valid = missing_is_valid)

  vapply(
    X=pave_mutation_types_list,
    FUN = function(pave_mutation_types){
      most_severe_term <- head(priority_mappings[priority_mappings[["Terms"]] %in% pave_mutation_types,"Terms"], n=1)

      if(length(most_severe_term) == 0)
        return("")

      return(most_severe_term)
      # add test to make sure so_terms.tsv is sorted in order of EffectPriority since otherwise this doesn't work
    },
    FUN.VALUE = character(1))

}

#' Select the most severe consequence (SO)
#'
#' Take a character vector which may contain multiple so mutation types separated by '&'
#' And choose only the most severe consequence
#'
#' @param so_mutation_types a character vector of SO terms, where multiple so_mutation_types per field are & delimited, and you want to choose the most severe consequence .
#' @param missing_is_valid should NA values be considered valid mutation classes or should they throw an error? (flag)
#' @return the most severe consequence for each element in so_mutation_types
#' @export
#'
#' @examples
#' select_most_severe_consequence_so(
#'  c(
#'    "intergenic_variant&feature_truncation&splice_acceptor_variant",
#'    "initiator_codon_variant&inframe_insertion"
#'  )
#')
#' #> Result:
#' #> c("splice_acceptor_variant", "initiator_codon_variant")
select_most_severe_consequence_so <- function(so_mutation_types, missing_is_valid = FALSE){
  assertions::assert_character(so_mutation_types)

  so_mutation_types_list  <- strsplit(so_mutation_types, split = "&")

  select_most_severe_consequence_so_list(so_mutation_types_list, missing_is_valid = missing_is_valid)
}

#' Select the most severe consequence (PAVE)
#'
#' Take a character vector which may contain multiple PAVE mutation types separated by '&'
#' And choose only the most severe consequence
#'
#' @param pave_mutation_types a character vector of PAVE terms, where multiple pave_mutation_types per field are & delimited, and you want to choose the most severe consequence .
#' @inheritParams select_most_severe_consequence_so
#'
#' @return the most severe consequence for each element in pave_mutation_types
#' @export
#'
#' @examples
#' select_most_severe_consequence_pave(
#'  c(
#'    "upstream_gene_variant&phased_synonymous&5_prime_UTR_variant",
#'    "missense_variant&frameshift_variant"
#'  )
#')
#' #> Result:
#' #> c("phased_synonymous", "frameshift_variant")
select_most_severe_consequence_pave <- function(pave_mutation_types, missing_is_valid = FALSE){
  assertions::assert_character(pave_mutation_types)

  pave_mutation_types_list  <- strsplit(pave_mutation_types, split = "&")

  select_most_severe_consequence_pave_list(pave_mutation_types_list, missing_is_valid = missing_is_valid)
}


# Validation (assertions) --------------------------------------------------------------


assert_all_mutations_are_valid_so <- function(mutation_types, missing_is_valid = FALSE){
  unique_mutation_types <- unique(unlist(strsplit(mutation_types, split = "&")))

  # Allow NAs if missing_is_valid
  if(missing_is_valid)
    unique_mutation_types <- na.omit(unique_mutation_types)

  unknown_mutation_types <- unique_mutation_types[!unique_mutation_types %in% mutation_types_so()]
  if(length(unknown_mutation_types) > 0 ){
    cli::cli_abort(
      c(
        "Found {.strong {length(unknown_mutation_types)}} mutation type{?s} which {?was/were} NOT {?a/} valid SO terms: [{.strong {unknown_mutation_types}}]",
        x = "Please ensure all mutation types are in {.code mutation_types_so()}"
        )
      )
  }
  return(invisible(TRUE))
}

assert_all_mutations_are_valid_pave <- function(mutation_types, missing_is_valid = FALSE){
  unique_mutation_types <- unique(unlist(strsplit(mutation_types, split = "&")))

  # Allow NAs if missing_is_valid
  if(missing_is_valid)
    unique_mutation_types <- na.omit(unique_mutation_types)

  unknown_mutation_types <- unique_mutation_types[!unique_mutation_types %in% mutation_types_pave()] # Add missing logic to
  if(length(unknown_mutation_types) > 0 ){
    cli::cli_abort(
      c(
        "Found {.strong {length(unknown_mutation_types)}} mutation type{?s} which {?was/were} NOT {?a/} valid PAVE terms: [{.strong {unknown_mutation_types}}]",
        x = "Please ensure all mutation types are in {.code mutation_types_pave()}"
      )
    )
  }
  return(invisible(TRUE))
}

