
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

mutation_types_so_with_priority <- function(){
  mutation_types_so_wp <- utils::read.csv(
    file = system.file('so_terms.tsv', package = "mutationtypes"),
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
#' @param so_mutation_types a vector of SO terms you want to convert to MAF variant classifications (character)
#' @param split_on_ampersand should '&' separated SO terms be automatically converted to single SO terms based on highest severity? (flag)
#' @param inframe is the mutation inframe? (logical). Used to map protein_altering_variant to valid MAF columns
#' @param variant_type a vector describing each mutations type. Valid elements include: "SNP", "DNP", "TNP", "ONP", "DEL", "INS". Used to map frameshift_variant to more specific MAF columns (character)
#' @param verbose verbose (flag)
#' @return matched MAF variant classification terms (character)
#' @export
#'
#' @examples
#' mutation_types_convert_so_to_maf(c('INTRAGENIC', 'INTRAGENIC', 'intergenic_region'))
mutation_types_convert_so_to_maf <- function(so_mutation_types, variant_type = NULL, inframe = NULL, split_on_ampersand = TRUE, verbose = TRUE){

  # Assertions
  assertions::assert_character(so_mutation_types)

  if(split_on_ampersand)
    so_mutation_types <- select_most_severe_consequence_so(so_mutation_types)

  so_mutation_types_uniq <- unique(so_mutation_types)

  if(verbose) cli::cli_h1('Validating Input')

  # Check input mutation types are valid so terms
  assert_all_mutations_are_valid_so(so_mutation_types_uniq)

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

  #browser()
  maf_mutation_types <- data.table::fcase(
    !so_mutation_types %in% c("frameshift_variant", "protein_altering_variant"), so2maf_df[['MAF']][match(so_mutation_types, so2maf_df[['SO']])],
    so_mutation_types == "frameshift_variant" & variant_type == "DEL", "Frame_Shift_Del",
    so_mutation_types == "frameshift_variant" & variant_type == "INS", "Frame_Shift_Ins",
    so_mutation_types == "protein_altering_variant" & variant_type == "DEL" & inframe == TRUE, "In_Frame_Del",
    so_mutation_types == "protein_altering_variant" & variant_type == "INS" & inframe == TRUE, "In_Frame_Ins",
    so_mutation_types == "protein_altering_variant" & variant_type == "DEL" & inframe == FALSE, "Frame_Shift_Del",
    so_mutation_types == "protein_altering_variant" & variant_type == "INS" & inframe == FALSE, "Frame_Shift_Ins",

    default=NA_character_
    )

  assertions::assert_has_no_missing_values(maf_mutation_types)

  return(maf_mutation_types)
}


#' Identify Mutation Dictionary Used
#'
#' @param mutation_types mutation types to test (character)
#' @param split_on_ampersand split mutation types in a single string separated by ampersand (&) into 2 distinct mutation type columns (flag)
#' @param verbose verbose (flag)
#'
#' @return one of c('SO', 'MAF', 'UNKNOWN').
#' Will return 'UNKNOWN' unless ALL mutation types fit with one of the supported dictionaries
#' @export
#'
#' @examples
#' mutation_types_identify(c('bob', 'billy', 'missense_variant'))
mutation_types_identify <- function(mutation_types, split_on_ampersand = TRUE, verbose = TRUE){

  # assertions
  if(!is.character(mutation_types)) cli::cli_abort('mutation_types must be a character vector, not {class(mutation_types)}')
  if(!(is.logical(verbose) & length(verbose) == 1)) cli::cli_abort('{.arg verbose} must be a flag, not a {class(verbose)}')
  if(!(is.logical(split_on_ampersand) & length(verbose) == 1))  cli::cli_abort('{.arg split_on_ampersand} must be a flag, not a {class(split_on_ampersand)}')

  # Split on ampersand so multi-consequence mutation_types
  # (e.g. splice_donor_variant&intron_variant) are still classified correctly)
  if(split_on_ampersand)
    mutation_types <- unlist(strsplit(mutation_types, split =  "&", fixed = TRUE))


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

#' Select the most severe consequence (SO)
#'
#' @noRd
#'
#' @param so_mutation_types_list a list, where each element is a vector containing the set of SO terms you want to choose the most severe consequence from.
#'
#' @return the most severe consequence for each vector in so_mutation_types_list
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
select_most_severe_consequence_so_list <- function(so_mutation_types_list){
  if(!is.list(so_mutation_types_list)) cli::cli_abort("{.strong {so_mutation_types_list}} must be a list, not a {.strong {class(so_mutation_types_list)}}")
  priority_mappings = mutation_types_so_with_priority()

  assert_all_mutations_are_valid_so(unlist(so_mutation_types_list))

  vapply(
    X=so_mutation_types_list,
    FUN = function(so_mutation_types){
      most_severe_term <- priority_mappings[priority_mappings[["Terms"]] %in% so_mutation_types,"Terms"][[1]]
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
#'
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
select_most_severe_consequence_so <- function(so_mutation_types){
  so_mutation_types_list  <- strsplit(so_mutation_types, split = "&")

  select_most_severe_consequence_so_list(so_mutation_types_list)
}

# assert_all_mutations_are_valid_so <- assertions::assert_create(
#   func = function(mutation_types) {
#     unique_mutation_types <- unique(unlist(strsplit(mutation_types, split = "&")))
#     unknown_mutation_types <- unique_mutation_types[!unique_mutation_types %in% mutation_types_so()]
#     if(length(unique_mutation_types) > 0 ){
#       "Found {.strong {length(unknown_mutation_types)}} mutation type{?s} which {?was/were} NOT valid SO terms: [{.strong {unknown_mutation_types}}]"
#     }}
# )


assert_all_mutations_are_valid_so <- function(mutation_types){
  unique_mutation_types <- unique(unlist(strsplit(mutation_types, split = "&")))
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
