# nhanesDB - retrieve nhanes data from the local container
# Laha Ale and Robert Gentleman 06/08/2023
# With updates by Deepayan Sarkar (May 2024)

.nhanesTablesDB <-
  function(data_group, year,
           nchar = 128,  details = FALSE,
           namesonly = FALSE, includerdc = FALSE)
  {
    .checkDataGroupDB(data_group)
    ## check if they are using the short name
    if (data_group %in% names(nhanes_group))
      data_group <- nhanes_group[data_group]

    if (!is.numeric(year)) stop("Invalid year: ", year)
    .begin_year <- if (.is.even(year)) year - 1 else year
    
    # Define the table reference
    metadata_questionnaire_descriptions <-
      dplyr::tbl(cn(), I(MetadataTable("QuestionnaireDescriptions")))
    
    # Build the query using dplyr
    query = metadata_questionnaire_descriptions |>
      dplyr::filter(DataGroup == data_group,
                    BeginYear == .begin_year) |>
      dplyr::mutate(Component = concat(substr(DataGroup, 1, 1),
                                       tolower(substr(DataGroup, 2, 20))))
    
    if (details) {
      query = query |>
        dplyr::select(Data.File.Name = TableName,
                      Data.File.Description = Description,
                      Component,
                      Begin.Year = BeginYear,
                      EndYear)
    } else {
      query = query |>
        dplyr::select(Data.File.Name = TableName,
                      Data.File.Description = Description)
    }
    
    # Fetch the results by executing the query
    df <- dplyr::collect(query)
    
    if (namesonly) {
      return(unique(df$Data.File.Name))
    } else {
      return(df)
    }
  }

.checkDataGroupDB <-
  function(data_group)
{
  if (!(data_group %in% names(nhanes_group)))
    stop("Invalid survey group!")
  
}

.nhanesTableVarsDB <-
  function(data_group, nh_table, details = FALSE,
           nchar = 128, namesonly = FALSE)
{
  
  # FIXME: We need to add Use.Constraints when DB is updated
  if (missing(nh_table)) {
    nh_table <- data_group
    data_group <- NULL
  }
  if (!is.null(data_group)) {
    .checkDataGroupDB(data_group)
  }
  .checkTableNames(nh_table)
  
  # Define the schema-qualified table references
  metadata_questionnaire_descriptions <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireDescriptions")))
  metadata_questionnaire_variables <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireVariables")))
  
  # Build the query using dplyr
  query <-
    metadata_questionnaire_variables |>
    dplyr::filter(TableName == nh_table) |>
    dplyr::mutate(Variable.Description = substr(Description, 1, nchar)) |>
    dplyr::select(Variable.Name = Variable,
                  Variable.Description,
                  TableName) |>
    dplyr::inner_join(metadata_questionnaire_descriptions, by = "TableName") |>
    dplyr::mutate(Component = paste0(substr(DataGroup, 1, 1),
                                     tolower(substr(DataGroup, 2, 20)))) |>
    dplyr::select(Variable.Name,
                  Variable.Description,
                  Data.File.Name = TableName,
                  # Data.File.Description = substr(Description, 1, nchar),
                  DataGroup,
                  Begin.Year = BeginYear,
                  EndYear,
                  Component)
  if (!is.null(data_group)) {
    query <-
      query |>
      dplyr::filter(str_detect(tolower(DataGroup), tolower(data_group)))
  }
  # Fetch the results by executing the query
  df <- dplyr::collect(query)
  if (namesonly) {
    return(unique(df$Variable.Name))
  } else if (!details) {
    return(df[,c('Variable.Name','Variable.Description')])
  } else {
    return(df)
  }
}

.nhanesDB <-
  function(nh_table, includelabels = FALSE, translated = TRUE)
{
  .checkTableNames(nh_table)
  ## get appropriate DB table name (downgrading to raw if translated not available)
  db_table <- .convertTranslatedTable(nh_table, translated)
  nh_df <- dplyr::tbl(cn(), I(db_table)) |> dplyr::collect() # get data
  if (!isTRUE(includelabels)) return(nh_df)

  # Otherwise includelabels == TRUE, so obtain labels
  metadata_questionnaire_variables <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireVariables")))
  
  # Build the label query using dplyr
  label_query <-
    metadata_questionnaire_variables |>
    dplyr::filter(TableName == nh_table) |>
    dplyr::select(Variable, Description)
  
  # Execute the label query and construct column labels
  var_label <- dplyr::collect(label_query)
  column_labels <-
    with(as.data.frame(var_label),
         structure(Description, names = Variable))
  column_labels <- column_labels[names(nh_df)]
  # add labels as attribute for each column
  for (i in seq_len(ncol(nh_df))) {
    attr(nh_df[[i]], "label") <- column_labels[i]
  }
  nh_df
}


.nhanesSearchVarNameDB <-
  function(varnames = NULL,
           ystart = NULL,
           ystop = NULL,
           includerdc = FALSE,
           nchar = 128,
           namesonly = TRUE)
{
  ## FIXME: simplify query when namesonly = TRUE?
  
  # Define the schema-qualified table references
  metadata_questionnaire_descriptions <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireDescriptions")))
  metadata_questionnaire_variables <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireVariables")))
  
  # Build the query using dplyr
  query <-
    metadata_questionnaire_variables |>
    dplyr::filter(Variable %in% varnames) |>
    dplyr::mutate(Variable.Description = substr(Description, 1, nchar))|>
    dplyr::select(Variable.Name = Variable,
                  Variable.Description,
                  TableName) |>
    dplyr::inner_join(metadata_questionnaire_descriptions, by = "TableName") |>
    dplyr::mutate(Data.File.Description = substr(Description, 1, nchar),
                  Component = paste0(substr(DataGroup, 1, 1),
                                     tolower(substr(DataGroup, 2, 20)))) |>
    dplyr::select(Variable.Name,
                  Variable.Description,
                  Data.File.Name = TableName,
                  Data.File.Description,
                  Begin.Year = BeginYear,
                  EndYear,
                  Component)
  
  # Apply additional filters based on ystart and ystop
  if (!is.null(ystart)) {
    query <- dplyr::filter(query, Begin.Year >= ystart)
  }
  if (!is.null(ystop)) {
    query <- dplyr::filter(query, EndYear <= ystop)
  }
  
  # Fetch the results by executing the query
  df <- query |> dplyr::collect()
  
  if(is.null(df)){
    warning(paste0("Variable ", varnames, " is not found in the database!"))
  }
  
  if(namesonly){
    df = unique(df$Data.File.Name)
  }
  df
}


.nhanesSearchTableNamesDB <-
  function(pattern = NULL,
           ystart = NULL,
           ystop = NULL,
           includerdc = FALSE,
           includewithdrawn = FALSE,
           nchar = 128,
           details = FALSE)
{
  metadata_questionnaire_descriptions <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireDescriptions")))
  
  # to use grepl we have load the whole table first, the table is not very big.
  df <-
    dplyr::collect(metadata_questionnaire_descriptions) |> 
    dplyr::filter(grepl(pattern, TableName)) |>
    # dplyr::filter(dplyr::sql(paste0("TableName LIKE '%", pattern, "%'"))) |> 
    dplyr::mutate(Years = paste0(BeginYear, '-', EndYear)) |>
    dplyr::select(TableName,
                  Years,
                  Date.Published = DatePublished)
  
  # Apply additional filters based on ystart and ystop
  if (!is.null(ystart)) df <- dplyr::filter(df, BeginYear >= ystart)
  if (!is.null(ystop)) df <- dplyr::filter(df, EndYear <= ystop)
  if (is.null(df) || nrow(df)== 0) {
    warning("Cannot find any table name like: ", pattern)
  }
  if (details) return(df)
  else return(unique(df$TableName))
  ## FIXME: do we want to use years as names?
  ## return(with(df, structure(TableName, names = Years)))
}


.nhanesTranslateDB <- function(nh_table, colnames = NULL, data = FALSE, nchar = 32,
                               mincategories = 2, details = FALSE, dxa = FALSE)
{
  .checkTableNames(nh_table)
  if (!is.null(data)){
    return(.nhanesDB(nh_table))
  }
  if (length(nh_table) > 1 ) stop("you can only select one table")
  # Define the schema-qualified table references
  metadata_variable_codebook <-
    dplyr::tbl(cn(), I(MetadataTable("VariableCodebook")))
  
  # Build the query using dplyr
  query <-
    if (details) {
      metadata_variable_codebook |>
        dplyr::filter(TableName == nh_table) |>
        dplyr::select(Variable = Variable,
                      Code.or.Value = CodeOrValue,
                      Value.Description = ValueDescription,
                      Count = Count,
                      Cumulative = Cumulative,
                      Skip.to.Item = SkipToItem)
    }
    else {
      metadata_variable_codebook |>
        dplyr::filter(TableName == nh_table) |>
        dplyr::select(Variable = Variable,
                      Code.or.Value = CodeOrValue,
                      Value.Description = ValueDescription)
    }
  
  if (!is.null(colnames)) {
    query <- dplyr::filter(query, Variable %in% colnames)
  }
  
  # Fetch the results by executing the query
  df <- dplyr::collect(query)
  varIndex <- which(names(df) == "Variable") # should always be 1, but just in case...
  ans <- split(df[-varIndex], df[[varIndex]])
  # reset row names --- not needed for tibbles
  ## for (i in seq_along(ans)) { row.names(ans[[i]]) <- NULL }
  ans
}


.nhanesSearchDB <-
  function(search_terms = NULL,
           exclude_terms = NULL,
           data_group = NULL,
           ignore.case = FALSE,
           ystart = NULL,
           ystop = NULL,
           includerdc = FALSE,
           nchar = 128,
           namesonly = FALSE)
{
  # Define the schema-qualified table references
  metadata_questionnaire_descriptions <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireDescriptions")))
  metadata_questionnaire_variables <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireVariables")))
  
  # Join the tables
  query <-
    metadata_questionnaire_variables |>
    dplyr::inner_join(metadata_questionnaire_descriptions, by = "TableName") |>
    dplyr::mutate(Variable.Name = Variable,
                  Variable.Description = substr(Description.x, 1, nchar),
                  Data.File.Name = TableName,
                  Data.File.Description = substr(Description.y, 1, nchar),
                  Component = paste0(substr(DataGroup, 1, 1),
                                     tolower(substr(DataGroup, 2, 20))),
                  Begin.Year = BeginYear,
                  EndYear = EndYear)
  
  # Apply search terms
  search_terms <- paste(search_terms, collapse = "|")
  query <-
    dplyr::filter(query,
                  str_detect(tolower(Description.x),
                             tolower(search_terms)))
  
  # Create exclude filter expressions
  if (!is.null(exclude_terms)) {
    exclude_terms <- paste(exclude_terms, collapse = "|")
    query <- dplyr::filter(query,
                           !str_detect(tolower(Description.x),
                                       tolower(exclude_terms)))
  }

  # Apply data group filter
  if (!is.null(data_group)) {
    if (length(data_group) > 1) {
      query <- dplyr::filter(query, str_detect(DataGroup, data_group[1]))
      for (term in data_group[2:length(data_group)]) {
        query <- dplyr::filter(query, str_detect(DataGroup, term))
      }
    } else {
      query <- dplyr::filter(query, str_detect(DataGroup, data_group))
    }
  }
  
  # Apply year filters
  if (!is.null(ystart)) { query <- dplyr::filter(query, BeginYear >= ystart) }
  if (!is.null(ystop)) { query <- dplyr::filter(query, EndYear <= ystop) }
  query <- dplyr::select(query,
                         Variable.Name,
                         Variable.Description,
                         Data.File.Name,
                         Data.File.Description,
                         Begin.Year,
                         EndYear,
                         Component)
  
  # Fetch the results by executing the query
  df <- dplyr::collect(query)
  if (namesonly) { df <- unique(df$Data.File.Name) }
  df
}


.nhanesCodebookDB <- function(nh_table, colname)
{
  # FIXME: we need to handle multiple targets once DB is updated!
  .checkTableNames(nh_table)
  
  # Define the schema-qualified table reference
  metadata_questionnaire_variables <-
    dplyr::tbl(cn(), I(MetadataTable("QuestionnaireVariables")))
  
  # Build the query
  query <-
    metadata_questionnaire_variables |>
    dplyr::filter(TableName == nh_table)
  
  if (!is.null(colname)) { query <- dplyr::filter(query, Variable %in% colname) }

  ## FIXME: Some tables have more; need a way to study and handle them
  query <- dplyr::select(query, 
                         'Variable Name:' = Variable,
                         'SAS Label:' = SasLabel,
                         'English Text:' = Description,
                         'Target:' = Target)
  
  # Fetch the results by executing the query
  res <- dplyr::collect(query)
  
  if (length(res[[1]]) == 0) {
    stop("The variable '", colname,
         "' is not found in the data file / table '", nh_table, "'.")
  }
  colname <- res$`Variable Name:`
  res.list <- split(res, seq(nrow(res)))
  res.list <- lapply(res.list, as.list)
  names(res.list) <- colname
  ## str(res.list)
  trans <- nhanesTranslate(nh_table, colname, details = TRUE)
  for (code in colname) {
    if (code %in% names(trans)) {
      res.list[[code]] <- c(res.list[[code]], trans[code])
    }
  }
  res.list 
}

