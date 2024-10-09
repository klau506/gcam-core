#### module_prj_creation.R
####
#### Project creation auxiliary functions


#' create_prj
#'
#' Function to create a GCAM project provided a database and a queries file
#' @param db_name name of the database. It will The extension will be automatically added if not present
#' @param desired_scen desired scenarios. If NULL, all the scenarios present in the database will be considered
#' @param prj_name name of the project. If NULL, it will be the defult option, i.e., the database name. Otherwise specify
#' @return create the specified project
create_prj <- function(db_name, desired_scen = NULL, prj_name = NULL) {
  db_path <- file.path(base_path,'output')
  query_path <- file.path(base_path, 'diets_analysis', 'inputs', 'queries')
  queries <<- '/queries_beh_policyCost.xml'

  # prj name checks and/or definition
  if (!is.null(prj_name)) {
    assert_that(substr(prj_name, nchar(prj_name) - 3, nchar(prj_name)) == ".dat",
                msg = 'In `load_prj` function: The specified project name does not contain the extension (.dat)')
  } else {
    prj_name = paste0(db_name, '.dat')
  }


  # scenarios checks and/or definition
  conn <- rgcam::localDBConn(db_path, db_name)
  available_scen <- rgcam::listScenariosInDB(conn)$name
  if (!is.null(desired_scen)) {
    assert_that(all(desired_scen %in% available_scen))
  } else {
    desired_scen <- available_scen
  }


  print("---------------------------------------------------------")

  if (exists(prj_name)) {
    print('prj already present!')
  } else {
    print('create prj')

    # initialize the project
    prj <- NULL

    # read the query file and list all queries' titles
    xml <- xml2::read_xml(file.path(base_path, 'diets_analysis', 'inputs', 'queries', 'queries_beh.xml'))
    query_titles <- xml_text(xml_find_all(xml, "//*[@title]/@title"))

    # extract the data by query
    for (q in query_titles) {
      dt_sec <- scenario_query(q, db_path, db_name, prj_name, desired_scen)
      prj_tmp <- rgcam::addQueryTable(
        project = prj_name, qdata = dt_sec, saveProj = FALSE,
        queryname = q, clobber = FALSE
      )
      if (!is.null(prj)) {
        prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
      } else {
        prj <- prj_tmp
      }
      rm(prj_tmp,dt_sec)
    }

    # for (sc in desired_scen) {
    #   print(sc)
    #
    #   # create prj
    #   prj <- rgcam::addScenario(conn, prj_name, sc,
    #                              paste0(query_path, queries),
    #                              clobber = FALSE, saveProj = FALSE)
    # }

    # add 'nonCO2' large query
    if (!"nonCO2 emissions by sector (excluding resource production)" %in% rgcam::listQueries(prj)) {
      dt_sec <- data_query("nonCO2 emissions by sector (excluding resource production)", db_path, db_name, prj_name, desired_scen)
      prj_tmp <- rgcam::addQueryTable(
        project = prj_name, qdata = dt_sec, saveProj = FALSE,
        queryname = "nonCO2 emissions by sector (excluding resource production)", clobber = FALSE
      )
      prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = TRUE)
      rm(prj_tmp)
    } else {
      saveProject(prj, file = prj_name)
    }

    print(rgcam::listQueries(prj, anyscen = F))
  }

}


#' data_query
#'
#' Aux. function to load heavy queries
#' @param type query name
#' @param db_path database path
#' @param db_name database name
#' @param prj_name project name
#' @param scenarios scenarios to be considered
#' @return dataframe with the specified query information
data_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(file.path(base_path, 'diets_analysis', 'inputs', 'queries', 'queries_beh_nonCO2.xml'))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  full_nonCO2_emissions_list = c('BC','BC_AWB','C2F6','CF4','CH4','CH4_AGR','CH4_AWB','CO','CO_AWB','H2',
                                 'H2_AWB','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC23','HFC236fa',
                                 'HFC245fa','HFC32','HFC365mfc','HFC43','N2O','N2O_AGR','N2O_AWB','NH3','NH3_AGR',
                                 'NH3_AWB','NMVOC','NMVOC_AGR','NMVOC_AWB','NOx','NOx_AGR','NOx_AWB','OC','OC_AWB',
                                 'PM10','PM2.5','SF6','SO2_1','SO2_1_AWB','SO2_2','SO2_2_AWB','SO2_3','SO2_3_AWB',
                                 'SO2_4','SO2_4_AWB')

  for (sc in scenarios) {
    emiss_list = unique(full_nonCO2_emissions_list)
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp = rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,migabble = FALSE),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = NULL,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )

      tmp = data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt = dplyr::bind_rows(dt,tmp)
      }
      rm(prj_tmp)

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}




#' scenario_query
#'
#' Aux. function to load heavy queries split by scenario
#' @param type query name
#' @param db_path database path
#' @param db_name database name
#' @param prj_name project name
#' @param scenarios scenarios to be considered
#' @return dataframe with the specified query information
scenario_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(file.path(base_path, 'diets_analysis', 'inputs', 'queries', 'queries_beh.xml'))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  for (sc in scenarios) {
    prj_tmp = rgcam::addSingleQuery(
      conn = rgcam::localDBConn(db_path,
                                db_name,migabble = FALSE),
      proj = prj_name,
      qn = type,
      query = qq,
      scenario = sc,
      regions = NULL,
      clobber = TRUE,
      transformations = NULL,
      saveProj = FALSE,
      warn.empty = FALSE
    )

    tmp = data.frame(prj_tmp[[sc]][type])
    if (nrow(tmp) > 0) {
      dt = dplyr::bind_rows(dt,tmp)
    }
    rm(prj_tmp)

  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}
