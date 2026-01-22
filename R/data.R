#' Distribution of preferences by candidate by division in the Australian Federal Election (2022 and 2025)
#' 
#' Provides details on how votes are distributed and transferred among candidates 
#' in all count stages of the preferential voting system. All electoral divisions 
#' in the Australian Federal Election are included. 
#' 
#' Two datasets are provided: 
#' \itemize{
#'   \item \code{aecdop_2022}: 2022 Federal Election (35,096 rows)
#'   \item \code{aecdop_2025}: 2025 Federal Election (30,888 rows)
#' }
#'
#' @format A tibble of 14 columns:
#' \describe{
#'   \item{StateAb}{State or territory abbreviation (e.g., "ACT", "NSW", "VIC")}
#'   \item{DivisionID}{Numeric identifier for the electoral division}
#'   \item{DivisionNm}{Name of the electoral division (e.g., "Bean", "Canberra")}
#'   \item{CountNumber}{Round in the counting procedure, starting from 0 (first preference)}
#'   \item{BallotPosition}{Position of the candidate on the ballot paper}
#'   \item{CandidateID}{Unique numeric identifier for the candidate}
#'   \item{Surname}{Candidate's surname}
#'   \item{GivenNm}{Candidate's given name(s)}
#'   \item{PartyAb}{Party abbreviation (e.g., "UAPP", "ALP", "LP")}
#'   \item{PartyNm}{Full party name (e.g., "United Australia Party", "Australian Labor Party")}
#'   \item{Elected}{Whether the candidate was elected: "Y" (yes) or "N" (no)}
#'   \item{HistoricElected}{Whether the candidate was elected in the previous election: "Y" (yes) or "N" (no)}
#'   \item{CalculationType}{Type of calculation: 
#'     \describe{  
#'       \item{Preference Count}{Number of votes received}
#'       \item{Preference Percent}{Percentage of votes received}
#'       \item{Transfer Count}{Number of votes transferred from other candidates}
#'       \item{Transfer Percent}{Percentage of votes transferred from other candidates}
#'     }
#'   }
#'   \item{CalculationValue}{Numeric value for the calculation type (votes or percentage)}
#' }
#'
#' @source Australian Electoral Commission (AEC)
#'   \href{https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm}{Distribution of Preferences 2022}
#'   \href{https://results.aec.gov.au/31496/Website/HouseDownloadsMenu-31496-Csv.htm}{Distribution of Preferences 2025}
#'
#' @examples
#' # Load the datasets
#' data(aecdop_2022)
#' data(aecdop_2025)
#'
#' # First preferences for Bean division in 2022
#' aecdop_2022 |> 
#'   dplyr::filter(DivisionNm == "Bean",
#'          CountNumber == 0,
#'          CalculationType == "Preference Count")
#' @name aecdop
#' @aliases aecdop_2022 aecdop_2025
NULL

#' @rdname aecdop
"aecdop_2022"

#' @rdname aecdop
"aecdop_2025"

#' Electoral boundaries map for the 2025 Australian Federal Election
#' 
#' Provides the points that make up the boundaries of each electoral division in the 
#' 2025 Australian Federal Election. 
#' 
#' @format A tibble of 8 columns:
#' \describe{
#'   \item{long}{Longitude of point in polygon}
#'   \item{lat}{Latitude of point in polygon}
#'   \item{hole}{Whether the polygon has a hole}
#'   \item{piece}{Polygon piece number}
#'   \item{group}{Polygon group number}
#'   \item{order}{Order of polygon within group}
#'   \item{id}{Unique identifier for polygon}
#'   \item{elect_div}{Electoral division name}
#' }
#'
#' @source Australian Electoral Commission (AEC)
#'   \url{https://www.aec.gov.au/electorates/maps.htm}
#'
#' @examples
#' library(ggplot2)
#' library(ggthemes)
#' 
#' # Load the dataset
#' data(elb_map)
#'
#' # Plot the map
#' ggplot(elb_map) + 
#'   geom_polygon(
#'     aes(x = long, y = lat, group = group),
#'     fill = "grey90", color = "white") +
#'   theme_map()
"elb_map"

#' Centroids of electoral divisions in the 2025 Australian Federal Election
#' 
#' Provides the centroids of all electorates in the 2025 Australian Federal Election. 
#' The dataset is computed from 2025 Electoral Boundaries data.
#' 
#' @format A tibble of 5 columns:
#' \describe{
#'   \item{id}{Unique identifier for electorate}
#'   \item{elect_div}{Electoral division name}
#'   \item{area_sqkm}{Area of the electorate in square kilometres}
#'   \item{long}{Longitude of the electoratecentroid}
#'   \item{lat}{Latitude of the electorate centroid}
#' }
#' 
#' @source Australian Electoral Commission (AEC)
#'   \url{https://www.aec.gov.au/electorates/maps.htm}
#'
#' @examples
#' library(ggplot2)
#' library(ggthemes)
#' 
#' # Load the dataset
#' data(elb_centroid)
#'
#' # Plot the centroids on top of the electoral boundaries
#' ggplot(elb_map) + 
#'   geom_polygon(
#'     aes(x = long, y = lat, group = group),
#'     fill = "grey90", color = "white") +
#'   geom_point(
#'     data = elb_centroid,
#'     aes(x = long, y = lat),
#'     size = 1, alpha = 0.8
#'   ) +
#'   theme_map()
"elb_centroid"