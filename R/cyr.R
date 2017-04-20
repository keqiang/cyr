port_number <- '1234'
cytoscape_url <- paste("http://localhost:", port_number, "/v1", sep="")

cytoscape_network_url <- paste(cytoscape_url, 'networks', sep = "/")

table_type_node <- 'defaultnode'
table_type_edge <- 'defaultedge'
table_type_network <- 'defaultnetwork'

table_column_selected <- 'selected'
table_column_shared_name <- 'shared name'
table_column_suid <- 'SUID'

getRequest <- function (url) {
  response <- httr::GET(url)
  jsonlite::fromJSON(httr::content(response, "text", encoding = 'utf-8'))
}

postRequest <- function (url, body) {
  response <- httr::POST(url, body = body, encode = 'json')
  jsonlite::fromJSON(httr::content(response, "text", encoding = 'utf-8'))
}

putRequesst <- function (url, body) {
  httr::PUT(url, body = body, encode = 'json')
}

#' @export
cytoscape.networks.list <- function () {
  getRequest(paste(cytoscape_url, 'networks.names', sep = "/"))
}

#' @export
cytoscape.networks.nodes.list <- function (network_id, only_summary = TRUE, summary_using_columns = c('name', 'SUID')) {
  all_nodes <- getRequest(paste(cytoscape_network_url, network_id, 'tables', table_type_node, 'rows', sep = "/"))

  if (only_summary && !is.null(summary_using_columns) && length(summary_using_columns) > 0) {
    all_nodes[, summary_using_columns]
  } else {
    all_nodes
  }
}

#' @export
cytoscape.network.nodes.get <- function (network_id, node_id) {
  getRequest(paste(cytoscape_network_url, network_id, 'nodes', node_id, sep = "/"))
}

#' @export
cytoscape.networks.nodes.neighbors.list <- function (network_id, node_id) {
  getRequest(paste(cytoscape_network_url, network_id, 'nodes', node_id, 'neighbors', sep = "/"))
}

#' @export
cytoscape.networks.nodes.selected.list <- function (network_id, only_summary = TRUE, summary_using_columns = c('name', 'SUID')) {
  all_nodes <- getRequest(paste(cytoscape_network_url, network_id, 'tables', table_type_node, 'rows', sep = "/"))
  if (only_summary && !is.null(summary_using_columns) && length(summary_using_columns) > 0) {
    all_nodes[all_nodes[table_column_selected]==TRUE, summary_using_columns]
  } else {
    all_nodes[all_nodes[table_column_selected]==TRUE, ]
  }
}

#' @export
cytoscape.networks.nodes.selected.set <- function (network_id, node_name_list) {
  selected <- rep(TRUE, length(node_name_list))
  df <- data.frame('name' = node_name_list, selected)
  request_body <- list('key' = table_column_shared_name, 'dataKey' = 'name', 'data' = df)
  response <- putRequesst(paste(cytoscape_network_url, network_id, 'tables', table_type_node, sep = "/"), jsonlite::toJSON(request_body, auto_unbox = T))
}

#' @export
cytoscape.networks.nodes.selected.set_by_id <- function (network_id, node_id_list) {
  selected <- rep(TRUE, length(node_id_list))
  df <- data.frame('SUID' = node_id_list, selected)
  request_body <- list('key' = table_column_suid, 'dataKey' = 'SUID', 'data' = df)
  response <- putRequesst(paste(cytoscape_network_url, network_id, 'tables', table_type_node, sep = "/"), jsonlite::toJSON(request_body, auto_unbox = T))
}

#' @export
cytoscape.networks.edges.list <- function (network_id, only_summary = TRUE, summary_using_columns = c('name', 'SUID')) {
  all_edges <- getRequest(paste(cytoscape_network_url, network_id, 'tables', table_type_edge, 'rows', sep = "/"))

  if (only_summary && !is.null(summary_using_columns) && length(summary_using_columns) > 0) {
    all_edges[, summary_using_columns]
  } else {
    all_edges
  }
}

#' @export
cytoscape.networks.edges.selected.list <- function (network_id, only_summary = TRUE, summary_using_columns = c('name', 'SUID')) {
  all_edges <- getRequest(paste(cytoscape_network_url, network_id, 'tables', table_type_edge, 'rows', sep = "/"))
  if (only_summary && !is.null(summary_using_columns) && length(summary_using_columns) > 0) {
    all_edges[all_edges[table_column_selected]==TRUE, summary_using_columns]
  } else {
    all_edges[all_edges[table_column_selected]==TRUE, ]
  }
}

#' @export
cytoscape.networks.edges.selected.set <- function (network_id, edge_id_list) {
  selected <- rep(TRUE, length(edge_id_list))
  df <- data.frame('SUID' = edge_id_list, selected)
  request_body <- list('key' = table_column_suid, 'dataKey' = 'SUID', 'data' = df)
  response <- putRequesst(paste(cytoscape_network_url, network_id, 'tables', table_type_edge, sep = "/"), jsonlite::toJSON(request_body, auto_unbox = T))
}
