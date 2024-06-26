% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_genes_queries.R
\name{query_gene_nanostring_expression}
\alias{query_gene_nanostring_expression}
\title{Query Gene Nanostring Expression}
\usage{
query_gene_nanostring_expression(
  cohorts = NA,
  samples = NA,
  entrez = NA,
  gene_types = NA,
  paging = NA,
  ...
)
}
\arguments{
\item{cohorts}{A vector of strings}

\item{samples}{A vector of strings}

\item{entrez}{A vector of integers}

\item{gene_types}{A vector of strings}

\item{paging}{A named list}

\item{...}{Arguments to create_result_from_api_query}
}
\description{
Query Gene Nanostring Expression
}
