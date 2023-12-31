% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_L_q_g.R
\name{calc_L_q_g}
\alias{calc_L_q_g}
\title{Calculate the steady-state number of customers in the "general queue"}
\usage{
calc_L_q_g(K, n, prob_vec)
}
\arguments{
\item{K}{:=truncation parameter}

\item{n}{:= number of general servers}

\item{prob_vec}{:= state probability matrix}
}
\value{
L_q_g value
}
\description{
Calculate the steady-state number of customers in the "general queue"
}
\examples{
K <- 14
n <- 2
prob_vec <- []
calc_L_q_r(K,m,prob_vec)
}
