% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_research_project.R
\name{complete_setup}
\alias{complete_setup}
\title{Finish project set-up using the CMOR research project templates}
\usage{
complete_setup(
  project = getwd(),
  git = getOption("cmor.tools.git"),
  raw_data_in_git = getOption("cmor.tools.git_rawdata"),
  data_in_git = getOption("cmor.tools.git_data"),
  output_in_git = getOption("cmor.tools.git_output"),
  github = getOption("cmor.tools.github"),
  private = getOption("cmor.tools.github_private")
)
}
\arguments{
\item{project}{Path to the project folder. Default is to use the current
working directory.}

\item{git}{Logical (default = \code{TRUE}). Whether to create a git
repository.}

\item{raw_data_in_git}{Logical (default = \code{TRUE}). If FALSE, data in the
\code{data/raw_data/} directory will be excluded from the git repository.}

\item{data_in_git}{Logical. If \code{FALSE} (the default), data in the
\code{data/} directory (but not the \code{data/raw_data/} subdirectory,
unless \code{raw_data_in_git} is also set to \code{FALSE}) will be
excluded from the git repository.}

\item{output_in_git}{Logical. If \code{FALSE} (the default), the
\code{output} folder will be excluded from the git repository.}

\item{github}{Logical (default = \code{TRUE}). Whether to create a GitHub
repository.}

\item{private}{Logical (default = \code{TRUE}). Should the GitHub repo be
private or public? Ignored if \code{github = FALSE}.}
}
\description{
This function should be used in the newly-created project after
    \code{create_research_project()} to create a git repository, connect to
    github, and add a basic project README.
}
