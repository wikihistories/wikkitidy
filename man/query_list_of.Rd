% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wiki-action-request.R
\name{query_list_of}
\alias{query_list_of}
\title{Specify which pages to list from the action API}
\usage{
query_list_of(.req, list, ...)
}
\arguments{
\item{.req}{A httr2_request, e.g. generated by \code{wiki_action_request}}

\item{list}{The \href{https://www.mediawiki.org/wiki/API:Lists}{type of list} to return}

\item{...}{Additional parameters to the query, e.g. to set configure list}
}
\value{
An HTTP response: an S3 list with class httr2_request
}
\description{
See \href{https://www.mediawiki.org/wiki/API:Lists}{API:Lists} for available
list actions. Each list action returns a list of pages, typically including
their pageid, \href{https://www.mediawiki.org/wiki/Manual:Namespace}{namespace}
and title. Individual lists have particular properties that can be requested,
which are usually prefaced with a two-word code based on the name of the
list (e.g. specific properties for the \code{categorymembers} list action are
prefixed with \code{cm}).
}
\details{
When the request is performed, the data is returned in the body of the
request under the \code{query} object, labeled by the chosen list action.

If you want to study the actual pages listed, it is adviseable to retrieve
the pages directly using a generator, rather than listing their IDs using a
list action. When using a list action, a second request is required to get
further information about each page. Using a generator, you can query pages
and retrieve their relevant properties in a single API call.
}
\examples{
# Get the ten most recently added pages in Category:Physics

resp <- wiki_action_request() \%>\%
  query_list_of("categorymembers", cmsort = "timestamp",
    cmdir = "desc", cmtitle = "Category:Physics") \%>\%
  httr2::req_perform() \%>\%
  httr2::resp_body_json()

resp$query$categorymembers
}
