\name{resolveSynonymy}
\alias{resolveSynonymy}
\title{
Resolve Synonymies.
}
\description{
Resolve synonymies using table \code{neptune_taxonomy}. The synonymies in this table correspond to an expert consensus (TNL).
}
\usage{
resolveSynonymy(dataset, username = "", password = "")
}

\arguments{
  \item{dataset}{
A dataframe imported using \code{\link{getNeptuneData}}.
}
  \item{username}{
Enter here your username to NSB as a character string. Compulsory.
}
  \item{password}{
Enter here your password to NSB as a character string. Compulsory.
}
}
\details{
The function use recursion to resolve long synonym paths (spA synonym to spB, spB synonym to spC,...) and is hence rather slow. The resulting "resolved" taxa do not always have a taxon status "V", they can be "G", "U" and "Q" as well.
}
\value{
Result in a dataframe similar to the input, with 4 extra columns: resolved_taxon_id, resolved_species, resolved_genus, resolved_subspecies. If the taxon was originally not a synonym thos columns contain the original taxon.
}
\author{
Johan Renaudie.
}

\seealso{
\code{\link{getNeptuneData}}
}
\examples{
n <- getNeptuneData(username="****", password="****", fossil_group="R", age_range=c(4,6), ocean="IND")
m <- resolveSynonymy(m, username="****", password="****")
}