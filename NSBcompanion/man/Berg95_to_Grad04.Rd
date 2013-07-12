\name{Berg95_to_Grad04}
\alias{Berg95_to_Grad04}

\title{
Berggren et al. (1995) to Gradstein et al. (2004) conversion.
}
\description{
Age models in the Neptune Database are scaled on Berggren et al. 1995 timescale. This function converts to Gradstein et al. 2004 using linear interpolation between the two scales.
}
\usage{
Berg95_to_Grad04(dataset)
}

\arguments{
  \item{dataset}{
A dataframe such as output by \code{\link{getNeptuneData}}.
}
}
\details{
Load dataframe 'agescale'. Otherwise it is merely a wrapper around function \code{\link{approx}}.
}
\value{
The same dataframe as the input but with an extra column 'sample_age_ma_Gradstein04'.
}
\references{
Berggren, W. A., D. V. Kent, C. C. Swisher III, and M.-P. Aubry. 1995. A revised Cenozoic geochronology and chronostratigra- phy. In W. A. Berggren et al., eds. Geochronology time scales and global stratigraphic correlation. SEPM Special Publication 54:129–212.
Gradstein, F. M., J. G. Ogg, A. G. Smith, F. P. Agterberg, W. Bleeker, R. A. Cooper, V. Davydov, P. Gibbard, L. A. Hinnov, M. R. House, L. Lourens, H.-P. Luterbacher, J. McArthur, M. J. Melchin, L. J. Robb, P. M. Sadler, J. Shergold, M. Villeneuve, B. R. Wardlaw, J. Ali, H. Brinkhuis, F. J. Hilgen, J. Hooker, R. J. Howarth, A. H. Knoll, J. Laskar, S. Monechi, J. Powell, K. A. Plumb, I. Raffi, U. Röhl, A. Sanfilippo, B. Schmitz, N. J. Shackleton, G. A. Shields, H. Strauss, J. Van Dam, J. Veizer, T. van Kolfschoten and D. Watson. 2004. A Geologic Time Scale 2004. Cambridge University Press, 589pp.
}

\seealso{
\code{\link{getNeptuneAgeModelMetaData}}
}
\examples{
	n <- getNeptuneData(username="***", password="***", fossil_group="R",age_range=c(6,8))
	n <- Berg95_to_Grad04(n)
	#Compare the difference of sample density on one or the other scale.
	plot(density(n$sample_age_ma[!is.na(n$sample_age_ma)]))
    lines(density(n$sample_age_ma_Gradstein04[!is.na(n$sample_age_ma_Gradstein04)]), col="red")
}