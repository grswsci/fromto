% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fromto.R
\name{fromto}
\alias{fromto}
\title{fromto}
\usage{
fromto(genes, from, to)
}
\arguments{
\item{genes}{gene names}

\item{from}{one type of gene name}

\item{to}{another type of gene name}

\item{You}{can freely convert the following GeneIDs!
Symbol
NCBI_GeneID
HGNC_GeneID
SwissProt_GeneID
Ensembl_GeneID
OMIM_GeneID
Proteins_GeneID
Description_GeneID
Transcripts_GeneID
Gene_Group_Identifier_GeneID
Synonyms_GeneID}
}
\value{
data frame
}
\description{
Converted gene names
}
\examples{
# examples
GeneNames = c("PDCD1","CD274","MKI67")
df = fromto(genes = GeneNames, from = "Symbol",to = "NCBI_GeneID")
print(df)

}
