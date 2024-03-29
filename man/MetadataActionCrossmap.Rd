% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata-actions-crossmap.R
\name{MetadataActionCrossmap}
\alias{MetadataActionCrossmap}
\title{Metadata action: crossmap}
\description{
Metadata action: crossmap

Metadata action: crossmap
}
\details{
This action is to join and split multientry data.
For example, in NIST MSP, \code{Name} is a single compound name,
and all subsequent \code{Synon} are synonyms. In MassBank data,
these need to go all into \code{CH$NAME}. There are now two ways to do this:
\itemize{
\item one: the internal representation is the "joined" one.
On reading from MSP, we merge \code{Name} and \code{Synon} to e.g. \code{names}.
On writing to MassBank, \code{names} -> \code{CH$NAME}.
\item the other: the internal representation is the "separated" one.
On reading from MSP, \code{Name} -> \code{name} and \code{Synon} -> \code{synonyms}
On writing to MassBank, we merge \code{name} and \code{synonyms} to \code{CH$NAME}.
On reading from MassBank, \code{CH$NAME[1]} to \code{name}, all other \code{CH$NAME} entries to \code{synonyms}.
}

This can also \emph{split} a field on \emph{reading}!

\code{source}: fields of origin
\code{target}: target fields
\code{read_split}: After joining the \code{source} fields together, how to distribute them to \code{target}?
E.g. \verb{[1, *]} extracts the first entry into the first \code{target} field and the rest into the second \code{target} field.
\code{write_split}: Same question when writing.
}
\examples{

# Split on write, join on read
 
crossmap <- get_proto_action(
   "crossmap",
   source=c('Name', 'Synon'),
   target='names',
   read_split = list(names="*"),
   write_split = list('Name' = '1', 'Synon' = '*'))

backend <-  get_proto_backend(Name=list('Anton', 'Franz', c()), Synon=list(c(), c('Francis', 'Franziskus'), c()))
fw <- crossmap$execute_read(backend)
fw@variables <- fw@variables \%>\% select(names)
bw <- crossmap$execute_write(fw)

# Split on read, join on write

backend <-  get_proto_backend(CHNAME=list(c('N1', 'N2', 'N3'), c('Francis', 'Franziskus'), c('Nsingle'), c()))
crossmap <- get_proto_action(
   "crossmap",
   source=c('CHNAME'),
   target=c('name', 'synonyms'),
   read_split = list(name="1", synonyms="*"),
   write_split = list(CHNAME="*")
   )
fw <- crossmap$execute_read(backend)
fw@variables <- fw@variables \%>\% select(-CHNAME)
bw <- crossmap$execute_write(fw)

}
\section{Super class}{
\code{\link[SpectraMapping:MetadataActionBase]{SpectraMapping::MetadataActionBase}} -> \code{MetadataActionCrossmap}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MetadataActionCrossmap-process_read}{\code{MetadataActionCrossmap$process_read()}}
\item \href{#method-MetadataActionCrossmap-process_write}{\code{MetadataActionCrossmap$process_write()}}
\item \href{#method-MetadataActionCrossmap-clone}{\code{MetadataActionCrossmap$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="execute_read"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-execute_read'><code>SpectraMapping::MetadataActionBase$execute_read()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="execute_write"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-execute_write'><code>SpectraMapping::MetadataActionBase$execute_write()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="initialize"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-initialize'><code>SpectraMapping::MetadataActionBase$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="log_level"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-log_level'><code>SpectraMapping::MetadataActionBase$log_level()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="merge_settings"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-merge_settings'><code>SpectraMapping::MetadataActionBase$merge_settings()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SpectraMapping" data-topic="MetadataActionBase" data-id="set_settings"><a href='../../SpectraMapping/html/MetadataActionBase.html#method-MetadataActionBase-set_settings'><code>SpectraMapping::MetadataActionBase$set_settings()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MetadataActionCrossmap-process_read"></a>}}
\if{latex}{\out{\hypertarget{method-MetadataActionCrossmap-process_read}{}}}
\subsection{Method \code{process_read()}}{
read implementation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MetadataActionCrossmap$process_read(data, params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{\code{MsBackendMapping} to execute the metadata mapping step on}

\item{\code{params}}{List of parameters for a single action (one \code{params} entry fully merged.)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MetadataActionCrossmap-process_write"></a>}}
\if{latex}{\out{\hypertarget{method-MetadataActionCrossmap-process_write}{}}}
\subsection{Method \code{process_write()}}{
write implementation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MetadataActionCrossmap$process_write(data, params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{\code{MsBackendMapping} to execute the metadata mapping step on}

\item{\code{params}}{List of parameters for a single action (one \code{params} entry fully merged.)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MetadataActionCrossmap-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MetadataActionCrossmap-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MetadataActionCrossmap$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
