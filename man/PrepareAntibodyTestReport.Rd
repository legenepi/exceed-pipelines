% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrepareAntibodyTestReport.R
\docType{class}
\name{PrepareAntibodyTestReport}
\alias{PrepareAntibodyTestReport}
\title{PrepareAntibodyTestReport class}
\format{
An R6 class object.
}
\description{
PrepareAntibodyTestReport class
}
\details{
Prepare antibody test report.
}
\section{Super classes}{
\code{\link[exceedapi:SnapshotBackend]{exceedapi::SnapshotBackend}} -> \code{\link[exceedapi:API]{exceedapi::API}} -> \code{\link[exceedapi:Step]{exceedapi::Step}} -> \code{\link[exceed.pipelines:GenericExport]{exceed.pipelines::GenericExport}} -> \code{PrepareAntibodyTestReport}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-transform}{\code{PrepareAntibodyTestReport$transform()}}
\item \href{#method-clone}{\code{PrepareAntibodyTestReport$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="exceedapi" data-topic="Step" data-id="add_step">}\href{../../exceedapi/html/Step.html#method-add_step}{\code{exceedapi::Step$add_step()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceedapi" data-topic="Step" data-id="add_step_object">}\href{../../exceedapi/html/Step.html#method-add_step_object}{\code{exceedapi::Step$add_step_object()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceedapi" data-topic="Step" data-id="inverse_transform">}\href{../../exceedapi/html/Step.html#method-inverse_transform}{\code{exceedapi::Step$inverse_transform()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceedapi" data-topic="Step" data-id="progress_bar">}\href{../../exceedapi/html/Step.html#method-progress_bar}{\code{exceedapi::Step$progress_bar()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceedapi" data-topic="Step" data-id="run">}\href{../../exceedapi/html/Step.html#method-run}{\code{exceedapi::Step$run()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="calulate_checksums">}\href{../../exceed.pipelines/html/GenericExport.html#method-calulate_checksums}{\code{exceed.pipelines::GenericExport$calulate_checksums()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_archive">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_archive}{\code{exceed.pipelines::GenericExport$create_archive()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_codebook">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_codebook}{\code{exceed.pipelines::GenericExport$create_codebook()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_encrypted_archive">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_encrypted_archive}{\code{exceed.pipelines::GenericExport$create_encrypted_archive()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_manifest">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_manifest}{\code{exceed.pipelines::GenericExport$create_manifest()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_manifest_csv">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_manifest_csv}{\code{exceed.pipelines::GenericExport$create_manifest_csv()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_manifest_pdf">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_manifest_pdf}{\code{exceed.pipelines::GenericExport$create_manifest_pdf()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_report">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_report}{\code{exceed.pipelines::GenericExport$create_report()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="create_unencrypted_archive">}\href{../../exceed.pipelines/html/GenericExport.html#method-create_unencrypted_archive}{\code{exceed.pipelines::GenericExport$create_unencrypted_archive()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="export_tables">}\href{../../exceed.pipelines/html/GenericExport.html#method-export_tables}{\code{exceed.pipelines::GenericExport$export_tables()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="generate_password">}\href{../../exceed.pipelines/html/GenericExport.html#method-generate_password}{\code{exceed.pipelines::GenericExport$generate_password()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="get_config">}\href{../../exceed.pipelines/html/GenericExport.html#method-get_config}{\code{exceed.pipelines::GenericExport$get_config()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="get_source_path">}\href{../../exceed.pipelines/html/GenericExport.html#method-get_source_path}{\code{exceed.pipelines::GenericExport$get_source_path()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="get_version">}\href{../../exceed.pipelines/html/GenericExport.html#method-get_version}{\code{exceed.pipelines::GenericExport$get_version()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="initialize">}\href{../../exceed.pipelines/html/GenericExport.html#method-initialize}{\code{exceed.pipelines::GenericExport$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="make_filename">}\href{../../exceed.pipelines/html/GenericExport.html#method-make_filename}{\code{exceed.pipelines::GenericExport$make_filename()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="prepare_export">}\href{../../exceed.pipelines/html/GenericExport.html#method-prepare_export}{\code{exceed.pipelines::GenericExport$prepare_export()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="render_markdown">}\href{../../exceed.pipelines/html/GenericExport.html#method-render_markdown}{\code{exceed.pipelines::GenericExport$render_markdown()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="show_config">}\href{../../exceed.pipelines/html/GenericExport.html#method-show_config}{\code{exceed.pipelines::GenericExport$show_config()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="show_summary">}\href{../../exceed.pipelines/html/GenericExport.html#method-show_summary}{\code{exceed.pipelines::GenericExport$show_summary()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="write_csv">}\href{../../exceed.pipelines/html/GenericExport.html#method-write_csv}{\code{exceed.pipelines::GenericExport$write_csv()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="exceed.pipelines" data-topic="GenericExport" data-id="write_lines">}\href{../../exceed.pipelines/html/GenericExport.html#method-write_lines}{\code{exceed.pipelines::GenericExport$write_lines()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-transform"></a>}}
\if{latex}{\out{\hypertarget{method-transform}{}}}
\subsection{Method \code{transform()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PrepareAntibodyTestReport$transform(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PrepareAntibodyTestReport$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
