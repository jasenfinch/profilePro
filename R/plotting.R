#' plotChromatogram
#' @rdname plotChromatogram
#' @description Plot ion chromatogram from MetaboProfile.
#' @param processed S4 object of class MetaboProfile
#' @param cls sample information column to use for plot colours. 
#' @param group average samples within groups. If cls is NULL, plot average chromatogram across all samples.
#' @param alpha plot line transparancy alpha
#' @param aggregationFun value to pass to \code{xcms::chromatogram()} for chromatogram type. Defaults to base peak ("max").
#' @importFrom ggthemes scale_colour_ptol
#' @importFrom xcms chromatogram
#' @importFrom magrittr set_names
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom ggplot2 ggplot geom_line theme_bw aes labs theme element_text
#' @importFrom dplyr bind_rows group_by summarise

setMethod('plotChromatogram',signature = 'MetaboProfile',
          function(processed, cls = NULL, group = F, alpha = 1, aggregationFun = 'max'){
            x <- processed %>%
              extractProcObject()
            
            nam <- processed %>%
              sampleInfo() %>%
              .$name
            
            pls <- names(x) %>%
              map(~{
                m <- .
                d <- x[[m]]
                chrom <- d %>%
                  chromatogram(aggregationFun = 'max') %>%
                  map(~{
                    tibble(rtime = .@rtime,
                           intensity = .@intensity)
                  }) %>%
                  set_names(nam) %>%
                  bind_rows(.id = 'Sample') %>%
                  mutate(rtime = rtime/60)
                
                if (group == TRUE) {
                  chrom <- chrom %>%
                    mutate(rtime = round(rtime,1))
                }
                
                if (!is.null(cls)) {
                  chrom <- chrom %>%
                    left_join(processed %>%
                                sampleInfo() %>%
                                select(name,Class = cls),by = c('Sample' = 'name'))
                  if (group == TRUE) {
                    chrom <- chrom %>%
                      group_by(Class,rtime) %>%
                      summarise(intensity = mean(intensity))
                  }
                } else {
                  if (group == TRUE) {
                    chrom <- chrom %>%
                      group_by(rtime) %>%
                      summarise(intensity = mean(intensity)) %>%
                      mutate(Sample = 1)
                  }
                }
                
                if (!is.null(cls) & group == TRUE) {
                  pl <- ggplot(chrom,aes(x = rtime,y = intensity,group = Class))
                } else {
                  pl <- ggplot(chrom,aes(x = rtime,y = intensity,group = Sample))
                }
                pl <- pl +
                  theme_bw() +
                  labs(title = m,
                       x = 'Retention Time (minutes)',
                       y = 'Intensity') +
                  theme(plot.title = element_text(face = 'bold'),
                        axis.title = element_text(face = 'bold'),
                        legend.title = element_text(face = 'bold'),
                        legend.position = 'bottom')
                
                if (!is.null(cls)) {
                  pl <- pl +
                    geom_line(aes(colour = Class),alpha = alpha)
                  
                  if (length(unique(chrom$Class)) < 12) {
                    pl <- pl +
                      scale_colour_ptol()
                  }
                } else {
                  pl <- pl +
                    geom_line(alpha = alpha)
                }
                
                return(pl)
              })
            
            pls <- wrap_plots(pls)
            
            return(pls + plot_layout(ncol = 1))
            
          }
)