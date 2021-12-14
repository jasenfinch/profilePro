#' Plot ion chromatogram
#' @rdname plotChromatogram
#' @description Plot ion chromatogram from MetaboProfile.
#' @param processed_data S4 object of class MetaboProfile
#' @param cls sample information column to use for plot colours. 
#' @param group average samples within groups. If cls is NULL, plot average chromatogram across all samples.
#' @param alpha plot line transparancy alpha
#' @param aggregationFun value to pass to \code{xcms::chromatogram()} for chromatogram type. Defaults to base peak ("max").
#' @param ... arguments to pass to \code{erah::plotChr()}
#' @importFrom ggthemes scale_colour_ptol
#' @importFrom xcms chromatogram
#' @importFrom magrittr set_names
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom ggplot2 ggplot geom_line theme_bw aes labs theme element_text
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom erah plotChr

setMethod('plotChromatogram',signature = 'MetaboProfile',
          function(processed_data, 
                   cls = NULL, 
                   group = FALSE, 
                   alpha = 1, 
                   aggregationFun = 'max', 
                   ...){
            
            info <- processed_data %>% 
              sampleInfo()
            
            if (str_detect(technique(processed_data),'eRah')) {
              processed_data %>%
                extractProcObject() %>%
                plotChr(...)
            } else {
              x <- processed_data %>%
                extractProcObject()
              
              if (!is.list(x)) {
                x <- list(x)
              }
              
              chrom <- x %>%
                map(~{
                  .x %>%
                    as.MSnExp.OnDiskMSnExp() %>% 
                    chromatogram(aggregationFun = aggregationFun) %>%
                    map(~{
                      tibble(rtime = .@rtime,
                             intensity = .@intensity)
                    }) %>%
                    set_names(info$name) %>%
                    bind_rows(.id = 'Sample') %>%
                    mutate(rtime = rtime/60)
                }) %>% 
                set_names(names(x)) %>% 
                bind_rows(.id = 'mode')
              
              if (group == TRUE) {
                chrom <- chrom %>%
                  mutate(rtime = round(rtime,1))
              }
              
              if (!is.null(cls)) {
                chrom <- chrom %>%
                  left_join(info %>%
                              select(name,Class = cls),by = c('Sample' = 'name'))
                if (group == TRUE) {
                  chrom <- chrom %>%
                    group_by(mode,Class,rtime) %>%
                    summarise(intensity = mean(intensity))
                }
              } else {
                if (group == TRUE) {
                  chrom <- chrom %>%
                    group_by(mode,rtime) %>%
                    summarise(intensity = mean(intensity)) %>%
                    mutate(Sample = 1)
                }
              }
              
              pls <- chromPlot(chrom,
                               cls = cls,
                               group = group,
                               alpha = alpha,
                               mode = mode)
              
              return(pls)
            }
            
          }
)

#' @importFrom ggplot2 element_blank element_line scale_x_continuous scale_y_continuous
#' @importFrom stringr str_replace_all
chromPlot <- function(chrom,
                      cls,
                      group,
                      alpha,
                      mode = NA){
  
  chrom <- chrom %>% 
    mutate(mode = str_replace_all(mode,
                                  'n',
                                  'Negative Mode') %>% 
             str_replace_all('p',
                             'Positive Mode') %>% 
             str_replace_all('1',
                             ''))
  
  title <- ifelse(length(unique(chrom$mode)) > 1,
                  'Ion Chromatograms',
                  'Ion Chromatogram')
  
  if (!is.null(cls) & group == TRUE) {
    pl <- ggplot(chrom,aes(x = rtime,
                           y = intensity,
                           group = Class))
  } else {
    pl <- ggplot(chrom,aes(x = rtime,
                           y = intensity,
                           group = Sample))
  }
  
  pl <- pl +
    theme_bw() +
    labs(title = title,
         x = 'Retention Time (minutes)',
         y = 'Intensity') +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold',
                                    size = 10)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~mode,
               ncol = 1,
               scales = 'free')
  
  if (!is.null(cls)) {
    pl <- pl +
      geom_line(aes(colour = Class),alpha = alpha) +
      guides(colour = guide_legend(title = cls))
    
    if (length(unique(chrom$Class)) < 12) {
      pl <- pl +
        scale_colour_ptol()
    }
  } else {
    pl <- pl +
      geom_line(alpha = alpha)
  }
  
  return(pl)
}

#' Plot total ion counts
#' @rdname plotTIC
#' @description Plot sample total ion counts.
#' @param processed S4 object of class MetaboProfile
#' @param by info column to plot against
#' @param colour info column to provide colour labels  
#' @importFrom ggplot2 geom_hline geom_point facet_wrap guides guide_legend
#' @importFrom stats median IQR
#' @importFrom ggthemes scale_fill_ptol
#' @importFrom stringr str_detect

setMethod('plotTIC',signature = 'MetaboProfile',
          function(processed,by = 'injOrder', colour = 'block'){
            info <- processed %>%
              sampleInfo()
            
            if (str_detect(technique(processed),'GCMS')) {
              d <- processed %>%
                processedData() %>%
                {tibble(TIC = rowSums(.),
                        Colour = info[,colour] %>% deframe() %>% factor(),
                        Index = info[,by] %>% deframe()
                )}
              
              TICmedian <- d %>%
                summarise(Median = median(TIC),
                          Q1 = Median - IQR(TIC),
                          Q3 = Median + IQR(TIC),
                          LowerOut = Q1 - IQR(TIC) * 1.5,
                          UpperOut = Q3 + IQR(TIC) * 1.5)
            } else {
              d <- processed %>%
                processedData() %>%
                map(~{
                  rowSums(.) %>%
                    tibble(TIC = .) %>%
                    mutate(Colour = info[,colour] %>% unlist() %>% factor(),
                           Index = info[,by] %>% unlist())
                }) %>%
                bind_rows(.id = 'Mode')
              
              TICmedian <- d %>%
                group_by(Mode) %>%
                summarise(Median = median(TIC),
                          Q1 = Median - IQR(TIC),
                          Q3 = Median + IQR(TIC),
                          LowerOut = Q1 - IQR(TIC) * 1.5,
                          UpperOut = Q3 + IQR(TIC) * 1.5)  
            }
            
            TICmedian[TICmedian < 0] <- 0
            
            pl <- ggplot(d,aes(x = Index,y = TIC,fill = Colour)) +
              geom_hline(data = TICmedian,aes(yintercept = Median)) +
              geom_hline(data = TICmedian,aes(yintercept = Q1),linetype = 2) +
              geom_hline(data = TICmedian,aes(yintercept = Q3),linetype = 2) +
              geom_hline(data = TICmedian,aes(yintercept = LowerOut),linetype = 3) +
              geom_hline(data = TICmedian,aes(yintercept = UpperOut),linetype = 3) +
              geom_point(shape = 21) +
              theme_bw() +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold')) +
              labs(title = 'Sample TICs',
                   caption = 'The solid line shows the median TIC across the sample set. 
The dashed line shows the inter-quartile range (IQR) and 
the dotted line shows the outlier boundary (1.5 X IQR).',
                   y = 'Total Ion Count',
                   x = by) +
              guides(colour = guide_legend(title = colour))
            
            if (!str_detect(technique(processed),'GCMS')) {
              pl <- pl +
                facet_wrap(~Mode)
            }
            
            if (length(unique(d$Colour)) <= 12) {
              pl <- pl +
                scale_fill_ptol()
            }
            return(pl)
          }
)
