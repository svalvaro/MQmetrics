#' Protein coverage and degradation
#'
#' @param peptides
#'
#' @return
#' @export
#'
#' @examples
PlotProteinCoverage <- function(peptides){
  table_peptides <- peptides %>% select(contains(c( 'Intensity', 'Start position',
                                                    'End position'))) %>%
    select(-contains('LFQ')) %>%  select(-'Intensity')

  pep_melt <- melt(table_peptides, id.vars = c('Start position', 'End position'))

  # If intensity is 0, remove it.

  pep_melt <- pep_melt[!pep_melt$value==0,]


    # ggplot(peptides, aes(x = `Start position`, y = `End position`))+
    #   geom_point()+
    #   theme_bw()


   ggplot(pep_melt , aes(x = `Start position`, y = log10(value)))+
        geom_hex(bins = 20)+
        theme_bw()+
        facet_wrap(.~ variable, ncol =1)

   # ggplot(pep_melt , aes(x = `Start position`, y = log10(value)))+
   #      geom_point()+
   #      theme_bw()+
   #      facet_wrap(.~ variable, ncol =1)


}
