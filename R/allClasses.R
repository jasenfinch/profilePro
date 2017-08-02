#' @export

setClass('ProfileParameters',
         slots = list(
           technique = 'character',
           infoName = 'character',
           processingParameters = 'list'
         )
)


#' @export

setClass('MetaboProfile',
         slots = list(
           log = 'character',
           files = 'character',
           processingParameters = 'ProfileParameters',
           Info = 'tbl_df',
           Data = 'list',
           processingResults = 'list'
         )
)