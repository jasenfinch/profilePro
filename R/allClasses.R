#' @export

setClass('ProfileParameters',
         slots = list(
           technique = 'character',
           parameters = 'list'
         )
)


#' @export
setClass('MetaboProfile',
         slots = list(
           log = 'character',
           Info = 'tbl_df',
           Data = 'list',
           processingResults = 'list'
         )
)