# Find all actions

.onLoad <- function(pkgname, libname) {
  options(SpectraMapping = list(verbose = 1))  
}




.actions_registry <- list(
  mapping = MetadataActionMapping,
  nesting = MetadataActionNest,
  extract = MetadataActionExtract,
  translate = MetadataActionTranslate,
  crossmap = MetadataActionCrossmap,
  type = MetadataActionType,
  mutate = MetadataActionMutate,
  tabular = MetadataActionTabular,
  split = MetadataActionSplit,
  default = MetadataActionDefault,
  order = MetadataActionOrder
)

