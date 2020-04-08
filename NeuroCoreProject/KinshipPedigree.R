
KinshipPedigree <- function(pedigree)
{
  require(kinship2)
  
  if (all(c("id","sire","dam") %in% names(pedigree))) #if all column names = id, sire, dam
    # %in% identify if an element belongs to a vector
  {
    pedigree <- as(2*kinship(pedigree$id,dadid=pedigree$sire,momid=pedigree$dam),"dsCMatrix") #
    #kinship function to determine kinship relationship from individual, mom and dad IDs
    #as() coerce an object to a given class, here dsCMatrix = class of symmetric, sparse numeric matrices in the compressed form 
  } else
    pedigree <- as(2*kinship(pedigree[,1],dadid=pedigree[,2],momid=pedigree[,3]),"dsCMatrix")
  
  return(pedigree)
}