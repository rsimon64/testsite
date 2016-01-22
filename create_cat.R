# read data

dfl = "D:/data/catalogs/potato_2014/data-dict-add.xlsx"
dic = readxl::read_excel(dfl)

code_to_val <- function(avar, acode){
  out = acode
  if(is.na(acode)) return("NA")
  if(acode == "-") return("NA")
  if(acode == "") return("NA")
  if(acode == " ") return("NA")
  #print(acode)
  
  try({
    rec = dic[dic$Name == avar, ]
    if(nrow(rec) == 1){
      x = rec[which(names(rec) == acode)]
      #print(x)
      #if(!is.na(x)) 
      out = x
    } 
  })
  as.character(out)
}


fil = "D:/data/catalogs/potato_2014/Catalogo2014_03_28.xls"
dat = readxl::read_excel(fil)

vrs = dat[dat$`Variety or Clone` == "V", ]
cls = dat[dat$`Variety or Clone` == "C", ]

to_item = function(x){
  txt = ""
  for(i in 1:nrow(x)) {
    txt = paste0(txt, "{name: \"", x$`Accession number`[i], "\"}")
    if(i < nrow(x)) txt  = paste(txt, ",\n")
  }
txt  
}
ccls = to_item(cls)
cvrs = to_item(vrs)


txt = paste("var categoryData = {
	varieties: {
            name: \"_PVT\",
            description: \"_PVC\",
            items: [\n", cvrs, 
            "]
	},
            clones: {
            name: \"_PACT\",
            description: \"_PACC\",
            items: [
            ",
            ccls,
            "\n]}	
};
            "
            )

write(txt, "categoryData.js")


to_cat_item <- function(rec){
  attr_to_json <- function(rec){
    txt = ""
    for(j in 12:59){
      txt = paste0(txt, "\nname: \"",names(rec)[j],"\",")
      val = code_to_val(names(rec)[j], as.character(rec[j]))
      
      txt = paste0(txt, "\nvalu: \"",val,"\"\n}")
      if(j < 59) txt = paste0(txt, ",{")
    }
    txt
  }
  atts = attr_to_json(rec)
  desc = ""
  if(!is.na(rec$Name)) desc = paste0(desc, rec$Name)
  if(!is.na(rec$Description)) desc = paste0(desc, ": ", rec$Description)
  
  
  txt = paste0("\n'", rec$`Accession number`, "': {",
    "name: ", "\"",rec$`Accession number`,"\",",
    "description: ", "\"",  desc,"\",",
    "\nattr: [{",
       "\nname: \"Tuber image\",",
      "\nvalu: \"tuber/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    "\nname: \"Fry image\",",
    "\nvalu: \"fry/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    "\nname: \"Chips image\",",
    "\nvalu: \"chips/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    
    "\nname: \"Sprout image\",",
    "\nvalu: \"sprout/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    
    "\nname: \"Plant image\",",
    "\nvalu: \"plant/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    "\nname: \"Flower image\",",
    "\nvalu: \"flower/", stringr::str_replace(rec$`Accession number`,"CIP",""),".jpg\"},\n{",
    "\nname: \"Fingerprint image\",",
    "\nvalu: \"finger/", rec$`Accession number`,".jpg\"},\n{",
    "\nname: \"CIP population group\",",
    "\nvalu: \"", rec$`Population Group`, "\"},\n{",
    "\nname: \"Female parent\",",
    "\nvalu: \"", rec$`Parent Female`, "\"},\n{",
    "\nname: \"Male parent\",",
    "\nvalu: \"", rec$`Parent Male`, "\"}, \n{",
    atts,
     
    "]}"
    ,""           )
  txt
}
txt = "var itemData = {"
for(i in 1:nrow(dat)){
  txt = paste0(txt, to_cat_item(dat[i, ]))
  if( i < nrow(dat)) txt = paste0(txt, ",\n ")
}
txt = paste0(txt, "};" )

write(txt, "itemData.js")


