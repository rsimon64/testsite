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


fil = "D:/data/catalogs/potato_2014/Catalogo2016_01_26.xls"
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
    for (j in 6:61){
      val = code_to_val(names(rec)[j], as.character(rec[j]))
      val = stringr::str_trim(val)
      if (length(val) > 0 ) {
        if (val != "NA") {
        txt = paste0(txt, "\n{name: \"", stringr::str_trim(names(rec)[j]),"\",")
        if(stringr::str_detect(val, "[0-9]{1}\\.[0-9]{1,7}")) {
          val = as.character(round(as.numeric(val), 2))
          #val = sprintf("%.2f", round(val, 2))
          #cat(val)
        }
        txt = paste0(txt, " valu: \"",val,"\"},")
        #if (j < 61) txt = paste0(txt, ",")        
        }
      }
    }
    # Remove trailing comma
    txt = stringr::str_sub(txt,1, (stringr::str_length(txt)-1))
    txt
  }
  atts = attr_to_json(rec)
  desc = ""
  if(!is.na(rec$`Accession name`)) desc = paste0(desc, rec$`Accession name`)
  if(!is.na(rec$Description) & rec$Description != "NA") desc = paste0(desc, ": ", 
                                            stringr::str_trim(rec$Description))
  
  
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
    "\nvalu: \"finger/", rec$`Accession number`,".jpg\"},\n",
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


