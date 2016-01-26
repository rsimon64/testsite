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
        if (val != "NA" & !is.na(val)) {
        txt = paste0(txt, "\n{name: \"", stringr::str_trim(names(rec)[j]),"\",")
        if(stringr::str_detect(val, "[0-9]{1}\\.[0-9]{1,7}")
           & !stringr::str_detect(val, " x ")
           & !stringr::str_detect(names(rec)[j], "Collecting number")) {
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
  get_img_attr <- function(acc_id){
    imgs <- list()
    get_img_lnk <- function(acc_id, img_type){
      id = stringr::str_replace(acc_id, "CIP", "")
      fp = paste0(img_type,"/", id, ".jpg\"")
      if (!file.exists(fp)) return(NA)
      paste0(
        "\n{name: \"",img_type, " image\", valu: \"", fp, "}, "
      )
    }
    imgs[1] = get_img_lnk(acc_id, "Tuber")
    imgs[2] = get_img_lnk(acc_id, "Fry")
    imgs[3] = get_img_lnk(acc_id, "Chips")
    imgs[4] = get_img_lnk(acc_id, "Sprout")
    imgs[5] = get_img_lnk(acc_id, "Flower")
    imgs[6] = get_img_lnk(acc_id, "Finger")
    fil = !is.na(imgs)
    imgs = imgs[fil]
    paste(imgs, collapse = "\n")
  }
  imgs = get_img_attr(rec$`Accession number`)

  txt = paste0("\n'", rec$`Accession number`, "': {",
    "name: ", "\"",rec$`Accession number`,"\",",
    "description: ", "\"",  desc,"\",",
    "\nattr: [",
    imgs,
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
txt = stringr::str_replace(txt, "\\n", "")
write(txt, "itemData.js")


