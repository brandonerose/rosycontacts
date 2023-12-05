
validate_vcf <- function(raw_vcf,silent=F){
  all(
    any(startsWith(raw_vcf,"BEGIN:VCARD"))&
      any(startsWith(raw_vcf,"END:VCARD"))&
      any(startsWith(raw_vcf,"N:"))&
      any(startsWith(raw_vcf,"FN:"))
  )
  if(!silent)message("Valid VCF file!")
  raw_vcf
}

validate_rosycontacts<- function(rosycontacts,silent = T){
  if(any(!names(rosycontacts)%in%names(empty_rosycontacts)))stop("not a valid rosycontacts object")
  if(!silent)message("Valid rosycontacts object!")
  rosycontacts
}

load_vcf<-function(file_path,rosycontacts){
  # IMPORT ----
  if(missing(rosycontacts)){
    rosycontacts<- empty_rosycontacts
  }
  rosycontacts <- rosycontacts %>% validate_rosycontacts()
  vcf <- raw_vcf <- file_path %>% readLines() %>% validate_vcf()
  # RUN ----
  out <- process_vcf(vcf)
  # SAVE ----
  rosycontacts$original[["raw_vcf"]] <- raw_vcf
  rosycontacts$original[["list_lines"]] <-out$contact_list_original
  rosycontacts$original[["list"]] <- out$contact_list
  rosycontacts$original[["long"]] <- out$contacts_long %>% clean_tel_numbers() %>% check_names()
  rosycontacts$original[["wide"]] <- long_to_wide(rosycontacts$original[["long"]])
  return(rosycontacts)
}

long_to_list <- function (contacts_long) {
  contacts_long <- contacts_long %>% clean_df_of_NA()
  contact_ids <- contacts_long$contact_id %>% unique()
  out_list <- NULL
  for (contact_id in contact_ids) {
    contacts_long_ <- contacts_long[which(contacts_long$contact_id==contact_id),]
    out_list_ <- "BEGIN:VCARD"
    out_list_ <- out_list_ %>% append("VERSION:3.0")
    for (i in 1:nrow(contacts_long_)) {
      out_list_ <- out_list_ %>% append(paste0(contacts_long_$element[i],contacts_long_$type[i],":",contacts_long_$content[i])) #%>% wrap_string_to_lines(75,spacer = " "))
    }
    out_list_ <- out_list_ %>% append("END:VCARD")
    out_list<- out_list %>% append(out_list_)
  }
  out_list
}

list_to_vcf <- function (list) {
  out_list <- NULL
  for (i in 1:length(list)) {
    out_list <- out_list %>% append(list[i] %>% wrap_string_to_lines(75,spacer = " "))
  }
  out_list
}

wide_import <- function (file_path) {
  contacts_wide <- file_path %>% rio::import() %>% clean_df_of_NA()
  contacts_wide$contact_id <- 1:nrow(contacts_wide)
  return(contacts_wide)
}

long_to_wide <- function(contacts_long){
  contacts_long <-contacts_long %>% clean_df_of_NA()
  contacts_wide <- NULL
  for (i in unique(contacts_long$contact_id)){
    contacts_long_ <- contacts_long[which(contacts_long$contact_id==i),]
    c<-which(contacts_long_$element%in%c("EMAIL","TEL","URL","ADR","PHOTO"))
    if(length(c)>0){
      a<- contacts_long_$wide_column[c]
      b<- contacts_long_$type[c]
      contacts_wide_ <- contacts_long_$content %>% append(b) %>% t() %>% as.data.frame()
      colnames(contacts_wide_)<-contacts_long_$wide_column %>% append(paste0(a,"_type"))
    }else{
      contacts_wide_ <- contacts_long_$content %>% t() %>% as.data.frame()
      colnames(contacts_wide_)<-contacts_long_$wide_column
    }
    contacts_wide_$contact_id <- i
    contacts_wide <-contacts_wide %>% dplyr::bind_rows(contacts_wide_)
  }
  N_split <- contacts_wide$N %>% stringr::str_split(";")
  contacts_wide$FN_4_LASTNAME<-N_split %>% sapply(function(IN){IN[1]})
  contacts_wide$FN_2_FIRSTNAME<-N_split %>% sapply(function(IN){IN[2]})
  contacts_wide$FN_3_ADDITIONALNAME<-N_split %>% sapply(function(IN){IN[3]})
  contacts_wide$FN_1_PREFIX<-N_split %>% sapply(function(IN){IN[4]})
  contacts_wide$FN_5_SUFFIX<-N_split %>% sapply(function(IN){IN[5]})
  contacts_wide$FN <- NULL
  contacts_wide$N <- NULL
  contacts_wide$TEL_dup <- contacts_wide$contact_id %>% sapply(function(ID){
    dup_rows <- which(contacts_long$contact_id==ID&contacts_long$element=="TEL"&as.logical(contacts_long$is_dup))
    if(length(dup_rows)>0){
      all_dup_ids <- contacts_long$contact_id[which(contacts_long$element=="TEL"&contacts_long$content%in%contacts_long$content[dup_rows])] %>%
        unique() %>%
        paste0(collapse = " | ")
      return(all_dup_ids)
    }else{
      return("")
    }
  })
  contacts_wide$name_dup <- contacts_wide$contact_id %>% sapply(function(ID){
    contacts_long$name_dup[which(contacts_long$contact_id==ID&contacts_long$element=="N")]
  })
  return(contacts_wide[,order(colnames(contacts_wide))])
}

wide_to_long <- function(contacts_wide){
  contacts_wide <- contacts_wide %>% clean_df_of_NA()
  contacts_long <- NULL
  contacts_wide$N <- paste0(
    contacts_wide$FN_4_LASTNAME,";",
    contacts_wide$FN_2_FIRSTNAME,";",
    contacts_wide$FN_3_ADDITIONALNAME,";",
    contacts_wide$FN_1_PREFIX,";",
    contacts_wide$FN_5_SUFFIX,";"
  )
  contacts_wide$FN <- paste0(
    contacts_wide$FN_1_PREFIX," ",
    contacts_wide$FN_2_FIRSTNAME," ",
    contacts_wide$FN_3_ADDITIONALNAME," ",
    contacts_wide$FN_4_LASTNAME," ",
    contacts_wide$FN_5_SUFFIX
  )
  contacts_wide$FN <- gsub("  "," ",contacts_wide$FN)
  contacts_wide$FN <- gsub("  "," ",contacts_wide$FN)
  contacts_wide$FN <- gsub("  "," ",contacts_wide$FN)
  contacts_wide$FN <- gsub("  "," ",contacts_wide$FN)
  contacts_wide$FN <- trimws(contacts_wide$FN)
  contacts_wide$FN_4_LASTNAME<-NULL
  contacts_wide$FN_2_FIRSTNAME<-NULL
  contacts_wide$FN_3_ADDITIONALNAME<-NULL
  contacts_wide$FN_1_PREFIX<-NULL
  contacts_wide$FN_5_SUFFIX<-NULL
  for (i in contacts_wide$contact_id){
    contacts_wide_ <- contacts_wide[which(contacts_wide$contact_id==i),]
    contacts_wide_$contact_id<-NULL
    contacts_long_ <-data.frame(
      contact_id = i,
      element = "",
      content = as.character(contacts_wide_),
      type = "",
      wide_column = colnames(contacts_wide_)
    )
    y<-grep("_type",contacts_long_$wide_column,invert = F)
    if(length(y)>0){
      contacts_long_type_ <- contacts_long_[y,]
      contacts_long_ <- contacts_long_[grep("_type",contacts_long_$wide_column,invert = T),]
      contacts_long_type_$wide_column <- gsub("_type","",contacts_long_type_$wide_column)
      contacts_long_type_$type <- contacts_long_type_$content
      contacts_long_type_$content <- NULL
      for (j in 1:nrow(contacts_long_type_)){
        contacts_long_$type[which(contacts_long_$wide_column==contacts_long_type_$wide_column[j])] <- contacts_long_type_$type[j]
      }
    }
    contacts_long_$element <- contacts_long_$wide_column %>% strsplit(split = "_") %>% sapply(function(IN){IN[1]})
    contacts_long <- contacts_long %>% dplyr::bind_rows(contacts_long_)
  }
  contacts_long <- contacts_long[which(contacts_long$content!=""),]
  contacts_long <- contacts_long %>% clean_df_of_NA()
  return(contacts_long)
}

clean_tel_numbers <- function(contacts_long){
  contacts_long <- contacts_long %>% clean_df_of_NA()
  contacts_long$content[which(contacts_long$element=="TEL")] <- gsub("[()]|[)]| |[-]|\\\\","",contacts_long$content[which(contacts_long$element=="TEL")])
  #drop US international codes
  teles<- contacts_long$content[which(contacts_long$element=="TEL")]

  teles[which(startsWith(teles,"1")&stringr::str_length(teles)==11)] <-
    teles[which(startsWith(teles,"1")&stringr::str_length(teles)==11)] %>%
    stringr::str_trunc(10,side="left",ellipsis = "") #paste0("+1-", #decide whether to include international code

  teles[which(startsWith(teles,"+1")&stringr::str_length(teles)==12)] <-
    teles[which(startsWith(teles,"+1")&stringr::str_length(teles)==12)] %>%
    stringr::str_trunc(10,side="left",ellipsis = "")

  teles[which(stringr::str_length(teles)==10&numbers_only(teles))] <-
    teles[which(stringr::str_length(teles)==10&numbers_only(teles))] %>%
    convert_to_phone_format()
  pauses <- teles %>% strsplit(",") %>% sapply(length) %>% magrittr::is_greater_than(1) %>% which()
  if(length(pauses)>0){
    teles[pauses] <- teles[pauses] %>% strsplit(",") %>% sapply(function(IN){
      phone_number <-convert_to_phone_format(IN[1])
      pauses2 <-paste0(IN[-1],collapse = ",")
      c(phone_number,pauses2) %>% paste0(collapse = ",")
    })
  }
  teles[which(stringr::str_length(teles)==10&numbers_only(teles))] <-
    teles[which(stringr::str_length(teles)==10&numbers_only(teles))] %>%
    convert_to_phone_format()

  teles[which(startsWith(teles,"+"))]
  dups <- teles[teles %>% duplicated() %>% which()] %>% unique()

  contacts_long$content[which(contacts_long$element=="TEL")] <- teles

  contacts_long$is_dup <- NA
  contacts_long$is_dup[which(contacts_long$element=="TEL")] <- F
  contacts_long$is_dup[which(contacts_long$element=="TEL"&contacts_long$content%in%dups)]<-T
  contacts_long <- contacts_long %>% clean_df_of_NA()
  return(contacts_long)
}

check_names <- function(contacts_long){
  contacts_long <- contacts_long %>% clean_df_of_NA()
  x<-contacts_long$content[which(contacts_long$element=="N")]
  x<-x[which(x!=";;;;;")]
  x<-x[duplicated(x)]
  contacts_long$content <- gsub("’","'",contacts_long$content)
  contacts_long$name_dup<-contacts_long$content %>% sapply(function(IN){
    x<-which(contacts_long$element=="N"&contacts_long$content==IN)
    if(length(x)>1){
      contacts_long$contact_id[x] %>% paste0(collapse = " | ") %>% return()
    }else{return("")}
  })
  contacts_long <- contacts_long %>% clean_df_of_NA()
  return(contacts_long)
}

process_vcf <- function(vcf){
  drops <- c("##fileformat=VCFv ","VERSION:","PRODID:")
  for (drop in drops){
    vcf<-vcf[which(!startsWith(vcf,drop))]
  }
  start <- which(vcf=="BEGIN:VCARD")
  end <- which(vcf=="END:VCARD")
  wild <-".*?(?=:|;)"
  vcf_unique <- stringr::str_extract(vcf, wild) %>% unique() %>% drop_nas()
  vcf_unique <- vcf_unique[which(!vcf_unique%in%c("BEGIN","END","PRODID"))]
  contact_list_original<-list()
  contact_list<-list()
  contacts_long <- NULL
  contact_ids <- 1:length(start)
  i<-257
  for (i in contact_ids){
    #i<- contact_ids %>% sample(1)
    # print(i)
    x<-vcf[(start[i]+1):(end[i]-1)]
    cols  <- stringr::str_extract(x, wild)
    if("PHOTO"%in%cols){
      all <- 1:length(x)
      k<-which(cols=="PHOTO")
      real<-which(!is.na(cols))
      unreal<-which(is.na(cols))
      other<- real[which(real>k)]
      photo_rows <- all[which(all>k&all%in%unreal&all<=ifelse(length(other)>0,other,length(x)))]
      photo_rows <- c(k,photo_rows)
      photo <- x[photo_rows] %>% trimws() %>% paste0(collapse = "")
      x<-x[-photo_rows]
      x<-x %>% append(photo)
    }
    cols  <- stringr::str_extract(x, wild)
    cols
    contact_list_original[i]<-list(x)
    item_cols<-cols[which(stringr::str_starts(cols,"item"))]
    if(length(item_cols)>0){
      print(i)
      items <- item_cols %>% strsplit("\\.") %>% sapply(function(IN){IN[[1]]}) %>% unique()
      item <- items %>% sample(1)
      for(item in items){
        cols2<- cols[which(stringr::str_starts(cols,item))]
        z <- cols2 %>% strsplit("\\.") %>% sapply(function(IN){IN[[2]]})
        e<- paste0(item,".",z[1])
        f<- paste0(item,".",z[2])
        a<-stringr::str_starts(x,e)
        b<-stringr::str_starts(x,f)
        c<- x[which(a)]
        d<- x[which(b)]
        c<-c %>% strsplit(":") %>% unlist()
        d<-d %>% strsplit(":") %>% unlist()
        x <- x[which(!(a|b))]
        c<-gsub(e,z[1],c)
        d<-gsub(f,z[2],d)
        g<-c[1] %>% strsplit(";") %>% unlist()
        h <- g[-1]
        types <- NULL
        if(length(h)>0){
          h<-gsub("type=","",h)
        }
        x[length(x)+1] <- paste0(z[1],";",paste0("type=",h,collapse = ";"),":",c[2])
        # print(z)
      }
      cols  <- stringr::str_extract(x, wild)
    }
    contact_list[i]<-list(x)
    contacts_long_ <- NULL
    for(j in 1:length(x)){
      y <- x[j] %>% strsplit(":") %>% unlist()
      element <- cols[j]
      type <- gsub(element,"",y[1])
      # type <- gsub(";type="," | ",type)
      # type <- stringr::str_replace( type," [|] ","")
      if(!element%in%c("PRODID")){
        contacts_long_ <- contacts_long_ %>% dplyr::bind_rows(
          data.frame(
            contact_id = i,
            element = element,
            content = y[2],
            type = type
          )
        )
      }
    }
    cols <- cols[which(cols!="PRODID")]
    df2 <- cols %>% table() %>% as.integer() %>% t() %>% as.data.frame()
    colnames(df2)<-cols %>% table() %>% names()
    contacts_long_ <- contacts_long_[order(contacts_long_$element),]
    rows_to_drop_1 <- which(!contacts_long_$element%in%does_not_repeat)
    rows_to_not_add_1 <- which(contacts_long_$element%in%does_not_repeat)
    nums <- contacts_long_$element[rows_to_drop_1] %>% rle()
    nums <- nums$lengths
    nums <- nums %>% sapply(function(IN){paste0("_",1:IN)}) %>% unlist()
    # nums[which(nums=="_1")] <- ""
    contacts_long_$wide_column <- ""
    contacts_long_$wide_column[rows_to_drop_1] <- paste0(contacts_long_$element[rows_to_drop_1],nums)
    contacts_long_$wide_column[rows_to_not_add_1] <- contacts_long_$element[rows_to_not_add_1]
    contacts_long <-contacts_long %>% dplyr::bind_rows(contacts_long_)
  }
  contacts_long <- contacts_long %>% clean_df_of_NA()
  return(
    list(
      contacts_long = contacts_long,
      contact_list_original = contact_list_original,
      contact_list = contact_list
    )
  )
}

drop_rosycontacts <- function(rosycontacts,your_folder,file_type = "xlsx"){
  if(!file_type%in%c("xlsx","csv"))stop("file_type error: only 'xlsx' and 'csv' are allowed!")
  your_folder %>% dir.create(showWarnings = F)
  your_folder %>% file.path("original")%>% dir.create(showWarnings = F)
  your_folder %>% file.path("final")%>% dir.create(showWarnings = F)
  for (type in c("original","final")) {
    if(!purrr::is_empty(rosycontacts[[type]])){
      wide <- rosycontacts[[type]]$wide
      wide <- wide[,which(colnames(wide)!="PHOTO")]
      wide <- wide[,which(colnames(wide)!="PHOTO_type")]
      wide %>% rio::export(file.path(your_folder,type,paste0("contacts_wide.",file_type)))
      wide %>% rio::export(file.path(your_folder,type,paste0("contacts_wide_optional_edit.",file_type)))
      long <- rosycontacts[[type]]$long
      rosycontacts[[type]]$raw_vcf %>% writeLines(file.path(your_folder,type,paste0(type,".vcf")))
      if (type == "original") {
        photo_rows<-which(long$element=="PHOTO")
        if(length(photo_rows)>0){
          photos_path <- your_folder %>% file.path("original","photos")
          photos_path %>% dir.create(showWarnings = F)
          for (photo_row in photo_rows){
            # if(){# add url vs base64
            # type = b vs URI
            # }
            #add JPG vs other check
            # rosycontacts[[type]]$long$type[photo_row]
            inconn <- long$content[photo_row]
            outconn <- file(file.path(photos_path,paste0("photo_",long$contact_id[photo_row],".jpg")),"wb")
            base64enc::base64decode(what=inconn, output=outconn)
            close(outconn)
          }
        }
        long <- long[which(long$element!="PHOTO"),]
        long <- long[which(long$element!="PHOTO_type"),]
        long %>% rio::export(file.path(your_folder,type,paste0("contacts_long.",file_type)))
      }
    }
  }
  message("Saved to '",your_folder,"'")
}

original_to_final <- function(rosycontacts){
  rosycontacts <- rosycontacts$original$long %>% long_to_final(rosycontacts)
  return(rosycontacts)
}

long_to_final <- function (contacts_long,rosycontacts) {
  contacts_long <- contacts_long %>% clean_df_of_NA()
  rosycontacts$final[["list"]] <- contacts_long %>% long_to_list()
  rosycontacts$final[["raw_vcf"]] <-  rosycontacts$final[["list"]] %>% list_to_vcf()
  rosycontacts$final[["long"]] <- contacts_long %>% clean_tel_numbers() %>% check_names()
  rosycontacts$final[["wide"]] <- long_to_wide(rosycontacts$final[["long"]])
  return(rosycontacts)
}

#the following properties make use of this parameter: PHOTO, ADR, LABEL, TEL, EMAIL, IMPP, LOGO, MEMBER, SOUND, and KEY.
