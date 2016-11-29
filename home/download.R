output$datadnld = renderUI({
  if(useSB){
    sites = sub("^(.*_.*)_[0-9]*-.*\\.Rda","\\1",dnld$ff) # the sites included in the download data
  }else{
    sites = gsub("raw/(.*_.*)_.*\\.Rda","\\1",dnld$ff)
  }
  sitenms = sort(sitenames[match(sites, allsites$SITEID)]) # the site names
  tagList(
    selectizeInput("todnld", NULL, sitenms, selected = NULL, multiple = TRUE, options = NULL),
    downloadButton("dnldit","Download your data", class="color: #fff; background-color: #337ab7; border-color: #fff")
  )
})

output$dnldit = downloadHandler(
  filename = function() { paste0('StreamPULSE_',Sys.Date(),'.csv') },
  content = function(file) {
    print(input$todnld)
    print(sitenames)
    sitestodl = allsites$SITEID[which(sitenames%in%input$todnld)] # get siteid from name
    print(sitestodl)
    print(dnld$ff)
    for(sf in sitestodl){
      f = grep(sf, dnld$ff, value=TRUE)
      gf = item_file_download(sbrpath, names=f, destinations=file.path(tdatf,f), overwrite_file=TRUE, session=asb) # save locally
      for(f in gf) load(f) # load that site locally
    }
    file.remove(dir(tdatf, full.names=TRUE)) # remove locally
    lf = ls()
    objs = unlist(sapply(sitestodl, function(sf) grep(sf, lf, value=TRUE)))
    tmpl = lapply(objs, function(o){
      get(o) %>% mutate(Site=sub("^(.*_.*)_[0-9]*-.*","\\1",o)) %>%
      gather(variable, value, -DateTime_UTC, -Site) %>% filter(!is.na(value))
    })
    tmpd = Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) %>% distinct() %>% arrange(Site, variable, DateTime_UTC) # stack them up
    write_csv(tmpd, file)
  }
)




#
# lf = ls()
# objs = sapply(toget, function(sf) grep(sf, lf, value=TRUE)) # all of the data files
# tmpl = lapply(objs, function(o) get(o) %>% mutate(Site=gsub("(.*_.*)_.*","\\1",o)) %>% gather(variable, value, -DateTime_UTC, -Site) )
# tmpd = Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) # stack them up
#
# tmpf = get(objs[2]) %>% mutate(Site=gsub("(.*_.*)_.*","\\1",objs[2])) %>% gather(variable, value, -DateTime_UTC, -Site)
#
# toget = c("NC_EastEno","NC_Eno")
# sapply(toget, function(s){
#   f = grep(s, kk, value=TRUE)
#   s3load(f, bucket="streampulse")
# })
#
