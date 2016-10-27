# MODIFY DOWNLOAD TO PULL FROM SCIENCEBASE

b = item_list_files(datRaw)
dnld = reactiveValues(ff=b$fname)

output$datadnld = renderUI({
  sites = gsub("^(.*_.*)_[1-9].*\\.Rda","\\1",dnld$ff)
  tagList(
    selectizeInput("todnld", NULL, sites, selected = NULL, multiple = TRUE, options = NULL),
    downloadButton("dnldit","Download your data!", class="color: #fff; background-color: #337ab7; border-color: #fff")
  )
})

output$dnldit = downloadHandler(
    filename = function() { paste0('StreamPULSE_',Sys.Date(),'.csv') },
    content = function(file) {
      for(sf in input$todnld){
        f = grep(sf, dnld$ff, value=TRUE)
        gf = item_file_download(datRaw, names=f, destinations=file.path(tdatf,f), overwrite_file=TRUE) # save locally
        # Rda on SB
        load(gf) # load locally
        # CSV on SB
        #  tmp = read_csv(gf)
        #  assign(gsub("(.*)\\.csv","\\1",basename(gf)), tmp)
      }
      file.remove(dir(tdatf, full.names=TRUE)) # remove locally
      # On AWS
      #  for(sf in input$todnld){ # sapply doesn't work with this for some reason...
      #    f = grep(sf, dnld$ff, value=TRUE)
      #    s3load(f, bucket="streampulse")
      #  }
      lf = ls()
      objs = sapply(input$todnld, function(sf) grep(sf, lf, value=TRUE))
      tmpl = lapply(objs, function(o){
        get(o) %>% mutate(Site=gsub("(.*_.*)_[1-9].*","\\1",o)) %>%
        gather(variable, value, -DateTime_UTC, -Site) %>% filter(!is.na(value))
      })
      tmpd = Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) # stack them up
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
