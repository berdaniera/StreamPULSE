b = get_bucket('streampulse')
keys = tibble(k=unlist(b)[names(unlist(b))%in%c('Key')])
kk = grep("raw/",keys$k,perl=TRUE,value=TRUE)[-1]
dnld = reactiveValues(ff=kk)

output$datadnld = renderUI({
  sites = gsub("raw/(.*_.*)_.*\\.Rda","\\1",dnld$ff)
  tagList(
    selectizeInput("todnld", NULL, sites, selected = NULL, multiple = TRUE, options = NULL),
    downloadButton("dnldit","Download your data!", icon=icon("download"), width="100%", style="color: #fff; background-color: #337ab7; border-color: #fff")
  )
})

output$dnldit = downloadHandler(
   filename = function() { paste0('StreamPULSE_',Sys.Date(),'.csv') },
   content = function(file) {
     for(sf in input$todnld){ # sapply doesn't work with this for some reason...
       f = grep(sf, dnld$ff, value=TRUE)
       s3load(f, bucket="streampulse")
     }
     lf = ls()
     objs = sapply(input$todnld, function(sf) grep(sf, lf, value=TRUE))
     tmpl = lapply(objs, function(o) get(o) %>% mutate(Site=gsub("(.*_.*)_.*","\\1",o)) %>% gather(variable, value, -DateTime_UTC, -Site) )
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
