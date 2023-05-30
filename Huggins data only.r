library(RMark)

#import data
turkey=import.chdata("turkey.txt")
turkey$ch2=as.character(turkey$ch2)
turkey$ch3=as.character(turkey$ch3)

# run analysis with 3 occasions
# remove any with all 0 observations
df=turkey
last_time=nchar(df$ch)[1]
df=df[df$ch!=paste(rep("0",last_time),collapse=""),]

# process data and run model
dp=process.data(df,model="Huggins")
ddl=make.design.data(dp)
fit_models=function()
{
  p.0=list(formula=~1,share=TRUE)
  p.1=list(formula=~time,share=TRUE)
  p.2=list(formula=~time:c,share=TRUE)
  cml=create.model.list("Huggins")
  results=mark.wrapper(cml,data=dp,ddl=ddl)
  return(results)
}
results=fit_models()
results

# run analysis with 5 occasions
df=turkey
df$ch=df$ch2
last_time=nchar(df$ch)[1]
df=df[df$ch!=paste(rep("0",last_time),collapse=""),]

dp=process.data(df,model="Huggins")
ddl=make.design.data(dp)
fit_models=function()
{
  p.0=list(formula=~1,share=TRUE)
  p.1=list(formula=~time,share=TRUE)
  p.2=list(formula=~time:c,share=TRUE)
  cml=create.model.list("Huggins")
  results=mark.wrapper(cml,data=dp,ddl=ddl)
  return(results)
}
results=fit_models()
results


# run analysis with 10 occasions
df=turkey
df$ch=df$ch3
last_time=nchar(df$ch)[1]
df=df[df$ch!=paste(rep("0",last_time),collapse=""),]

dp=process.data(df,model="Huggins")
ddl=make.design.data(dp)
fit_models=function()
{
  p.0=list(formula=~1,share=TRUE)
  p.1=list(formula=~time,share=TRUE)
  p.2=list(formula=~time:c,share=TRUE)
  cml=create.model.list("Huggins")
  results=mark.wrapper(cml,data=dp,ddl=ddl)
  return(results)
}
results=fit_models()
results

