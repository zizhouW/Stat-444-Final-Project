getmubar<-function(muhats) {
  function(x){
    Ans <-sapply(muhats, FUN=function(muhat){muhat(x)})
    apply(Ans, MARGIN=1, FUN=mean)
  }
}
ave_y_mu_sq <- function(sample, predfun){
  mean(abs(sample$y - predfun(sample$x))^2)
}
ave_mu_mu_sq <- function(predfun1, predfun2,x){
  mean((predfun1(x)- predfun2(x))^2)
}

apse <- function(Ssamples, Tsamlpes, fitFunc, df){
  N_S <-length(Ssamples)
  mean(sapply(1:N_S,
       FUN = function(j){
         S_j <- Ssamples[[j]]
         muhat <- fitFunc(S_j, df=df)
         T_j <- Tsamlpes[[j]]
         ave_y_mu_sq(T_j,muhat)
       })
  )
}

kfold <-function(N,k=N,indices=NULL){
  if (is.null(indices)) {
    indices <-sample(1:N, N,replace=FALSE)
    } else {
      N <-length(indices)
    }
  
  if (k >N)stop("k must not exceed N")
  gsize <-rep(round(N/k), k)
  extra <-N -sum(gsize)
  if (extra >0) {
    for (i in 1:extra) {
      gsize[i] <-gsize[i] +1
    }
  }
  if (extra <0) {
    for (i in 1:abs(extra)) {
      gsize[i] <-gsize[i] -1
    }
  }
  running_total <-c(0,cumsum(gsize))
  lapply(1:k,
         FUN=function(i) {
           indices[seq(from =1+running_total[i],
                       to =running_total[i+1],
                       by =1)]
         }
  )
}
getKfoldSamples <-function (x, y, k,indices=NULL){
  groups <-kfold(length(x), k, indices)
  Ssamples <-lapply(groups,
                    FUN=function(group) {
                      list(x=x[-group],y=y[-group])
                      })
  Tsamples <-lapply(groups,
                    FUN=function(group) {
                      list(x=x[group],y=y[group])
                      })
  list(Ssamples =Ssamples,Tsamples =Tsamples)
}

#named with testwithv because when you fold k you get v!
testwithv <- function(x,y,k, fitFunc, df) {
  samples = getKfoldSamples(x,y,k)
  apse(samples$Ssamples, samples$Tsamples, fitFunc , df)
  
}


x = seq(1,100)
y = seq(2,200, by=2)

k=5
df = 3

#replace fitFunc wtih your function
testwithv(x,y,k,
          fitFunc = function(sample, df){
            x = sample$x
            y = sample$y
            fit <- lm(y ~ x)
            muhat <- function(newX) {
              print(newX)
              print(predict(fit, newdata = list(x=newX)))
              predict(fit, newdata = list(x=newX))
            }
            muhat
          }, df)

