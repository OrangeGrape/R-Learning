#include <stdio.h>

int simpleKmean(float *X, float *Y, int K,int *Seg){
  //declare Algorithm parameter
  int Npts = length(X);
  //float Xmax = maximum(X);
  float Xmin = minimum(X);
  
  int i,ik,Segmin,Done=0,sumDone,count;
  float muX[k];
  float muY[k];
  float Urange[k];
  float Umin,sumX,sumY;
  int Seg[Npts];
  
  //initial muX, muY
  for(ik=0;ik<k;ik++){
    muX[ik] = Xmin +(1/2 + ik)*Npts/k;
    muY[ik] = 0;
  }
  
  //optimizing loop
  do{
    sumDone = 0;
    //finding Seg which minimize Costfunction
    for(i=0;i<Npts;i++){
      Segmin = 0;
      for(ik=0;ik<k;ik++){
        Urange[ik] = sqrt(pow(X[i]-muX[ik],2) + pow(Y[i]-muY[ik],2));
        if(ik == 0){
          Segmin = ik;
          Umin = Urange[ik];
        }else if(Urange[ik]<Umin){
          Segmin = ik;
          Umin = Urange[ik];
        }
      }
      Seg[i]= Segmin;
    }
    
    //update muX muY from minimized Seg
    for(ik=1;ik<k;ik++){
      sumX = 0;
      sumY = 0;
      count = 0;
      for(i=0;i<Npts;i++){
        if(Seg[i]==ik){
          sumX += X[i];
          sumY += Y[i];
          count ++;
        }
      }
      sumDone += (muX[ik] == sumX/count)&&(muY[ik] == sumY/count);
      muX[ik] = sumX/count;
      muY[ik] = sumY/count;
    }
    Done = (sumDone == k);
  }while(!Done);
  
  return(Seg[]);
}


int main(void){
//input
  int k = 6, it = 1000;
  float X[it];
  float Y[it];
  
  simpleKmeans(X,Y,k);
  
  return( Seg[it] );
}
