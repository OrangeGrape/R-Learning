#include <stdio.h>
#include <math.h>

void simpleKmeans(float *X, float *Y, int k,int *Seg){
/*algorithm step by step  
  1.initial parameter
  2.initial center points MuX[k] MuY[k]
  3.calculate euclidian distace
    ik=1 [u(1,1) u(1,2) ... u(1,Npts)] 
    ik=2 [u(2,1) u(2,2) ... u(2,Npts)] 
      .  [  .      .           .     ] 
      .  [  .      .           .     ] 
      .  [  .      .           .     ] 
    ik=k [u(k,1) u(k,2) ... u(k,Npts)] 
  4.find minimum within same index
    Seg  [ikmin(1)   ...  ikmin(Npts)]
  5.update muX[k] muY[k];
*/
  
  //declare Algorithm parameter
  int Npts = sizeof(X)/sizeof(float);
  float Xmin = X[0];
  
  int i,iter=0,maxiter=10000,ik,Segmin,Done=0,sumDone,count;
  float muX[k];
  float muY[k];
  float Urange[k];
  float Umin,sumX,sumY;
  
  //initial muX, muY
  for(ik=0;ik<k;ik++){
    muX[ik] = Xmin +(1/2 + ik)*Npts/k;
    muY[ik] = 0;
  }
  
  //optimizing loop
  do{
    sumDone = 0;
    //finding Seg which minimize objective function
    for(i=0;i<Npts;i++){
  
      ik=0;
      Urange[ik] = sqrt(pow(X[i]-muX[ik],2) + pow(Y[i]-muY[ik],2));
      Segmin = ik;
      Umin = Urange[ik];

      for(ik=1;ik<k;ik++){
        Urange[ik] = sqrt(pow(X[i]-muX[ik],2) + pow(Y[i]-muY[ik],2));
        if(Urange[ik]<Umin){
          Segmin = ik;
          Umin = Urange[ik];
        }
      }
      Seg[i]= Segmin;
    }
    
    //update muX muY from minimized Seg
    for(ik=0;ik<k;ik++){
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
    //Meaning all k center points have no update (sumdone == k) giving Done = 1
    iter++;
    printf("iter: %d\tDone: %d\n",iter,Done);
  } while(!Done || iter < maxiter);
  
}


int main(void){
//input
  int i,k = 6, it = 1000;
  float X[it];
  float Y[it];
  int Seg[it];
  for(i=0;i<it;i++){
     X[i]=0;
     Y[i]= exp(-pow(i-300,2)/(2*pow(1,2)))/sqrt(2*M_PI*pow(1,2))-exp(-pow(i-600,2)/(2*pow(1,2)))/(2*sqrt(2*M_PI*pow(1,2)));
     Seg[i]=0;
  }
  simpleKmeans(X,Y,k,Seg);
  FILE *file = fopen("Kmean_result.txt","w");
  
  for(i=0;i<it;i++){
    fprintf(file,"%d\t%f\t%f\t%d\n",i,X[i],Y[i],Seg[i]);
  }
  
  return 0;
}

