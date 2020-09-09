#include <Rcpp.h>
using namespace Rcpp;
#include <R.h>
#include <Rmath.h>

// [[Rcpp::export]]
NumericMatrix qgraph_layout_Cpp(
    int pniter,
    int pvcount,
    int pecount,
    NumericVector maxdelta,
    double parea,
    double pcoolexp,
    double prepulserad,
    IntegerVector Ef,  /* Edges from */
    IntegerVector Et, /*Edges t0*/
    NumericVector W,
    NumericVector xInit,
    NumericVector yInit,
    LogicalVector Cx,
    LogicalVector Cy,
    int digits
) {
  /*
   Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
   edgelist matrix d.  Positions (stored in (x,y)) should be initialized
   prior to calling this routine.
   */
  
  int n = pvcount;
  int m = pecount;
  double frk;
  double ded;
  double xd;
  double yd;
  double rf;
  double af;
  int i;
  int j;
  int k;
  int l;
  int niter = pniter;
  //double maxdelta;
  double area = parea;
  double coolexp = pcoolexp;
  double repulserad = prepulserad;
  
  
  /*Allocate memory for transient structures*/
  // dx=(double *)R_alloc(n,sizeof(double));
  // dy=(double *)R_alloc(n,sizeof(double));
  // t=(double *)R_alloc(n,sizeof(double));
  // Rcpp way:
  NumericVector dx(n);
  NumericVector dy(n);
  NumericVector t(n);
  
  // Copy xIint and yInit:
  NumericVector x(n);
  NumericVector y(n);
  for (i = 0; i < n; i++){
    x[i] = xInit[i];
    y[i] = yInit[i];
  }
  
  frk=sqrt(area/(double)n);
  
  /*Run the annealing loop*/
  for(i=niter;i>=0;i--){
    /*Clear the deltas*/
    for(j=0;j<n;j++){
      dx[j]=0.0;
      dy[j]=0.0;
    }
    /*Increment deltas for each undirected pair*/
    for(j=0;j<n;j++)
    {
      /*Set the temperature (maximum move/iteration)*/
      t[j]=maxdelta[j]*pow((double)i/(double)niter,coolexp);
      
      if (j<n){
        for(k=j+1;k<n;k++){
          /*Obtain difference vector*/
          xd=x[j]-x[k];
          yd=y[j]-y[k];
          ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
          xd/=ded;                /*Rescale differences to length 1*/
          yd/=ded;
          /*Calculate repulsive "force"*/
          rf=frk*frk*(1.0/ded-ded*ded/repulserad);
          dx[j]+=xd*rf;        /*Add to the position change vector*/
          dx[k]-=xd*rf;
          dy[j]+=yd*rf;
          dy[k]-=yd*rf;
        }
      }
    }
    /*Calculate the attractive "force"*/
    for(j=0;j<m;j++){
      k=Ef[j];
      l=Et[j];
      
      xd=x[k]-x[l];
      yd=y[k]-y[l];
      ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
    if (std::abs(ded) > 0.000001)
    {
      xd/=ded;                /*Rescale differences to length 1*/
      yd/=ded;
    }
    af=ded*ded/frk*W[j];
    dx[k]-=xd*af;        /*Add to the position change vector*/
    dx[l]+=xd*af;
    dy[k]-=yd*af;
    dy[l]+=yd*af;
    
    }
    /*Dampen motion, if needed, and move the points*/
    for(j=0;j<n;j++){
      ded=sqrt(dx[j]*dx[j]+dy[j]*dy[j]);
      if(ded>t[j]){                 /*Dampen to t*/
        ded=t[j]/ded;
        dx[j]*=ded;
        dy[j]*=ded;
      }
      if (!Cx[j]){
        x[j]+=Rf_fround(dx[j],digits);               /*Update positions (correcting for floating point errors)*/
      }
      if (!Cy[j]){
        y[j]+=Rf_fround(dy[j],digits);
      }
    }
  }
  
  NumericMatrix Layout(n,2);
  
  // Fill layout:
  for (i=0;i<n;i++){
    Layout(i,0) = x[i];
    Layout(i,1) = y[i];
  }
  
  return Layout;
}
