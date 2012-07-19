#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>

void qgraph_layout_fruchtermanreingold_R(
	int *pniter,
	int *pvcount,
	int *pecount,
	//double *pmaxdelta,
	double *maxdelta,
	double *parea,
	double *pcoolexp,
	double *prepulserad,
	int *Ef,  /* Edges from */
	int *Et, /*Edges t0*/
	double *W,
	double *x,
	double *y,
	int *Cx,
	int *Cy) {
/*
Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
edgelist matrix d.  Positions (stored in (x,y)) should be initialized
prior to calling this routine.
*/

int n;
int m;
double frk;
double *t;
double ded;
double xd;
double yd;
double *dx;
double *dy;
double rf;
double af;
int i;
int j;
int k;
int l; 
int niter;
//double maxdelta;
double area;
double coolexp;
double repulserad;

n = (int)*pvcount;
m = (int)*pecount;
niter=*pniter;
//maxdelta=*pmaxdelta;
area=*parea;
coolexp=*pcoolexp;
repulserad=*prepulserad;


/*Allocate memory for transient structures*/
dx=(double *)R_alloc(n,sizeof(double));
dy=(double *)R_alloc(n,sizeof(double));
t=(double *)R_alloc(n,sizeof(double));
  
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
		t[j]=maxdelta[j]*pow(i/(double)niter,coolexp);
		
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
    /*Calculate the attractive "force"*/
    for(j=0;j<m;j++){
      k=Ef[j];
      l=Et[j];
      
      xd=x[k]-x[l];
      yd=y[k]-y[l];
      ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
      if (ded != 0)
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
	  if (!Cx[j])
       x[j]+=dx[j];               /*Update positions*/
      if (!Cy[j])
	   y[j]+=dy[j];
    }
  }
}
