#define USE_FC_LEN_T
#define STRICT_R_HEADER
#ifdef __STANDALONE__
#include <stdio.h>
#define Rprintf printf

void xerrwd_(int *ix, int *fatal) {
#else
#include <R.h>
#include <Rinternals.h>
void F77_SUB(xerrwd)(int *ix, int *fatal) {
#endif

char err[47][180] = {
"DLSODA-  ISTATE (=I1) illegal.",                                                                                                                                                  //1,   
"DLSODA-  ITASK (=I1) illegal. ",                                                                                                                                                  //2,   
"DLSODA-  ISTATE .gt. 1 but DLSODA not initialized.",                                                                                                                              //3,   
"DLSODA-  NEQ (=I1) .lt. 1     ",                                                                                                                                                  //4,   
"DLSODA-  ISTATE = 3 and NEQ increased (I1 to I2). ",                                                                                                                              //5,   
"DLSODA-  ITOL (=I1) illegal.  ",                                                                                                                                                  //6,   
"DLSODA-  IOPT (=I1) illegal.  ",                                                                                                                                                  //7,   
"DLSODA-  JT (=I1) illegal.    ",                                                                                                                                                  //8,   
"DLSODA-  ML (=I1) illegal: .lt.0 or .ge.NEQ (=I2) ",                                                                                                                              //9,   
"DLSODA-  MU (=I1) illegal: .lt.0 or .ge.NEQ (=I2) ",                                                                                                                              //10,  
"DLSODA-  IXPR (=I1) illegal.  ",                                                                                                                                                  //11,  
"DLSODA-  MXSTEP (=I1) .lt. 0  ",                                                                                                                                                  //12,  
"DLSODA-  MXHNIL (=I1) .lt. 0  ",                                                                                                                                                  //13,  
"DLSODA-  TOUT (=R1) behind T (=R2)      \n      Integration direction is given by H0 (=R1)  ",                                                                                    //14,  
"DLSODA-  HMAX (=R1) .lt. 0.0  ",                                                                                                                                                  //15,  
"DLSODA-  HMIN (=R1) .lt. 0.0  ",                                                                                                                                                  //16,  
"DLSODA-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)",                                                                                                                    //17,  
"DLSODA-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)",                                                                                                                    //18,  
"DLSODA-  RTOL(I1) is R1 .lt. 0.0        ",                                                                                                                                        //19,  
"DLSODA-  ATOL(I1) is R1 .lt. 0.0        ",                                                                                                                                        //20,  
"DLSODA-  EWT(I1) is R1 .le. 0.0         ",                                                                                                                                        //21,  
"DLSODA-  TOUT(=R1) too close to T(=R2) to start integration.",                                                                                                                    //22,  
"DLSODA-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  ",                                                                                                                    //23,  
"DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   ",                                                                                                                    //24,  
"DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   ",                                                                                                                    //25,  
"DLSODA-  At start of problem, too much accuracy   \n      requested for precision of machine..  See TOLSF (=R1) ",                                                                //26,  
"DLSODA-  Trouble in DINTDY.  ITASK = I1, TOUT = R1",                                                                                                                              //27,  
"DLSODA-  MXORDN (=I1) .lt. 0  ",                                                                                                                                                  //28,  
"DLSODA-  MXORDS (=I1) .lt. 0  ",                                                                                                                                                  //29,  
"DINTDY-  K (=I1) illegal      ",                                                                                                                                                  //51,  
"DINTDY-  T (=R1) illegal      \n      T not in interval TCUR - HU (= R1) to TCUR (=R2)      ",                                                                                    //52,  
"DLSODA-  Warning..Internal T (=R1) and H (=R2) are\n      such that in the machine, T + H = T on the next step  \n     (H = step size). Solver will continue anyway.",            //101, 
"DLSODA-  Above warning has been issued I1 times.  \n     It will not be issued again for this problem.",                                                                          //102, 
"DLSODA-  Warning.. RWORK length is sufficient for now, but  \n      may not be later.  Integration will proceed anyway.   \n      Length needed is LENRW = I1, while LRW = I2.",  //103, 
"DLSODA-  Warning.. IWORK length is sufficient for now, but  \n      may not be later.  Integration will proceed anyway.   \n      Length needed is LENIW = I1, while LIW = I2.",  //104, 
"DLSODA- A switch to the BDF (stiff) method has occurred     ",                                                                                                                    //105, 
"DLSODA- A switch to the Adams (nonstiff) method has occurred",                                                                                                                    //106, 
"     at T = R1,  tentative step size H = R2,  step NST = I1 ",                                                                                                                    //107, 
"DLSODA-  At current T (=R1), MXSTEP (=I1) steps   \n      taken on this call before reaching TOUT     ",                                                                          //201, 
"DLSODA-  At T (=R1), EWT(I1) has become R2 .le. 0.",                                                                                                                              //202, 
"DLSODA-  At T (=R1), too much accuracy requested  \n      for precision of machine..  See TOLSF (=R2) ",                                                                          //203, 
"DLSODA-  At T(=R1) and step size H(=R2), the error\n      test failed repeatedly or with ABS(H) = HMIN",                                                                          //204, 
"DLSODA-  At T (=R1) and step size H (=R2), the    \n      corrector convergence failed repeatedly     \n      or with ABS(H) = HMIN   ",                                          //205, 
"DLSODA-  At current T(=R1), RWORK length too small\n      to proceed.  The integration was otherwise successful.",                                                                //206, 
"DLSODA-  At current T(=R1), IWORK length too small\n      to proceed.  The integration was otherwise successful.",                                                                //207, 
"DLSODA-  Run aborted.. apparent infinite loop.    "                                                                                                                               //303, 
};

	int  offset = 0;
	if (0);
	else if (*ix <  30) offset =   -1;
	else if (*ix < 100) offset =  -22;
	else if (*ix < 200) offset =  -70;
	else if (*ix < 300) offset = -163;
	else if (*ix < 400) offset = -258;
	
	Rprintf("%s\n", err[*ix+offset]);
}


#ifdef __STANDALONE__
int main(void)
{
	int ix, fatal=0;
	ix = 1; xerrwd_(&ix, &fatal);
	ix = 29; xerrwd_(&ix, &fatal);
	ix = 51; xerrwd_(&ix, &fatal);
	ix = 52; xerrwd_(&ix, &fatal);
	ix = 101; xerrwd_(&ix, &fatal);
	ix = 107; xerrwd_(&ix, &fatal);
	ix = 201; xerrwd_(&ix, &fatal);
	ix = 207; xerrwd_(&ix, &fatal);
	ix = 303; xerrwd_(&ix, &fatal);
	return 0;
}
#endif

