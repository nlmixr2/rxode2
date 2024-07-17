#ifndef linCmtB1_header
#define linCmtB1_header
#define A1 A[0]
#define A2 A[1]
#define A1last Alast[0]
#define A2last Alast[1]

static inline void oneCmtRateSSr1D(double *A, double *r1, double *k10) {
#define A1k10 A[1]
A1=(*r1)/(*k10);
A1k10=-(*r1)/(((*k10))*((*k10)));
#undef A1k10
}

static inline void oneCmtRateSSD(double *A, double *tinf, double *tau, double *r1, double *k10) {
#define A1k10 A[1]
double _0=(*tau)-(*tinf);
double _1=exp(-(*tau)*(*k10));
double _2=exp(-(*k10)*(*tinf));
double _3=1-_1;
double _4=1-_2;
double _5=exp(-(*k10)*(_0));
double _6=(*k10)*(_3);
double _7=_5*(*r1);
A1=_7*(_4)/(_6);
A1k10=-_5*(*r1)*(_4)/((((*k10))*((*k10)))*(_3))+exp(-(*k10)*(*tinf)-(*k10)*(_0))*(*r1)*(*tinf)/(_6)-_7*(_0)*(_4)/(_6)-exp(-(*k10)*(_0)-(*tau)*(*k10))*(*r1)*(*tau)*(_4)/((*k10)*((_3)*(_3)));
#undef A1k10
}

static inline void oneCmtRateD(double *A, double *Alast, double *t, double *b1, double *r1, double *k10) {
#define A1k10 A[1]
#define A1lastk10 Alast[1]
double _0=exp(-(*t)*(*k10));
double _1=1-_0;
double _3=(*r1)*(_1);
A1=(*b1)+_0*A1last+_3/(*k10);
double _2=(*t)*_0;
A1k10=_0*A1lastk10-_3/(((*k10))*((*k10)))-_2*A1last+_2*(*r1)/(*k10);
#undef A1k10
#undef A1lastk10
}

static inline void oneCmtBolusSSD(double *A, double *tau, double *b1, double *k10) {
#define A1k10 A[1]
double _0=exp(-(*tau)*(*k10));
double _1=1-_0;
A1=(*b1)/(_1);
A1k10=-_0*(*b1)*(*tau)/((_1)*(_1));
#undef A1k10
}

static inline void oneCmtKaRateSSr1D(double *A, double *r1, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
A1=(*r1)/(*ka);
A1ka=-(*r1)/(((*ka))*((*ka)));
A2=(*r1)/(*k20);
A2ka=0;
A2k20=-(*r1)/(((*k20))*((*k20)));
#undef A1ka
#undef A2ka
#undef A2k20
}

static inline void oneCmtKaRateSSr2D(double *A, double *r2, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
A1=0;
A1ka=0;
A2=(*r2)/(*k20);
A2ka=0;
A2k20=-(*r2)/(((*k20))*((*k20)));
#undef A1ka
#undef A2ka
#undef A2k20
}

static inline void oneCmtKaRateSStr1D(double *A, double *tinf, double *tau, double *r1, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
double _0=(*r1)/(*ka);
double _4=(*tau)-(*tinf);
double _7=exp(-(*ka)*(*tau));
double _8=exp(-(*ka)*(*tinf));
double _12=1-_7;
double _15=_8*(*r1);
double _25=_0-_15/(*ka);
A1=exp(-(*ka)*(_4))*(_25)/(_12);
double _5=((*ka))*((*ka));
double _9=(*ka)*(_4);
double _22=_15*(*tinf);
double _24=_22/(*ka);
double _26=exp(-(*ka)*(*tau)-_9);
double _28=_15/(_5);
double _31=_26*(*tau);
A1ka=exp(-(*ka)*(_4))*(-(*r1)/(_5)+_28+_24)/(_12)-exp(-(*ka)*(_4))*(_4)*(_25)/(_12)-_31*(_25)/((_12)*(_12));
double _1=(*r1)/(*k20);
double _2=(*ka)*(*k20);
double _13=exp(-(*k20)*(*tinf));
double _16=_13*(*r1);
double _19=exp(-(*k20)*(_4));
double _20=_16*(*ka);
double _27=_15/(-(*k20)+(*ka));
double _33=_1+_27;
A2=_19*(_33-_20/(_2-(((*k20))*((*k20)))))/(1-exp(-(*tau)*(*k20)))+(*ka)*(_19/(1-exp(-(*tau)*(*k20)))-exp(-(*ka)*(_4))/(_12))*(_25)/(-(*k20)+(*ka));
double _6=((*k20))*((*k20));
double _23=(-(*k20)+(*ka))*(-(*k20)+(*ka));
double _38=_15/(_23);
A2ka=(_19/(1-exp(-(*tau)*(*k20)))-exp(-(*ka)*(_4))/(_12))*(_25)/(-(*k20)+(*ka))+_19*(-_13*(*r1)/(_2-(_6))-_38-_22/(-(*k20)+(*ka))+_20*(*k20)/((_2-(((*k20))*((*k20))))*(_2-(((*k20))*((*k20))))))/(1-exp(-(*tau)*(*k20)))-(*ka)*(_19/(1-exp(-(*tau)*(*k20)))-exp(-(*ka)*(_4))/(_12))*(_25)/(_23)+(*ka)*(_19/(1-exp(-(*tau)*(*k20)))-exp(-(*ka)*(_4))/(_12))*(-(*r1)/(_5)+_28+_24)/(-(*k20)+(*ka))+(*ka)*(exp(-(*ka)*(_4))*(_4)/(_12)+_31/((_12)*(_12)))*(_25)/(-(*k20)+(*ka));
double _3=(*tau)*(*k20);
double _29=exp(-(*k20)*(_4)-_3);
double _32=_29*(*tau);
A2k20=_19*(-(*r1)/(_6)+_38+_20*(*tinf)/(_2-(_6))+_20*(-2*(*k20)+(*ka))/((_2-(((*k20))*((*k20))))*(_2-(((*k20))*((*k20))))))/(1-exp(-(*tau)*(*k20)))+(*ka)*(_19/(1-exp(-(*tau)*(*k20)))-exp(-(*ka)*(_4))/(_12))*(_25)/(_23)+(*ka)*(-_19*(_4)/(1-exp(-(*tau)*(*k20)))-_32/((1-exp(-(*tau)*(*k20)))*(1-exp(-(*tau)*(*k20)))))*(_25)/(-(*k20)+(*ka))-_19*(_4)*(_33-_20/(_2-(_6)))/(1-exp(-(*tau)*(*k20)))-_32*(_33-_20/(_2-(_6)))/((1-exp(-(*tau)*(*k20)))*(1-exp(-(*tau)*(*k20))));
#undef A1ka
#undef A2ka
#undef A2k20
}

static inline void oneCmtKaRateSStr2D(double *A, double *tinf, double *tau, double *r2, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
A1=0;
A1ka=0;
double _0=(*r2)/(*k20);
double _1=(*ka)*(*k20);
double _2=(*tau)-(*tinf);
double _5=exp(-(*tau)*(*k20));
double _6=exp(-(*k20)*(*tinf));
double _7=1-_5;
double _8=_6*(*r2);
double _9=exp(-(*k20)*(_2));
double _11=_8*(-(*k20)+(*ka));
A2=_9*(_0-_11/(_1-(((*k20))*((*k20)))))/(_7);
double _3=((*k20))*((*k20));
A2ka=_9*(-_6*(*r2)/(_1-(_3))+_8*(*k20)*(-(*k20)+(*ka))/((_1-(((*k20))*((*k20))))*(_1-(((*k20))*((*k20))))))/(_7);
A2k20=_9*(-(*r2)/(_3)+_8/(_1-(_3))+_8*(*tinf)*(-(*k20)+(*ka))/(_1-(_3))+_8*(-2*(*k20)+(*ka))*(-(*k20)+(*ka))/((_1-(((*k20))*((*k20))))*(_1-(((*k20))*((*k20))))))/(_7)-_9*(_2)*(_0-_11/(_1-(_3)))/(_7)-exp(-(*k20)*(_2)-(*tau)*(*k20))*(*tau)*(_0-_11/(_1-(_3)))/((_7)*(_7));
#undef A1ka
#undef A2ka
#undef A2k20
}

static inline void oneCmtKaRateD(double *A, double *Alast, double *t, double *b1, double *b2, double *r1, double *r2, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
#define A1lastka Alast[2]
#define A2lastka Alast[3]
#define A2lastk20 Alast[4]
double _3=(*ka)*A1last;
double _5=exp(-(*t)*(*ka));
double _8=(*r1)-_3;
double _17=_5*(_8);
A1=(*b1)+(*r1)/(*ka)-_17/(*ka);
double _4=((*ka))*((*ka));
double _9=(*t)*_5;
double _19=_9*(_8);
double _20=_5*(-(*ka)*A1lastka-A1last);
A1ka=-(*r1)/(_4)+_17/(_4)-_20/(*ka)+_19/(*ka);
double _0=(*r1)+(*r2);
double _1=(*r1)*(*ka);
double _2=(*ka)*(*k20);
double _6=exp(-(*t)*(*k20));
double _10=(*r2)*(-(*k20)+(*ka));
double _12=(-A1last-A2last)*(*ka);
A2=(*b2)+(_0)/(*k20)-_6*((((*k20))*((*k20)))*A2last+_1+_10+_12*(*k20))/(_2-(((*k20))*((*k20))))+_17/(-(*k20)+(*ka));
double _7=((*k20))*((*k20));
double _16=(-(*k20)+(*ka))*(-(*k20)+(*ka));
double _23=_17/(_16);
A2ka=-_6*(_0+(_7)*A2lastka+(-A1last-A2last)*(*k20)+(-A1lastka-A2lastka)*(*ka)*(*k20))/(_2-(_7))-_23+_20/(-(*k20)+(*ka))-_19/(-(*k20)+(*ka))+_6*(*k20)*((_7)*A2last+_1+_10+_12*(*k20))/((_2-(((*k20))*((*k20))))*(_2-(((*k20))*((*k20)))));
A2k20=-(_0)/(_7)-_6*(-(*r2)+2*(*k20)*A2last+(_7)*A2lastk20+_12+(-0-A2lastk20)*(*ka)*(*k20))/(_2-(_7))+_23+(*t)*_6*((_7)*A2last+_1+_10+_12*(*k20))/(_2-(_7))+_6*(-2*(*k20)+(*ka))*((_7)*A2last+_1+_10+_12*(*k20))/((_2-(((*k20))*((*k20))))*(_2-(((*k20))*((*k20)))))-0/(-(*k20)+(*ka));
#undef A1ka
#undef A2ka
#undef A2k20
#undef A1lastka
#undef A2lastka
#undef A2lastk20
}

static inline void oneCmtKaSSb1D(double *A, double *tau, double *b1, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
double _1=exp(-(*ka)*(*tau));
double _3=1-_1;
A1=(*b1)/(_3);
A1ka=-_1*(*b1)*(*tau)/((_3)*(_3));
double _0=(*ka)*(*b1);
double _2=exp(-(*tau)*(*k20));
double _4=1-_2;
A2=_0*(1.0/(-(_3))+(1.0/(_4)))/(-(*k20)+(*ka));
double _5=1/(_3);
double _6=1/(_4);
double _7=(-(*k20)+(*ka))*(-(*k20)+(*ka));
double _8=(_6);
double _10=_0*(-(_5)+_8);
double _11=_10/(_7);
A2ka=(*b1)*(-(_5)+_8)/(-(*k20)+(*ka))-_11+_1*(*ka)*(*b1)*(*tau)/((-(*k20)+(*ka))*((_3)*(_3)));
A2k20=_11-_2*(*ka)*(*b1)*(*tau)/((-(*k20)+(*ka))*((_4)*(_4)));
#undef A1ka
#undef A2ka
#undef A2k20
}

static inline void oneCmtKaSSb2D(double *A, double *tau, double *b2, double *ka, double *k20) {
#define A1ka A[2]
#define A2ka A[3]
#define A2k20 A[4]
A1=0;
A1ka=0;
double _0=exp(-(*tau)*(*k20));
double _1=1-_0;
A2=(*b2)/(_1);
A2ka=0;
A2k20=-_0*(*b2)*(*tau)/((_1)*(_1));
#undef A1ka
#undef A2ka
#undef A2k20
}
 

#undef A1
#undef A2
#undef A1last
#undef A2last
#endif
