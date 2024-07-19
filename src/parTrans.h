static inline int parTrans(int *trans,
                           double *p1, double *v1,
                           double *p2, double *p3,
                           double *p4, double *p5,
                           unsigned int *ncmt, double *rx_k, double *rx_v, double *rx_k12,
                           double *rx_k21, double *rx_k13, double *rx_k31){
  double btemp, ctemp, dtemp;
  if ((*p5) > 0.) {
    (*ncmt) = 3;
    switch (*trans) {
    case 1: // cl v q vp
      (*rx_k) = (*p1)/(*v1); // k = CL/V
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2)/(*v1); // k12 = Q/V
      (*rx_k21) = (*p2)/(*p3); // k21 = Q/Vp
      (*rx_k13) = (*p4)/(*v1); // k31 = Q2/V
      (*rx_k31) = (*p4)/(*p5); // k31 = Q2/Vp2
      break;
    case 2: // k=(*p1) v=(*v1) k12=(*p2) k21=(*p3) k13=(*p4) k31=(*p5)
      (*rx_k) = (*p1);
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2);
      (*rx_k21) = (*p3);
      (*rx_k13) = (*p4);
      (*rx_k31) = (*p5);
      break;
    case 11:
#undef beta
#define A (1/(*v1))
#define B (*p3)
#define C (*p5)
#define alpha (*p1)
#define beta (*p2)
#define gamma (*p4)
      (*ncmt)=3;
      (*rx_v)=1/(A+B+C);
      btemp = -(alpha*C + alpha*B + gamma*A + gamma*B + beta*A + beta*C)*(*rx_v);
      ctemp = (alpha*beta*C + alpha*gamma*B + beta*gamma*A)*(*rx_v);
      dtemp = sqrt(btemp*btemp-4*ctemp);
      (*rx_k21) = 0.5*(-btemp+dtemp);
      (*rx_k31) = 0.5*(-btemp-dtemp);
      (*rx_k)   = alpha*beta*gamma/(*rx_k21)/(*rx_k31);
      (*rx_k12) = ((beta*gamma + alpha*beta + alpha*gamma) -
                   (*rx_k21)*(alpha+beta+gamma) - (*rx_k) * (*rx_k31) + (*rx_k21)*(*rx_k21))/((*rx_k31) - (*rx_k21));
      (*rx_k13) = alpha + beta + gamma - ((*rx_k) + (*rx_k12) + (*rx_k21) + (*rx_k31));
      break;
    case 10:
#undef A
#define A (*v1)
      (*ncmt)=3;
      (*rx_v)=1/(A+B+C);
      btemp = -(alpha*C + alpha*B + gamma*A + gamma*B + beta*A + beta*C)*(*rx_v);
      ctemp = (alpha*beta*C + alpha*gamma*B + beta*gamma*A)*(*rx_v);
      dtemp = sqrt(btemp*btemp-4*ctemp);
      (*rx_k21) = 0.5*(-btemp+dtemp);
      (*rx_k31) = 0.5*(-btemp-dtemp);
      (*rx_k)   = alpha*beta*gamma/(*rx_k21)/(*rx_k31);
      (*rx_k12) = ((beta*gamma + alpha*beta + alpha*gamma) -
                   (*rx_k21)*(alpha+beta+gamma) - (*rx_k) * (*rx_k31) + (*rx_k21)*(*rx_k21))/((*rx_k31) - (*rx_k21));
      (*rx_k13) = alpha + beta + gamma - ((*rx_k) + (*rx_k12) + (*rx_k21) + (*rx_k31));
#undef A
#undef B
#undef C
#undef alpha
#undef beta
#undef gamma
#define beta Rf_beta
      break;
    default:
      return NA_REAL;
    }
  } else if ((*p3) > 0.) {
    (*ncmt) = 2;
    switch (*trans){
    case 1: // cl=(*p1) v=(*v1) q=(*p2) vp=(*p3)
      (*rx_k) = (*p1)/(*v1); // k = CL/V
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2)/(*v1); // k12 = Q/V
      (*rx_k21) = (*p2)/(*p3); // k21 = Q/Vp
      break;
    case 2: // k=(*p1), (*v1)=v k12=(*p2) k21=(*p3)
      (*rx_k) = (*p1);
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2);
      (*rx_k21) = (*p3);
      break;
    case 3: // cl=(*p1) v=(*v1) q=(*p2) vss=(*p3)
      (*rx_k) = (*p1)/(*v1); // k = CL/V
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2)/(*v1); // k12 = Q/V
      (*rx_k21) = (*p2)/((*p3)-(*v1)); // k21 = Q/(Vss-V)
      break;
    case 4: // alpha=(*p1) beta=(*p2) k21=(*p3)
      (*rx_v) = (*v1);
      (*rx_k21) = (*p3);
      (*rx_k) = (*p1)*(*p2)/(*rx_k21); // (*p1) = alpha (*p2) = beta
      (*rx_k12) = (*p1) + (*p2) - (*rx_k21) - (*rx_k);
      break;
    case 5: // alpha=(*p1) beta=(*p2) aob=(*p3)
      (*rx_v)=(*v1);
      (*rx_k21) = ((*p3)*(*p2)+(*p1))/((*p3)+1.0);
      (*rx_k) = ((*p1)*(*p2))/(*rx_k21);
      (*rx_k12) = (*p1) + (*p2) - (*rx_k21) - (*rx_k);
      break;
    case 11: // A2 V, alpha=(*p1), beta=(*p2), k21
#undef beta
#define A (1/(*v1))
#define B (*p3)
#define alpha (*p1)
#define beta (*p2)
      (*ncmt)=2;
      (*rx_v)   = 1/(A+B);
      (*rx_k21) = (A*beta + B*alpha)*(*rx_v);
      (*rx_k)   = alpha*beta/(*rx_k21);
      (*rx_k12) = alpha+beta-(*rx_k21)-(*rx_k);
      break;
    case 10: // A=(*v1), alpha=(*p1), beta=(*p2), B=(*p3)
      // Convert to A (right now A=(*v1) or A=1/(*v1))
#undef A
#define A (*v1)
      (*ncmt)=2;
      (*rx_v)   = 1/(A + B);
      (*rx_k21) = (A*beta + B*alpha)*(*rx_v);
      (*rx_k)   = alpha*beta/(*rx_k21);
      (*rx_k12) = alpha + beta - (*rx_k21) - (*rx_k);
#undef A
#undef B
#undef alpha
#undef beta
#define beta Rf_beta
      break;
    default:
      return NA_REAL;
    }
  } else if ((*p1) > 0.) {
    (*ncmt) = 1;
    switch(*trans){
    case 1: // cl v
      (*rx_k) = (*p1)/(*v1); // k = CL/V
      (*rx_v) = (*v1);
      break;
    case 2: // k V
      (*rx_k) = (*p1);
      (*rx_v) = (*v1);
      break;
    case 11: // alpha V
      (*rx_k) = (*p1);
      (*rx_v) = (*v1);
      break;
    case 10: // alpha A
      (*rx_k) = (*p1);
      (*rx_v) = 1/(*v1);
      break;
    default:
      return 0;
    }
  } else {
    return 0;
  }
  return 1;
}
