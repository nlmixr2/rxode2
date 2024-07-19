#ifndef __SOLCOMP_H__
#define __SOLCOMP_H__

static inline int solComp2C(double *k10, double *k12, double *k21,
                            double *L, double *C1, double *C2) {
  // blas and R uses row major matrices
  double sum = (*k10) + (*k12) + (*k21);
  double disc= sqrt(sum*sum - 4.0* (*k10)*(*k21));
  double div[2];
  double tmp;
  L[0] = 0.5*(sum + disc);
  L[1] = 0.5*(sum - disc);
  div[0] = L[1] - L[0];
  div[1] = L[0] - L[1];
  if (div[0]*div[1] == 0) return 0;
  C1[0] = (*k21) - L[0];
  C1[2] = (*k21) - L[1];
  C2[0] = C2[2] = (*k21);
  C1[1] = C1[3] = (*k12);
  tmp = (*k10) + (*k12);
  C2[1] = tmp - L[0];
  C2[3] = tmp - L[1];
  C1[0] = C1[0]/div[0];
  C1[1] = C1[1]/div[0];
  C2[0] = C2[0]/div[0];
  C2[1] = C2[1]/div[0];
  C1[2] = C1[2]/div[1];
  C1[3] = C1[3]/div[1];
  C2[2] = C2[2]/div[1];
  C2[3] = C2[3]/div[1];
  return 1;
}

static inline int solComp3C(double *k10, double *k12, double *k21,
                            double *k13, double *k31,
                            double *L, double *C1, double *C2, double *C3) {
  // Get Lambdas
  double A1 = *k10 + *k12 + *k13 + *k21 + *k31;
  double A2 = (*k10)* (*k21) + (*k10)*(*k31) +
    (*k12)*(*k31) + (*k13)*(*k21) + (*k21)*(*k31);
  double A3 = (*k21)*(*k31)*(*k10);
  double Q  = (A1*A1 - 3.0*A2)/9.0;
  double RQ = 2.0*sqrt(Q);
  double R  = (2.0*A1*A1*A1 - 9.0*A1*A2 + 27.0*A3)/54.0;
  double M  = Q*Q*Q - R*R;
  if (M < 0) return 0;//stop("Error: Not real roots.")
  double Th = acos(8.0*R/(RQ*RQ*RQ));
  L[0] = RQ*cos(Th/3.0) + A1/3.0;
  L[1] = RQ*cos((Th + M_2PI)/3.0) + A1/3.0;
  L[2] = RQ*cos((Th + 2*M_2PI)/3.0) + A1/3.0;
  double D[3];
  D[0] = (L[1] - L[0])*(L[2] - L[0]);
  D[1] = (L[0] - L[1])*(L[2] - L[1]);
  D[2] = (L[0] - L[2])*(L[1] - L[2]);
  if (D[0]*D[1]*D[2] == 0.0) return 0;

  C1[0] = (*k21 - L[0])*(*k31 - L[0]);
  C1[3] = (*k21 - L[1])*(*k31 - L[1]);
  C1[6] = (*k21 - L[2])*(*k31 - L[2]);

  C2[0] = (*k21)*(*k31 - L[0]);
  C2[3] = (*k21)*(*k31 - L[1]);
  C2[6] = (*k21)*(*k31 - L[2]);

  C3[0] = (*k31)*(*k21 - L[0]);
  C3[3] = (*k31)*(*k21 - L[1]);
  C3[6] = (*k31)*(*k21 - L[2]);

  C1[1] = (*k12)*(*k31 - L[0]);
  C1[4] = (*k12)*(*k31 - L[1]);
  C1[7] = (*k12)*(*k31 - L[2]);

  C2[1] = (*k10 + *k12 + *k13 - L[0])*(*k31 - L[0]) - (*k31)*(*k13);
  C2[4] = (*k10 + *k12 + *k13 - L[1])*(*k31 - L[1]) - (*k31)*(*k13);
  C2[7] = (*k10 + *k12 + *k13 - L[2])*(*k31 - L[2]) - (*k31)*(*k13);

  C3[1] = C3[4] = C3[7] = (*k12)*(*k31);

  C1[2] = (*k13)*(*k21 - L[0]);
  C1[5] = (*k13)*(*k21 - L[1]);
  C1[8] = (*k13)*(*k21 - L[2]);

  C2[2] = C2[5] = C2[8] = (*k21)*(*k13);

  C3[2] = (*k10 + *k12 + *k13 - L[0])*(*k21 - L[0]) - (*k21)*(*k12);
  C3[5] = (*k10 + *k12 + *k13 - L[1])*(*k21 - L[1]) - (*k21)*(*k12);
  C3[8] = (*k10 + *k12 + *k13 - L[2])*(*k21 - L[2]) - (*k21)*(*k12);

  C1[0] = C1[0]/D[0];
  C1[1] = C1[1]/D[0];
  C1[2] = C1[2]/D[0];

  C2[0] = C2[0]/D[0];
  C2[1] = C2[1]/D[0];
  C2[2] = C2[2]/D[0];

  C3[0] = C3[0]/D[0];
  C3[1] = C3[1]/D[0];
  C3[2] = C3[2]/D[0];

  C1[3] = C1[3]/D[1];
  C1[4] = C1[4]/D[1];
  C1[5] = C1[5]/D[1];

  C2[3] = C2[3]/D[1];
  C2[4] = C2[4]/D[1];
  C2[5] = C2[5]/D[1];

  C3[3] = C3[3]/D[1];
  C3[4] = C3[4]/D[1];
  C3[5] = C3[5]/D[1];

  C1[6] = C1[6]/D[2];
  C1[7] = C1[7]/D[2];
  C1[8] = C1[8]/D[2];

  C2[6] = C2[6]/D[2];
  C2[7] = C2[7]/D[2];
  C2[8] = C2[8]/D[2];

  C3[6] = C3[6]/D[2];
  C3[7] = C3[7]/D[2];
  C3[8] = C3[8]/D[2];

  return 1;
}

#endif
