#ifndef __rxode2random_fillVec_h__
#define __rxode2random_fillVec_h__

static inline arma::vec fillVec(arma::vec& in, int len){
  if ((int)in.size() == len){
    return in;
  } else if ((int)(in.size()) == 1){
    arma::vec out(len);
    std::fill_n(out.begin(), len, in[0]);
    return out;
  } else {
    arma::vec z;
    return z;
  }
}

#endif
