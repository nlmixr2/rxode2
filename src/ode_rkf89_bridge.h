#ifndef ODE_RKF89_BRIDGE_H_
#define ODE_RKF89_BRIDGE_H_

#include "ode/ode_rkf89.h"

class RxRkf89 : public ode::OdeRkf89 {
    t_dydt                  dydt_;
    int                    *full_neq_;
    rx_solving_options_ind *ind_;
    double                  t_stage_;
    long unsigned           max_steps_;

public:
    RxRkf89(t_dydt dydt, int *full_neq, int neqOde,
            rx_solving_options_ind *ind, double *yp,
            long unsigned max_steps)
        : ode::OdeRkf89((unsigned long)neqOde),
          dydt_(dydt), full_neq_(full_neq), ind_(ind),
          t_stage_(0.0), max_steps_(max_steps)
    {
        set_sol_external(yp);
    }

protected:
    void ode_fun(double * __restrict__ solin, double * __restrict__ fout) override {
        if (ind_->err != 0) {
            for (unsigned long i = 0; i < neq_; i++) fout[i] = 0.0;
            return;
        }
        dydt_(full_neq_, t_stage_, solin, fout);
        for (unsigned long i = 0; i < neq_; i++) {
            if (!std::isfinite(fout[i])) {
                ind_->err = 1;
                for (unsigned long j = 0; j < neq_; j++) fout[j] = 0.0;
                return;
            }
        }
    }





    void step_(double dt) override {
        // Local parameters (Feagin f0-indexed method)
        const double a1 = 0.44368940376498183109599404281370;
        const double a2 = 0.66553410564747274664399106422055;
        const double a3 = 0.99830115847120911996598659633083;
        const double a4 = 0.3155;
        const double a5 = 0.50544100948169068626516126737384;
        const double a6 = 0.17142857142857142857142857142857;
        const double a7 = 0.82857142857142857142857142857143;
        const double a8 = 0.66543966121011562534953769255586;
        const double a9 = 0.24878317968062652069722274560771;
        const double a10 = 0.1090;
        const double a11 = 0.8910;
        const double a12 = 0.3995;
        const double a13 = 0.6005;
        const double a14 = 1.0;
        const double a16 = 1.0;
        const double b1 = 0.44368940376498183109599404281370;
        const double b20 = 0.16638352641186818666099776605514;
        const double b21 = 0.49915057923560455998299329816541;
        const double b30 = 0.24957528961780227999149664908271;
        const double b32 = 0.74872586885340683997448994724812;
        const double b40 = 0.20661891163400602426556710393185;
        const double b42 = 0.17707880377986347040380997288319;
        const double b43 = -0.68197715413869494669377076815048e-1;
        const double b50 = 0.10927823152666408227903890926157;
        const double b53 = 0.40215962642367995421990563690087e-2;
        const double b54 = 0.39214118169078980444392330174325;
        const double b60 = 0.98899281409164665304844765434355e-1;
        const double b63 = 0.35138370227963966951204487356703e-2;
        const double b64 = 0.12476099983160016621520625872489;
        const double b65 = -0.55745546834989799643742901466348e-1;
        const double b70 = -0.36806865286242203724153101080691;
        const double b74 = -0.22273897469476007645024020944166e+1;
        const double b75 = 0.13742908256702910729565691245744e+1;
        const double b76 = 0.20497390027111603002159354092206e+1;
        const double b80 = 0.45467962641347150077351950603349e-1;
        const double b85 = 0.32542131701589147114677469648853;
        const double b86 = 0.28476660138527908888182420573687;
        const double b87 = 0.97837801675979152435868397271099e-2;
        const double b90 = 0.60842071062622057051094145205182e-1;
        const double b95 = -0.21184565744037007526325275251206e-1;
        const double b96 = 0.19596557266170831957464490662983;
        const double b97 = -0.42742640364817603675144835342899e-2;
        const double b98 = 0.17434365736814911965323452558189e-1;
        const double b100 = 0.54059783296931917365785724111182e-1;
        const double b106 = 0.11029825597828926530283127648228;
        const double b107 = -0.12565008520072556414147763782250e-2;
        const double b108 = 0.36790043477581460136384043566339e-2;
        const double b109 = -0.57780542770972073040840628571866e-1;
        const double b110 = 0.12732477068667114646645181799160;
        const double b117 = 0.11448805006396105323658875721817;
        const double b118 = 0.28773020709697992776202201849198;
        const double b119 = 0.50945379459611363153735885079465;
        const double b1110 = -0.14799682244372575900242144449640;
        const double b120 = -0.36526793876616740535848544394333e-2;
        const double b125 = 0.81629896012318919777819421247030e-1;
        const double b126 = -0.38607735635693506490517694343215;
        const double b127 = 0.30862242924605106450474166025206e-1;
        const double b128 = -0.58077254528320602815829374733518e-1;
        const double b129 = 0.33598659328884971493143451362322;
        const double b1210 = 0.41066880401949958613549622786417;
        const double b1211 = -0.11840245972355985520633156154536e-1;
        const double b130 = -0.12375357921245143254979096135669e+1;
        const double b135 = -0.24430768551354785358734861366763e+2;
        const double b136 = 0.54779568932778656050436528991173;
        const double b137 = -0.44413863533413246374959896569346e+1;
        const double b138 = 0.10013104813713266094792617851022e+2;
        const double b139 = -0.14995773102051758447170985073142e+2;
        const double b1310 = 0.58946948523217013620824539651427e+1;
        const double b1311 = 0.17380377503428984877616857440542e+1;
        const double b1312 = 0.27512330693166730263758622860276e+2;
        const double b140 = -0.35260859388334522700502958875588;
        const double b145 = -0.18396103144848270375044198988231;
        const double b146 = -0.65570189449741645138006879985251;
        const double b147 = -0.39086144880439863435025520241310;
        const double b148 = 0.26794646712850022936584423271209;
        const double b149 = -0.10383022991382490865769858507427e+1;
        const double b1410 = 0.16672327324258671664727346168501e+1;
        const double b1411 = 0.49551925855315977067732967071441;
        const double b1412 = 0.11394001132397063228586738141784e+1;
        const double b1413 = 0.51336696424658613688199097191534e-1;
        const double b150 = 0.10464847340614810391873002406755e-2;
        const double b158 = -0.67163886844990282237778446178020e-2;
        const double b159 = 0.81828762189425021265330065248999e-2;
        const double b1510 = -0.42640342864483347277142138087561e-2;
        const double b1511 = 0.28009029474168936545976331153703e-3;
        const double b1512 = -0.87835333876238676639057813145633e-2;
        const double b1513 = 0.10254505110825558084217769664009e-1;
        const double b160 = -0.13536550786174067080442168889966e+1;
        const double b165 = -0.18396103144848270375044198988231;
        const double b166 = -0.65570189449741645138006879985251;
        const double b167 = -0.39086144880439863435025520241310;
        const double b168 = 0.27466285581299925758962207732989;
        const double b169 = -0.10464851753571915887035188572676e+1;
        const double b1610 = 0.16714967667123155012004488306588e+1;
        const double b1611 = 0.49523916825841808131186990740287;
        const double b1612 = 0.11481836466273301905225795954930e+1;
        const double b1613 = 0.41082191313833055603981327527525e-1;
        const double b1615 = 1.0;
        const double c0 = 0.32256083500216249913612900960247e-1;
        const double c8 = 0.25983725283715403018887023171963;
        const double c9 = 0.92847805996577027788063714302190e-1;
        const double c10 = 0.16452339514764342891647731842800;
        const double c11 = 0.17665951637860074367084298397547;
        const double c12 = 0.23920102320352759374108933320941;
        const double c13 = 0.39484274604202853746752118829325e-2;
        const double c14 = 0.30726495475860640406368305522124e-1;
        // Stage 1 -> k_[0]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i];
        t_stage_ = t_;
        ode_fun_(soltemp_, k_[0]);
        // Stage 2 -> k_[1]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+k_[0][i]*b1*dt;
        t_stage_ = t_ + a1 * dt;
        ode_fun_(soltemp_, k_[1]);
        // Stage 3 -> k_[2]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b20+k_[1][i]*b21)*dt;
        t_stage_ = t_ + a2 * dt;
        ode_fun_(soltemp_, k_[2]);
        // Stage 4 -> k_[3]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b30+k_[2][i]*b32)*dt;
        t_stage_ = t_ + a3 * dt;
        ode_fun_(soltemp_, k_[3]);
        // Stage 5 -> k_[4]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b40+k_[2][i]*b42+k_[3][i]*b43)*dt;
        t_stage_ = t_ + a4 * dt;
        ode_fun_(soltemp_, k_[4]);
        // Stage 6 -> k_[5]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b50+k_[3][i]*b53+k_[4][i]*b54)*dt;
        t_stage_ = t_ + a5 * dt;
        ode_fun_(soltemp_, k_[5]);
        // Stage 7 -> k_[6]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b60+k_[3][i]*b63+k_[4][i]*b64+k_[5][i]*b65)*dt;
        t_stage_ = t_ + a6 * dt;
        ode_fun_(soltemp_, k_[6]);
        // Stage 8 -> k_[7]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b70+k_[4][i]*b74+k_[5][i]*b75+k_[6][i]*b76)*dt;
        t_stage_ = t_ + a7 * dt;
        ode_fun_(soltemp_, k_[7]);
        // Stage 9 -> k_[8]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b80+k_[5][i]*b85+k_[6][i]*b86+k_[7][i]*b87)*dt;
        t_stage_ = t_ + a8 * dt;
        ode_fun_(soltemp_, k_[8]);
        // Stage 10 -> k_[9]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b90+k_[5][i]*b95+k_[6][i]*b96+k_[7][i]*b97+k_[8][i]*b98)*dt;
        t_stage_ = t_ + a9 * dt;
        ode_fun_(soltemp_, k_[9]);
        // Stage 11 -> k_[10]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b100+k_[6][i]*b106+k_[7][i]*b107+k_[8][i]*b108+ k_[9][i]*b109)*dt;
        t_stage_ = t_ + a10 * dt;
        ode_fun_(soltemp_, k_[10]);
        // Stage 12 -> k_[11]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b110+k_[7][i]*b117+k_[8][i]*b118+k_[9][i]*b119+ k_[10][i]*b1110)*dt;
        t_stage_ = t_ + a11 * dt;
        ode_fun_(soltemp_, k_[11]);
        // Stage 13 -> k_[12]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b120+k_[5][i]*b125+k_[6][i]*b126+k_[7][i]*b127+ k_[8][i]*b128+k_[9][i]*b129+k_[10][i]*b1210+k_[11][i]*b1211)*dt;
        t_stage_ = t_ + a12 * dt;
        ode_fun_(soltemp_, k_[12]);
        // Stage 14 -> k_[13]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b130+k_[5][i]*b135+k_[6][i]*b136+k_[7][i]*b137+ k_[8][i]*b138+k_[9][i]*b139+k_[10][i]*b1310+k_[11][i]*b1311+k_[12][i]*b1312)*dt;
        t_stage_ = t_ + a13 * dt;
        ode_fun_(soltemp_, k_[13]);
        // Stage 15 -> k_[14]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b140+k_[5][i]*b145+k_[6][i]*b146+k_[7][i]*b147+k_[8][i]*b148+ k_[9][i]*b149+k_[10][i]*b1410+k_[11][i]*b1411+k_[12][i]*b1412+k_[13][i]*b1413)*dt;
        t_stage_ = t_ + a14 * dt;
        ode_fun_(soltemp_, k_[14]);
        // Stage 16 -> k_[15]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b150+k_[8][i]*b158+k_[9][i]*b159+k_[10][i]*b1510+k_[11][i]*b1511+ k_[12][i]*b1512+k_[13][i]*b1513)*dt;
        t_stage_ = t_;
        ode_fun_(soltemp_, k_[15]);
        // Stage 17 -> k_[16]
        for (unsigned long i = 0; i < neq_; i++)
            soltemp_[i] = sol_[i]+(k_[0][i]*b160+k_[5][i]*b165+k_[6][i]*b166+k_[7][i]*b167+k_[8][i]*b168+ k_[9][i]*b169+k_[10][i]*b1610+k_[11][i]*b1611+k_[12][i]*b1612+k_[13][i]*b1613+ k_[15][i]*b1615)*dt;
        t_stage_ = t_ + a16 * dt;
        ode_fun_(soltemp_, k_[16]);
        // Primary solution (sol_)
        for (unsigned long i = 0; i < neq_; i++)
            sol_[i] = sol_[i]+dt*(k_[0][i]*c0+k_[8][i]*c8+k_[9][i]*c9+k_[10][i]*c10+k_[11][i]*c11+k_[12][i]*c12+k_[13][i]*c13+k_[14][i]*c14);
        // Embedded solution (solemb_ = sol_ - xerr)
        for (unsigned long i = 0; i < neq_; i++)
            solemb_[i] = sol_[i] - c14*dt*(k_[0][i]+k_[14][i]-k_[15][i]-k_[16][i]);
    }

    void after_step(double) override {
        if (ind_->err != 0) throw std::runtime_error("rkf89: ode error");
        if (nstep_ > max_steps_) {
            if (ind_->rc[0] == 0) ind_->rc[0] = -2019;
            ind_->err = 1;
            throw std::runtime_error("rkf89: max steps exceeded");
        }
    }
};

#endif // ODE_RKF89_BRIDGE_H_
