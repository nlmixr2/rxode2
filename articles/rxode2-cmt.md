# Changing rxode2 compartment numbers

## How rxode2 assigns compartment numbers

rxode2 automatically assigns compartment numbers when parsing. For
example, with the Mavoglurant PBPK model the following model may be
used:

``` r
library(rxode2)
```

    #> rxode2 5.0.0 using 2 threads (see ?getRxThreads)
    #>   no cache: create with `rxCreateCache()`

``` r
pbpk <- function() {
  model({
    KbBR = exp(lKbBR)
    KbMU = exp(lKbMU)
    KbAD = exp(lKbAD)
    CLint= exp(lCLint + eta.LClint)
    KbBO = exp(lKbBO)
    KbRB = exp(lKbRB)

    ## Regional blood flows
    # Cardiac output (L/h) from White et al (1968)
    CO  = (187.00*WT^0.81)*60/1000 
    QHT = 4.0 *CO/100
    QBR = 12.0*CO/100
    QMU = 17.0*CO/100
    QAD = 5.0 *CO/100
    QSK = 5.0 *CO/100
    QSP = 3.0 *CO/100
    QPA = 1.0 *CO/100
    QLI = 25.5*CO/100
    QST = 1.0 *CO/100
    QGU = 14.0*CO/100
    # Hepatic artery blood flow
    QHA = QLI - (QSP + QPA + QST + QGU) 
    QBO = 5.0 *CO/100
    QKI = 19.0*CO/100
    QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI)
    QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB

    ## Organs' volumes = organs' weights / organs' density
    VLU = (0.76 *WT/100)/1.051
    VHT = (0.47 *WT/100)/1.030
    VBR = (2.00 *WT/100)/1.036
    VMU = (40.00*WT/100)/1.041
    VAD = (21.42*WT/100)/0.916
    VSK = (3.71 *WT/100)/1.116
    VSP = (0.26 *WT/100)/1.054
    VPA = (0.14 *WT/100)/1.045
    VLI = (2.57 *WT/100)/1.040
    VST = (0.21 *WT/100)/1.050
    VGU = (1.44 *WT/100)/1.043
    VBO = (14.29*WT/100)/1.990
    VKI = (0.44 *WT/100)/1.050
    VAB = (2.81 *WT/100)/1.040
    VVB = (5.62 *WT/100)/1.040
    VRB = (3.86 *WT/100)/1.040

    ## Fixed parameters
    BP = 0.61      # Blood:plasma partition coefficient
    fup = 0.028    # Fraction unbound in plasma
    fub = fup/BP   # Fraction unbound in blood

    KbLU = exp(0.8334)
    KbHT = exp(1.1205)
    KbSK = exp(-.5238)
    KbSP = exp(0.3224)
    KbPA = exp(0.3224)
    KbLI = exp(1.7604)
    KbST = exp(0.3224)
    KbGU = exp(1.2026)
    KbKI = exp(1.3171)

    ##-----------------------------------------
    S15 = VVB*BP/1000
    C15 = Venous_Blood/S15

    ##-----------------------------------------
    d/dt(Lungs) = QLU*(Venous_Blood/VVB - Lungs/KbLU/VLU)
    d/dt(Heart) = QHT*(Arterial_Blood/VAB - Heart/KbHT/VHT)
    d/dt(Brain) = QBR*(Arterial_Blood/VAB - Brain/KbBR/VBR)
    d/dt(Muscles) = QMU*(Arterial_Blood/VAB - Muscles/KbMU/VMU)
    d/dt(Adipose) = QAD*(Arterial_Blood/VAB - Adipose/KbAD/VAD)
    d/dt(Skin) = QSK*(Arterial_Blood/VAB - Skin/KbSK/VSK)
    d/dt(Spleen) = QSP*(Arterial_Blood/VAB - Spleen/KbSP/VSP)
    d/dt(Pancreas) = QPA*(Arterial_Blood/VAB - Pancreas/KbPA/VPA)
    d/dt(Liver) = QHA*Arterial_Blood/VAB + QSP*Spleen/KbSP/VSP +
      QPA*Pancreas/KbPA/VPA + QST*Stomach/KbST/VST +
      QGU*Gut/KbGU/VGU - CLint*fub*Liver/KbLI/VLI - QLI*Liver/KbLI/VLI
    d/dt(Stomach) = QST*(Arterial_Blood/VAB - Stomach/KbST/VST)
    d/dt(Gut) = QGU*(Arterial_Blood/VAB - Gut/KbGU/VGU)
    d/dt(Bones) = QBO*(Arterial_Blood/VAB - Bones/KbBO/VBO)
    d/dt(Kidneys) = QKI*(Arterial_Blood/VAB - Kidneys/KbKI/VKI)
    d/dt(Arterial_Blood) = QLU*(Lungs/KbLU/VLU - Arterial_Blood/VAB)
    d/dt(Venous_Blood) = QHT*Heart/KbHT/VHT + QBR*Brain/KbBR/VBR +
      QMU*Muscles/KbMU/VMU + QAD*Adipose/KbAD/VAD + QSK*Skin/KbSK/VSK +
      QLI*Liver/KbLI/VLI + QBO*Bones/KbBO/VBO + QKI*Kidneys/KbKI/VKI +
      QRB*Rest_of_Body/KbRB/VRB - QLU*Venous_Blood/VVB
    d/dt(Rest_of_Body) = QRB*(Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB)
  })
}
```

If you look at the printout, you can see where rxode2 assigned the
compartment number(s)

``` r
pbpk <- pbpk()
print(pbpk)
```

    #>  -- rxode2-based free-form 16-cmt ODE model ------------------------------------- 
    #> 
    #> States ($state or $stateDf): 
    #>    Compartment Number Compartment Name
    #> 1                   1            Lungs
    #> 2                   2            Heart
    #> 3                   3            Brain
    #> 4                   4          Muscles
    #> 5                   5          Adipose
    #> 6                   6             Skin
    #> 7                   7           Spleen
    #> 8                   8         Pancreas
    #> 9                   9            Liver
    #> 10                 10          Stomach
    #> 11                 11              Gut
    #> 12                 12            Bones
    #> 13                 13          Kidneys
    #> 14                 14   Arterial_Blood
    #> 15                 15     Venous_Blood
    #> 16                 16     Rest_of_Body
    #>  -- Model (Normalized Syntax): -- 
    #> function() {
    #>     model({
    #>         KbBR = exp(lKbBR)
    #>         KbMU = exp(lKbMU)
    #>         KbAD = exp(lKbAD)
    #>         CLint = exp(lCLint + eta.LClint)
    #>         KbBO = exp(lKbBO)
    #>         KbRB = exp(lKbRB)
    #>         CO = (187 * WT^0.81) * 60/1000
    #>         QHT = 4 * CO/100
    #>         QBR = 12 * CO/100
    #>         QMU = 17 * CO/100
    #>         QAD = 5 * CO/100
    #>         QSK = 5 * CO/100
    #>         QSP = 3 * CO/100
    #>         QPA = 1 * CO/100
    #>         QLI = 25.5 * CO/100
    #>         QST = 1 * CO/100
    #>         QGU = 14 * CO/100
    #>         QHA = QLI - (QSP + QPA + QST + QGU)
    #>         QBO = 5 * CO/100
    #>         QKI = 19 * CO/100
    #>         QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + 
    #>             QKI)
    #>         QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + 
    #>             QRB
    #>         VLU = (0.76 * WT/100)/1.051
    #>         VHT = (0.47 * WT/100)/1.03
    #>         VBR = (2 * WT/100)/1.036
    #>         VMU = (40 * WT/100)/1.041
    #>         VAD = (21.42 * WT/100)/0.916
    #>         VSK = (3.71 * WT/100)/1.116
    #>         VSP = (0.26 * WT/100)/1.054
    #>         VPA = (0.14 * WT/100)/1.045
    #>         VLI = (2.57 * WT/100)/1.04
    #>         VST = (0.21 * WT/100)/1.05
    #>         VGU = (1.44 * WT/100)/1.043
    #>         VBO = (14.29 * WT/100)/1.99
    #>         VKI = (0.44 * WT/100)/1.05
    #>         VAB = (2.81 * WT/100)/1.04
    #>         VVB = (5.62 * WT/100)/1.04
    #>         VRB = (3.86 * WT/100)/1.04
    #>         BP = 0.61
    #>         fup = 0.028
    #>         fub = fup/BP
    #>         KbLU = exp(0.8334)
    #>         KbHT = exp(1.1205)
    #>         KbSK = exp(-0.5238)
    #>         KbSP = exp(0.3224)
    #>         KbPA = exp(0.3224)
    #>         KbLI = exp(1.7604)
    #>         KbST = exp(0.3224)
    #>         KbGU = exp(1.2026)
    #>         KbKI = exp(1.3171)
    #>         S15 = VVB * BP/1000
    #>         C15 = Venous_Blood/S15
    #>         d/dt(Lungs) = QLU * (Venous_Blood/VVB - Lungs/KbLU/VLU)
    #>         d/dt(Heart) = QHT * (Arterial_Blood/VAB - Heart/KbHT/VHT)
    #>         d/dt(Brain) = QBR * (Arterial_Blood/VAB - Brain/KbBR/VBR)
    #>         d/dt(Muscles) = QMU * (Arterial_Blood/VAB - Muscles/KbMU/VMU)
    #>         d/dt(Adipose) = QAD * (Arterial_Blood/VAB - Adipose/KbAD/VAD)
    #>         d/dt(Skin) = QSK * (Arterial_Blood/VAB - Skin/KbSK/VSK)
    #>         d/dt(Spleen) = QSP * (Arterial_Blood/VAB - Spleen/KbSP/VSP)
    #>         d/dt(Pancreas) = QPA * (Arterial_Blood/VAB - Pancreas/KbPA/VPA)
    #>         d/dt(Liver) = QHA * Arterial_Blood/VAB + QSP * Spleen/KbSP/VSP + 
    #>             QPA * Pancreas/KbPA/VPA + QST * Stomach/KbST/VST + 
    #>             QGU * Gut/KbGU/VGU - CLint * fub * Liver/KbLI/VLI - 
    #>             QLI * Liver/KbLI/VLI
    #>         d/dt(Stomach) = QST * (Arterial_Blood/VAB - Stomach/KbST/VST)
    #>         d/dt(Gut) = QGU * (Arterial_Blood/VAB - Gut/KbGU/VGU)
    #>         d/dt(Bones) = QBO * (Arterial_Blood/VAB - Bones/KbBO/VBO)
    #>         d/dt(Kidneys) = QKI * (Arterial_Blood/VAB - Kidneys/KbKI/VKI)
    #>         d/dt(Arterial_Blood) = QLU * (Lungs/KbLU/VLU - Arterial_Blood/VAB)
    #>         d/dt(Venous_Blood) = QHT * Heart/KbHT/VHT + QBR * Brain/KbBR/VBR + 
    #>             QMU * Muscles/KbMU/VMU + QAD * Adipose/KbAD/VAD + 
    #>             QSK * Skin/KbSK/VSK + QLI * Liver/KbLI/VLI + QBO * 
    #>             Bones/KbBO/VBO + QKI * Kidneys/KbKI/VKI + QRB * Rest_of_Body/KbRB/VRB - 
    #>             QLU * Venous_Blood/VVB
    #>         d/dt(Rest_of_Body) = QRB * (Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB)
    #>     })
    #> }

You can also see this with the classic rxode2 model. In that case you
use the [`summary()`](https://rdrr.io/r/base/summary.html) function:

``` r
pbpk <- pbpk$simulationModel
summary(pbpk)
```

    #> rxode2 5.0.0 model named rx_4c9114305c501d69e89db5e9b587978a model (ready). 
    #> DLL: /tmp/Rtmpk9VFFh/rxode2/rx_4c9114305c501d69e89db5e9b587978a__.rxd/rx_4c9114305c501d69e89db5e9b587978a_.so
    #> NULL
    #> 
    #> Calculated Variables:
    #>  [1] "KbBR"  "KbMU"  "KbAD"  "CLint" "KbBO"  "KbRB"  "CO"    "QHT"   "QBR"  
    #> [10] "QMU"   "QAD"   "QSK"   "QSP"   "QPA"   "QLI"   "QST"   "QGU"   "QHA"  
    #> [19] "QBO"   "QKI"   "QRB"   "QLU"   "VLU"   "VHT"   "VBR"   "VMU"   "VAD"  
    #> [28] "VSK"   "VSP"   "VPA"   "VLI"   "VST"   "VGU"   "VBO"   "VKI"   "VAB"  
    #> [37] "VVB"   "VRB"   "fub"   "KbLU"  "KbHT"  "KbSK"  "KbSP"  "KbPA"  "KbLI" 
    #> [46] "KbST"  "KbGU"  "KbKI"  "S15"   "C15"  
    #> -- rxode2 Model Syntax --
    #> rxode2({
    #>     param(lKbBR, lKbMU, lKbAD, lCLint, eta.LClint, lKbBO, lKbRB, 
    #>         WT)
    #>     KbBR = exp(lKbBR)
    #>     KbMU = exp(lKbMU)
    #>     KbAD = exp(lKbAD)
    #>     CLint = exp(lCLint + eta.LClint)
    #>     KbBO = exp(lKbBO)
    #>     KbRB = exp(lKbRB)
    #>     CO = (187 * WT^0.81) * 60/1000
    #>     QHT = 4 * CO/100
    #>     QBR = 12 * CO/100
    #>     QMU = 17 * CO/100
    #>     QAD = 5 * CO/100
    #>     QSK = 5 * CO/100
    #>     QSP = 3 * CO/100
    #>     QPA = 1 * CO/100
    #>     QLI = 25.5 * CO/100
    #>     QST = 1 * CO/100
    #>     QGU = 14 * CO/100
    #>     QHA = QLI - (QSP + QPA + QST + QGU)
    #>     QBO = 5 * CO/100
    #>     QKI = 19 * CO/100
    #>     QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI)
    #>     QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB
    #>     VLU = (0.76 * WT/100)/1.051
    #>     VHT = (0.47 * WT/100)/1.03
    #>     VBR = (2 * WT/100)/1.036
    #>     VMU = (40 * WT/100)/1.041
    #>     VAD = (21.42 * WT/100)/0.916
    #>     VSK = (3.71 * WT/100)/1.116
    #>     VSP = (0.26 * WT/100)/1.054
    #>     VPA = (0.14 * WT/100)/1.045
    #>     VLI = (2.57 * WT/100)/1.04
    #>     VST = (0.21 * WT/100)/1.05
    #>     VGU = (1.44 * WT/100)/1.043
    #>     VBO = (14.29 * WT/100)/1.99
    #>     VKI = (0.44 * WT/100)/1.05
    #>     VAB = (2.81 * WT/100)/1.04
    #>     VVB = (5.62 * WT/100)/1.04
    #>     VRB = (3.86 * WT/100)/1.04
    #>     BP = 0.61
    #>     fup = 0.028
    #>     fub = fup/BP
    #>     KbLU = exp(0.8334)
    #>     KbHT = exp(1.1205)
    #>     KbSK = exp(-0.5238)
    #>     KbSP = exp(0.3224)
    #>     KbPA = exp(0.3224)
    #>     KbLI = exp(1.7604)
    #>     KbST = exp(0.3224)
    #>     KbGU = exp(1.2026)
    #>     KbKI = exp(1.3171)
    #>     S15 = VVB * BP/1000
    #>     C15 = Venous_Blood/S15
    #>     d/dt(Lungs) = QLU * (Venous_Blood/VVB - Lungs/KbLU/VLU)
    #>     d/dt(Heart) = QHT * (Arterial_Blood/VAB - Heart/KbHT/VHT)
    #>     d/dt(Brain) = QBR * (Arterial_Blood/VAB - Brain/KbBR/VBR)
    #>     d/dt(Muscles) = QMU * (Arterial_Blood/VAB - Muscles/KbMU/VMU)
    #>     d/dt(Adipose) = QAD * (Arterial_Blood/VAB - Adipose/KbAD/VAD)
    #>     d/dt(Skin) = QSK * (Arterial_Blood/VAB - Skin/KbSK/VSK)
    #>     d/dt(Spleen) = QSP * (Arterial_Blood/VAB - Spleen/KbSP/VSP)
    #>     d/dt(Pancreas) = QPA * (Arterial_Blood/VAB - Pancreas/KbPA/VPA)
    #>     d/dt(Liver) = QHA * Arterial_Blood/VAB + QSP * Spleen/KbSP/VSP + 
    #>         QPA * Pancreas/KbPA/VPA + QST * Stomach/KbST/VST + QGU * 
    #>         Gut/KbGU/VGU - CLint * fub * Liver/KbLI/VLI - QLI * Liver/KbLI/VLI
    #>     d/dt(Stomach) = QST * (Arterial_Blood/VAB - Stomach/KbST/VST)
    #>     d/dt(Gut) = QGU * (Arterial_Blood/VAB - Gut/KbGU/VGU)
    #>     d/dt(Bones) = QBO * (Arterial_Blood/VAB - Bones/KbBO/VBO)
    #>     d/dt(Kidneys) = QKI * (Arterial_Blood/VAB - Kidneys/KbKI/VKI)
    #>     d/dt(Arterial_Blood) = QLU * (Lungs/KbLU/VLU - Arterial_Blood/VAB)
    #>     d/dt(Venous_Blood) = QHT * Heart/KbHT/VHT + QBR * Brain/KbBR/VBR + 
    #>         QMU * Muscles/KbMU/VMU + QAD * Adipose/KbAD/VAD + QSK * 
    #>         Skin/KbSK/VSK + QLI * Liver/KbLI/VLI + QBO * Bones/KbBO/VBO + 
    #>         QKI * Kidneys/KbKI/VKI + QRB * Rest_of_Body/KbRB/VRB - 
    #>         QLU * Venous_Blood/VVB
    #>     d/dt(Rest_of_Body) = QRB * (Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB)
    #> })

In this case, `Venous_Blood` is assigned to compartment `15`. Figuring
this out can be inconvenient and also lead to re-numbering compartment
in simulation or estimation datasets. While it is easy and probably
clearer to specify the [compartment by
name](https://nlmixr2.github.io/rxode2/articles/rxode2-events.md), other
tools only support compartment numbers. Therefore, having a way to
number compartment easily can lead to less data modification between
multiple tools.

## Changing compartment numbers by pre-declaring the compartments

To add the compartments to the rxode2 model in the order you desire you
simply need to pre-declare the compartments with `cmt`. For example
specifying is `Venous_Blood` and `Skin` to be the 1st and 2nd
compartments, respectively, is simple:

``` r
pbpk2 <- function() {
  model({
    ## Now this is the first compartment, ie cmt=1
    cmt(Venous_Blood)
    ## Skin may be a compartment you wish to dose to as well,
    ##  so it is now cmt=2
    cmt(Skin)
    KbBR = exp(lKbBR)
    KbMU = exp(lKbMU)
    KbAD = exp(lKbAD)
    CLint= exp(lCLint + eta.LClint)
    KbBO = exp(lKbBO)
    KbRB = exp(lKbRB)

    ## Regional blood flows
    # Cardiac output (L/h) from White et al (1968)m
    CO  = (187.00*WT^0.81)*60/1000;
    QHT = 4.0 *CO/100;
    QBR = 12.0*CO/100;
    QMU = 17.0*CO/100;
    QAD = 5.0 *CO/100;
    QSK = 5.0 *CO/100;
    QSP = 3.0 *CO/100;
    QPA = 1.0 *CO/100;
    QLI = 25.5*CO/100;
    QST = 1.0 *CO/100;
    QGU = 14.0*CO/100;
    QHA = QLI - (QSP + QPA + QST + QGU); # Hepatic artery blood flow
    QBO = 5.0 *CO/100;
    QKI = 19.0*CO/100;
    QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI);
    QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB;

    ## Organs' volumes = organs' weights / organs' density
    VLU = (0.76 *WT/100)/1.051;
    VHT = (0.47 *WT/100)/1.030;
    VBR = (2.00 *WT/100)/1.036;
    VMU = (40.00*WT/100)/1.041;
    VAD = (21.42*WT/100)/0.916;
    VSK = (3.71 *WT/100)/1.116;
    VSP = (0.26 *WT/100)/1.054;
    VPA = (0.14 *WT/100)/1.045;
    VLI = (2.57 *WT/100)/1.040;
    VST = (0.21 *WT/100)/1.050;
    VGU = (1.44 *WT/100)/1.043;
    VBO = (14.29*WT/100)/1.990;
    VKI = (0.44 *WT/100)/1.050;
    VAB = (2.81 *WT/100)/1.040;
    VVB = (5.62 *WT/100)/1.040;
    VRB = (3.86 *WT/100)/1.040;

    ## Fixed parameters
    BP = 0.61;      # Blood:plasma partition coefficient
    fup = 0.028;    # Fraction unbound in plasma
    fub = fup/BP;   # Fraction unbound in blood

    KbLU = exp(0.8334);
    KbHT = exp(1.1205);
    KbSK = exp(-.5238);
    KbSP = exp(0.3224);
    KbPA = exp(0.3224);
    KbLI = exp(1.7604);
    KbST = exp(0.3224);
    KbGU = exp(1.2026);
    KbKI = exp(1.3171);

    ##-----------------------------------------
    S15 = VVB*BP/1000;
    C15 = Venous_Blood/S15

    ##-----------------------------------------
    d/dt(Lungs) = QLU*(Venous_Blood/VVB - Lungs/KbLU/VLU);
    d/dt(Heart) = QHT*(Arterial_Blood/VAB - Heart/KbHT/VHT);
    d/dt(Brain) = QBR*(Arterial_Blood/VAB - Brain/KbBR/VBR);
    d/dt(Muscles) = QMU*(Arterial_Blood/VAB - Muscles/KbMU/VMU);
    d/dt(Adipose) = QAD*(Arterial_Blood/VAB - Adipose/KbAD/VAD);
    d/dt(Skin) = QSK*(Arterial_Blood/VAB - Skin/KbSK/VSK);
    d/dt(Spleen) = QSP*(Arterial_Blood/VAB - Spleen/KbSP/VSP);
    d/dt(Pancreas) = QPA*(Arterial_Blood/VAB - Pancreas/KbPA/VPA);
    d/dt(Liver) = QHA*Arterial_Blood/VAB + QSP*Spleen/KbSP/VSP +
      QPA*Pancreas/KbPA/VPA + QST*Stomach/KbST/VST + QGU*Gut/KbGU/VGU -
      CLint*fub*Liver/KbLI/VLI - QLI*Liver/KbLI/VLI;
      d/dt(Stomach) = QST*(Arterial_Blood/VAB - Stomach/KbST/VST);
      d/dt(Gut) = QGU*(Arterial_Blood/VAB - Gut/KbGU/VGU);
      d/dt(Bones) = QBO*(Arterial_Blood/VAB - Bones/KbBO/VBO);
      d/dt(Kidneys) = QKI*(Arterial_Blood/VAB - Kidneys/KbKI/VKI);
      d/dt(Arterial_Blood) = QLU*(Lungs/KbLU/VLU - Arterial_Blood/VAB);
      d/dt(Venous_Blood) = QHT*Heart/KbHT/VHT + QBR*Brain/KbBR/VBR +
        QMU*Muscles/KbMU/VMU + QAD*Adipose/KbAD/VAD + QSK*Skin/KbSK/VSK +
        QLI*Liver/KbLI/VLI + QBO*Bones/KbBO/VBO + QKI*Kidneys/KbKI/VKI +
        QRB*Rest_of_Body/KbRB/VRB - QLU*Venous_Blood/VVB;
        d/dt(Rest_of_Body) = QRB*(Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB);
  })
}
```

You can see this change in the simple printout

``` r
pbpk2 <- pbpk2()
pbpk2
```

    #>  -- rxode2-based free-form 16-cmt ODE model ------------------------------------- 
    #> 
    #> States ($state or $stateDf): 
    #>    Compartment Number Compartment Name
    #> 1                   1     Venous_Blood
    #> 2                   2             Skin
    #> 3                   3            Lungs
    #> 4                   4            Heart
    #> 5                   5            Brain
    #> 6                   6          Muscles
    #> 7                   7          Adipose
    #> 8                   8           Spleen
    #> 9                   9         Pancreas
    #> 10                 10            Liver
    #> 11                 11          Stomach
    #> 12                 12              Gut
    #> 13                 13            Bones
    #> 14                 14          Kidneys
    #> 15                 15   Arterial_Blood
    #> 16                 16     Rest_of_Body
    #>  -- Model (Normalized Syntax): -- 
    #> function() {
    #>     model({
    #>         cmt(Venous_Blood)
    #>         cmt(Skin)
    #>         KbBR = exp(lKbBR)
    #>         KbMU = exp(lKbMU)
    #>         KbAD = exp(lKbAD)
    #>         CLint = exp(lCLint + eta.LClint)
    #>         KbBO = exp(lKbBO)
    #>         KbRB = exp(lKbRB)
    #>         CO = (187 * WT^0.81) * 60/1000
    #>         QHT = 4 * CO/100
    #>         QBR = 12 * CO/100
    #>         QMU = 17 * CO/100
    #>         QAD = 5 * CO/100
    #>         QSK = 5 * CO/100
    #>         QSP = 3 * CO/100
    #>         QPA = 1 * CO/100
    #>         QLI = 25.5 * CO/100
    #>         QST = 1 * CO/100
    #>         QGU = 14 * CO/100
    #>         QHA = QLI - (QSP + QPA + QST + QGU)
    #>         QBO = 5 * CO/100
    #>         QKI = 19 * CO/100
    #>         QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + 
    #>             QKI)
    #>         QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + 
    #>             QRB
    #>         VLU = (0.76 * WT/100)/1.051
    #>         VHT = (0.47 * WT/100)/1.03
    #>         VBR = (2 * WT/100)/1.036
    #>         VMU = (40 * WT/100)/1.041
    #>         VAD = (21.42 * WT/100)/0.916
    #>         VSK = (3.71 * WT/100)/1.116
    #>         VSP = (0.26 * WT/100)/1.054
    #>         VPA = (0.14 * WT/100)/1.045
    #>         VLI = (2.57 * WT/100)/1.04
    #>         VST = (0.21 * WT/100)/1.05
    #>         VGU = (1.44 * WT/100)/1.043
    #>         VBO = (14.29 * WT/100)/1.99
    #>         VKI = (0.44 * WT/100)/1.05
    #>         VAB = (2.81 * WT/100)/1.04
    #>         VVB = (5.62 * WT/100)/1.04
    #>         VRB = (3.86 * WT/100)/1.04
    #>         BP = 0.61
    #>         fup = 0.028
    #>         fub = fup/BP
    #>         KbLU = exp(0.8334)
    #>         KbHT = exp(1.1205)
    #>         KbSK = exp(-0.5238)
    #>         KbSP = exp(0.3224)
    #>         KbPA = exp(0.3224)
    #>         KbLI = exp(1.7604)
    #>         KbST = exp(0.3224)
    #>         KbGU = exp(1.2026)
    #>         KbKI = exp(1.3171)
    #>         S15 = VVB * BP/1000
    #>         C15 = Venous_Blood/S15
    #>         d/dt(Lungs) = QLU * (Venous_Blood/VVB - Lungs/KbLU/VLU)
    #>         d/dt(Heart) = QHT * (Arterial_Blood/VAB - Heart/KbHT/VHT)
    #>         d/dt(Brain) = QBR * (Arterial_Blood/VAB - Brain/KbBR/VBR)
    #>         d/dt(Muscles) = QMU * (Arterial_Blood/VAB - Muscles/KbMU/VMU)
    #>         d/dt(Adipose) = QAD * (Arterial_Blood/VAB - Adipose/KbAD/VAD)
    #>         d/dt(Skin) = QSK * (Arterial_Blood/VAB - Skin/KbSK/VSK)
    #>         d/dt(Spleen) = QSP * (Arterial_Blood/VAB - Spleen/KbSP/VSP)
    #>         d/dt(Pancreas) = QPA * (Arterial_Blood/VAB - Pancreas/KbPA/VPA)
    #>         d/dt(Liver) = QHA * Arterial_Blood/VAB + QSP * Spleen/KbSP/VSP + 
    #>             QPA * Pancreas/KbPA/VPA + QST * Stomach/KbST/VST + 
    #>             QGU * Gut/KbGU/VGU - CLint * fub * Liver/KbLI/VLI - 
    #>             QLI * Liver/KbLI/VLI
    #>         d/dt(Stomach) = QST * (Arterial_Blood/VAB - Stomach/KbST/VST)
    #>         d/dt(Gut) = QGU * (Arterial_Blood/VAB - Gut/KbGU/VGU)
    #>         d/dt(Bones) = QBO * (Arterial_Blood/VAB - Bones/KbBO/VBO)
    #>         d/dt(Kidneys) = QKI * (Arterial_Blood/VAB - Kidneys/KbKI/VKI)
    #>         d/dt(Arterial_Blood) = QLU * (Lungs/KbLU/VLU - Arterial_Blood/VAB)
    #>         d/dt(Venous_Blood) = QHT * Heart/KbHT/VHT + QBR * Brain/KbBR/VBR + 
    #>             QMU * Muscles/KbMU/VMU + QAD * Adipose/KbAD/VAD + 
    #>             QSK * Skin/KbSK/VSK + QLI * Liver/KbLI/VLI + QBO * 
    #>             Bones/KbBO/VBO + QKI * Kidneys/KbKI/VKI + QRB * Rest_of_Body/KbRB/VRB - 
    #>             QLU * Venous_Blood/VVB
    #>         d/dt(Rest_of_Body) = QRB * (Arterial_Blood/VAB - Rest_of_Body/KbRB/VRB)
    #>     })
    #> }

The first two compartments are `Venous_Blood` followed by `Skin`.

## Appending compartments to the model

You can also append “compartments” to the model. Because of the ODE
solving internals, you cannot add fake compartments to the model until
after all the differential equations are defined.

For example this is legal:

``` r
ode.1c.ka <- function(){
  model({
    C2 = center/V
    d / dt(depot) = -KA * depot
    d/dt(center) = KA * depot - CL*C2
    cmt(eff)
  })
}

ode.1c.ka <- ode.1c.ka()
print(ode.1c.ka)
```

    #>  -- rxode2-based free-form 2-cmt ODE model -------------------------------------- 
    #> 
    #> States ($state or $stateDf): 
    #>   Compartment Number Compartment Name
    #> 1                  1            depot
    #> 2                  2           center
    #>  -- Model (Normalized Syntax): -- 
    #> function() {
    #>     model({
    #>         C2 = center/V
    #>         d/dt(depot) = -KA * depot
    #>         d/dt(center) = KA * depot - CL * C2
    #>         cmt(eff)
    #>     })
    #> }

You can see this more clearly with the underlying classic `rxode2`
model:

``` r
ode.1c.ka$simulationModel
```

    #> rxode2 5.0.0 model named rx_492980c80e248a64c6883e3d0384b252 model (ready). 
    #> x$state: depot, center
    #> x$stateExtra: eff
    #> x$params: V, KA, CL
    #> x$lhs: C2

But compartments defined before all the differential equations is not
supported; So the model below:

    ode.1c.ka <- rxode2({
        cmt(eff)
        C2 = center/V;
        d / dt(depot) = -KA * depot
        d/dt(center) = KA * depot - CL*C2
    })

will give an error:

    Error in rxModelVars_(obj) : 
      Evaluation error: Compartment 'eff' needs differential equations defined.
