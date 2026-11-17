# OrdinaryDiffEq side of bench/gpu-steppers-julia.R: the same problems and
# tolerances as rxode2's device integrators, written as CSV to ARGS[1].
# Needs OrdinaryDiffEqTsit5, OrdinaryDiffEqVerner and OrdinaryDiffEqRosenbrock.
using OrdinaryDiffEqTsit5, OrdinaryDiffEqVerner, OrdinaryDiffEqRosenbrock

lorenz!(du, u, p, t) = (du[1] = 10.0 * (u[2] - u[1]);
                        du[2] = u[1] * (28.0 - u[3]) - u[2];
                        du[3] = u[1] * u[2] - (8.0 / 3.0) * u[3])
rober!(du, u, p, t) = (du[1] = -0.04 * u[1] + 1e4 * u[2] * u[3];
                       du[2] = 0.04 * u[1] - 1e4 * u[2] * u[3] - 3e7 * u[2]^2;
                       du[3] = 3e7 * u[2]^2)
# two-compartment PK with first-order absorption, after a dose of 100
pk2!(du, u, p, t) = (du[1] = -1.5 * u[1];
                     du[2] = 1.5 * u[1] - (0.3 + 0.8) * u[2] / 10.0 + 0.8 * u[3] / 30.0;
                     du[3] = 0.8 * u[2] / 10.0 - 0.8 * u[3] / 30.0)

probs = [("lorenz", lorenz!, [1.0, 0.0, 0.0], 1.0),
         ("robertson", rober!, [1.0, 0.0, 0.0], 1e5),
         ("pk2", pk2!, [100.0, 0.0, 0.0], 48.0)]
algs = [("rkt54", "Tsit5", Tsit5()), ("vern76", "Vern7", Vern7()),
        ("ros54p", "Rodas5P", Rodas5P()),
        ("rkt54+ros54p", "AutoTsit5(Rodas5P())", AutoTsit5(Rodas5P())),
        ("vern76+ros54p", "AutoVern7(Rodas5P())", AutoVern7(Rodas5P()))]

open(ARGS[1], "w") do io
    println(io, "problem,method,julia,tol,y1,y2,y3,steps,accept,reject,fev,jac,retcode")
    for (pn, f, u0, tf) in probs, (rn, jn, alg) in algs, tol in (1e-6, 1e-8, 1e-10)
        # explicit methods on Robertson only run out of steps
        if pn == "robertson" && (rn == "rkt54" || rn == "vern76")
            continue
        end
        sol = solve(ODEProblem(f, u0, (0.0, tf)), alg; abstol = tol, reltol = tol,
                    save_everystep = false, dense = false, maxiters = 10^7)
        u = sol.u[end]
        s = sol.stats
        println(io, join([pn, rn, "\"" * jn * "\"", tol, u[1], u[2], u[3],
                          s.naccept + s.nreject, s.naccept, s.nreject, s.nf, s.njacs,
                          string(sol.retcode)], ","))
    end
end
