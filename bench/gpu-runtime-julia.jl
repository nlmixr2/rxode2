# DiffEqGPU side of bench/gpu-runtime-julia.R: ensembles solved one
# trajectory per work-item (EnsembleGPUKernel with GPUTsit5, GPUVern7 and
# GPURodas5P) and with threads (EnsembleThreads), timed after a warm-up.
# Writes CSV to ARGS[1]; ARGS[2] is a comma-separated list of ensemble sizes.
# Run with `julia -t <threads>`.  Needs DiffEqGPU, OrdinaryDiffEq,
# StaticArrays, KernelAbstractions and (optionally) OpenCL + pocl_jll.
using DiffEqGPU, OrdinaryDiffEq, StaticArrays, KernelAbstractions, Statistics

const backends = Any[("KA-CPU", CPU())]
try
    @eval using OpenCL, pocl_jll
    push!(backends, ("OpenCL-pocl", OpenCLBackend()))
catch err
    @warn "OpenCL.jl backend not available" err
end

lorenz(u, p, t) = SVector(p[1] * (u[2] - u[1]),
                          u[1] * (p[2] - u[3]) - u[2],
                          u[1] * u[2] - p[3] * u[3])
rober(u, p, t) = SVector(-p[1] * u[1] + 1e4 * u[2] * u[3],
                         p[1] * u[1] - 1e4 * u[2] * u[3] - 3e7 * u[2]^2,
                         3e7 * u[2]^2)

# parameters spread evenly, the same on the R side
function pars(name, n)
    g = n == 1 ? [0.0] : collect(0:(n - 1)) ./ (n - 1)
    name == "lorenz" ? [SVector(10.0, 10.0 + 20.0 * x, 8.0 / 3.0) for x in g] :
        [SVector(0.02 + 0.06 * x, 0.0, 0.0) for x in g]
end

probs = Dict("lorenz" => (lorenz, SVector(1.0, 0.0, 0.0), (0.0, 1.0)),
             "robertson" => (rober, SVector(1.0, 0.0, 0.0), (0.0, 1e3)))
gpuAlgs = [("rkt54", "GPUTsit5", GPUTsit5()), ("vern76", "GPUVern7", GPUVern7()),
           ("ros54p", "GPURodas5P", GPURodas5P())]
cpuAlgs = Dict("rkt54" => ("Tsit5", Tsit5()), "vern76" => ("Vern7", Vern7()),
               "ros54p" => ("Rodas5P", Rodas5P()))
cases = [("lorenz", "rkt54"), ("lorenz", "vern76"), ("lorenz", "ros54p"),
         ("robertson", "ros54p")]
const tol = 1e-8
sizes = parse.(Int, split(length(ARGS) > 1 ? ARGS[2] : "1000,10000,100000", ","))

function timeit(f; reps = 5)
    f()
    ts = Float64[]
    for _ in 1:reps
        push!(ts, @elapsed f())
    end
    median(ts)
end

out = open(ARGS[1], "w")
println(out, "problem,method,julia,backend,n,seconds,u1,u2,u3")
for (pname, meth) in cases
    f, u0, tspan = probs[pname]
    for n in sizes
        ps = pars(pname, n)
        prob = ODEProblem{false}(f, u0, tspan, ps[1])
        ens = EnsembleProblem(prob, prob_func = (pr, ctx) -> remake(pr, p = ps[ctx.sim_id]),
                              safetycopy = false)
        for (rname, jname, alg) in gpuAlgs
            rname == meth || continue
            for (bname, be) in backends
                local sol
                t = try
                    timeit(() -> (sol = solve(ens, alg, EnsembleGPUKernel(be), trajectories = n,
                                              adaptive = true, dt = 1e-3, save_everystep = false,
                                              abstol = tol, reltol = tol)))
                catch err
                    @warn "failed" pname jname bname err
                    NaN
                end
                u = isnan(t) ? [NaN, NaN, NaN] : sol.u[end].u[end]
                println(out, join([pname, meth, jname, bname, n, t, u...], ","))
                flush(out)
            end
        end
        cname, calg = cpuAlgs[meth]
        local sol
        t = timeit(() -> (sol = solve(ens, calg, EnsembleThreads(), trajectories = n,
                                      save_everystep = false, abstol = tol, reltol = tol)))
        u = sol.u[end].u[end]
        println(out, join([pname, meth, cname, "EnsembleThreads", n, t, u...], ","))
        flush(out)
    end
end
close(out)
