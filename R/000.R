.parseEnv <- new.env(parent = emptyenv())
## Mutable session state written while building/solving models.  These were
## namespace variables set with assignInMyNamespace(), which costs ms per call
## once other packages register S3 methods on rxode2 generics (#1425).
.rxState <- new.env(parent = emptyenv())
