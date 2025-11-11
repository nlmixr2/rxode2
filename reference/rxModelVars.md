# All model variables for a rxode2 object

Return all the known model variables for a specified rxode2 object

## Usage

``` r
rxModelVars(obj)

rxModelVarsS3(obj)

# S3 method for class 'rxUi'
rxModelVarsS3(obj)

# Default S3 method
rxModelVarsS3(obj)
```

## Arguments

- obj:

  rxode2 family of objects

## Value

A list of rxode2 model properties including:

- `params` a character vector of names of the model parameters

- `lhs` a character vector of the names of the model calculated
  parameters

- `state` a character vector of the compartments in rxode2 object

- `trans` a named vector of translated model properties including what
  type of jacobian is specified, the `C` function prefixes, as well as
  the `C` functions names to be called through the compiled model.

- `md5` a named vector that gives the digest of the model (`file_md5`)
  and the parsed model (`parsed_md5`)

- `model` a named vector giving the input model (`model`), normalized
  model (no comments and standard syntax for parsing, `normModel`), and
  interim code that is used to generate the final C file `parseModel`

## Details

These items are only calculated after compilation; they are built-into
the rxode2 compiled DLL.

To allow extension, an s3 hook is added in the function `rxModelVarsS3`.

## See also

Other Query model information:
[`rxDfdy()`](https://nlmixr2.github.io/rxode2/reference/rxDfdy.md),
[`rxInits()`](https://nlmixr2.github.io/rxode2/reference/rxInits.md),
[`rxLhs()`](https://nlmixr2.github.io/rxode2/reference/rxLhs.md),
[`rxParams()`](https://nlmixr2.github.io/rxode2/reference/rxParams.md),
[`rxState()`](https://nlmixr2.github.io/rxode2/reference/rxState.md)

## Author

Matthew L. Fidler
