# datagen dhall

## prereqs

- make sure you have installed `dhall-to-json` with a version that is compatible
  with [v9.0.0 of the dhall-lang Prelude](https://github.com/dhall-lang/dhall-lang/tree/v9.0.0/Prelude). E.g `brew install dhall-json` works if `dhall-json 1.4.0`

## run
generate HR example data
```sh
dhall-to-json --file dhall/hr.dhall
```