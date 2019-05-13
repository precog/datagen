# datagen

## prereqs

- clone this repos
- make sure you have installed `stack`
- make sure `stack` is set up correctly (usually `stack setup`)

## build

```
$ stack build

```

## run

```
# create a number of UUIDs in ./campaigns.csv
stack exec datagen-exe -- genuuids -n 20

# Use the generated ./campaigns.csv to create 
# ./stats.ldjson
stack exec datagen-exe -- gen -n 20
```
