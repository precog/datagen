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
create a number of UUIDs in `./campaigns.csv`
```sh
stack exec datagen-exe -- genuuids -n 20
```

Use the generated ./campaigns.csv to create `./stats.ldjson`
```sh
stack exec datagen-exe -- gen -n 20
```

Change the floating point number in the second column of
`./campaigns.csv` to control the probability of successful
ad delivery for a campaign's randomly generated stats. The
number should be between 0.0 and 1.0.

```csv
ca109dae-b1af-4d6b-b8eb-6ae608f2aa3d, 0.8 // <--
```
