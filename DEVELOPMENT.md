# Development instructions

## Add new dataset to SCCA package

```
df = read_csv("Carnivors.csv")
save(df, "data/carnivors.RData")
```

## Clone private repo

Create Personal Access Token on Github. Install `devtools` and run the following code:

``` r
Sys.setenv(GITHUB_PAT = "YOUR_TOKEN")
```

Now run the install code in the readme. 
