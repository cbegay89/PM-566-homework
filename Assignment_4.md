Assignment 4
================
Cynthia Begay

``` r
a = matrix(1, 4, 5)

# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n)
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  x = rowSums(mat, na.rm = TRUE)
  #x = apply(mat, 1, sum)
  x
}
```

``` r
fun1alt(dat)
fun1(dat)
```

``` r
# Cumulative product by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k)
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  ans
 }


fun2alt <- function(mat) {
  x = apply(mat, 1, FUN = cumsum)
  t(x)
}
```

``` r
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq      max neval
    ##     fun1(dat) 5.198548 5.347064 4.350133 5.477651 6.406585 1.695482   100
    ##  fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun2(dat) 3.216156 2.768222 1.428063 2.366462 2.013992 0.1438529   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   4.037   1.445   5.894

``` r
library(parallel)
cl <- makePSOCKcluster(4L)
clusterSetRNGStream(cl, 1231) #Equivalent to set.seed(1231)

system.time({
  ans <- unlist(parLapply(
    cl = cl,
    1:4000,
    sim_pi,
    n=10000
  ))
  
  parallel::stopCluster(cl)
  
  print(mean(ans))
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##   0.010   0.002   2.761

``` r
#### Part 2: SQL #####
#install.packages(c("RSQLite", "DBI"))
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

``` sql
PRAGMA table_info(film)
```

<div class="knitsql-table">

| cid | name                   | type    | notnull | dflt\_value | pk |
| :-- | :--------------------- | :------ | ------: | :---------- | -: |
| 0   | film\_id               | INTEGER |       0 | NA          |  0 |
| 1   | title                  | TEXT    |       0 | NA          |  0 |
| 2   | description            | TEXT    |       0 | NA          |  0 |
| 3   | release\_year          | INTEGER |       0 | NA          |  0 |
| 4   | language\_id           | INTEGER |       0 | NA          |  0 |
| 5   | original\_language\_id | INTEGER |       0 | NA          |  0 |
| 6   | rental\_duration       | INTEGER |       0 | NA          |  0 |
| 7   | rental\_rate           | REAL    |       0 | NA          |  0 |
| 8   | length                 | INTEGER |       0 | NA          |  0 |
| 9   | replacement\_cost      | REAL    |       0 | NA          |  0 |

Displaying records 1 - 10

</div>

``` sql
PRAGMA table_info(film_category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | film\_id     | INTEGER |       0 | NA          |  0 |
| 1   | category\_id | INTEGER |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

``` sql
PRAGMA table_info(category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | category\_id | INTEGER |       0 | NA          |  0 |
| 1   | name         | TEXT    |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

Question 1: How many movies are there available in each rating category?

``` sql
SELECT film_id, title, rating
FROM film
ORDER by rating
```

<div class="knitsql-table">

| film\_id | title             | rating |
| -------: | :---------------- | :----- |
|        2 | ACE GOLDFINGER    | G      |
|        4 | AFFAIR PREJUDICE  | G      |
|        5 | AFRICAN EGG       | G      |
|       11 | ALAMO VIDEOTAPE   | G      |
|       22 | AMISTAD MIDSUMMER | G      |
|       25 | ANGELS LIFE       | G      |
|       26 | ANNIE IDENTITY    | G      |
|       39 | ARMAGEDDON LOST   | G      |
|       43 | ATLANTIS CAUSE    | G      |
|       46 | AUTUMN CROW       | G      |

Displaying records 1 - 10

</div>

There are 1000 observations with an associated rating.

``` sql
SELECT film_id, title, rating
FROM film
WHERE rating IN ('G')
```

<div class="knitsql-table">

| film\_id | title             | rating |
| -------: | :---------------- | :----- |
|        2 | ACE GOLDFINGER    | G      |
|        4 | AFFAIR PREJUDICE  | G      |
|        5 | AFRICAN EGG       | G      |
|       11 | ALAMO VIDEOTAPE   | G      |
|       22 | AMISTAD MIDSUMMER | G      |
|       25 | ANGELS LIFE       | G      |
|       26 | ANNIE IDENTITY    | G      |
|       39 | ARMAGEDDON LOST   | G      |
|       43 | ATLANTIS CAUSE    | G      |
|       46 | AUTUMN CROW       | G      |

Displaying records 1 - 10

</div>

There are 180 movies with a G rating.

``` sql
SELECT film_id, title, rating
FROM film
WHERE rating IN ('PG')
```

<div class="knitsql-table">

| film\_id | title                | rating |
| -------: | :------------------- | :----- |
|        1 | ACADEMY DINOSAUR     | PG     |
|        6 | AGENT TRUMAN         | PG     |
|       12 | ALASKA PHANTOM       | PG     |
|       13 | ALI FOREVER          | PG     |
|       19 | AMADEUS HOLY         | PG     |
|       37 | ARIZONA BANG         | PG     |
|       41 | ARSENIC INDEPENDENCE | PG     |
|       63 | BEDAZZLED MARRIED    | PG     |
|       65 | BEHAVIOR RUNAWAY     | PG     |
|       72 | BILL OTHERS          | PG     |

Displaying records 1 - 10

</div>

There are 194 movies with a PG rating.

``` sql
SELECT film_id, title, rating
FROM film
WHERE rating IN ('PG-13')
```

<div class="knitsql-table">

| film\_id | title                       | rating |
| -------: | :-------------------------- | :----- |
|        7 | AIRPLANE SIERRA             | PG-13  |
|        9 | ALABAMA DEVIL               | PG-13  |
|       18 | ALTER VICTORY               | PG-13  |
|       28 | ANTHEM LUKE                 | PG-13  |
|       33 | APOLLO TEEN                 | PG-13  |
|       35 | ARACHNOPHOBIA ROLLERCOASTER | PG-13  |
|       36 | ARGONAUTS TOWN              | PG-13  |
|       44 | ATTACKS HATE                | PG-13  |
|       45 | ATTRACTION NEWTON           | PG-13  |
|       48 | BACKLASH UNDEFEATED         | PG-13  |

Displaying records 1 - 10

</div>

There are 223 movies with a PG-13 rating.

``` sql
SELECT film_id, title, rating
FROM film
WHERE rating IN ('R')
```

<div class="knitsql-table">

| film\_id | title                | rating |
| -------: | :------------------- | :----- |
|        8 | AIRPORT POLLOCK      | R      |
|      213 | DATE SPEED           | R      |
|       17 | ALONE TRIP           | R      |
|       20 | AMELIE HELLFIGHTERS  | R      |
|       21 | AMERICAN CIRCUS      | R      |
|       23 | ANACONDA CONFESSIONS | R      |
|       24 | ANALYZE HOOSIERS     | R      |
|       30 | ANYTHING SAVANNAH    | R      |
|       32 | APOCALYPSE FLAMINGOS | R      |
|       40 | ARMY FLINTSTONES     | R      |

Displaying records 1 - 10

</div>

There are 195 movies with an R rating.

``` sql
SELECT film_id, title, rating
FROM film
WHERE rating IN ('NC-17')
```

<div class="knitsql-table">

| film\_id | title              | rating |
| -------: | :----------------- | :----- |
|        3 | ADAPTATION HOLES   | NC-17  |
|       10 | ALADDIN CALENDAR   | NC-17  |
|       14 | ALICE FANTASIA     | NC-17  |
|       15 | ALIEN CENTER       | NC-17  |
|       16 | ALLEY EVOLUTION    | NC-17  |
|       27 | ANONYMOUS HUMAN    | NC-17  |
|       29 | ANTITRUST TOMATOES | NC-17  |
|       31 | APACHE DIVINE      | NC-17  |
|       34 | ARABIA DOGMA       | NC-17  |
|       38 | ARK RIDGEMONT      | NC-17  |

Displaying records 1 - 10

</div>

There are 210 movies with an NC-17 rating.

Question 2: What is the average replacement cost and rental rate for
each rating category.

``` sql
SELECT rating,
       AVG(rental_rate) AS avg_rate,
       AVG(replacement_cost) AS avg_replace
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_rate | avg\_replace |
| :----- | --------: | -----------: |
| G      |  2.912222 |     20.12333 |
| NC-17  |  2.970952 |     20.13762 |
| PG     |  3.051856 |     18.95907 |
| PG-13  |  3.034843 |     20.40256 |
| R      |  2.938718 |     20.23103 |

5 records

</div>

The average rental rates and average replacement costs by ratings are:
Rated G, average rental rate = $2.91, average replacement cost = $20.12
Rated PG, average rental rate = $3.05, average replacement cost = $18.96
Rated PG-13, average rental rate = $3.03, average replacement cost =
$20.40 Rated R, average rental rate = $2.93, average replacement cost =
$20.23 Rated NC-17, average rental rate = $2.97, average replacement
cost = $20.14

Question 3: Use table film\_category together with film to find the how
many films there are within each category ID

``` sql
SELECT film.film_id, film_category.category_id
  FROM film
  INNER JOIN film_category ON film.film_id = film_category.film_id
  GROUP BY category_id;
```

<div class="knitsql-table">

| film\_id | category\_id |
| -------: | -----------: |
|       19 |            1 |
|       18 |            2 |
|       48 |            3 |
|       14 |            4 |
|        7 |            5 |
|        1 |            6 |
|       33 |            7 |
|        5 |            8 |
|        6 |            9 |
|       46 |           10 |

Displaying records 1 - 10

</div>

There are 16 categories total. The number of films per category are:
Category 1: 19 Category 2: 18 Category 3: 48 Category 4: 14 Category 5:
7 Category 6: 1 Category 7: 33 Category 8: 5 Category 9: 6 Category 10:
46 Category 11: 2 Category 12: 12 Category 13: 22 Category 14: 26
Category 15: 10 Category 16: 41

Question 4: Incorporate table category into the answer to the previous
question to find the name of the most popular category.

``` sql
SELECT film.film_id, film_category.category_id, category.name
  FROM ((film
  INNER JOIN film_category ON film.film_id = film_category.film_id)
  INNER JOIN category ON film_category.category_id = category.category_id)
  GROUP BY name;
```

<div class="knitsql-table">

| film\_id | category\_id | name        |
| -------: | -----------: | :---------- |
|       19 |            1 | Action      |
|       18 |            2 | Animation   |
|       48 |            3 | Children    |
|       14 |            4 | Classics    |
|        7 |            5 | Comedy      |
|        1 |            6 | Documentary |
|       33 |            7 | Drama       |
|        5 |            8 | Family      |
|        6 |            9 | Foreign     |
|       46 |           10 | Games       |

Displaying records 1 - 10

</div>

The most popular category is catgory 3 “Children” with 48 movies.
