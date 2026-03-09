# open a local shiny app for viewing the result of a PLP analyses from a database

open a local shiny app for viewing the result of a PLP analyses from a
database

## Usage

``` r
viewDatabaseResultPlp(
  mySchema,
  myServer,
  myUser,
  myPassword,
  myDbms,
  myPort = NULL,
  myTableAppend
)
```

## Arguments

- mySchema:

  Database result schema containing the result tables

- myServer:

  server with the result database

- myUser:

  Username for the connection to the result database

- myPassword:

  Password for the connection to the result database

- myDbms:

  database management system for the result database

- myPort:

  Port for the connection to the result database

- myTableAppend:

  A string appended to the results tables (optional)

## Value

Opens a shiny app for interactively viewing the results

## Details

Opens a shiny app for viewing the results of the models from a database

## Examples
