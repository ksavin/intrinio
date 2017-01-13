# intrinio

Download company financial statements, standardized EDGAR reports, historical market data, macroeconomic statistics via [Intrinio Web API](intrinio.com).

*The package is in early development, breaking changes are possible*

## Installation
```r
devtools::install_github('ksavin/intrinio')
```

## Authorization
Run `intrAuth('yourlogin', 'yourpassword')` to authorize API access.   
Your username and password can be located in your Intrinio account under "Access Keys"
![AccessKeys](https://s28.postimg.org/u4erhg0m5/Access_Keys.png)

## Options
Frequently needed parameters, such as verbosity and output format can be obtained and modified with `intrOptions()`
```r
intrOptions() # View currenly specified global parameters

# Modify global parameters. 
# Results will now be in data.table and a prompt message will appear 
# if the request takes too many (50 by default) API credits.
intrOptions(outFormat = 'data.table', costWarning = TRUE) 
```

## Loading data with wrappers
Wrappers are small convenience functions that load specific data. Currently most of them are under development.  
Wrappers' names typically start with "i.".
If the resulting query is large, Intrinio returns data in chunks, which are loaded sequentially.

```r
# Loads detail information for the specified tickers
# Package handles various errors. Here WRONGTICKER is skipped with a warning
secDetails <- i.securities(c('AAPL', 'WRONGTICKER', 'MSFT')) 

# loads last 3 pages from securities list
secList <- i.securities(startPage = 392) 
```

## Loading data with query constructors
Wrappers usually call API query constructors, but these are available to be used as well.  

`intrCall` is the basic constructor, that loads (potentially) multipage data for a single call   
`intrCallMap` allows iterating over multiple queries for a single endpoint. Its syntax is similar to `mapply` and `Map` from base R.   

The code below loads data from the previous section with query constructors.

```r
secList <- intrCall('securities', startPage = 392)
secDetails <- intrCallMap('securities', identifier = c('AAPL', 'WRONGTICKER', 'MSFT'), idCols = FALSE)
```
Query constructors are designed to be universal.   
Requests below load a list of available income statements for AAPL, MSFT and GOOG and loads full income statements for the years 2009+.
```r
intrOptions(outFormat = 'data.table')

# Loads a list of available income statements
ISlist <- intrCallMap('fundamentals/standardized', identifier = c('AAPL', 'MSFT', 'GOOG'),
                       MoreArgs = list(statement = 'income_statement', type = 'FY'))

ISlist <- ISlist[fiscal_year >= 2009]
IS <- intrCallMap('financials/standardized',
                   identifier = ISlist$identifier,
                   fiscal_year = ISlist$fiscal_year,
                   MoreArgs = list(statement = 'income_statement', fiscal_period = 'FY'))
                       
```
See [API documentaion](http://docs.intrinio.com/) for the list of available endpoints and queries.
