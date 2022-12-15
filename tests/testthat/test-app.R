test_that("Login fails with the bad password", {
  ### make the request:

  credentials <- jsonlite::base64_enc("guest:guest12aaa3")
  headers <- list("Authorization" = sprintf("Basic %s", credentials))
  req <<- Request$new(
    path = "/login",
    headers = headers
  )
  response <- app$process_request(req)

  expect_equal(response$status_code, 401)
})

test_that("Login works", {
  ### make the request:

  credentials <- jsonlite::base64_enc("guest:guest123")
  credentials <- jsonlite::base64_enc("idragan:3xC@libur")
  headers <- list("Authorization" = sprintf("Basic %s", credentials))
  req <<- Request$new(
    path = "/login",
    headers = headers
  )
  response <- app$process_request(req)
  ck <- list(user =  response$cookies$user$value, sid = response$cookies$sid$value)
  assign('ck', ck, envir = .GlobalEnv)
  x <- response$body
  expect_equal(x, 'OK')
})


test_that(" Endpoint /getvars works", {


  ### make the request:
  req <- Request$new(
    path = "/getvars",
    parameters_query = list(nocache = 'true'),
    cookies = ck
  )
  response <- app$process_request(req)
  x <- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_identical(x,jsonlite::fromJSON(bubbleJSON, simplifyDataFrame = FALSE, simplifyMatrix = FALSE) )
})

test_that(" Endpoint /histogram works", {
  ### make the request:
  req <- Request$new(
    path = "/histogram",
    parameters_query = list(var = "Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma", datasets ="sophia.db"),
    cookies = ck
  )
  response <- app$process_request(req)

  x <- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_equal(names(x), c(  "global"  ))
})

test_that(" Endpoint /histogram works for factors", {
  ### make the request:
  req <- Request$new(
    path = "/histogram",
    parameters_query = list(var = "ethnicity", cohorts ="sophia.db, test.db"),
    cookies = ck
  )
  response <- app$process_request(req)

  x<- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_equal(x$global$Total, 1838)
})

test_that(" Endpoint /runAlgorithm works with linear regression", {
  ### make the request:
  req <- Request$new(
    path = "/runAlgorithm",

    body = jsonlite::toJSON(list(
      algorithm = list(id = 'linear-regression',
                       variable = "Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma",
                       coVariables = c("Urea.nitrogen..Mass.volume..in.Serum.or.Plasma" , "Albumin..Mass.volume..in.Serum.or.Plasma")),
      datasets =c("sophia.db"))),
    method = 'POST',
    cookies = ck,
    content_type = 'application/json'
  )
  response <- app$process_request(req)
  x <- jsonlite::fromJSON(response$body)

  expect_equal(x$Ntotal, 107)
})

test_that(" Endpoint /runAlgorithm works with logistic regression", {
  ### make the request:
  req <- Request$new(
    path = "/runAlgorithm",

    body = jsonlite::toJSON(list(algorithm = list(id = 'logistic-regression',
                                                  coVariables = c("Urea.nitrogen..Mass.volume..in.Serum.or.Plasma" , "Albumin..Mass.volume..in.Serum.or.Plasma"),
                                                  variable = "race",
                                                  'pos-level' = 'White'),
                                 datasets =c("sophia.db", 'test.db'))),
    method = 'POST',
    cookies = ck,
    content_type = 'application/json'
  )
  response <- app$process_request(req)
  x <- jsonlite::fromJSON(response$body)
  expect_equal(x$Ntotal, 214)
})

### !!!!!
test_that(" Endpoint /descriptivestats works ", {
  ### make the request:
  req <- Request$new(
    path = "/descriptivestats",
    method = 'POST',
    body = '{
      "variables": [ "Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma"
      ],
      "covariables": ["Albumin..Mass.volume..in.Serum.or.Plasma"],
      "datasets": [
        "sophia.db",
        "test.db",
        "omop_test.db"
      ]
    }',
    cookies = ck
  )
  response <- app$process_request(req)

  xx<- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
  expect_equal(length(xx), 2)
})

test_that(" Endpoint /descriptivestats works 2 ", {
  ### make the request:
  req <- Request$new(
    path = "/descriptivestats",
    method = 'POST',
    body = '{
    {"variables":["Apolipoprotein.C.II..Mass.volume..in.Serum.or.Plasma"],
    "covariables":["Cholesterol.in.VLDL..Moles.volume..in.Serum.or.Plasma"],
    "datasets":["accelerate.db"]}',
    cookies = ck
  )
  response <- app$process_request(req)

  xx<- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
  expect_equal(length(xx), 2)
})


  test_that(" Endpoint /descriptivestats works with one variable ", {
  ### make the request:
  req <- Request$new(
    path = "/descriptivestats",
    method = 'POST',
    parameters_query = list(variables = c("Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma")),
    body = '{
      "variables": [ "Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma"
      ],
      "datasets": [
        "sophia.db",
        "test.db",
        "omop_test.db"
      ]
    }',

    cookies = ck
  )
  response <- app$process_request(req)
  xx<- jsonlite::fromJSON(response$body, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
  expect_false(is.null(names(xx$quants)))
})



test_that("Logout works", {
  ### make the request:

  req <- Request$new(
    path = "/logout",
    cookies = ck
  )
  response <- app$process_request(req)

  expect_equal(response$body, 'OK')
})


test_that("Session timeout works", {
  ### make the request:

  credentials <- jsonlite::base64_enc("guest:guest123")
  headers <- list("Authorization" = sprintf("Basic %s", credentials))
  req <<- Request$new(
    path = "/login",
    headers = headers
  )
  response <- app$process_request(req)
  ck <- list(user =  response$cookies$user$value, sid = response$cookies$sid$value)

  x <- response$body
  expect_equal(x, 'OK')
  Sys.sleep(config$sessionTimeout + 2)

 req <- Request$new(
   path = "/descriptivestats",
   parameters_query = list(covariables = "Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma", variables ="Urea.nitrogen..Mass.volume..in.Serum.or.Plasma"),
   cookies = ck
 )
 response <- app$process_request(req)

 expect_equal(response$status_code, 401)
})
