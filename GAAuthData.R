# GAAuthData - Bror Skardhamar (bror@skardhamar.com) - IIH Nordic (iihnordic.com)
# Extract Data from Google Analytics API V.3 using OAuth
# Heavily inspired by Theodore Van Rooy - https://github.com/greentheo/ROAuthSamples/

# NOTICE: On every reload the access token is refreshed, could be optimized by appending the timestamp to the access.token list that is saved
# NOTICE: The way the c.token is being gathered is manual, this should be done differently, perhaps via an online gateway of some kind
# FIX: There is a bunch of the management data extraction that outputs data.frames with different length of columns
# FIX: The way the data formats are being converted is done by a loop, this should be done by vector-operations

# Global technical variables guide:
# Step 1: Go to https://code.google.com/apis/console/ - login with the desired Google Account
# Step 2: Create a Client ID for installed applications
# Step 3: Insert Client ID in c.id
# Step 4: Insert Client secret in c.secret
# Step 5: Run getAuth()
# Step 6: Copy the ?code-parameter from the redirect.uri, and insert this in c.token
# Step 7: Run loadToken()
# Step 8: You should have access now, try to run getData('ga:profile-id')

c.id <- 'insert client id here'
c.secret <- 'insert client secret here'
c.token <- 'insert code-parameter here'

redirect.uri <- ('http://localhost/oauth2callback') # as of now: dummy url
store.auth <- '~/R/gauth/token.RData' # define internal folder

# Global data variables
date.format <- '%d-%m-%Y' # european

# Libraries required 
library(RCurl)
library(rjson)

getAuth = function() {
  url <- paste('https://accounts.google.com/o/oauth2/auth?',
               'scope=https://www.googleapis.com/auth/analytics.readonly&',
               'state=%2Fprofile&',
               'redirect_uri=', redirect.uri, '&',
               'response_type=code&',
               'client_id=', c.id, '&',
               'approval_prompt=force&',
               'access_type=offline', sep='', collapse='')
  
  getURL(url)
  browseURL(url)
  # Manual next-step: input code-parameter to c.token variable and run loadToken()
}

loadToken = function() {
  opts = list(verbose=T)
  a = fromJSON(postForm('https://accounts.google.com/o/oauth2/token', .opts=opts, code=c.token, client_id=c.id,
                        client_secret=c.secret, redirect_uri=redirect.uri, grant_type="authorization_code", 
                        style="POST"))
  if (length(a) == 1) {
    print('You need to update the token - run getAuth()')
  } else {
    access.token <<- a
    save(access.token, file=store.auth)
  }
}

refreshToken = function(accesstoken) {
  rt = fromJSON(postForm('https://accounts.google.com/o/oauth2/token', 
                         refresh_token=accesstoken$refresh_token, 
                         client_id=c.id,
                         client_secret=c.secret, 
                         grant_type="refresh_token", 
                         style="POST" ))
  accesstoken$access_token = rt$access_token
  accesstoken$expires_in = rt$expires_in
  access.token.timeStamp <<- as.numeric(Sys.time())
  return(accesstoken)
}

getToken = function(refresh=F) {
  if (refresh) {
    if (!exists(as.character(substitute(access.token)))) {
      load(store.auth)
    }
    access.token <<- refreshToken(access.token)
    save(access.token, file=store.auth)
    return(access.token$access_token)
  } else {
    if (!exists(as.character(substitute(access.token.timeStamp)))) {
      return(getToken(refresh=T))      
    } else {
      if ((as.numeric(Sys.time()) - access.token.timeStamp) >= 3600) {
        return(getToken(refresh=T))
      } else {
        if (exists(as.character(substitute(access.token))) && (!is.na(access.token))) {
          save(access.token, file=store.auth)
          return(access.token$access_token)
        } else {
          if (file.exists(store.auth)) {
            load(store.auth) # load file
            access.token <<- refreshToken(access.token)
            return(access.token$access_token)
          } else {
            getAuth() # need to auth again
          }
        }
      }
    }
  }
}

processManData = function(url) {
  ga.data <- fromJSON(getURL(url))
  
  ga.data.df <- as.data.frame(do.call(rbind, ga.data$items)) # convert to data.frame
  ga.data.df <- subset(ga.data.df, select = -c(kind, selfLink, childLink)) # remove columns
  return(ga.data.df)
}


getAccounts = function(start=1, max=1000) {
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts',
               '?access_token=', a,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  output <- processManData(url)
  return(output)
}

getWebProperties = function(accountId, start=1, max=1000) {
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId ,'/webproperties',
               '?access_token=', a,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  output <- processManData(url)
  return(output)
}

getProfiles = function(accountId='~all', webPropertyId='~all', start=1, max=1000) { # FIX: deparse error
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, '/webproperties/', webPropertyId , '/profiles',
               '?access_token=', a,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  output <- processManData(url)
  return(output)
}

getGoals = function(accountId='~all', webPropertyId='~all', profileId='~all', start=1, max=1000) {
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, '/webproperties/', webPropertyId , '/profiles/', profileId, '/goals',
               '?access_token=', a,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  output <- processManData(url)
  return(output)
}

getSegments = function(start=1, max=1000) { # FIX: deparse error
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/management/segments',
               '?access_token=', a,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  output <- processManData(url)
  return(output)
}

getData = function(ids, start.date=format(Sys.time(), "%Y-%m-%d"), end.date=format(Sys.time(), "%Y-%m-%d"), metrics='ga:visits', dimensions='ga:date', sort='', filters='', segment='', fields='', remove.prefix=T, fix.date=T, fix.format=T, start=1, max=1000) {
  a <- getToken() # acquire token
  url <- paste('https://www.googleapis.com/analytics/v3/data/ga',
               '?access_token=', a,
               '&ids=', ids, # req
               '&start-date=', start.date, # req, YYYY-MM-DD
               '&end-date=', end.date, # req, YYYY-MM-DD 
               '&metrics=', metrics, # req
               '&dimensions=', dimensions,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  if (sort != '') { url <- paste(url, '&sort=', sort, sep='', collapse='') }
  if (filters != '') { url <- paste(url, '&filters=', filters, sep='', collapse='') }
  if (segment != '') { url <- paste(url, '&segment=', segment, sep='', collapse='') }
  if (fields != '') { url <- paste(url, '&fields=', fields, sep='', collapse='') }
  
  ga.data <- fromJSON(getURL(url))
  ga.headers <- as.data.frame(do.call(rbind, ga.data$columnHeaders)) # get column names
  ga.data.df <- as.data.frame(do.call(rbind, ga.data$rows)) # convert to data.frame
  ga.data.df <- data.frame(lapply(ga.data.df, as.character), stringsAsFactors=F) # convert to characters
  
  if (remove.prefix) { ga.headers$name <- sub('ga:', '', ga.headers$name) }
  names(ga.data.df) <- ga.headers$name # insert column names
  
  if (fix.date) { # will not work without remove.prefix=T
    if ('date' %in% names(ga.data.df)) {
      ga.data.df$'date' <- as.Date(format(as.Date(ga.data.df$'date', '%Y%m%d'), date.format), format=date.format)
    }
  }
  
  if (fix.format) {
    int <- c('hour', 'visits', 'visitors', 'newVisits', 
             'daysSinceLastVisit', 'visitCount', 'transactionRevenue',
             'bounces', 'timeOnSite', 'entranceBounceRate', 'visitBounceRate',
             'avgTimeOnSite', 'organicSearches', 'adSlotPosition', 'impressions',
             'adClicks', 'adCost', 'CPM', 'CPC', 'CTR', 'costPerTransaction',
             'costPerGoalConversion', 'costPerConversion', 'RPC', 'ROI',
             'margin', 'goal', 'latitude', 'longitude', 'socialActivityTimestamp',
             'socialActivities', 'pageDepth', 'entrances', 'pageviews', 
             'uniquePageviews', 'timeOnPage', 'exits', 'entranceRate', 
             'pageviewsPerVisit', 'avgTimeOnPage', 'exitRate', 'searchResultViews',
             'searchUniques', 'searchVisits', 'searchDepth', 'searchRefinements',
             'searchDuration', 'searchExits', 'avgSearchResultViews', 
             'percentVisitsWithSearch', 'avgSearchDepth', 'avgSearchDuration',
             'searchExitRate', 'searchGoal', 'goalValueAllPerSearch')
    
    # stupid stupid solution - fix, do vector
    for(i in 1:length(names(ga.data.df))) {
      if (names(ga.data.df)[i] %in% int) {
        ga.data.df[[i]] <- as.numeric(ga.data.df[[i]])
      }
    }
  }
  
  return(ga.data.df)
}

# test getData('ga:20108692')
# test request: getData('ga:20108692', '2011-01-01', '2011-12-31', 'ga:visits', 'ga:city')