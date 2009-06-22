> import Network.Browser
> import Network.HTTP
> import Network.HTTP.Wget
> import Network.URI
> import Data.Maybe
> import System.IO

> {- | Download a URL.  (Left errorMessage) if an error,
> (Right doc) if success. -}
> downloadURL :: String -> IO (Either String String)
> downloadURL url =
>     do resp <- simpleHTTP request
>        case resp of
>          Left x -> return $ Left ("Error connecting: " ++ show x)
>          Right r -> 
>              case rspCode r of
>                (2,_,_) -> return $ Right (rspBody r)
>                (3,_,_) -> -- A HTTP redirect
>                  case findHeader HdrLocation r of
>                    Nothing -> return $ Left (show r)
>                    Just url -> downloadURL url
>                _ -> return $ Left (show r)
>     where request = Request {rqURI = uri,
>                              rqMethod = GET,
>                              rqHeaders = [],
>                              rqBody = ""}
>           uri = fromJust $ parseURI url

> downloadHTTP url = do
>      rsp <- simpleHTTP (getRequest url)
>              -- fetch document and return it (as a 'String'.)
>      getResponseBody rsp

Still needs XML parser and a main function that returns all reservation for
a given user in the format that the Observer data structure expects.