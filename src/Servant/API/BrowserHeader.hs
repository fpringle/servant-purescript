-- | A header which gets sent by the browser and is thus of no concern for the client consumer of the API.
module Servant.API.BrowserHeader where

import GHC.TypeLits
import Servant
import Servant.Foreign
import Servant.Links
import Servant.Subscriber.Subscribable

data BrowserHeader (sym :: Symbol) a

type instance IsElem' e (BrowserHeader :> s) = IsElem e s

instance (HasLink sub) => HasLink (BrowserHeader sym a :> sub) where
    type MkLink (BrowserHeader sym a :> sub) b = MkLink (Header sym a :> sub) b
    toLink toA Proxy = toLink toA (Proxy :: Proxy (Header sym a :> sub))

instance
    ( KnownSymbol sym
    , FromHttpApiData a
    , HasServer sublayout context
    , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
    ) =>
    HasServer (BrowserHeader sym a :> sublayout) context
    where
    type ServerT (BrowserHeader sym a :> sublayout) m = ServerT (Header sym a :> sublayout) m

    route Proxy = route (Proxy :: Proxy (Header sym a :> sublayout))

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @sublayout) pc nt . s

-- Ignore BrowserHeader in HasForeign:
instance
    (KnownSymbol sym, HasForeign lang ftype sublayout) =>
    HasForeign lang ftype (BrowserHeader sym a :> sublayout)
    where
    type Foreign ftype (BrowserHeader sym a :> sublayout) = Foreign ftype sublayout

    foreignFor lang p Proxy = foreignFor lang p (Proxy :: Proxy sublayout)

type instance IsSubscribable' endpoint (BrowserHeader sym a :> sub) = IsSubscribable endpoint sub
