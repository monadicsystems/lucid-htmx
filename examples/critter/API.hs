module API where

import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server
import Core


data PageLogin -- = PageLogin
data FormNewUser -- = PageNewUserForm
data FormNewCreet -- = PageNewCreetForm
data PageHome = PageHome [Creet]
data PageUserDetail = PageUserDetail AuthorizedUser

type Home = Get '[HTML] PageHome

type NewUserForm = "user" :> "new" :> Get '[HTML] FormNewUser

type NewCreetForm = "creet" :> "new" :> Get '[HTML] FormNewCreet

-- TODO: Need auth
type User = "user" :> Capture "user_id" (ID AuthorizedUser) :> Get '[HTML] AuthorizedUser

type Creet = "creet" :> Capture "creet_id" (ID Creet) :> Get '[HTML] Creet


