{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}

import Control.Applicative ((<$>), optional)
import Control.Exception ( bracket )
import Control.Monad ( msum, forM_, mapM_ )
import Control.Monad.Reader ( ask )
import Control.Monad.State ( get, put )
import Data.Data ( Data, Typeable )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server (Response, ServerPart, dir, nullDir, nullConf, ok, simpleHTTP, toResponse, Method(..), method, seeOther, look, lookText, decodeBody, BodyPolicy, defaultBodyPolicy )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- types for characters
type Initiative = (Integer, Integer) -- base and num of dice
data Character = Character {
	charId :: Integer,  
	charName :: String, 
	charInitiative :: Maybe Initiative 
} deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Character)


-- global app state
data AppState = AppState {
	nextCharId :: Integer,
	stateCharacters :: [Character]
} deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AppState)

initialAppState :: AppState
initialAppState = AppState 1 []

-- state manipulation

peekCharacters :: Query AppState [Character]
peekCharacters =  stateCharacters <$> ask

addCharacter :: Character -> Update AppState [Character]
addCharacter c = do
	a@AppState{..} <- get
	let newChars = c { charId = nextCharId } : stateCharacters
	let newNextId = nextCharId + 1
	put $ a { stateCharacters = newChars, nextCharId = newNextId }
	return newChars

$(makeAcidic ''AppState [ 'peekCharacters, 'addCharacter ])


-- web templates
templateCharacters :: [Character] -> ServerPart Response
templateCharacters cs =
	ok $ template "characters" $ do
		H.h1 "Characters"
		H.table $ forM_ cs (\c -> 
			H.tr $ do
				H.td $ (toHtml (charId c))
				H.td $ (toHtml (charName c))
				H.td $ (toHtml (show (charInitiative c)))
			)

		H.form ! action "/char" ! enctype "multipart/form-data" ! A.method "POST" $ do
			label ! A.for "msg" $ "enter new name"
			input ! type_ "text" ! A.id "name" ! name "name"
			input ! type_ "submit" ! value "add character"


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: AcidState AppState -> ServerPart Response
handlers acid = do
	decodeBody myPolicy
	msum
		[ dir "char" $ do
			method GET
			cs <- query' acid PeekCharacters
			templateCharacters cs
		, dir "char" $ do
			method POST
			name <- look "name"
			-- decodeBody
			c <- update' acid (AddCharacter (Character 0 name Nothing))
			seeOther ("/char" :: String) (toResponse ())
		, homePage
		]



main :: IO ()
main =
	bracket (openLocalState initialAppState)
            (createCheckpointAndClose)
            (\acid ->
    	        simpleHTTP nullConf (handlers acid))



template :: Text -> Html -> Response
template title body = toResponse $
	H.html $ do
		H.head $ do
			H.title (toHtml title)
		H.body $ do
			body
			p $ a ! href "/" $ "back home"





homePage :: ServerPart Response
homePage = 
	ok $ template "home page" $ do
		H.h1 "Hello!"