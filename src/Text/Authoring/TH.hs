{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.TH (rawQ, escQ) where


import Control.Applicative
import Data.Char (isSpace)
import Data.Monoid
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Parser.LookAhead
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, string)
import Safe (readMay)
import System.IO

import Text.Authoring.Combinator.Writer (raw, esc)

escQ = rawQ {quoteExp =  parseE (QQConfig { escaper = appE (varE 'esc)})}

rawQ :: QuasiQuoter
rawQ = QuasiQuoter { 
  quoteExp = parseE (QQConfig { escaper = appE (varE 'raw)}),
  quotePat  = error "Don't use Authoring QuasiQuotes in expression context" ,  
  quoteType = error "Don't use Authoring QuasiQuotes in expression context" ,  
  quoteDec  = error "Don't use Authoring QuasiQuotes in expression context" 
  }

data QQConfig = QQConfig 
  { escaper :: ExpQ -> ExpQ }


parseE :: QQConfig -> String -> ExpQ
parseE cfg str = do
  let res = parseString parseLang (Columns 0 0) str
  case res of
    Failure xs -> do 
      runIO $ do
        displayIO stdout $ renderPretty 0.8 80 $ xs <> linebreak
        putStrLn "Due to parse failure entire quote will be processed as a string."
      joinE $ map (cvtE cfg) $ [StrPart str]
    Success x -> joinE $ map (cvtE cfg) x


cvtE :: QQConfig -> Component -> ExpQ
cvtE cfg (StrPart x)    = escaper cfg $ appE (varE 'pack) $ stringE x
cvtE cfg (EmbedShow x)  = escaper cfg $ appE (varE 'pack) $ appE (varE 'showJoin) (varE $ mkName $ trim x)
cvtE _   (EmbedMonad x) = (varE $ mkName $ trim x)                           

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

showJoin :: Show a => a -> String
showJoin x = maybe sx id rsx
  where 
    sx :: String
    sx = show x
    rsx :: Maybe String
    rsx = readMay sx

joinE :: [ExpQ] -> ExpQ
joinE = foldl ap [e| return () |] 
  where
    ap a b = appE (appE (varE '(>>) ) a ) b

data Component 
  = StrPart    String
  | EmbedMonad String
  | EmbedShow  String deriving (Eq,Show)

parseLang :: Parser [Component]
parseLang = (many $ choice [try parseEmbedMonad, try parseEmbedShow, parseStrPart]) <* eof

parseStrPart :: Parser Component
parseStrPart = StrPart <$> go <?> "String Part"
  where
    go = do
      notFollowedBy $  choice [string "#{", string "@{"]
      h <- anyChar
      t <- manyTill anyChar (lookAhead $ choice [string "#{", string "@{", eof >> return ""])
      return $ h:t

parseEmbedMonad :: Parser Component
parseEmbedMonad = EmbedMonad <$> between (string "@{") (string "}") (some $ noneOf "}")
          <?> "Embed MonadAuthoring @{...}"


parseEmbedShow :: Parser Component
parseEmbedShow = EmbedShow <$> between (string "#{") (string "}") (some $ noneOf "}")
          <?> "Embed an instance of Show #{...}"
