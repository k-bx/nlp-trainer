{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conllu.IO
import Conllu.Type
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

tshow :: Show a => a -> Text
tshow = T.pack . show

tshowt :: Text -> Text
tshowt x = "\"" <> esc x <> "\""
  where
    esc = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

genElmEntities :: Document -> Text
genElmEntities Document {..} =
  "  [ \n" <> (T.intercalate "    , \n  " (map renderSent _sents)) <> " ]\n"
  where
    renderSent Sentence {..} =
      "  [ " <> (T.intercalate "    , \n    " (mapMaybe renderToken _tokens)) <> " ]\n"
    renderToken SToken {..} =
      Just
        ("Entity " <> showIndex _ix <> " " <> tshowt (T.pack (fromMaybe "" _form)) <>
         " " <>
         tshowt (T.pack (fromMaybe "" _lemma)) <>
         " " <>
         (tshowt (fromMaybe "" (tshow <$> _upostag))) <>
         " " <>
         tshowt (T.pack (fromMaybe "" _xpostag)) <>
         " \"\" " <>
         fromMaybe "0" (showIndex <$> _dephead) <>
         " \"\" [] \"\"")
    renderToken _ = Nothing
    -- TODO: implement non-IntIndex cases properly (I'm lazy, needs refactoring elm code)
    showIndex :: Index -> Text
    showIndex (IntIndex i) = tshow i
    showIndex _ = "0"

main :: IO ()
main = do
  let enFile =
        "/home/kb/workspace/UniversalDependencies/UD_English-EWT/en-ud-train.conllu"
  let ukFile =
        "/home/kb/workspace/UniversalDependencies/UD_Ukrainian-IU/uk-ud-train.conllu"
  enDoc <- readConlluFile enFile
  ukDoc <- readConlluFile ukFile
  let enElmEntities = genElmEntities (enDoc{_sents=take 200 (_sents enDoc)})
  let ukElmEntities = genElmEntities (ukDoc{_sents=take 200 (_sents ukDoc)})
  T.writeFile "en_entities.elm" enElmEntities
  T.writeFile "uk_entities.elm" ukElmEntities
