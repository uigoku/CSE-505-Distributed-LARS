{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Reasoner.Wire where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Expr
import Data.Foldable (fold, for_)
import Data.List.NonEmpty (intersperse, nonEmpty)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Reasoner.Tickless.Database
import System.ZMQ4 hiding (events)
import Text.PrettyPrint.Leijen.Text (displayTStrict, pretty, renderOneLine)

import qualified Data.Set as S
import qualified Language.LARS as LARS
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data WireAtom =
    WireAtom PhaseChange
             LARS.GroundBasicAtom
    deriving (Eq, Show, Ord)

data ReasonerError translation
    = TranslationError translation
    | NetworkParseError ByteString
                        Text
    | AnswerSetError Text

unWire :: WireAtom -> (LARS.GroundBasicAtom, PhaseChange)
unWire (WireAtom p b) = (b, p)

-- | Parse a network message. This wrapper assumes UTF8 encoding over the wire.
parseNetwork :: LARS.Parser c -> ByteString -> Either Text c
parseNetwork p s =
    first (pack . P.errorBundlePretty) . P.parse p "<network>" . decodeUtf8 $ s

parseWire :: MonadError (ReasonerError a) m => ByteString -> m WireAtom
parseWire x = liftEither . first (NetworkParseError x) . parseNetwork wire $ x

toWire :: WireAtom -> ByteString
toWire (WireAtom phase basic) =
    let basic' =
            byteString . encodeUtf8 . displayTStrict . renderOneLine . pretty $
            basic
        phase' =
            case phase of
                Positive -> ";+"
                Negative -> ";-"
     in toStrict . toLazyByteString $ basic' <> phase'

wire :: LARS.Parser WireAtom
wire = do
    a <- LARS.ground <$> LARS.atom
    _ <- P.char ';'
    p <- phase
    pure (WireAtom p a)
  where
    phase = (Positive <$ P.char '+') P.<|> (Negative <$ P.char '-')

wireMatch :: S.Set LARS.BasicAtom -> WireAtom -> Bool
wireMatch xs (WireAtom _ x) = LARS.positive (LARS.unground x) `S.member` xs

toWireAtom ::
       MonadError (ReasonerError a) m
    => PhaseChange
    -> LARS.GroundAtom
    -> m WireAtom
toWireAtom p (LARS.Atom b) = pure $ WireAtom p b
toWireAtom p (LARS.AtAtom b (String _)) = pure $ WireAtom p b
toWireAtom _ _ = throwError $ AnswerSetError "Fatal: Invalid atom created"

-- | Setup network for reasoner, returning the set of filters.
setupSockets ::
       (MonadLogger m, MonadReader Sockets m, Foldable t, MonadIO m)
    => t String
    -> String
    -> m ()
setupSockets inputs output = do
    logInfoN "Setting up network"
    ask >>= \Sockets {..} -> do
        for_ inputs $ \i -> do
            logDebugN ("Subscribing to " <> pack i)
            liftIO (connect subscriber i)
        liftIO $ do
            bind publisher output
            subscribe subscriber ""

-- | Publishes a changeset to the network
publishChanges ::
       (MonadLogger m, MonadReader Sockets m, MonadIO m) => [WireAtom] -> m ()
publishChanges changeset = asks publisher >>= publishChanges' changeset

publishChanges' ::
       (MonadLogger m, MonadIO m)
    => [WireAtom]
    -> Socket Pub
    -> m ()
publishChanges' changeset pub =
    case nonEmpty . map toWire $ changeset of
        Nothing -> pure ()
        Just xs -> do
            logDebugN $
                "Sending changeset " <> decodeUtf8 (fold $ intersperse " " xs)
            liftIO $ sendMulti pub xs

data Sockets = Sockets
    { publisher :: Socket Pub
    , subscriber :: Socket Sub
    }

sockets :: (Socket Pub -> Socket Sub -> IO a) -> IO a
sockets k =
    withContext $ \ctx ->
        withSocket ctx Pub $ \pub -> withSocket ctx Sub $ \sub -> (k pub sub)

-- | Given an answer set, calculate the changeset to send back out to the
-- network. This function is self contained and will take care of storing the
-- ongoing changes as an effect.
calculateChangeset ::
       (MonadError (ReasonerError t) m, MonadState (S.Set LARS.GroundAtom) m)
    => S.Set LARS.GroundAtom
    -> m [WireAtom]
calculateChangeset answer = do
    oldAnswer <- get
    removed <-
        traverse (toWireAtom Negative) . S.toList $
        oldAnswer `S.difference` answer
    added <-
        traverse (toWireAtom Positive) . S.toList $
        answer `S.difference` oldAnswer
    put answer
    pure $ removed <> added

filterString :: LARS.BasicAtom -> ByteString
filterString (LARS.BasicAtom _ x _) = encodeUtf8 x

-- | Log a 'ReasonerError' in a 'MonadLogger' environment.
logErrorR :: (MonadLogger m, Show t) => ReasonerError t -> m ()
logErrorR (TranslationError t) =
    logErrorN $ "Error during translation: " <> pack (show t)
logErrorR (NetworkParseError s t) =
    logErrorN $
    "Error while parsing data (" <> decodeUtf8 s <> ") from network: " <> t
logErrorR (AnswerSetError t) =
    logErrorN $ "Error while parsing answer set: " <> t

