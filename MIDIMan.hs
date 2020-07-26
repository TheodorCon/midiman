{-# LANGUAGE ViewPatterns #-}

import System.IO

import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Word
import Data.Binary
import Data.Char

import Control.Applicative
import Numeric (showHex, readHex)

main = readMidiFile "John Denver - Take Me Home Country Roads.mid"

data MIDI = MIDI MThdChunk [MTrkChunk]
    deriving (Eq, Show)
data MThdChunk = MThd LengthHD FormatHD TracksHD Division
    deriving (Eq, Show)
type LengthHD = Int
type FormatHD = Int
type TracksHD = Int

data Division = TickPerQN Int | TicksFrameSeconds Int Int
    deriving (Eq, Show)

data MTrkChunk = MTrk Int String String
    deriving (Eq, Show)

data Event = Sysex SysexEvent 
           | Meta MetaEvent 
           | Mode ChannelModeEvent 
           | Voice ChannelVoiceEvent

data SysexEvent = F0 String --F0 <length> <sysex_data>
                | F7 String --F7 <length> <any_data>

data MetaEvent = SeqNum Int -- FF 00 02 ss ss
               | Text Int String --
               | Copyright Int String --
               | SeqName Int String --
               | Instrument Int String --
               | Lyric Int String --
               | Marker Int String --
               | CuePoint Int String --
               | ChanPref Int --
               | TrackEnd --
               | Tempo Int --
               | SMTPEOffset Int Int Int Int Float --
               | TimeSign Int Int Int Int --
               | KeySign Int Bool --
               | SeqSpec Int String String --

data ChannelVoiceEvent = NoteOff Int Int Int
                       | PolyKeyPress Int Int Int
                       | CtrlChange Int Int Int
                       | PrgChange Int Int Int
                       | ChanKeyPress Int Int Int
                       | PitchBend Int Int Int

data ChannelModeEvent = AllSoundOff Int
                      | ResetCtrls Int
                      | LocalCtrl Int Int
                      | AllNotesOff Int
                      | OmniOff Int
                      | OmniOn Int
                      | MonoOn Int Int 
                      | PolyOn Int

-- readMidiFile :: FilePath -> IO (MIDI)
readMidiFile filePath = do 
    s <- BS.readFile filePath
    let Just (MIDI hd ((MTrk _ _ _):(MTrk _ a b):rks), _) = run bsMIDI s in
        return $ foldl (\acc y -> acc ++ y ++ " ") "" $ map (\c -> ((\s -> if length s == 1 then '0' : s else s) . (map toUpper) . (flip showHex "") . strToInt) [c]) (a ++ b)

newtype BSParser a = BSParser { run :: ByteString -> Maybe (a, ByteString) }
-- data BSParser a = BSParser (ByteString -> Maybe (a, ByteString))

instance Functor BSParser where
    fmap func (BSParser pA) = BSParser pB where
        pB str = case pA str of
            Nothing -> Nothing
            Just (result, rest) -> Just (func result, rest)

instance Applicative BSParser where
    pure val = BSParser (\str -> Just (val, str))
    (<*>) (BSParser pA) (BSParser pB) = BSParser (\str -> case pA str of 
        Nothing -> Nothing
        Just (resultA, restA) -> case pB restA of 
            Nothing -> Nothing
            Just (resultB, restB) -> Just (resultA resultB, restB))

instance Alternative BSParser where 
    empty = BSParser (\_ -> Nothing)
    (<|>) (BSParser pA) (BSParser pB) = BSParser (\str -> (pA str) <|> (pB str))

repeatParser :: BSParser a -> BSParser [a]
repeatParser parser = toList <$> parser <*> (repeatParser parser) 
    <|> (\el -> [el]) <$> parser where 
        toList el list = el : list

bsChar :: Char -> BSParser Char
bsChar ch = BSParser prs where
    prs (BS.uncons -> Nothing) = Nothing
    prs (BS.uncons -> Just (first, rest)) = if ch == w2c first 
        then Just (ch, rest)
        else Nothing

bsAnyChar :: BSParser Char
bsAnyChar = BSParser prs where
    prs (BS.uncons -> Nothing) = Nothing
    prs (BS.uncons -> Just (first, rest)) = Just (w2c first, rest)

bsMultiChar :: Int -> BSParser [Char]
bsMultiChar 1 = (\ch->[ch]) <$> bsAnyChar
bsMultiChar times = toList <$> bsAnyChar <*> (bsMultiChar (times - 1)) where 
    toList ch str = ch : str

bsString :: String -> BSParser String
bsString = sequenceA . map bsChar

bsHexString :: String -> BSParser String
bsHexString hex = bsString $ hexStringToCharString hex

bsMThd :: BSParser MThdChunk
bsMThd = func <$> (bsString "MThd" 
     *> (bsMultiChar 4))
    <*> (bsMultiChar 2) 
    <*> (bsMultiChar 2) 
    <*> (bsMultiChar 2) where
        func a b c d = MThd (strToInt a) (strToInt b) (strToInt c) (strToDivision d) where
            strToDivision str = let asInt = strToInt d in 
                if odd asInt 
                    then let fullNumber = div (asInt - 1) 2
                             firstSeven = div fullNumber 256
                             lastEight  = mod fullNumber 256 in
                                TicksFrameSeconds firstSeven lastEight
                    else TickPerQN (div asInt 2)

bsMTrk :: BSParser MTrkChunk
bsMTrk = func <$> (bsString "MTrk" 
     *> (BSParser (\str -> if BS.null str 
        then Nothing
        else case run (bsMultiChar 4) str of 
            Nothing -> Nothing
            Just (strInt, restA) -> let times = strToInt strInt in
                case run (bsMultiChar times) restA of
                    Nothing -> Nothing
                    Just (dataString, restB) -> Just ((times, dataString), restB)))) where
                func (times, dataString) = let (delta, event) = splitString dataString "" in 
                    MTrk times delta event where
                        splitString (x:xs) acc = if strToInt [x] < 128 
                            then (acc ++ [x], xs)
                            else splitString xs (acc ++ [x])

bsSysex :: BSParser SysexEvent
bsSysex = F0 <$> (bsHexString "F0" *> BSParser prs) 
      <|> F7 <$> (bsHexString "F7" *> BSParser prs) where 
        prs str = if BS.null str
            then Nothing
            else let Just (strInt, rest) = run bsAnyChar str in
                run (bsMultiChar (ord strInt)) rest

bsMIDI :: BSParser MIDI
bsMIDI = func <$> bsMThd <*> (repeatParser bsMTrk) where
        func td rk = MIDI td rk

-- Utilites

strToInt :: String -> Int
strToInt (x:[]) = ord x
strToInt (x:xs) = (ord x) * (256 ^ (length xs)) + (strToInt xs)

hexStringToCharString :: String -> String
hexStringToCharString str =  let separate = words str 
                                 handleErr (a, "") = (a, "")
                                 handleErr (a, b) = error $ "Could not process hexadecimal: " ++ (show b)
                                 numbers = map (fst . handleErr . (!! 0) . readHex) separate 
                                 byteChars = map chr numbers in 
    byteChars