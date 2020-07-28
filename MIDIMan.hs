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
               | Text String --FF 01 <len> <text>
               | Copyright String --FF 02 <len> <text>
               | SeqName String --FF 03 <len> <text>
               | Instrument String --FF 04 <len> <text>
               | Lyric String --FF 05 <len> <text>
               | Marker String --FF 06 <len> <text>
               | CuePoint String --FF 07 <len> <text>
               | ChanPref Int --FF 20 01 cc
               | TrackEnd --FF 2F 00
               | Tempo Int --FF 51 03 tt tt tt
               | SMTPEOffset Int Int Int Int Float --FF 54 05 hh mm ss fr ff
               | TimeSign Int Int Int Int --
               | KeySign Int Bool --
               | SeqSpec Int String String --

bsSeqNum :: BSParser MetaEvent 
bsSeqNum = (SeqNum . strToInt) <$> (bsHexString "FF 00 02" *> bsMultiChar 2)

bsText :: BSParser MetaEvent
bsText = Text <$> (bsHexString "FF 01" *> bsVarStr 1) 

bsCopyright :: BSParser MetaEvent
bsCopyright = Copyright <$> (bsHexString "FF 02" *> bsVarStr 1) 

bsSeqName :: BSParser MetaEvent
bsSeqName = SeqName <$> (bsHexString "FF 03" *> bsVarStr 1) 

bsInstrument :: BSParser MetaEvent
bsInstrument = Instrument <$> (bsHexString "FF 04" *> bsVarStr 1) 

bsLyric :: BSParser MetaEvent
bsLyric = Lyric <$> (bsHexString "FF 05" *> bsVarStr 1) 

bsMarker :: BSParser MetaEvent
bsMarker = Marker <$> (bsHexString "FF 06" *> bsVarStr 1) 

bsCuePoint :: BSParser MetaEvent
bsCuePoint = CuePoint <$> (bsHexString "FF 07" *> bsVarStr 1) 

bsChanPref :: BSParser MetaEvent
bsChanPref = ChanPref <$> (bsHexString "FF 20 01" *> bsInt 1)

bsTrackEnd :: BSParser MetaEvent
bsTrackEnd = (const TrackEnd) <$> bsHexString "FF 2F 00" 

bsTempo :: BSParser MetaEvent
bsTempo = Tempo <$> (bsHexString "FF 51 03" *> bsInt 1)

bsSMTPE :: BSParser MetaEvent
bsSMTPE = SMTPEOffset <$> (bsHexString "FF 54 05" *> bsInt 1) 
                      <*> bsInt 1
                      <*> bsInt 1
                      <*> bsInt 1
                      <*> ((\i -> (fromIntegral i) / 100) <$> bsInt 1)

bsVarStr :: Int -> BSParser String 
bsVarStr lenChars = BSParser prs where
    prs str = let lengthResult = run (bsMultiChar lenChars) str in
        case lengthResult of
            Nothing -> Nothing
            Just (lengthString, rest) -> let length = strToInt lengthString in
                run (bsMultiChar length) rest

bsInt :: Int -> BSParser Int
bsInt len = strToInt <$> bsMultiChar len

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