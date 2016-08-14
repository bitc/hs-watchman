module System.Directory.Watchman.BSER.Protocol
    ( sendBSERMessage
    , readBSERMessage
    ) where

import Control.Monad (unless)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import System.Directory.Watchman.BSER

sendBSERMessage :: Socket -> BSERValue -> IO ()
sendBSERMessage sock val = do
    let encoded = encode val
        lengthPrefix = runPut $ put (compactBSERInt (BL.length encoded))
        packet = headerBSER `BL.append` lengthPrefix `BL.append` encoded
    BL.sendAll sock packet
    where
    headerBSER = BL.pack [0x00, 0x01]

recvN :: Socket -> Int -> IO BL.ByteString
recvN sock n = recvN' sock [] n

recvN' :: Socket -> [BL.ByteString] -> Int -> IO BL.ByteString
recvN' sock buf n = do
    x <- BL.recv sock (fromIntegral n)
    let remaining = n - fromIntegral (BL.length x)
    if  remaining == 0 ||
        remaining == n -- Remote side closed the socket
            then pure $ BL.concat (reverse (x:buf))
            else recvN' sock (x:buf) remaining

readBSERMessage :: Socket -> IO BSERValue
readBSERMessage sock = do
    -- TODO Clean this up
    header <- recvN sock 2 -- Swallow the protocol header
    unless (BL.length header == 2) $
        fail $ "Error reading header. Received: " ++ show header

    tagBuf <- recvN sock 1
    unless (BL.length tagBuf == 1) $
        fail "Error reading tag"

    lenBuf <- case BL.head tagBuf of
        0x03 -> recvN sock 1
        0x04 -> recvN sock 2
        0x05 -> recvN sock 4
        0x06 -> recvN sock 8
        _ ->
            -- TODO Better error handling
            fail "Invalid BSER Message"
    let lengthPrefix = tagBuf `BL.append` lenBuf
    len <- case decodeOrFail lengthPrefix of
        Left (_, _, err) -> fail err
        Right (_, _, val) -> case readBSERInt val of
            Left err -> fail err
            Right l -> pure l
    encoded <- recvN sock len
    case decodeOrFail encoded of
        Left (_, _, err) -> fail err
        Right (_, _, val) -> pure val
