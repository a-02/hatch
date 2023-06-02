{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hatch.Encode
import Hatch.Types
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [individualRecordTests,combinedRecordTests]

individualRecordTests :: TestTree
individualRecordTests = testGroup  
  "Individual Record Tests" $ 
    [ testCase "File Header Record" 
       $ recordUnitTest parseFileHeaderRecord "File Header Record" exampleFileHeaderRecord
    , testCase "File Control Record"
       $ recordUnitTest parseFileControlRecord "File Control Record" exampleFileControlRecord
    , testCase "Batch Header Record"
       $ recordUnitTest parseBatchHeaderRecord "Batch Header Record" exampleBatchHeaderRecord
    , testCase "Batch Control Record"
       $ recordUnitTest parseBatchControlRecord "Batch Control Record" exampleBatchControlRecord
    ] ++ (recordUnitTest' <$> [ARC,BOC,CCD,CTX,POP,PPD,RCK,TEL,WEB])

combinedRecordTests :: TestTree
combinedRecordTests = testGroup
  "Combined Record Tests" $
    [ testCase "Batch Record"
       $ recordUnitTest parseBatchRecord "Batch Record" exampleBatchRecord
    , testCase "ACH"
       $ recordUnitTest parseACH "ACH" exampleACH
    ] ++ (recordUnitTestFromFile <$> exampleFiles)

exampleFiles :: [String]
exampleFiles = 
  [ "examples/arc-debit.ach"
  , "examples/boc-debit.ach"
  , "examples/ccd-debit.ach"
  , "examples/ctx-debit.ach"
  , "examples/pop-debit.ach"
  , "examples/ppd-debit.ach"
  , "examples/rck-debit.ach"
  , "examples/tel-debit.ach"
  , "examples/web-credit.ach"
  ]

recordUnitTestFromFile :: String -> TestTree
recordUnitTestFromFile filepath = testCase filepath $ 
  T.readFile filepath >>= recordUnitTest parseACH filepath



exampleBatchRecord :: T.Text
exampleBatchRecord = T.concat
  [ exampleBatchHeaderRecord
  , testEDR CCD
  , testEDR CCD
  , testEDR CCD
  , exampleBatchControlRecord
  ]

exampleACH :: T.Text
exampleACH = T.concat
  [ exampleFileHeaderRecord
  , exampleBatchRecord 
  , exampleBatchRecord 
  , exampleFileControlRecord
  , examplePadding
  , examplePadding
  , examplePadding
  , examplePadding
  , examplePadding
  ]

examplePadding :: T.Text
examplePadding = T.replicate 94 "9" `T.snoc` '\n'

exampleFileHeaderRecord :: T.Text
exampleFileHeaderRecord =   "101 03130001202313801041908161055A094101Federal Reserve Bank   My Bank Name           12345678\n"

exampleFileControlRecord :: T.Text
exampleFileControlRecord =  "9000001000001000000010023138010000000250000000000000000                                       \n"

exampleBatchHeaderRecord :: T.Text
exampleBatchHeaderRecord =  "5225Name on Account                     231380104 CCDVndr Pay        190816   1031300010000001\n"

exampleBatchControlRecord :: T.Text
exampleBatchControlRecord = "82250000010023138010000000250000000000000000231380104                          121042880000001\n"


testEDR :: StandardEntryClassCode -> T.Text
testEDR = \case
  ARC -> "62723138010412345678         0000250000123879654      ABC Company             0121042880000001\n"
  BOC -> "62723138010412345678         0000250000123879654      ABC Company             0121042880000001\n"
  CCD -> "627231380104744-5678-99      0000500000location1234567Best Co. #123456789012S 0031300010000001\n" 
  CTX -> "62723138010412345678         010000000045689033       0002Receiver Company  011121042880000001\n"
  POP -> "62723138010412345678         0000250500123456789PHILPAWade Arnold             0121042880000001\n"
  PPD -> "627231380104123456789        0200000000               Debit Account           0121042880000001\n"
  RCK -> "62723138010412345678         0000002400123123123      Wade Arnold             0121042880000001\n"
  TEL -> "62723138010412345678         0000050000               Receiver Account Name   0121042880000001\n"
  WEB -> "62223138010412345678         0000010000#989654        John Doe              S 1121042880000001\n"
  _   -> "Unsupported Standard Entry Class Code! I have yet to implement it."

-- This might have to live somewhere else eventually.
recordUnitTest :: ACHParser a -> String -> T.Text -> Assertion 
recordUnitTest parser name text =
  let res = runParser parser name text
   in case res of
        Left bundle -> assertFailure (show bundle)
        Right _ -> return ()

recordUnitTest' :: StandardEntryClassCode -> TestTree
recordUnitTest' secc = 
  let testName = show secc ++ " Entry Detail Record"
      edr = testEDR secc
   in case secc of
      ARC -> testCase testName $
        recordUnitTest parseARCEntryDetailRecord testName edr
      BOC -> testCase testName $
        recordUnitTest parseBOCEntryDetailRecord testName edr
      CCD -> testCase testName $
        recordUnitTest parseCCDEntryDetailRecord testName edr
      CTX -> testCase testName $
        recordUnitTest parseCTXEntryDetailRecord testName edr
      POP -> testCase testName $
        recordUnitTest parsePOPEntryDetailRecord testName edr
      PPD -> testCase testName $
        recordUnitTest parsePPDEntryDetailRecord testName edr
      RCK -> testCase testName $
        recordUnitTest parseRCKEntryDetailRecord testName edr
      TEL -> testCase testName $
        recordUnitTest parseTELEntryDetailRecord testName edr
      WEB -> testCase testName $
        recordUnitTest parseWEBEntryDetailRecord testName edr
      _   -> error "Unsupported Standard Entry Class Code!"

