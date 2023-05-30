{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
tests = testGroup "All" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "File Header Record" 
       $ recordUnitTest parseFileHeaderRecord "File Header Record" exampleFileHeaderRecord
    , testCase "Batch Header Record"
       $ recordUnitTest parseBatchHeaderRecord "Batch Header Record" exampleBatchHeaderRecord
    , testCase "ARC Entry Detail Record"
       $ recordUnitTest parseARCEntryDetailRecord "ARC Entry Detail Record" exampleEntryDetailRecord
    , testCase "BOC Entry Detail Record"
       $ recordUnitTest parseBOCEntryDetailRecord "BOC Entry Detail Record" exampleEntryDetailRecord
    , testCase "CCD Entry Detail Record"
       $ recordUnitTest parseCCDEntryDetailRecord "CCD Entry Detail Record" exampleCCDEntryDetailRecord
    ]

exampleFileHeaderRecord :: T.Text
exampleFileHeaderRecord = "101 03130001202313801041908161055A094101Federal Reserve Bank   My Bank Name           12345678\n"

exampleBatchHeaderRecord :: T.Text
exampleBatchHeaderRecord = "5225Name on Account                     231380104 CCDVndr Pay        190816   1031300010000001\n"

exampleEntryDetailRecord :: T.Text
exampleEntryDetailRecord = "62723138010412345678         0000250000123879654      ABC Company             0121042880000001\n"

exampleCCDEntryDetailRecord :: T.Text
exampleCCDEntryDetailRecord = "627231380104744-5678-99      0000500000location1234567Best Co. #123456789012S 0031300010000001/n" 

recordUnitTest :: ACHParser a -> String -> T.Text -> Assertion 
recordUnitTest parser name text =
  let res = runParser parser name text
   in case res of
        Left bundle -> assertFailure (show bundle)
        Right _ -> return ()

