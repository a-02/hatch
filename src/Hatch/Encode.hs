{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}


module Hatch.Encode where

import Data.Void
import Chronos
import Hatch.Types
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

encode :: T.Text -> ACH
encode = encode

type ACHParser = Parsec Void T.Text

integer :: ACHParser Int
integer = L.lexeme sc L.decimal

sc :: ACHParser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

parseFileHeaderRecord :: ACHParser FileHeaderRecord
parseFileHeaderRecord = do
  fileHeaderRecordTypeCode <- parseFileHeaderRecordTypeCode
  priorityCode <- parsePriorityCode
  immediateDestination <- parseRightJustified 10 digitChar
  immediateOrigin <- parseRightJustified 10 digitChar
  fileCreationDate <- parseDay
  fileCreationTime <- parseTimeMaybe
  fileIDModifier <- parseFileIDModifier
  recordSize <- parseRecordSize
  blockingFactor <- parseBlockingFactor
  formatCode <- parseFormatCode
  destination <- parseLeftJustified 23 printChar
  originName <- parseLeftJustified 23 printChar
  referenceCode <- Just <$> 
    label "referenceCode" (string "        " <|> parseLeftJustified 8 alphaNumChar)
  _ <- eol
  return FileHeaderRecord{..}

parseBatchHeaderRecord :: ACHParser BatchHeaderRecord
parseBatchHeaderRecord = do
  batchHeaderRecordTypeCode <- parseBatchHeaderRecordTypeCode
  serviceClassCode <- parseServiceClassCode
  companyName <- parseLeftJustified 16 printChar
  companyDiscretionaryData <- choice
    [ Nothing <$ count 20 spaceChar
    , Just <$> parseLeftJustified 20 printChar
    ]
  companyIdentification <- parseLeftJustified 10 printChar
  standardEntryClassCode <- parseStandardEntryClassCode
  companyEntryDescription <- parseLeftJustified 10 printChar
  companyDescriptiveDate <- choice
    [ Nothing <$ count 6 spaceChar
    , Just <$> parseLeftJustified 6 printChar
    ]
  effectiveEntryDate <- parseDay 
  settlementDate <- string "   " -- is this right? waiting for the guide to get here
  originatorStatusCode <- OriginatorStatusCode <$ char '1'
  originatingDFIIdentification <- parseRightJustified 8 digitChar
  batchNumber <- parseRightJustified 7 digitChar
  _ <- eol
  return BatchHeaderRecord{..}

parseARCEntryDetailRecord :: ACHParser EntryDetailRecordARC
parseARCEntryDetailRecord = do
  arcRecordTypeCode <- parseEntryDetailRecordTypeCode
  arcTransactionCode <- parseTransactionCodeSmall
  arcRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar  
  arcCheckDigit <- digitChar
  arcDFIAccountNumber <- parseLeftJustified 17 printChar
  arcAmount <- parseRightJustified 10 digitChar
  arcCheckSerialNumber <- parseLeftJustifiedNumber 15
  -- ^ the guide was useful after all! 
  -- despite being labelled as an "alphanumeric" field,
  -- only a check serial NUMBER is allowed here. no funny business
  -- like writing "Check #12345" in there.
  -- see 2023 NACHA guide, section V chapter 37, formatting requirements
  arcIndividualName <- parseOptionalNonNumber 22
  arcDiscretionaryData <- parseOptionalNonNumber 2
  arcAddendaRecordIndicator <- parseAddendaRecordIndicator
  arcTraceNumber <- parseRightJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordARC{..}

parseBOCEntryDetailRecord :: ACHParser EntryDetailRecordBOC
parseBOCEntryDetailRecord = do
  bocRecordTypeCode <- parseEntryDetailRecordTypeCode
  bocTransactionCode <- parseTransactionCodeSmall
  bocRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  bocCheckDigit <- digitChar
  bocDFIAccountNumber <- parseLeftJustified 17 printChar
  bocAmount <- parseRightJustified 10 digitChar
  bocCheckSerialNumber <- parseLeftJustified 15 printChar
  bocIndividualName <- parseOptionalNonNumber 22
  bocDiscretionaryData <- parseOptionalNonNumber 2
  bocAddendaRecordIndicator <- parseAddendaRecordIndicator
  bocTraceNumber <- parseLeftJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordBOC{..}

parseCCDEntryDetailRecord :: ACHParser EntryDetailRecordCCD
parseCCDEntryDetailRecord = do
  ccdRecordTypeCode <- parseEntryDetailRecordTypeCode
  ccdTransactionCode <- parseTransactionCodeFull
  ccdRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  ccdCheckDigit <- digitChar
  ccdDFIAccountNumber <- parseLeftJustified 17 printChar
  ccdAmount <- parseRightJustified 10 digitChar
  ccdIdentificationNumber <- parseOptionalNonNumber 15
  ccdReceivingCompanyName <- parseLeftJustified 22 printChar
  ccdDiscretionaryData <- parseOptionalNonNumber 2
  ccdAddendaRecordIndicator <- parseAddendaRecordIndicator
  ccdTraceNumber <- parseLeftJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordCCD{..}

parseCCDAddenda :: ACHParser AddendaRecordCCD
parseCCDAddenda = do
  addendaCCDRecordTypeCode <- parseAddendaRecordTypeCode
  addendaCCDAddendaTypeCode <- AddendaTypeCode <$ string "05"
  addendaCCDPaymentRelatedInformation <- parseOptionalNonNumber 80
  addendaCCDAddendaSequenceNumber <- parseRightJustified 4 digitChar
  addendaCCDEntryDetailSequenceNumber <- parseRightJustified 7 digitChar
  _ <- eol
  return AddendaRecordCCD{..}

parseCTXEntryDetailRecord :: ACHParser EntryDetailRecordCTX
parseCTXEntryDetailRecord = do
  ctxRecordTypeCode <- parseEntryDetailRecordTypeCode
  ctxTransactionCode <- parseTransactionCodeFull
  ctxRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  ctxCheckDigit <- digitChar
  ctxDFIAccountNumber <- parseLeftJustified 17 printChar
  ctxAmount <- parseRightJustified 10 digitChar
  ctxIdentificationNumber <- parseOptionalNonNumber 15
  ctxNumberOfAddendaRecords <- parseRightJustified 4 digitChar
  ctxReceivingCompanyName <- parseLeftJustified 16 printChar
  ctxReserved <- Reserved <$ string "  "
  ctxDiscretionaryData <- parseOptionalNonNumber 2
  ctxAddendaRecordIndicator <- parseAddendaRecordIndicator
  ctxTraceNumber <- parseLeftJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordCTX{..}

parseCTXAddenda :: ACHParser AddendaRecordCTX
parseCTXAddenda = do
  addendaCTXRecordTypeCode <- parseAddendaRecordTypeCode
  addendaCTXAddendaTypeCode <- AddendaTypeCode <$ string "05"
  addendaCTXPaymentRelatedInformation <- parseOptionalNonNumber 80
  addendaCTXAddendaSequenceNumber <- parseRightJustified 4 digitChar
  addendaCTXEntryDetailSequenceNumber <- parseRightJustified 7 digitChar
  _ <- eol
  return AddendaRecordCTX{..}

parsePOPEntryDetailRecord :: ACHParser EntryDetailRecordPOP
parsePOPEntryDetailRecord = do
  popRecordTypeCode <- parseEntryDetailRecordTypeCode
  popTransactionCode <- parseTransactionCodeSmall
  popRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  popCheckDigit <- digitChar
  popDFIAccountNumber <- parseLeftJustified 17 printChar
  popAmount <- parseRightJustified 10 digitChar
  popCheckSerialNumber <- parseLeftJustifiedNumber 9
  popTerminalCity <- parseLeftJustified 4 printChar
  popTerminalState <- parseLeftJustified 2 printChar
  popIndividualName <- parseOptionalNonNumber 22
  popDiscretionaryData <- parseOptionalNonNumber 2
  popAddendaRecordIndicator <- (False <$ char '0') <|> (True <$ char '1')
  popTraceNumber <- parseLeftJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordPOP{..}

parsePPDEntryDetailRecord :: ACHParser EntryDetailRecordPPD
parsePPDEntryDetailRecord = do
  ppdRecordTypeCode <- parseEntryDetailRecordTypeCode
  ppdTransactionCode <- parseTransactionCodeFull
  ppdRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  ppdCheckDigit <- digitChar
  ppdDFIAccountNumber <- parseLeftJustified 17 printChar
  ppdAmount <- parseRightJustified 10 digitChar
  ppdIndividualIdentificationNumber <- parseOptionalNumber 15
  ppdIndividualName <- parseLeftJustified 22 printChar
  ppdDiscretionaryData <- parseOptionalNonNumber 2
  ppdAddendaRecordIndicator <- (False <$ char '0') <|> (True <$ char '1')
  ppdTraceNumber <- parseLeftJustified 15 digitChar
  _ <- eol
  return EntryDetailRecordPPD{..}

parsePPDAddenda :: ACHParser AddendaRecordPPD
parsePPDAddenda = do
  addendaPPDRecordTypeCode <- parseAddendaRecordTypeCode
  addendaPPDAddendaTypeCode <- AddendaTypeCode <$ string "05"
  addendaPPDPaymentRelatedInformation <- parseOptionalNonNumber 80
  addendaPPDAddendaSequenceNumber <- parseRightJustified 4 digitChar
  addendaPPDEntryDetailSequenceNumber <- parseRightJustified 7 digitChar
  _ <- eol
  return AddendaRecordPPD{..}

parseRCKEntryDetailRecord :: ACHParser EntryDetailRecordRCK
parseRCKEntryDetailRecord = do
  rckRecordTypeCode <- parseEntryDetailRecordTypeCode
  rckTransactionCode <- parseTransactionCodeSmall
  rckRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  rckCheckDigit <- digitChar
  rckDFIAccountNumber <- parseLeftJustified 17 printChar
  rckAmount <- parseRightJustified 10 digitChar
  rckCheckSerialNumber <- parseLeftJustifiedNumber 15
  rckIndividualName <- parseLeftJustified 22 printChar
  rckDiscretionaryData <- parseOptionalNonNumber 2
  rckAddendaRecordIndicator <- (False <$ char '0') <|> (True <$ char '1')
  rckTraceNumber <- parseLeftJustified 15 digitChar
  return EntryDetailRecordRCK{..}

parseTELEntryDetailRecord :: ACHParser EntryDetailRecordTEL
parseTELEntryDetailRecord = do
  telRecordTypeCode <- parseEntryDetailRecordTypeCode
  telTransactionCode <- parseTransactionCodeSmall
  telRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  telCheckDigit <- digitChar
  telDFIAccountNumber <- parseLeftJustified 17 printChar
  telAmount <- parseRightJustified 10 digitChar
  telIndividualIdentificationNumber <- parseOptionalNumber 15
  telIndividualName <- parseLeftJustified 22 printChar
  telPaymentTypeCode <- parseOptionalNonNumber 2
  telAddendaRecordIndicator <- (False <$ char '0') <|> (True <$ char '1')
  telTraceNumber <- parseRightJustified 15 digitChar 
  return EntryDetailRecordTEL{..}

parseWEBEntryDetailRecord :: ACHParser EntryDetailRecordWEB
parseWEBEntryDetailRecord = do
  webRecordTypeCode <- parseEntryDetailRecordTypeCode
  webTransactionCode <- parseTransactionCodeSmall
  webRDFIRoutingTransitNumber <- parseRightJustified 8 digitChar
  webCheckDigit <- digitChar
  webDFIAccountNumber <- parseLeftJustified 17 printChar
  webAmount <- parseRightJustified 10 digitChar
  webIndividualIdentificationNumber <- parseOptionalNumber 15
  webIndividualName <- parseLeftJustified 22 printChar 
  webPaymentTypeCode <- parseOptionalNonNumber 2
  webAddendaRecordIndicator <- (False <$ char '0') <|> (True <$ char '1')
  webTraceNumber <- parseLeftJustified 15 digitChar
  return EntryDetailRecordWEB{..}

parseBatchControlRecord :: ACHParser BatchControlRecord
parseBatchControlRecord = do
  batchControlRecordTypeCode <- CodeBatchControl <$ char '8'
  batchControlServiceClassCode <- parseServiceClassCode
  entryCount <- parseRightJustified 6 digitChar
  entryHash <- parseRightJustified 10 digitChar
  totalDebitInBatch <- parseRightJustified 12 digitChar
  totalCreditInBatch <- parseRightJustified 12 digitChar
  batchControlCompanyIdentification <- parseLeftJustified 10 printChar
  messageAuthenticationCode <- parseOptionalNonNumber 19
  batchControlReserved <- Reserved <$ count 6 spaceChar
  batchControlOriginatingDFIIdentification <- parseRightJustified 8 digitChar
  batchControlBatchNumber <- parseRightJustified 7 digitChar
  return BatchControlRecord{..}

parseFileControlRecord :: ACHParser FileControlRecord
parseFileControlRecord = do
  fileControlRecordTypeCode <- CodeFileControl <$ char '9'
  fileControlBatchCount <- parseRightJustified 6 digitChar
  fileControlBlockCount <- parseRightJustified 6 digitChar
  fileControlEntryCount <- parseRightJustified 8 digitChar
  fileControlEntryHash <- parseRightJustified 10 digitChar
  totalDebitInFile <- parseRightJustified 12 digitChar
  totalCreditInFile <- parseRightJustified 12 digitChar
  fileControlReserved <- Reserved <$ count 39 spaceChar
  return FileControlRecord{..}

parseDiscretionaryData :: ACHParser DiscretionaryData
parseDiscretionaryData = (<|>)
  (ElectronicOnly <$ string "  ")
  (PreauthorizedCheckOnly <$ string "1*") 

parseTransactionCodeFull :: ACHParser TransactionCodeFull
parseTransactionCodeFull = choice
  [ AutomatedDepositChecking <$ string "22"
  , PrenoteCheckingCredit    <$ string "23"
  , ZeroDollarCheckingCredit <$ string "24"
  , AutomatedPaymentChecking <$ string "27"
  , PrenoteCheckingDebit     <$ string "28"
  , ZeroDollarCheckingDebit  <$ string "29"
  , AutomatedDepositSavings  <$ string "32"
  , PrenoteSavingsCredit     <$ string "33"
  , ZeroDollarSavingsCredit  <$ string "34"
  , AutomatedPaymentSavings  <$ string "37"
  , PrenoteSavingsDebit      <$ string "38"
  , ZeroDollarSavingsDebit   <$ string "39"
  ]

parseAddendaRecordTypeCode :: ACHParser RecordTypeCode
parseAddendaRecordTypeCode = CodeAddenda <$ char '7'

parseAddendaRecordIndicator :: ACHParser Bool
parseAddendaRecordIndicator = (<|>)
  (True <$ char '1')
  (False <$ char '0')

parseOptionalNonNumber :: Int -> ACHParser (Maybe T.Text)
parseOptionalNonNumber i = (<|>)
  (Nothing <$ count i spaceChar)
  (Just <$> parseLeftJustified i printChar)

parseOptionalNumber :: Int -> ACHParser (Maybe T.Text)
parseOptionalNumber i = (<|>)
  (Nothing <$ count i spaceChar)
  (Just <$> parseLeftJustified i printChar)

parseTransactionCodeSmall :: ACHParser TransactionCodeSmall
parseTransactionCodeSmall = 
  (AutomatedPayment <$ string "27") <|> (Prenote <$ string "28")

parseEntryDetailRecordTypeCode :: ACHParser RecordTypeCode
parseEntryDetailRecordTypeCode = CodeEntryDetail <$ char '6'

parseFileHeaderRecordTypeCode :: ACHParser RecordTypeCode
parseFileHeaderRecordTypeCode = CodeFileHeader <$ char '1'

parseBatchHeaderRecordTypeCode :: ACHParser RecordTypeCode
parseBatchHeaderRecordTypeCode = CodeBatchRecord <$ char '5'

parseServiceClassCode :: ACHParser ServiceClassCode
parseServiceClassCode = choice
  [ Mixed200 <$ string "200"
  , CreditsOnly220 <$ string "220"
  , DebitsOnly225 <$ string "225"
  ]

parseStandardEntryClassCode :: ACHParser StandardEntryClassCode
parseStandardEntryClassCode = choice
  [ ACK <$ string "ACK"
  , ADV <$ string "ADV"
  , ARC <$ string "ARC"
  , ATX <$ string "ATX"
  , BOC <$ string "BOC"
  , CCD <$ string "CCD"
  , CIE <$ string "CIE"
  , COR <$ string "COR"
  , CTX <$ string "CTX"
  , DNE <$ string "DNE"
  , ENR <$ string "ENR"
  , IAT <$ string "IAT"
  , MTE <$ string "MTE"
  , POP <$ string "POP"
  , POS <$ string "POS"
  , PPD <$ string "PPD"
  , RCK <$ string "RCK"
  , SHR <$ string "SHR"
  , TEL <$ string "TEL"
  , TRC <$ string "TRC"
  , TRX <$ string "TRX"
  , WEB <$ string "WEB"
  , XCK <$ string "XCK"
  ]

parseRecordTypeCode :: ACHParser RecordTypeCode
parseRecordTypeCode = choice
  [ CodeFileHeader <$ char '1'
  , CodeBatchRecord <$ char '5'
  , CodeEntryDetail <$ char '6'
  , CodeAddenda <$ char '7'
  , CodeBatchControl <$ char '8'
  , CodeFileControl <$ char '9'
  ]

parsePriorityCode :: ACHParser PriorityCode
parsePriorityCode = PriorityCode <$ chunk "01"

-- For fields that look like "   1234567". Numbers in an ACH file are
-- always right-justified.
parseRightJustified :: Int -> ACHParser Char -> ACHParser T.Text
parseRightJustified amt p = do
   spaces <- T.pack <$> many (char ' ')
   content <- countText (amt - T.length spaces) p
   return $ spaces `T.append` content

-- For fields that look like "Bank of America     ".
-- Text fields in an ACH file are always left-justified.
parseLeftJustified :: Int -> ACHParser Char -> ACHParser T.Text
parseLeftJustified amt p = do
   first <- alphaNumChar
   rest <- countText (amt - 1) p
   return $ first `T.cons` rest

parseLeftJustifiedNumber :: Int -> ACHParser T.Text
parseLeftJustifiedNumber amt = do
  digits <- T.pack <$> many digitChar
  rest <- countText (amt - T.length digits) (char ' ')
  return $ digits `T.append` rest

parseDay :: ACHParser Date
parseDay = do
  (yy:mm:dd:_) <- count 3 ((read :: String -> Int) <$> count 2 digitChar)
  let yy' = if yy < cutoffYear then 2000 + yy else 1900 + yy
  return $ Date (Year yy') (Month mm) (DayOfMonth dd)

parseTimeMaybe :: ACHParser (Maybe TimeOfDay)
parseTimeMaybe = (do _ <- try $ string "    "; return Nothing) <|> do
  (hh:mm:_) <- count 2 ((read :: String -> Int) <$> count 2 digitChar)
  if hh > 24 || mm > 60
  then fail "Can't parse HHMM time. Parsed digits too large." 
  else return $ Just (TimeOfDay hh mm 0)

parseFileIDModifier :: ACHParser Char
parseFileIDModifier = digitChar <|> upperChar

parseRecordSize :: ACHParser RecordSize
parseRecordSize = RecordSize <$ string "094"

parseBlockingFactor :: ACHParser BlockingFactor
parseBlockingFactor = BlockingFactor <$ string "10"

parseFormatCode :: ACHParser FormatCode
parseFormatCode = FormatCode <$ char '1'

parseDestination :: ACHParser T.Text
parseDestination = countText 23 alphaNumChar

parseCompanyName :: ACHParser T.Text
parseCompanyName = parseDestination

parseReferenceCode :: ACHParser T.Text
parseReferenceCode = countText 8 alphaNumChar

cutoffYear :: Int
cutoffYear = 68 -- There were no ACH transactions before 1968.

countText :: Int -> ACHParser Char -> ACHParser T.Text
countText n' p = go id n'
  where
    go f n =
      if n <= 0
      then return (f mempty) 
      else do
        x <- p
        go (f . (x `T.cons`)) (n - 1)
