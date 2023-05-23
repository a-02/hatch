{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Hatch.Types where

import Chronos
import Data.Text

data ACH = ACH
  { fileHeaderRecord :: FileHeaderRecord
  , batchRecords :: [BatchRecord]
  , fileControlRecord :: FileControlRecord
  }

data BatchRecord = BatchRecord
  { batchHeaderRecord :: BatchHeaderRecord
  , entryDetails :: [EntryDetail]
  , batchControlRecord :: BatchControlRecord
  }

newtype EntryDetail = EntryDetail
  { entryDetailRecord :: EntryDetailRecord
  }

data FileHeaderRecord = FileHeaderRecord
  { fileHeaderRecordTypeCode :: RecordTypeCode -- HAS to be "1".
  , priorityCode :: PriorityCode -- Currently, only "01" is used.
  , immediateDestination :: Text -- 10 characters, blank space first
  , immediateOrigin :: Text -- 10 digit company number
  , fileCreationDate :: Date -- YYMMDD
  , fileCreationTime :: Maybe TimeOfDay -- optional, HHMM
  , fileIDModifier :: Char -- first file of multiple should be labelled "A" or "0"
  , recordSize :: RecordSize -- always "094"
  , blockingFactor :: BlockingFactor -- records per block, always "10"
  , formatCode :: FormatCode -- always "1"
  , destination :: Text -- 23 chars 
  , originName :: Text -- 23 chars, company name
  , referenceCode :: Maybe Text -- 8 chars, can just be spaces
  }

data RecordTypeCode =
  CodeFileHeader   | -- "1"
  CodeBatchRecord  | -- "5"
  CodeEntryDetail  | -- "6"
  CodeAddenda      | -- "7"
  CodeBatchControl | -- "8"
  CodeFileControl    -- "9"

data PriorityCode = PriorityCode

data RecordSize = RecordSize

data BlockingFactor = BlockingFactor

data FormatCode = FormatCode

data BatchHeaderRecord = BatchHeaderRecord
  { batchHeaderRecordTypeCode :: RecordTypeCode
  , serviceClassCode :: ServiceClassCode
  , companyName :: Text -- 16 chars
  , companyDiscretionaryData :: Maybe Text -- 20 chars
  , companyIdentification :: Text -- 10 digits
  , standardEntryClassCode :: StandardEntryClassCode -- like PPD or CCD
  , companyEntryDescription :: Text -- 10 chars, normally something like "PAYROLL   "
  , companyDescriptiveDate :: Maybe Text
  , effectiveEntryDate :: Day -- YYMMDD
  , settlementDate :: Text
  , originatorStatusCode :: OriginatorStatusCode -- "1", says youre not the government
  , originatingDFIIdentification :: Text -- 8 digits
  , batchNumber :: Text -- 7 digits, in ascending order
  } 

data ServiceClassCode =
  Mixed200 |
  CreditsOnly220 |
  DebitsOnly225

data StandardEntryClassCode =
  ACK | -- ACH Payment Acknowledge
  ADV | -- Automated Accounting Advice
  ARC | -- Accounts Receivable Entry
  ATX | -- Financial ERI Acknowledgement
  BOC | -- Back Office Conversion Entry
  CCD | -- Corporate Credit or Debit Entry
  CIE | -- Customer Initiated Entry
  COR | -- Notification or Refused Notification of Change
  CTX | -- Coporate Trade Exchange
  DNE | -- Death Notification Entry (federal govt only) 
  ENR | -- Automated Entrollment Entry
  IAT | -- International ACH Transaction
  MTE | -- Machine Transfer Entry
  POP | -- Point of Purchase Entry
  POS | -- Point of Sale Entry
  PPD | -- Prearranged Payment and Deposit
  RCK | -- Re-presented Check Entry
  SHR | -- Shared Network Transactoin
  TEL | -- Telephone-Initiated Entry
  TRC | -- Truncated Entry
  TRX | -- Truncated Entry Exchange
  WEB | -- Internet/Mobile Initiated Entry
  XCK   -- Destroyed Check Entry

data OriginatorStatusCode = OriginatorStatusCode

data EntryDetailRecord =
  ARCRecord EntryDetailRecordARC |
  BOCRecord EntryDetailRecordBOC |
  CCDRecord (EntryDetailRecordCCD, Maybe AddendaRecordCCD) |
  CTXRecord (EntryDetailRecordCTX, [AddendaRecordCTX])     |
--  IATRecord EntryDetailRecordIAT |
  POPRecord EntryDetailRecordPOP |
  PPDRecord EntryDetailRecordPPD |
  RCKRecord EntryDetailRecordRCK |
  TELRecord EntryDetailRecordTEL |
  WEBRecord (EntryDetailRecordWEB, Maybe AddendaRecordWEB) |
  Unsupported

data EntryDetailRecordARC = EntryDetailRecordARC
  { arcRecordTypeCode :: RecordTypeCode -- "6" for all entry details
  , arcTransactionCode :: TransactionCodeSmall -- 2 digits, either 27 or 28
  , arcRDFIRoutingTransitNumber :: Text -- routing number of the receivers finanical institution
  , arcCheckDigit :: Char -- 9th char ot the routing number, technically
  , arcDFIAccountNumber :: Text -- 17 chars, left-justified & blank-filled
  , arcAmount :: Text -- 10 chars, first 8 are dollars, last 2 are cents. $845,678.90 would just be 0084567890
  , arcCheckSerialNumber -- 15 chars
  , arcIndividualName :: Maybe Text -- 22 chars, the receiver's name
  , arcDiscretionaryData :: Maybe Text -- 2 chars, defined by whichever bank youre using
  , arcAddendaRecordIndicator :: Bool -- "0" for no addenda, "1" for one addenda
  , arcTraceNumber :: Text -- 15 characters. weirdly constructed
  }

data EntryDetailRecordBOC = EntryDetailRecordBOC
  { bocRecordTypeCode :: RecordTypeCode
  , bocTransactionCode :: TransactionCodeSmall
  , bocRDFIRoutingTransitNumber :: Text -- routing number of the receivers finanical institution
  , bocCheckDigit :: Char -- 9th char ot the routing number, technically
  , bocDFIAccountNumber :: Text -- 17 chars, left-justified & blank-filled
  , bocAmount :: Text -- 10 chars, first 8 are dollars, last 2 are cents. $845,678.90 would just be 0084567890
  , bocCheckSerialNumber -- 15 chars
  , bocIndividualName :: Maybe Text -- 22 chars, the receiver's name
  , bocDiscretionaryData :: Maybe Text -- 2 chars, defined by whichever bank youre using
  , bocAddendaRecordIndicator :: Bool -- "0" for no addenda, "1" for one addenda
  , bocTraceNumber :: Text -- 15 characters. weirdly constructed
  }

data EntryDetailRecordCCD = EntryDetailRecordCCD
  { ccdRecordTypeCode :: RecordTypeCode
  , ccdTransactionCode :: TransactionCodeFull
  , ccdRDFIRoutingTransitNumber :: Text -- routing number of the receivers finanical institution
  , ccdCheckDigit :: Char -- 9th char ot the routing number, technically
  , ccdDFIAccountNumber :: Text -- 17 chars, left-justified & blank-filled
  , ccdAmount :: Text -- 10 chars, first 8 are dollars, last 2 are cents. $845,678.90 would just be 0084567890
  , ccdIdentificationNumber :: Maybe Text-- 15 chars
  , ccdReceivingCompanyName :: Text -- 22 chars, the receiver's name
  , ccdDiscretionaryData :: DiscretionaryData -- 2 chars, defined by whichever bank youre using
  , ccdAddendaRecordIndicator :: Bool -- "0" for no addenda, "1" for one addenda
  , ccdTraceNumber :: Text -- 15 characters. weirdly constructed
  }

data EntryDetailRecordCTX = EntryDetailRecordCTX
  { ctxRecordTypeCode :: RecordTypeCode
  , ctxTransactionCode :: TransactionCodeFull
  , ctxRDFIRoutingTransitNumber :: Text -- routing number of the receivers finanical institution
  , ctxCheckDigit :: Char -- 9th char ot the routing number, technically
  , ctxDFIAccountNumber :: Text -- 17 chars, left-justified & blank-filled
  , ctxAmount :: Text -- 10 chars, first 8 are dollars, last 2 are cents. $845,678.90 would just be 0084567890
  , ctxIdentificationNumber :: Maybe Text-- 15 chars
  , ctxNumberOfAddendaRecords :: Text-- 4 chars
  , ctxReceivingCompanyName :: Text -- 16 chars, the receiver's name
  , ctxReserved :: Reserved -- 2 chars, reserved. leave it blank
  , ctxDiscretionaryData :: Maybe Text -- 2 chars, defined by whichever bank youre using
  , ctxAddendaRecordIndicator :: Bool -- "1" for one addenda
  , ctxTraceNumber :: Text -- 15 characters. weirdly constructed
  }

data EntryDetailRecordIAT = EntryDetailRecordIAT
  { iatRecordTypeCode :: RecordTypeCode
  , iatTransactionCode :: TransactionCodeFull
  , iatGatewayOperatorIdentification :: Text
  , iatCheckDigit :: Char
  , iatNumberOfAddendaRecords :: Text
  , iatFieldSixReserved :: Reserved -- 13 chars, leave blank
  , iatAmount :: Text -- 10chars
  , iatForeignReceiversAccountNumber :: Text -- 35 chars
  , iatFieldNineReserved :: Reserved -- 2 chars
  , iatGatewayOperatorOFACScreeningIndicator :: Maybe Reserved -- 1 char
  , iatSecondaryOFACScreeningIndicator :: Maybe Reserved -- 1 char
  , iatAddendaRecordIndicator :: Text -- "1", addenda follows.
  }

data EntryDetailRecordPOP = EntryDetailRecordPOP
  { popRecordTypeCode :: RecordTypeCode
  , popTransactionCode :: TransactionCodeSmall
  , popRDFIRoutingTransitNumber :: Text
  , popCheckDigit :: Char
  , popAmount :: Text
  , popCheckSerialNumber :: Text -- 9 chars 
  , popTerminalCity :: Text -- 22 chars
  , popTerminalState :: Text -- 2 chars
  , popIndividualName :: Maybe Text -- 22 chars
  , popDiscretionaryData :: Maybe Text -- 2 chars
  , popAddendaRecordIndicator :: Maybe Bool -- 0 or 1
  , popTraceNumber :: Text -- 15 chars
  }

data EntryDetailRecordPPD = EntryDetailRecordPPD
  { ppdEntryDetailRecord :: RecordTypeCode
  , ppdTransactionCode :: TransactionCodeFull
  , ppdRDFIRoutingTransitNumber :: Text
  , ppdCheckDigit :: Char
  , ppdAmount :: Text
  , ppdIndividualIdentificationNumber :: Maybe Text
  , ppdIndividualName :: Text
  , ppdDiscretionaryDataField :: Text
  , ppdAddendaRecordIndicator :: Bool
  , ppdTraceNumber :: Text
  }

data EntryDetailRecordRCK = EntryDetailRecordRCK
  { rckRecordTypeCode :: RecordTypeCode
  , rckTransactionCode :: TransactionCodeSmall
  , rckRDFIRoutingTransitNumber :: Text
  , rckCheckDigit :: Char
  , rckDFIAccountNumber :: Text
  , rckAmount :: Text
  , rckCheckSerialNumber :: Text -- 9 chars 
  , rckIndividualName :: Text -- 22 chars
  , rckDiscretionaryData :: Maybe Text -- 2 chars
  , rckAddendaRecordIndicator :: Bool -- 0 or 1
  , rckTraceNumber :: Text -- 15 chars
  }

data EntryDetailRecordTEL = EntryDetailRecordTEL
  { telRecordTypeCode :: RecordTypeCode
  , telTransactionCode :: TransactionCodeSmall
  , telRDFIRoutingTransitNumber :: Text
  , telCheckDigit :: Char
  , telDFIAccountNumber :: Text
  , telAmount :: Text
  , telIndividualIdentificationNumber :: Text -- 9 chars 
  , telIndividualName :: Text -- 22 chars
  , telPaymentTypeCode :: PaymentTypeCode -- 2 chars
  , telAddendaRecordIndicator :: Bool -- 0 or 1
  , telTraceNumber :: Text -- 15 chars
  }

data EntryDetailRecordWEB = EntryDetailRecordWEB
  { webRecordTypeCode :: RecordTypeCode
  , webTransactionCode :: TransactionCodeSmall
  , webRDFIRoutingTransitNumber :: Text
  , webCheckDigit :: Char
  , webDFIAccountNumber :: Text
  , webAmount :: Text
  , webIndividualIdentificationNumber :: Text -- 9 chars 
  , webIndividualName :: Text -- 22 chars
  , webPaymentTypeCode :: PaymentTypeCode -- 2 chars
  , webAddendaRecordIndicator :: Bool -- 0 or 1
  , webTraceNumber :: Text -- 15 chars
  }

data PaymentTypeCode = Recurring | SinglePayment

data DiscretionaryData = ElectronicOnly | PreauthorizedCheckOnly

data Reserved = Reserved

data TransactionCodeSmall = AutomatedPayment | Prenote

data TransactionCodeFull =
  AutomatedDepositChecking | -- 22
  PrenoteCheckingCredit    | -- 23
  ZeroDollarCheckingCredit | -- 24
  AutomatedPaymentChecking | -- 27
  PrenoteCheckingDebit     | -- 28
  ZeroDollarCheckingDebit  | -- 29
  AutomatedDepositSavings  | -- 32
  PrenoteSavingsCredit     | -- 33
  ZeroDollarSavingsCredit  | -- 34
  AutomatedPaymentSavings  | -- 37
  PrenoteSavingsDebit      | -- 38
  ZeroDollarSavingsDebit     -- 39

data BatchControlRecord = BatchControlRecord
  { batchControlRecordTypeCode :: RecordTypeCode
  , batchControlServiceClassCode :: ServiceClassCode
  , entryCount :: Text
  , entryHash :: Text
  , totalDebitInBatch :: Text
  , totalCreditInBatch :: Text
  , batchControlCompanyIdentification :: Text
  , messageAuthenticationCode :: Maybe Text
  , batchControlReserved :: Reserved
  , batchControlOriginatingDFIIdentification :: Text
  , batchControlBatchNumber :: Text -- must match field 13 of BatchHeaderRecord
  }

data FileControlRecord = FileControlRecord
  { fileControlRecordTypeCode :: RecordTypeCode
  , fileControlBatchCount :: Text
  , fileControlBlockCount :: Text
  , fileControlEntryCount :: Text
  , fileControlEntryHash :: Text
  , totalDebitInFile :: Text
  , totalCreditInFile :: Text
  , fileControlReserved :: Reserved
  }

-- trace numbers must be ascending

data Addenda = 
  CCDAddenda AddendaRecordCCD |
  CTXAddenda AddendaRecordCTX |
  WEBAddenda AddendaRecordWEB

data AddendaRecordCCD = AddendaRecordCCD
  { addendaCCDRecordTypeCode :: RecordTypeCode
  , addendaCCDAddendaTypeCode :: AddendaTypeCode
  , addendaCCDPaymentRelatedInformation :: Maybe Text
  , addendaCCDAddendaSequenceNumber :: Text
  , addendaCCDEntryDetailSequenceNumber :: Text
  }

data AddendaRecordCTX = AddendaRecordCTX
  { addendaCTXRecordTypeCode :: RecordTypeCode
  , addendaCTXAddendaTypeCode :: AddendaTypeCode
  , addendaCTXPaymentRelatedInformation :: Maybe Text
  , addendaCTXAddendaSequenceNumber :: Text
  , addendaCTXEntryDetailSequenceNumber :: Text
  }

data AddendaRecordWEB = AddendaRecordWEB
  { addendaWEBRecordTypeCode :: RecordTypeCode
  , addendaWEBAddendaTypeCode :: AddendaTypeCode
  , addendaWEBPaymentRelatedInformation :: Maybe Text
  , addendaWEBAddendaSequenceNumber :: Text
  , addendaWEBEntryDetailSequenceNumber :: Text
  }

data AddendaTypeCode = AddendaTypeCode
