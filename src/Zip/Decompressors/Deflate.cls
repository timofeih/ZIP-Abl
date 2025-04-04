 
/*------------------------------------------------------------------------
   File        : Deflate
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 31 10:29:05 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Decompressors.HuffmanTree FROM PROPATH.
USING Zip.Decompressors.IDecompressor FROM PROPATH.
USING Zip.Exceptions.ZipException FROM PROPATH.
USING Zip.IO.BinaryStream FROM PROPATH.
USING Zip.IO.OutputData FROM PROPATH.
USING Zip.Utils.Binary FROM PROPATH.
USING Zip.Utils.IntegerIntegerMap FROM PROPATH.

CLASS Zip.Decompressors.Deflate IMPLEMENTS IDecompressor: 

    DEFINE PROTECTED PROPERTY oFixedDistTree AS HuffmanTree NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY oFixedLitTree AS HuffmanTree NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY inputBitStream AS BinaryStream NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY outData AS OutputData NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC Deflate ( inputBitStream AS BinaryStream, cFileName AS CHARACTER ):
        ASSIGN 
            THIS-OBJECT:inputBitStream = inputBitStream
            THIS-OBJECT:outData        = NEW OutputData(cFileName).
    END CONSTRUCTOR.

    METHOD PUBLIC VOID DecodeHuffmanCodes( OUTPUT oLitTree AS HuffmanTree, OUTPUT oDistTree AS HuffmanTree ):
        DEFINE VARIABLE iHLit            AS INTEGER           NO-UNDO.
        DEFINE VARIABLE iHDist           AS INTEGER           NO-UNDO.
        DEFINE VARIABLE iHCLen           AS INTEGER           NO-UNDO.
        DEFINE VARIABLE iOrder           AS INTEGER           EXTENT 19 NO-UNDO INITIAL [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15].
        DEFINE VARIABLE iCodes           AS INTEGER           EXTENT 19 NO-UNDO.
        DEFINE VARIABLE itr              AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oLengthCode      AS IntegerIntegerMap NO-UNDO.
        DEFINE VARIABLE oTree            AS HuffmanTree       NO-UNDO.
        DEFINE VARIABLE iData            AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oAlphabet        AS IntegerIntegerMap NO-UNDO.
        DEFINE VARIABLE iCount           AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oCodeLengthHLIT  AS IntegerIntegerMap NO-UNDO.
        DEFINE VARIABLE oCodeLengthHDIST AS IntegerIntegerMap NO-UNDO.
        
        ASSIGN 
            iHlit       = THIS-OBJECT:inputBitStream:ReadBits(5)
            iHDist      = THIS-OBJECT:inputBitStream:ReadBits(5)
            iHCLen      = THIS-OBJECT:inputBitStream:ReadBits(4)
            iCodes      = 0
            oLengthCode = NEW IntegerIntegerMap().
       
        DO itr  = 1 TO iHCLen + 4:
            iCodes[itr] = THIS-OBJECT:inputBitStream:ReadBits(3).
        END.  
        
        DO itr = 1 TO 19:
            oLengthCode:Add(iOrder[itr], iCodes[itr]).
        END.

        oTree = NEW HuffmanTree(oLengthCode).
        
        oLengthCode = ?.
        
        oAlphabet= NEW IntegerIntegerMap().
        itr = 0.
        
        DO WHILE itr < 258 + iHLit + iHDist:
            iData = oTree:DecodeNextSymbol(THIS-OBJECT:inputBitStream).
            
            IF iData > 15 THEN 
            DO:
                CASE iData:
                    WHEN 16 THEN 
                        ASSIGN 
                            iCount = THIS-OBJECT:inputBitStream:ReadBits(2) + 3
                            iData  = oAlphabet:Get(itr - 1).
                    WHEN 17 THEN 
                        ASSIGN 
                            iCount = THIS-OBJECT:inputBitStream:ReadBits(3) + 3
                            iData  = 0.
                    WHEN 18 THEN 
                        ASSIGN 
                            iCount = THIS-OBJECT:inputBitStream:ReadBits(7) + 11
                            iData  = 0.
                    OTHERWISE 
                    UNDO, THROW NEW ZipException(SUBSTITUTE('Symbol out of range')).
                END CASE.
                
                DO WHILE iCount > 0:
                    oAlphabet:Add(itr, iData).    
                    iCount = iCount - 1.
                    itr = itr + 1. 
                END.
            END.
            ELSE 
            DO:
                oAlphabet:Add(itr, iData).
                itr = itr + 1.
            END.
                
        END.
        
        oTree = ?.
        
        IF oAlphabet:Length() > 258 + iHLit + iHDist THEN 
        DO:
            UNDO, THROW NEW ZipException(SUBSTITUTE("Run exceeds number of codes")).
        END.
        
        oCodeLengthHLIT = NEW IntegerIntegerMap().
        
        DO itr = 0 TO iHLit + 256:
            oCodeLengthHLIT:Add(itr, oAlphabet:GetByPosValue(itr)).
        END.
        
        IF oCodeLengthHLIT:Get(256) = 0 THEN 
            UNDO, THROW NEW ZipException(SUBSTITUTE("End-of-block symbol has zero code length")).

        oLitTree = NEW HuffmanTree(oCodeLengthHLIT).
        oCodeLengthHLIT = ?.
            
        oCodeLengthHDIST = NEW IntegerIntegerMap().
                    
        DO itr = iHLit + 257 TO oAlphabet:Length():
            oCodeLengthHDIST:Add(itr - (iHLit + 257), oAlphabet:GetByPosValue(itr)).
        END.
        
        IF  oCodeLengthHDIST:Length() = 1 AND oCodeLengthHDIST:Get(0)= 0 THEN
            oDistTree = ?.
        ELSE           
            oDistTree = NEW HuffmanTree(oCodeLengthHDIST).

        oCodeLengthHDIST = ?.

        RETURN.

    END METHOD.

    METHOD PROTECTED INTEGER DecodeDistance( iDistanceCode AS INTEGER ):
        DEFINE VARIABLE numExtraBits AS INTEGER NO-UNDO.
        
        IF iDistanceCode < 0 OR iDistanceCode > 31 THEN 
            UNDO, THROW NEW ZipException(SUBSTITUTE("Invalid distance symbol")).

        IF iDistanceCode <= 3 THEN
            RETURN iDistanceCode + 1.
            
        IF iDistanceCode <= 29 THEN 
        DO:
            numExtraBits = INTEGER(TRUNCATE(iDistanceCode / 2,0)) - 1.
            
            RETURN 
                INTEGER(BINARY:ShiftLeft((iDistanceCode MODULO 2 + 2),numExtraBits)) + 1 + 
                THIS-OBJECT:inputBitStream:ReadBits(numExtraBits).
        END.

        UNDO, THROW NEW ZipException(SUBSTITUTE("Reserved distance symbol")).

    END METHOD.

    METHOD PROTECTED INTEGER DecodeRunLength( iSymbol AS INTEGER ):
        DEFINE VARIABLE numExtraBits AS INTEGER NO-UNDO.
       
        IF iSymbol < 265 THEN 
            RETURN iSymbol - 254.
        
        IF iSymbol < 285 THEN 
        DO:
            numExtraBits = INTEGER(TRUNCATE((iSymbol - 261 ) / 4,0)).           
            RETURN 
                INTEGER(BINARY:ShiftLeft((((iSymbol - 265) MODULO 4) + 4), numExtraBits)) + 3 + 
                THIS-OBJECT:inputBitStream:ReadBits(numExtraBits). 
        END.

        IF iSymbol = 285 THEN 
            RETURN 258.

        UNDO, THROW NEW ZipException(SUBSTITUTE("Reserved length symbol")).

    END METHOD.

    METHOD PROTECTED VOID DecompressHuffmanBlock( oLitTree AS HuffmanTree, oDistTree AS HuffmanTree ):
        DEFINE VARIABLE iSymbol       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iLength       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iDistance     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iDistanceCode AS INTEGER NO-UNDO.
        
        DO WHILE TRUE:
            iSymbol = oLitTree:DecodeNextSymbol(THIS-OBJECT:inputBitStream).
                        
            IF iSymbol = 256 THEN 
                LEAVE.
            
            IF iSymbol < 256 THEN
                THIS-OBJECT:outData:push(iSymbol).
            ELSE 
            DO:
                iLength = THIS-OBJECT:DecodeRunLength(iSymbol).
                
                IF iLength < 3 OR iLength > 258 THEN 
                    UNDO, THROW NEW ZipException(SUBSTITUTE("Invalid run length")).
                
                IF NOT VALID-OBJECT (oDistTree) THEN
                    UNDO, THROW NEW ZipException(SUBSTITUTE("Length symbol encountered with empty distance code")).
                    
                ASSIGN 
                    iDistanceCode = oDistTree:DecodeNextSymbol(THIS-OBJECT:inputBitStream)
                    iDistance     = THIS-OBJECT:DecodeDistance(iDistanceCode).
                
                IF iDistance < 1 OR iDistance > 32768 THEN 
                    UNDO, THROW NEW ZipException(SUBSTITUTE("Invalid distance")).
                
                THIS-OBJECT:outData:copy(iLength, iDistance).
            END.
        END.
        
        RETURN.

    END METHOD.

    METHOD PROTECTED IntegerIntegerMap makeFixedDistanceCode(  ):
        DEFINE VARIABLE itr                AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oFixedDistanceCode AS IntegerIntegerMap NO-UNDO.

        oFixedDistanceCode = NEW IntegerIntegerMap().

        DO itr = 0 TO 32:
            oFixedDistanceCode:Add(itr, 5).
        END.

        RETURN oFixedDistanceCode.

    END METHOD.

    METHOD PROTECTED IntegerIntegerMap makeFixedLiteralLengthCode(  ):
        DEFINE VARIABLE itr                     AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oFixedLiteralLengthCode AS IntegerIntegerMap NO-UNDO.
        
        oFixedLiteralLengthCode = NEW IntegerIntegerMap().

        DO itr = 0 TO 143:
            oFixedLiteralLengthCode:Add(itr, 8).
        END.

        DO itr = 144 TO 255:
            oFixedLiteralLengthCode:Add(itr, 9).
        END.
        
        DO itr = 256 TO 279:
            oFixedLiteralLengthCode:Add(itr, 7).
        END.
        
        DO itr = 280 TO 287:
            oFixedLiteralLengthCode:Add(itr, 8).
        END.

        RETURN oFixedLiteralLengthCode.

    END METHOD.

    METHOD PUBLIC VOID decompressUncompressedBlock(  ):
        DEFINE VARIABLE cLen    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cNLen   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iLen    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iNLen   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE itr     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iSymbol AS INTEGER   NO-UNDO.
        
        DO WHILE (THIS-OBJECT:inputBitStream:getBitPosition()<>1):
            THIS-OBJECT:inputBitStream:ReadBit().
        END.
        
        ASSIGN 
            cLen  = THIS-OBJECT:inputBitStream:ReadBitsL(16)
            iLen  = BINARY:BinToInt(cLen)
            cNLen = THIS-OBJECT:inputBitStream:ReadBitsL(16).
        
        IF BINARY:BinaryNOTL(cLen) <> cNLen THEN 
            UNDO, THROW NEW ZipException(SUBSTITUTE("Invalid length in uncompressed block")).
        
        DO itr = 1 TO iLen:
            iSymbol = THIS-OBJECT:inputBitStream:ReadBits(8).
            THIS-OBJECT:outData:push(iSymbol).
        END.
        
        RETURN.

    END METHOD.

    METHOD PUBLIC INT64 Decompress(  ):
        DEFINE VARIABLE cBFinal   AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE iBType    AS INTEGER     NO-UNDO.
        DEFINE VARIABLE oLitTree  AS HuffmanTree NO-UNDO.
        DEFINE VARIABLE oDistTree AS HuffmanTree NO-UNDO.

        DO WHILE TRUE:
            ASSIGN 
                cBFinal = THIS-OBJECT:inputBitStream:ReadBit()
                iBType  = THIS-OBJECT:inputBitStream:ReadBits(2).
            /*            MESSAGE cBFinal iBType VIEW-AS ALERT-BOX.*/
            
            CASE iBType:
                WHEN 0 THEN 
                    THIS-OBJECT:decompressUncompressedBlock().
                WHEN 1 THEN // fixed
                    DO:
                        IF NOT VALID-OBJECT (THIS-OBJECT:oFixedLitTree) OR NOT VALID-OBJECT (THIS-OBJECT:oFixedDistTree) THEN
                        DO:
                            ASSIGN 
                                THIS-OBJECT:oFixedLitTree  = NEW HuffmanTree(THIS-OBJECT:makeFixedLiteralLengthCode())
                                THIS-OBJECT:oFixedDistTree = NEW HuffmanTree(THIS-OBJECT:makeFixedDistanceCode()). 
                        END.
                       
                        THIS-OBJECT:DecompressHuffmanBlock(THIS-OBJECT:oFixedLitTree, THIS-OBJECT:oFixedDistTree).

                    END.
                WHEN 2 THEN // dynamic 
                    DO:
                        THIS-OBJECT:DecodeHuffmanCodes(OUTPUT oLitTree, OUTPUT oDistTree).
                        
                        THIS-OBJECT:DecompressHuffmanBlock(oLitTree, oDistTree).
                        ASSIGN 
                            oLitTree  = ?
                            oDistTree = ?.
                    END.
                WHEN 3 THEN 
                    UNDO, THROW NEW ZipException(SUBSTITUTE('Reserved block type')).
                OTHERWISE
                UNDO, THROW NEW ZipException(SUBSTITUTE('Unreachable value')). 
            END CASE.
            
            IF cBFinal = '1' THEN 
                LEAVE.
        END.
        
        RETURN THIS-OBJECT:outData:close().
    END METHOD.

    DESTRUCTOR PUBLIC Deflate ( ):
        ASSIGN 
            THIS-OBJECT:outData        = ?
            THIS-OBJECT:oFixedLitTree  = ?
            THIS-OBJECT:oFixedDistTree = ?. 
    END DESTRUCTOR.

END CLASS.