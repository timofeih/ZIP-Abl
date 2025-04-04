
/*------------------------------------------------------------------------
    File        : HuffmanTree
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hutorovski
    Created     : Thu Mar 20 00:17:34 EET 2025
    Notes       : 
  ----------------------------------------------------------------------*/

USING Zip.IO.BinaryStream FROM PROPATH.
USING Zip.Utils.Binary FROM PROPATH.
USING Zip.Utils.IntegerIntegerMap FROM PROPATH.

CLASS Zip.Decompressors.HuffmanTree:  
    DEFINE PRIVATE TEMP-TABLE ttTree NO-UNDO 
        FIELD binCode AS CHARACTER 
        FIELD symbol  AS INTEGER 
        INDEX binCode binCode
        INDEX symbol  symbol. 
    
    CONSTRUCTOR PUBLIC HuffmanTree ( oCodeLength AS IntegerIntegerMap ):
        DEFINE VARIABLE itr               AS INTEGER           NO-UNDO.
        DEFINE VARIABLE codeLengthMax     AS INTEGER           NO-UNDO.
        DEFINE VARIABLE blCount           AS INTEGER           NO-UNDO EXTENT.
        DEFINE VARIABLE nextCode          AS INTEGER           NO-UNDO EXTENT.
        DEFINE VARIABLE iCode             AS INTEGER           NO-UNDO.
        DEFINE VARIABLE oSortedCodeLength AS IntegerIntegerMap NO-UNDO.
        
        DEFINE VARIABLE iLength           AS INTEGER           NO-UNDO.
        DEFINE VARIABLE iSymbol           AS INTEGER           NO-UNDO.
        
        
        EMPTY TEMP-TABLE ttTree NO-ERROR.
        
        DO itr = 0 TO oCodeLength:Length():
            codeLengthMax = MAXIMUM (codeLengthMax, oCodeLength:Get(itr)).
        END.
        
        EXTENT(blCount) = codeLengthMax + 1.
        blCount = 0. 
        
        DO itr = 0 TO oCodeLength:Length():
            IF oCodeLength:Get(itr) > 0 THEN 
                blCount[oCodeLength:Get(itr) + 1] = blCount[oCodeLength:Get(itr) + 1] + 1.
        END.
        
        EXTENT (nextCode) = codeLengthMax.
        iCode= 0.
        
        DO itr = 1 TO EXTENT (blCount) - 1:
            iCode = (iCode + blCount[itr]) * 2.
            nextCode[itr] = iCode.
        END.
       
        oSortedCodeLength = oCodeLength:SortByKey().

        DO itr = 0 TO oSortedCodeLength:Length():
            ASSIGN 
                iLength = oSortedCodeLength:GetByPosValue(itr)
                iSymbol = oSortedCodeLength:GetByPosKey(itr).
            IF iLength > 0 THEN
            DO:
                CREATE ttTree.
                ASSIGN 
                    ttTree.binCode    = BINARY:str_pad(BINARY:IntToBin(nextCode[iLength]),iLength,'0')
                    ttTree.symbol     = iSymbol
                    nextCode[iLength] = nextCode[iLength] + 1.
                
            END.
        END.
        
        oSortedCodeLength = ?.

    END CONSTRUCTOR.

    METHOD PROTECTED INTEGER GetSymbol( cBinCode AS CHARACTER ):
        
        FIND ttTree WHERE ttTree.binCode = cBinCode NO-ERROR.

        IF NOT AVAILABLE ttTree THEN 
            RETURN ?.

        RETURN ttTree.symbol.

    END METHOD.

    METHOD PUBLIC INTEGER DecodeNextSymbol( BStream AS BinaryStream ):
        DEFINE VARIABLE cBinStr AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iSymbol AS INTEGER   NO-UNDO.
        DEFINE VARIABLE itr     AS INTEGER   NO-UNDO.

        cBinStr = ''.

        DO WHILE TRUE:
            ASSIGN 
                cBinStr = cBinStr + BStream:ReadBit() 
                iSymbol = THIS-OBJECT:GetSymbol(cBinStr).
            
            IF iSymbol <> ? THEN 
                LEAVE.
        END.       

        RETURN iSymbol.

    END METHOD.

END CLASS.