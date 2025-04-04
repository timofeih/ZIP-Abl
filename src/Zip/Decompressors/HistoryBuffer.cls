 
/*------------------------------------------------------------------------
   File        : HistoryBuffer
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 31 10:30:33 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Exceptions.InvalidArgumentException FROM PROPATH.
USING Zip.Exceptions.ZipException FROM PROPATH.
USING Zip.IO.OutputData FROM PROPATH.

CLASS Zip.Decompressors.HistoryBuffer: 

    DEFINE PROTECTED PROPERTY iCurrentIndex AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY iSize AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY mBuffer AS MEMPTR NO-UNDO 
        GET.
        SET.

    CONSTRUCTOR PUBLIC HistoryBuffer ( iSize AS INTEGER ):
        IF iSize < 1 THEN
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('Size must be positive')).
        
        ASSIGN 
            THIS-OBJECT:iSize         = iSize
            THIS-OBJECT:iCurrentIndex = 0.
        
        SET-SIZE (THIS-OBJECT:mBuffer) = iSize.
        
    END CONSTRUCTOR.

    METHOD PUBLIC VOID copy( outData AS OutputData, iLength AS INTEGER, iDistance AS INTEGER ):
        DEFINE VARIABLE iReadIndex AS INTEGER NO-UNDO.
        DEFINE VARIABLE itr        AS INTEGER NO-UNDO.
        DEFINE VARIABLE iSymbol    AS INTEGER NO-UNDO.
        
        IF iLength < 0 OR iDistance < 1 OR iDistance > THIS-OBJECT:iSize THEN 
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('Invalid length or distance')).
        
        iReadIndex = (THIS-OBJECT:iCurrentIndex + THIS-OBJECT:iSize - iDistance) MODULO THIS-OBJECT:iSize.
                
        DO itr = 1 TO iLength:
            ASSIGN 
                iSymbol    = GET-BYTE (THIS-OBJECT:mBuffer, iReadIndex + 1)
                iReadIndex = (iReadIndex + 1) MODULO THIS-OBJECT:iSize.
            outData:push(iSymbol).
        END.
        
        RETURN.

    END METHOD.

    METHOD PUBLIC VOID append( iSymbol AS INTEGER ):
        
        IF THIS-OBJECT:iCurrentIndex < 0 OR THIS-OBJECT:iCurrentIndex >= THIS-OBJECT:iSize THEN 
            UNDO, THROW NEW ZipException(SUBSTITUTE('Unreachable state')).
        
        PUT-BYTE(THIS-OBJECT:mBuffer,THIS-OBJECT:iCurrentIndex + 1) = iSymbol.
        
        THIS-OBJECT:iCurrentIndex = (THIS-OBJECT:iCurrentIndex + 1) MODULO THIS-OBJECT:iSize.
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC HistoryBuffer ( ):
        SET-SIZE (THIS-OBJECT:mBuffer) = 0.
    END DESTRUCTOR.

END CLASS.