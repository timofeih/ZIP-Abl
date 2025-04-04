 
/*------------------------------------------------------------------------
   File        : OutputData
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 31 10:29:43 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.IO.FileOutputStream FROM PROPATH.
USING Zip.Decompressors.HistoryBuffer FROM PROPATH.
USING Zip.Exceptions.InvalidArgumentException FROM PROPATH.
USING Zip.Utils.Binary FROM PROPATH.
USING Zip.Utils.Crc32 FROM PROPATH.

CLASS Zip.IO.OutputData: 

    DEFINE PROTECTED PROPERTY oFileStream AS FileOutputStream NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY iCrc32 AS INT64 NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY iBufferSize AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY iIndex AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY mBuffer AS MEMPTR NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY oHistory AS HistoryBuffer NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY oFile AS Zip.Utils.File NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC OutputData ( cFileName AS CHARACTER):
        THIS-OBJECT(cFileName, 1024 * 32).
    END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC OutputData ( cFileName AS CHARACTER, iHistoryBufferSize AS INTEGER ):
        
        oFile = NEW Zip.Utils.File(cFileName).
        
        IF oFile:FileExists THEN
        DO:
            OS-DELETE VALUE (oFile:AbsoluteFileName()).
        END.

        IF NOT oFile:CreateFile() THEN
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('Cannot write to file "&1"', cFileName)).
        
        ASSIGN 
            THIS-OBJECT:iCrc32      = 0xFFFFFFFF
            THIS-OBJECT:iIndex      = 1
            THIS-OBJECT:iBufferSize = 1024 * 1
            THIS-OBJECT:oFileStream = NEW FileOutputStream(cFileName).
            
        SET-SIZE (THIS-OBJECT:mBuffer) = THIS-OBJECT:iBufferSize.
        
        IF iHistoryBufferSize <> 0 THEN 
            THIS-OBJECT:oHistory    = NEW HistoryBuffer(iHistoryBufferSize).
        
        IF THIS-OBJECT:oFileStream:Closed THEN 
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('Creation file ("&1") error!', THIS-OBJECT:oFile:AbsoluteFileName())). 
        
    END CONSTRUCTOR.

    METHOD PUBLIC INT64 close(  ):
        
        THIS-OBJECT:SaveBufferToFile().
        
        THIS-OBJECT:oFileStream:Flush().
        THIS-OBJECT:oFileStream:Close().
        
        RETURN BINARY:BinaryXOR(THIS-OBJECT:iCrc32, 0xffffffff).

    END METHOD.

    METHOD PROTECTED VOID SaveBufferToFile(  ):
    
        THIS-OBJECT:oFileStream:Write(THIS-OBJECT:mBuffer, 1, THIS-OBJECT:iIndex - 1).
    
        THIS-OBJECT:iIndex = 1.
        SET-SIZE (THIS-OBJECT:mBuffer) = 0.
        SET-SIZE (THIS-OBJECT:mBuffer) = THIS-OBJECT:iBufferSize.

        RETURN.
    
    END METHOD.

    METHOD PUBLIC VOID push( iSymbol AS INTEGER ):
        
        PUT-BYTE (THIS-OBJECT:mBuffer,THIS-OBJECT:iIndex) = iSymbol.
                        
        IF VALID-OBJECT (THIS-OBJECT:oHistory) THEN 
            THIS-OBJECT:oHistory:append(iSymbol).
        
        ASSIGN 
            THIS-OBJECT:iCrc32 = Crc32:ComputeByte(THIS-OBJECT:iCrc32, iSymbol)       
            THIS-OBJECT:iIndex = THIS-OBJECT:iIndex + 1.

        IF THIS-OBJECT:iIndex > THIS-OBJECT:iBufferSize THEN
        DO:
            THIS-OBJECT:SaveBufferToFile().
        END.
        
        RETURN.

    END METHOD.

    METHOD PUBLIC VOID copy( iLength AS INTEGER, iDistance AS INTEGER ):
        
        IF VALID-OBJECT (THIS-OBJECT:oHistory) THEN
            THIS-OBJECT:oHistory:copy(THIS-OBJECT, iLength, iDistance).
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC OutputData ( ):
        ASSIGN 
            THIS-OBJECT:oFileStream = ?
            THIS-OBJECT:oFile       = ?
            THIS-OBJECT:oHistory    = ?.
        
        SET-SIZE (THIS-OBJECT:mBuffer) = 0.
    END DESTRUCTOR.

END CLASS.