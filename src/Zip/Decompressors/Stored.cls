 
/*------------------------------------------------------------------------
   File        : Stored
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Wed Apr 02 11:00:57 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Decompressors.IDecompressor FROM PROPATH.
USING Zip.IO.BinaryStream FROM PROPATH.
USING Zip.IO.OutputData FROM PROPATH.

CLASS Zip.Decompressors.Stored IMPLEMENTS IDecompressor: 

    DEFINE PROTECTED PROPERTY iFileLength AS INT64 NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY inputBitStream AS BinaryStream NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY outData AS OutputData NO-UNDO 
        GET.
        SET.

    CONSTRUCTOR PUBLIC Stored ( inputBitStream AS BinaryStream, cFileName AS CHARACTER, iFileLength AS INT64  ):
        ASSIGN 
            THIS-OBJECT:inputBitStream = inputBitStream
            THIS-OBJECT:outData        = NEW OutputData(cFileName, 0)
            THIS-OBJECT:iFileLength    = iFileLength.
    END CONSTRUCTOR.

    METHOD PUBLIC INT64 Decompress(  ):
        DEFINE VARIABLE itr AS INT64 NO-UNDO.

        DO itr = 1 TO THIS-OBJECT:iFileLength:
            THIS-OBJECT:outData:push(THIS-OBJECT:inputBitStream:ReadBits(8)).
        END.
        
        RETURN THIS-OBJECT:outData:close().

    END METHOD.

END CLASS.