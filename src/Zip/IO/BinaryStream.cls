 
/*------------------------------------------------------------------------
   File        : BinaryStream
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Wed Mar 19 17:15:20 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Utils.Binary FROM PROPATH.

CLASS Zip.IO.BinaryStream: 

    DEFINE PRIVATE PROPERTY BitPos AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY ByteBlock AS INTEGER NO-UNDO 
        GET.
        SET. 

    DEFINE PRIVATE PROPERTY BytePos AS INT64 NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY IOStream AS Progress.IO.InputStream NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC BinaryStream ( oIOStrem AS Progress.IO.InputStream ):
        ASSIGN 
            THIS-OBJECT:IOStream = oIOStrem
            THIS-OBJECT:BitPos   = 1.
    END CONSTRUCTOR.

    METHOD PUBLIC INTEGER getBitPosition(  ):
        
        RETURN THIS-OBJECT:BitPos.

    END METHOD.

    METHOD PUBLIC CHARACTER ReadBit(  ):        
        DEFINE VARIABLE res      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE mTmpData AS MEMPTR    NO-UNDO.

        /*        IF IOStream:Read(THIS-OBJECT:ByteBlock, 1, 1) = 0 THEN*/

        IF THIS-OBJECT:BitPos = 1 THEN
        DO:
            SET-SIZE (mTmpData) = 0.
            SET-SIZE (mTmpData) = 1.
            
            IF IOStream:Read(mTmpData, 1, 1) = 0 THEN 
            DO: 
                RETURN ''.
            END.
            THIS-OBJECT:ByteBlock = GET-BYTE (mTmpData,1).
            SET-SIZE (mTmpData) = 0.
        END.

        res = STRING(GET-BITS(THIS-OBJECT:ByteBlock, THIS-OBJECT:BitPos, 1) = 1, '1/0').
        
        THIS-OBJECT:BitPos = THIS-OBJECT:BitPos + 1.
        IF THIS-OBJECT:BitPos > 8 THEN
            THIS-OBJECT:BitPos = 1.

        RETURN res.

    END METHOD.

    METHOD PUBLIC CHARACTER ReadBitsL( iCount AS INTEGER ):
        DEFINE VARIABLE res AS CHARACTER NO-UNDO.
        DEFINE VARIABLE itr AS INTEGER     NO-UNDO.

        DO itr = 1 TO iCount:
            res = THIS-OBJECT:ReadBit() + res.
        END.

        RETURN res.

    END METHOD.

    METHOD PUBLIC INTEGER ReadBits( iCount AS INTEGER ):
        DEFINE VARIABLE iRes AS INTEGER   NO-UNDO.
        DEFINE VARIABLE itr  AS INTEGER   NO-UNDO.
        
        iRes = 0.
        
        DO itr = 1 TO iCount:
            PUT-BITS(iRes,itr,1) = INTEGER(THIS-OBJECT:ReadBit()).
        END.
        
        RETURN iRes.
    END METHOD.

    DESTRUCTOR PUBLIC BinaryStream ( ):
        
    END DESTRUCTOR.

END CLASS.