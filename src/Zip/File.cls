 
/*------------------------------------------------------------------------
   File        : File
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 17 10:59:02 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Zip.File INHERITS Zip.Utils.File: 

    DEFINE PROTECTED PROPERTY FileLength AS INT64 NO-UNDO 
        GET():
            ASSIGN 
                FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
            RETURN FILE-INFO:FILE-SIZE.
        END GET. 

    DEFINE PROTECTED PROPERTY mCache AS MEMPTR NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC File ( cFileName AS CHARACTER  ):
        SUPER(cFileName,?).
                  
        SET-SIZE (THIS-OBJECT:mCache) = 0.
    END CONSTRUCTOR.
    

/*    METHOD PUBLIC Progress.IO.FileInputStream CreateInputStream( iOffset AS INT64 ):    */
/*                                                                                     */
/*        DEFINE VARIABLE oFIOStream AS Progress.IO.FileInputStream NO-UNDO.           */
/*                                                                                     */
/*        oFIOStream = NEW Progress.IO.FileInputStream(THIS-OBJECT:AbsoluteFileName()).*/
/*                                                                                     */
/*        oFIOStream:SkipBytes(iOffset).                                               */
/*                                                                                     */
/*        RETURN oFIOStream.                                                           */
/*                                                                                     */
/*    END METHOD.                                                                      */

    METHOD PUBLIC MEMPTR GetBlock( iOffset AS INT64, iLength AS INT64 ):
        DEFINE VARIABLE mRes AS MEMPTR NO-UNDO.

        COPY-LOB FROM FILE THIS-OBJECT:AbsoluteFileName() STARTING AT iOffset FOR iLength TO mRes NO-CONVERT.

        RETURN mRes.
    END METHOD.

    METHOD PUBLIC MEMPTR GetEOCDBlock(  ):
        DEFINE VARIABLE iFrameShift  AS INT64 NO-UNDO.
        DEFINE VARIABLE iBlockStart  AS INT64 NO-UNDO.
        DEFINE VARIABLE iBlockLength AS INT64 NO-UNDO.
        
        IF THIS-OBJECT:FileLength < 65557 THEN 
        DO:
            ASSIGN 
                iFrameShift  = 0
                iBlockStart  = 1
                iBlockLength = THIS-OBJECT:FileLength.
        END.
        ELSE 
        DO:
            ASSIGN 
                iBlockStart  = THIS-OBJECT:FileLength - 65557
                iFrameShift  = iBlockStart - 1
                iBlockLength = 65557.
        END.

        RETURN THIS-OBJECT:GetBlock(iBlockStart,iBlockLength).

/*        IF GET-SIZE (THIS-OBJECT:mCache) > 0 THEN                                                                            */
/*            SET-SIZE (THIS-OBJECT:mCache) = 0.                                                                               */
/*                                                                                                                             */
/*        COPY-LOB FROM FILE THIS-OBJECT:ZipFileName STARTING AT iBlockStart FOR iBlockLength TO THIS-OBJECT:mCache NO-CONVERT.*/
/*                                                                                                                             */
/*        RETURN THIS-OBJECT:mCache.                                                                                           */

    END METHOD.

    DESTRUCTOR PUBLIC File ( ):
        SET-SIZE (mCache) = 0.
    END DESTRUCTOR.

END CLASS.