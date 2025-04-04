
/*------------------------------------------------------------------------
    File        : test_zip_reader.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hutorovski
    Created     : Fri Mar 14 09:49:46 EET 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.File FROM PROPATH.
USING Zip.Structures.CentralDirectory FROM PROPATH.
USING Zip.Structures.EOCD FROM PROPATH.
USING Zip.Structures.LocalFileHeader FROM PROPATH.
USING Zip.Structures.Parser FROM PROPATH.
USING Zip.Zip FROM PROPATH.

&SCOPED-DEFINE READ_HEADER_BLOCK_SIZE 4
&SCOPED-DEFINE LOCAL_FILE_HEADER 0x04034b50

DEFINE VARIABLE cZipFileFullName       AS CHARACTER                       INITIAL 'test.zip' NO-UNDO.

DEFINE VARIABLE oFile                  AS File                            NO-UNDO.
DEFINE VARIABLE oEOCD                  AS EOCD                            NO-UNDO.
/*DEFINE VARIABLE oCentralDir      AS CentralDirectory NO-UNDO.*/

DEFINE VARIABLE oCentralDirArray       AS OpenEdge.Core.Collections.Array NO-UNDO.
DEFINE VARIABLE oLocalFileHeadersArray AS OpenEdge.Core.Collections.Array NO-UNDO.

/* ************************  Function Prototypes ********************** */

FUNCTION GetDate RETURNS DATE (INPUT iDate AS INTEGER) FORWARD.

FUNCTION GetFlags RETURNS CHARACTER  (INPUT iBytes AS INTEGER) FORWARD.

FUNCTION GetInt64 RETURNS INT64 (INPUT mData AS MEMPTR) FORWARD.

FUNCTION GetInteger RETURNS INTEGER (INPUT mData AS MEMPTR) FORWARD.

FUNCTION GetTime RETURNS INTEGER (INPUT iData AS INTEGER) FORWARD.

FUNCTION intToHex RETURNS CHARACTER (i_iint AS INT64) FORWARD.


/* **********************  Internal Procedures  *********************** */


PROCEDURE Read_EOCD:
    DEFINE INPUT PARAMETER oFile AS File NO-UNDO.
    DEFINE OUTPUT PARAMETER oEOCD AS EOCD NO-UNDO.
    DEFINE OUTPUT PARAMETER oCentralDirArray       AS OpenEdge.Core.Collections.Array NO-UNDO.
    DEFINE OUTPUT PARAMETER oLocalFileHeadersArray AS OpenEdge.Core.Collections.Array NO-UNDO.
    
    DEFINE VARIABLE oCentralDir      AS CentralDirectory NO-UNDO.
    DEFINE VARIABLE oLocalFileHeader AS LocalFileHeader  NO-UNDO.
    
    &SCOPED-DEFINE EOCD 0x06054b50
    
    DEFINE VARIABLE oParser          AS Parser           NO-UNDO.
    DEFINE VARIABLE oParser2         AS Parser           NO-UNDO.
    
    DEFINE VARIABLE iFileLength      AS INT64            NO-UNDO.
    DEFINE VARIABLE mByteData        AS MEMPTR           NO-UNDO.
    DEFINE VARIABLE mTmpData         AS MEMPTR           NO-UNDO.
    DEFINE VARIABLE mTmpData2        AS MEMPTR           NO-UNDO.
    DEFINE VARIABLE mTmpData3        AS MEMPTR           NO-UNDO.
    DEFINE VARIABLE iPos             AS INTEGER          NO-UNDO.
    
    DEFINE VARIABLE iFrameShift      AS INT64            NO-UNDO.
    DEFINE VARIABLE iBlockStart      AS INT64            NO-UNDO.
    DEFINE VARIABLE iBlockLength     AS INT64            NO-UNDO.
    DEFINE VARIABLE iEOCDPos         AS INT64            NO-UNDO.
    
    DEFINE VARIABLE iter             AS INTEGER          NO-UNDO.
    
          
    /*    FILE-INFO:FILE-NAME = cZipFileFullName.                                                              */
    /*    ASSIGN                                                                                               */
    /*        iFileLength = FILE-INFO:FILE-SIZE                                                                */
    /*        iEOCDPos    = 0.                                                                                 */
    /*                                                                                                         */
    /*    IF iFileLength < 65557 THEN                                                                          */
    /*    DO:                                                                                                  */
    /*        ASSIGN                                                                                           */
    /*            iFrameShift  = 0                                                                             */
    /*            iBlockStart  = 1                                                                             */
    /*            iBlockLength = iFileLength.                                                                  */
    /*    END.                                                                                                 */
    /*    ELSE                                                                                                 */
    /*    DO:                                                                                                  */
    /*        ASSIGN                                                                                           */
    /*            iBlockStart  = iFileLength - 65557                                                           */
    /*            iFrameShift  = iBlockStart - 1                                                               */
    /*            iBlockLength = 65557.                                                                        */
    /*    END.                                                                                                 */
    /*                                                                                                         */
    /*    SET-SIZE (mByteData) = 0.                                                                            */
    /*    COPY-LOB FROM FILE cZipFileFullName STARTING AT iBlockStart FOR iBlockLength TO mByteData NO-CONVERT.*/

    mByteData = oFile:GetEOCDBlock().

    DO iPos = (GET-SIZE(mByteData) - 18) TO 1 BY -1:    
        IF GET-BYTE (mByteData, iPos) + GET-BYTE (mByteData, iPos + 1) * 256 + GET-BYTE (mByteData, iPos + 2) * 65536 + GET-BYTE (mByteData, iPos + 3) * 16777216 = {&EOCD } THEN
        DO:
            iEOCDPos = iPos.
            LEAVE.
        END.
    END.

    IF iEOCDPos = 0 THEN 
    DO:
        MESSAGE 'NOT ZIP FILE' VIEW-AS ALERT-BOX TITLE 'ERROR '.
        RETURN.
    END.

    oParser = NEW Parser(GET-POINTER-VALUE (mByteData), GET-SIZE (mByteData), iEOCDPos, 0).

    oEOCD = NEW EOCD(oParser).
    
    oParser =?.
    
    /*    IF iFrameShift > 0 THEN*/
    /*    DO:                    */
    /*        SET-SIZE (mByteData) = 0.                                                                                                     */
    /*        COPY-LOB FROM FILE cZipFileFullName STARTING AT oEOCD:OffsetToStartOfCD + 1 FOR oEOCD:SizeOfCDInBytes TO mByteData NO-CONVERT.*/
    mTmpData2 = oFile:GetBlock(oEOCD:OffsetToStartOfCD + 1, oEOCD:SizeOfCDInBytes).

    ASSIGN 
        oParser = NEW Parser(GET-POINTER-VALUE (mTmpData2), GET-SIZE (mTmpData2), 1, oEOCD:OffsetToStartOfCD + 1).
    /*    END.*/
    /*    ELSE                                                                                                           */
    /*    DO:                                                                                                            */
    /*        ASSIGN                                                                                                     */
    /*            oParser = NEW Parser(GET-POINTER-VALUE (mByteData), GET-SIZE (mByteData), oEOCD:OffsetToStartOfCD + 1).*/
    /*    END.                                                                                                           */
    
    ASSIGN
        oCentralDirArray                  = NEW OpenEdge.Core.Collections.Array()
        oCentralDirArray:AutoExpand       = TRUE
        oLocalFileHeadersArray            = NEW OpenEdge.Core.Collections.Array()
        oLocalFileHeadersArray:AutoExpand = TRUE.
   
    DO iter = 1 TO oEOCD:NumbersOfCDRecordsOnThisDisk:
        oCentralDir = NEW CentralDirectory(oParser).
    
        oCentralDirArray:Add(oCentralDir).
    
        /*        MESSAGE                                                                                            */
        /*            iter '/' oEOCD:NumbersOfCDRecordsOnThisDisk SKIP                                               */
        /*            'Version made by: ' oCentralDir:VersionMadeBy SKIP                                             */
        /*            'Minimum version needed to extract: ' oCentralDir:MinimumVersion SKIP                          */
        /*            'Bit flag: ' oCentralDir:BitFlag SKIP                                                          */
        /*            'Compression method: ' oCentralDir:CompressionMethod SKIP                                      */
        /*            'File last modification time: ' STRING(oCentralDir:FileLastModificationTime, 'HH:MM:SS') SKIP  */
        /*            'File last modification date: ' STRING(oCentralDir:FileLastModificationDate, '99/99/9999') SKIP*/
        /*            'CRC-32 of uncompressed data: ' oCentralDir:CRC32UncompressedData SKIP                         */
        /*            'File name: ' oCentralDir:FileName SKIP                                                        */
        /*            'File comment: ' oCentralDir:FileComment SKIP                                                  */
        /*            'Offset of local file header (from start of disk): ' oCentralDir:OffsetOfLocalFileHeader SKIP  */
        /*            VIEW-AS ALERT-BOX TITLE ' CentralDirectory '.                                                  */
       
        /*        IF iFrameShift > 0 THEN*/
        /*        DO:                    */
        SET-SIZE (mTmpData) = 0.
        /*            COPY-LOB FROM FILE cZipFileFullName STARTING AT oCentralDir:OffsetOfLocalFileHeader + 1 FOR 26 + 20 TO mTmpData NO-CONVERT.*/
        mTmpData = oFile:GetBlock(oCentralDir:OffsetOfLocalFileHeader + 1, 26 + 20).
        oParser2 = NEW Parser(GET-POINTER-VALUE (mTmpData), GET-SIZE (mTmpData), 1,oCentralDir:OffsetOfLocalFileHeader + 1). 
        /*        END.                                                                                                                    */
        /*        ELSE                                                                                                                    */
        /*        DO:                                                                                                                     */
        /*            oParser2 = NEW Parser(GET-POINTER-VALUE (mByteData), GET-SIZE (mByteData), oCentralDir:OffsetOfLocalFileHeader + 1).*/
        /*        END.                                                                                                                    */
        
        oLocalFileHeader = NEW LocalFileHeader(oParser2).
        
    
        oLocalFileHeadersArray:Add(oLocalFileHeader).    
    
        oParser2 = ?.
        
    /*        IF oLocalFileHeader:CompressedSize > 0 THEN                                                           */
    /*        DO:                                                                                                   */
    /*            mTmpData3 = oFile:GetBlock(oLocalFileHeader:FileDataStartOffset, oLocalFileHeader:CompressedSize).*/
    /*                                                                                                              */
    /*            COPY-LOB mTmpData3 TO FILE oLocalFileHeader:FileName NO-CONVERT.                                  */
    /*        END.                                                                                                  */
    /*                                                                                                              */
    /*        MESSAGE                                                                                               */
    /*            iter '/' oEOCD:NumbersOfCDRecordsOnThisDisk SKIP                                                  */
    /*            'File name: ' oLocalFileHeader:FileName SKIP                                                      */
    /*            'Version made by: ' oCentralDir:VersionMadeBy SKIP                                                */
    /*            'Minimum version needed to extract: ' oCentralDir:MinimumVersion SKIP                             */
    /*            'Bit flag: ' oCentralDir:BitFlag SKIP                                                             */
    /*            'Compression method: ' oCentralDir:CompressionMethod SKIP                                         */
    /*            'File comment: ' oCentralDir:FileComment SKIP                                                     */
    /*            'Compressed size: ' oLocalFileHeader:CompressedSize SKIP                                          */
    /*            'Uncompressed size: ' oLocalFileHeader:UncompressedSize SKIP                                      */
    /*            'Offset of local file header (from start of disk): ' oCentralDir:OffsetOfLocalFileHeader SKIP     */
    /*            'File data start offset: ' oLocalFileHeader:FileDataStartOffset                                   */
    /*            VIEW-AS ALERT-BOX TITLE ' LocalFileHeader '.                                                      */
    END.
    oParser =?.

END PROCEDURE. 

PROCEDURE Read_Local_file_header:
    DEFINE INPUT PARAMETER piStartBlockPos AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE mByteData                       AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE iCurrentPos                     AS INTEGER   NO-UNDO.
    
    
    DEFINE VARIABLE iVersion                        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFlags                          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCompressionMethod              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFileModificationTime           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFileModificationDate           AS DATE      NO-UNDO.
    DEFINE VARIABLE iCrc32Checksum                  AS INT64     NO-UNDO.
    DEFINE VARIABLE iCompressedSize                 AS INT64     NO-UNDO.
    DEFINE VARIABLE iUncompressedSize               AS INT64     NO-UNDO.
    DEFINE VARIABLE iFileNameLength                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtraFieldLength               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFileName                       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcExtraField                    AS LONGCHAR  NO-UNDO.
    
    DEFINE VARIABLE iDataDescriptorCrc32Checksum    AS INT64     NO-UNDO.
    DEFINE VARIABLE iDataDescriptorCompressedSize   AS INT64     NO-UNDO.
    DEFINE VARIABLE iDataDescriptorUncompressedSize AS INT64     NO-UNDO.

    iCurrentPos = piStartBlockPos.

    COPY-LOB FROM FILE cZipFileFullName STARTING AT iCurrentPos FOR 30 TO mByteData NO-CONVERT.

    /*    MESSAGE GET-BYTE (mByteData, 7) GET-BYTE (mByteData, 8) VIEW-AS ALERT-BOX.*/

    ASSIGN 
        iVersion              = GET-BYTE (mByteData, 5) + GET-BYTE (mByteData, 6) * 256
        cFlags                = GetFlags(GET-BYTE (mByteData, 7) + GET-BYTE (mByteData, 8) * 256)
        iCompressionMethod    = GET-BYTE (mByteData, 9) + GET-BYTE (mByteData, 10) * 256
        iFileModificationTime = GetTime(GET-BYTE (mByteData, 11) + GET-BYTE (mByteData, 12) * 256)
        iFileModificationDate = GetDate(GET-BYTE (mByteData, 13) + GET-BYTE (mByteData, 14) * 256)
        iCrc32Checksum        = GET-BYTE (mByteData, 15) + GET-BYTE (mByteData, 16) * 256 + GET-BYTE (mByteData, 17) * 65536 + GET-BYTE (mByteData, 18) * 16777216
        iCompressedSize       = GET-BYTE (mByteData, 19) + GET-BYTE (mByteData, 20) * 256 + GET-BYTE (mByteData, 21) * 65536 + GET-BYTE (mByteData, 22) * 16777216
        iUncompressedSize     = GET-BYTE (mByteData, 23) + GET-BYTE (mByteData, 24) * 256 + GET-BYTE (mByteData, 25) * 65536 + GET-BYTE (mByteData, 26) * 16777216
        iFileNameLength       = GET-BYTE (mByteData, 27) + GET-BYTE (mByteData, 28) * 256
        iExtraFieldLength     = GET-BYTE (mByteData, 29) + GET-BYTE (mByteData, 30) * 256
        .
    
    iCurrentPos = iCurrentPos + 30.
    
    COPY-LOB FROM FILE cZipFileFullName STARTING AT iCurrentPos FOR iFileNameLength TO mByteData NO-CONVERT.    
    
    ASSIGN 
        cFileName = GET-STRING(mByteData,1) .
    
    iCurrentPos = iCurrentPos + iFileNameLength.
    
    IF iExtraFieldLength > 0 THEN
    DO:
        COPY-LOB FROM FILE cZipFileFullName STARTING AT iCurrentPos FOR iExtraFieldLength TO mByteData NO-CONVERT.    
    
        ASSIGN 
            lcExtraField = HEX-ENCODE(mByteData) 
            iCurrentPos  = iCurrentPos + iExtraFieldLength.
    END.
    
    IF LOOKUP ('03',cFlags) > 0 THEN 
    DO:
        COPY-LOB FROM FILE cZipFileFullName STARTING AT iCurrentPos FOR 12 TO mByteData NO-CONVERT.  
        ASSIGN 
            iDataDescriptorCrc32Checksum    = GET-BYTE (mByteData, 1) + GET-BYTE (mByteData, 2) * 256 + GET-BYTE (mByteData, 3) * 65536 + GET-BYTE (mByteData, 4) * 16777216
            iDataDescriptorCompressedSize   = GET-BYTE (mByteData, 5) + GET-BYTE (mByteData, 6) * 256 + GET-BYTE (mByteData, 7) * 65536 + GET-BYTE (mByteData, 8) * 16777216
            iDataDescriptorUncompressedSize = GET-BYTE (mByteData, 9) + GET-BYTE (mByteData, 10) * 256 + GET-BYTE (mByteData, 11) * 65536 + GET-BYTE (mByteData, 12) * 16777216
            iCurrentPos                     = iCurrentPos + 12.
    END.
    
    
    
    MESSAGE 
        'Version: ' iVersion SKIP 
        'Flags: ' cFlags SKIP
        'Compression method: ' iCompressionMethod SKIP 
        'File modification time: ' STRING (iFileModificationTime, 'HH:MM:SS') SKIP
        'File modification date: '  STRING (iFileModificationDate, '99/99/9999') SKIP 
        'Crc-32 checksum: '  intToHex(iCrc32Checksum) SKIP 
        'Compressed size: ' iCompressedSize SKIP 
        'Uncompressed size: ' iUncompressedSize SKIP 
        'File name length: ' iFileNameLength SKIP 
        'Extra field length: ' iExtraFieldLength SKIP 
        'File name: ' cFileName SKIP 
        'Extra field: ' STRING(lcExtraField) SKIP 
        'Data descriptor' SKIP 
        '     CRC-32: ' iDataDescriptorCrc32Checksum SKIP
        '     compressed size: ' iDataDescriptorCompressedSize SKIP
        '     uncompressed size: ' iDataDescriptorUncompressedSize SKIP 
        VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE ZipFileList:
    DEFINE INPUT PARAMETER oEOCD AS EOCD NO-UNDO.
    DEFINE INPUT PARAMETER oCentralDirArray       AS OpenEdge.Core.Collections.Array NO-UNDO.
    DEFINE INPUT PARAMETER oLocalFileHeadersArray AS OpenEdge.Core.Collections.Array NO-UNDO.

    DEFINE VARIABLE itr AS INTEGER NO-UNDO.

    REPEAT itr = 1 TO oCentralDirArray:Size:
        DISPLAY 
            itr LABEL 'Nr.'
            CAST(oCentralDirArray:GetValue(itr),CentralDirectory):FileName LABEL 'File Name' FORMAT 'x(20)'
            CAST(oLocalFileHeadersArray:GetValue(itr),LocalFileHeader):UncompressedSize LABEL 'Size, B'
            CAST(oLocalFileHeadersArray:GetValue(itr),LocalFileHeader):CompressedSize LABEL 'C.Size, B' 
            CAST(oLocalFileHeadersArray:GetValue(itr),LocalFileHeader):FileLastModificationDate LABEL 'File date' FORMAT '99/99/9999'
            STRING(CAST(oLocalFileHeadersArray:GetValue(itr),LocalFileHeader):FileLastModificationTime,'HH:MM:SS') LABEL 'File time' FORMAT 'x(8)'
            WITH WIDTH 100 FRAME a.
    
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION GetDate RETURNS DATE ( INPUT iDate AS INTEGER ):
    RETURN DATE(GET-BITS(iDate,6,4),GET-BITS(iDate,1,5),GET-BITS(iDate,10,7) + 1980).        
END FUNCTION.

FUNCTION GetFlags RETURNS CHARACTER ( INPUT iBytes AS INTEGER  ):
    DEFINE VARIABLE res AS CHARACTER INITIAL '' NO-UNDO.
    DEFINE VARIABLE itr AS INTEGER   NO-UNDO.
    
    DO itr=1 TO 16:
        IF GET-BITS (iBytes,itr,1) = 1 THEN 
            res = res + ',' + STRING(itr, '99').
    END.
    
    RETURN LEFT-TRIM(res, ',').
END FUNCTION.

FUNCTION GetInt64 RETURNS INT64 ( INPUT mData AS MEMPTR  ): 
    DEFINE VARIABLE itr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE res  AS INT64   INITIAL 0 NO-UNDO.
    DEFINE VARIABLE idev AS INTEGER EXTENT 4 INITIAL [1,256,65536,16777216] NO-UNDO.

    DO itr = 1 TO GET-SIZE (mData):
        res = res + GET-BYTE (mData, itr) * idev[itr].
    END.

    RETURN res.
END FUNCTION.

FUNCTION GetInteger RETURNS INTEGER  ( INPUT mData AS MEMPTR  ): 
    DEFINE VARIABLE itr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE res  AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE idev AS INTEGER EXTENT 4 INITIAL [1,256,65536,16777216] NO-UNDO.

    DO itr = 1 TO GET-SIZE (mData):
        res = res + GET-BYTE (mData, itr) * idev[itr].
    END.

    RETURN res.
END FUNCTION.

FUNCTION GetTime RETURNS INTEGER ( INPUT iData AS INTEGER ):
    RETURN (60 * 60 * GET-BITS(iData,12,5)) + (60 * GET-BITS(iData,6,6)) + GET-BITS(iData,1,5) * 2.
END FUNCTION.

FUNCTION intToHex RETURNS CHARACTER (i_iint AS INT64):
    DEFINE VARIABLE chex  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rbyte AS RAW       NO-UNDO.
    
    DO WHILE i_iint > 0:
        PUT-BYTE( rbyte, 1 ) = i_iint MODULO 256.
        chex = STRING( HEX-ENCODE( rbyte ) ) + chex.
        i_iint = TRUNCATE( i_iint / 256, 0 ).
    END.
    RETURN chex.
END FUNCTION.

/* ***************************  Main Block  *************************** */

DEFINE VARIABLE oZip        AS Zip               NO-UNDO.
/*DEFINE VARIABLE oTree       AS HuffmanTree       NO-UNDO.*/
/*DEFINE VARIABLE oCodeLength AS IntegerIntegerMap NO-UNDO.*/
/*                                                         */
/*oCodeLength = NEW IntegerIntegerMap().                   */
/*                                                         */
/*oCodeLength:Add(16, 5).                                  */
/*oCodeLength:Add(17, 7).                                  */
/*oCodeLength:Add(18, 6).                                  */
/*oCodeLength:Add(0, 5).                                   */
/*oCodeLength:Add(8, 3).                                   */
/*oCodeLength:Add(7, 2).                                   */
/*oCodeLength:Add(9, 3).                                   */
/*oCodeLength:Add(6, 3).                                   */
/*oCodeLength:Add(10, 3).                                  */
/*oCodeLength:Add(5, 4).                                   */
/*oCodeLength:Add(11, 5).                                  */
/*oCodeLength:Add(4, 5).                                   */
/*oCodeLength:Add(12, 5).                                  */
/*oCodeLength:Add(3, 7).                                   */
/*oCodeLength:Add(13, 0).                                  */
/*oCodeLength:Add(2, 0).                                   */
/*oCodeLength:Add(14, 0).                                  */
/*oCodeLength:Add(1, 0).                                   */
/*oCodeLength:Add(15, 0).                                  */
/*                                                         */
/*                                                         */
/*oTree = NEW HuffmanTree(oCodeLength).                    */




oZip = NEW Zip().

oZip:OpenFile('test.zip').

/*                                                           */
/*/*oZip:FileList().*/                                       */
/*                                                           */
/*/*oZip:ExtractTo('test_folder/folder_2/testcrc32.p', '').*/*/
/*                                                           */
oZip:ExtractTo('gzip-1.3.12.tar.gz', '.\').
/*oZip:ExtractTo('test_zip_reader.p', '.\').*/
/*oZip:ExtractTo('test_short4.txt', '.\').*/


CATCH e1 AS Progress.Lang.Error:
    MESSAGE SUBSTITUTE("ERROR: [&1] '&2'",e1:GetMessageNum(1), e1:getmessage(1)) VIEW-AS ALERT-BOX TITLE 'ERROR'.
END CATCH.

/*MESSAGE SEARCH(cZipFileFullName) VIEW-AS ALERT-BOX.*/


/*oFile = NEW File(SEARCH('test3.zip')).                                                                   */
/*                                                                                                         */
/*RUN Read_EOCD(oFile, OUTPUT oEOCD, OUTPUT oCentralDirArray, OUTPUT oLocalFileHeadersArray).              */
/*                                                                                                         */
/*MESSAGE                                                                                                  */
/*    oCentralDirArray:Size oLocalFileHeadersArray:SIZE SKIP                                               */
/*                                                                                                         */
/*    /*    'Number of this disk: ' oEOCD:NumberOfThisDisk SKIP*/                                          */
/*    /*    'Disk where central directory starts: ' oEOCD:DiskWhereCDStarts SKIP*/                         */
/*    /*    'Numbers of central directory records on this disk: ' oEOCD:NumbersOfCDRecordsOnThisDisk SKIP*/*/
/*    /*    'Total number of central directory records: ' oEOCD:TotalNumberOfCDRecords SKIP*/              */
/*    /*    'Size of central directory in bytes: ' oEOCD:SizeOfCDInBytes SKIP*/                            */
/*    /*    'Offset to start of central directory: ' oEOCD:OffsetToStartOfCD SKIP*/                        */
/*    /*    'Comment length: ' oEOCD:CommentLength SKIP*/                                                  */
/*    'Comment: ' oEOCD:Comment SKIP                                                                       */
/*                                                                                                         */
/*    VIEW-AS ALERT-BOX.                                                                                   */
/*                                                                                                         */
/*RUN ZipFileList(oEOCD,oCentralDirArray,oLocalFileHeadersArray).                                          */



/*IF GET-BYTE (mByteData, 1) + GET-BYTE (mByteData, 2) * 256 + GET-BYTE (mByteData, 3) * 65536 + GET-BYTE (mByteData, 4) * 16777216 <> {&LOCAL_FILE_HEADER} THEN*/
/*DO:                                                                                                                                                           */
/*    MESSAGE 'NOT ZIP FILE' VIEW-AS ALERT-BOX TITLE 'ERROR '.                                                                                                  */
/*    RETURN.                                                                                                                                                   */
/*END.                                                                                                                                                          */


/*RUN Read_Local_file_header(1).*/





/*MESSAGE GET-BYTE (mByteData, 1) SKIP GET-BYTE (mByteData, 2) SKIP GET-BYTE (mByteData, 3) SKIP GET-BYTE (mByteData, 4)*/
/*    VIEW-AS ALERT-BOX.                                                                                                */


/*iPos = 1.                                                                                                                                       */
/*iStep = {&READ_HEADER_BLOCK_SIZE}.                                                                                                              */
/*                                                                                                                                                */
/*REPEAT:                                                                                                                                         */
/*    iStep = MINIMUM ({&READ_HEADER_BLOCK_SIZE},iFileLength - iPos + 1).                                                                         */
/*                                                                                                                                                */
/*    COPY-LOB FROM FILE cZipFileFullName STARTING AT iPos FOR iStep TO mByteData NO-CONVERT.                                                     */
/*                                                                                                                                                */
/*    IF GET-SIZE(mByteData) < {&READ_HEADER_BLOCK_SIZE} THEN                                                                                     */
/*    DO:                                                                                                                                         */
/*        LEAVE.                                                                                                                                  */
/*    END.                                                                                                                                        */
/*                                                                                                                                                */
/*    iSignature = GET-BYTE (mByteData, 1) + GET-BYTE (mByteData, 2) * 256 + GET-BYTE (mByteData, 3) * 65536 + GET-BYTE (mByteData, 4) * 16777216.*/
/*                                                                                                                                                */
/*    IF  iSignature = {&LOCAL_FILE_HEADER} THEN                                                                                                  */
/*    DO:                                                                                                                                         */
/*        COPY-LOB FROM FILE cZipFileFullName STARTING AT iPos + 26 FOR 2 TO mByteData NO-CONVERT.                                                */
/*        iNameLength = GET-BYTE(mByteData,1) + GET-BYTE(mByteData,2) * 256.                                                                      */
/*                                                                                                                                                */
/*        COPY-LOB FROM FILE cZipFileFullName STARTING AT iPos + 26 + 2 + 2 FOR iNameLength TO FILE 'ttt.txt' NO-CONVERT.                         */
/*                                                                                                                                                */
/*        MESSAGE 'iNameLength=' iNameLength //SKIP GET-STRING(mByteData,1)                                                                       */
/*            VIEW-AS ALERT-BOX TITLE ' Name '.                                                                                                   */
/*                                                                                                                                                */
/*       LEAVE.                                                                                                                                   */
/*    END.                                                                                                                                        */
/*    iPos = iPos + iStep.                                                                                                                        */
/*                                                                                                                                                */
/*    IF iPos >= iFileLength THEN LEAVE.                                                                                                          */
/*END.                                                                                                                                            */
