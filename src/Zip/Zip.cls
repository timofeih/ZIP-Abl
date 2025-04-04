 
/*------------------------------------------------------------------------
   File        : Zip
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 17 17:33:02 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assertion.AssertFile FROM PROPATH.
USING OpenEdge.Core.Collections.IIterator FROM PROPATH.
USING OpenEdge.Core.Collections.IStringKeyedMap FROM PROPATH.
USING OpenEdge.Core.Collections.StringKeyedMap FROM PROPATH.
USING Zip.CompressionMethod FROM PROPATH.
USING Zip.Decompressors.Deflate FROM PROPATH.
USING Zip.Decompressors.IDecompressor FROM PROPATH.
USING Zip.Decompressors.Stored FROM PROPATH.
USING Zip.Exceptions.InvalidArgumentException FROM PROPATH.
USING Zip.Exceptions.ZipException FROM PROPATH.
USING Zip.File FROM PROPATH.
USING Zip.IO.BinaryStream FROM PROPATH.
USING Zip.Structures.CentralDirectory FROM PROPATH.
USING Zip.Structures.EOCD FROM PROPATH.
USING Zip.Structures.LocalFileHeader FROM PROPATH.
USING Zip.Structures.Parser FROM PROPATH.
USING Zip.ZipConstants FROM PROPATH.

CLASS Zip.Zip FINAL: 
    
    DEFINE PROTECTED PROPERTY CentralDirMap AS IStringKeyedMap NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY EOCD AS Zip.Structures.EOCD NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY ZipFile AS Zip.File NO-UNDO 
        GET.
        SET. 


    METHOD PROTECTED IDecompressor CreateDecompressor(BStream AS BinaryStream,  oCD AS CentralDirectory, cDestinationFile AS CHARACTER ):        DEFINE VARIABLE oDecompressor AS IDecompressor NO-UNDO.
        
        CASE oCD:CompressionMethod:
            WHEN 0 THEN
                oDecompressor = NEW Stored(BStream, cDestinationFile, oCD:LocalFileHeader:FileDataLength).
            WHEN 8 THEN
                oDecompressor = NEW Deflate(BStream, cDestinationFile).
            OTHERWISE
            UNDO, THROW NEW ZipException(SUBSTITUTE('Unsopported compressiom method(&1).', oCD:CompressionMethod)).
        END CASE.

        RETURN oDecompressor.

    END METHOD.

    METHOD PUBLIC VOID ExtractTo( cFileName AS CHARACTER , cDestPath AS CHARACTER ):
        DEFINE VARIABLE oCD           AS CentralDirectory            NO-UNDO.
        DEFINE VARIABLE oDecompressor AS IDecompressor               NO-UNDO.
        DEFINE VARIABLE oFIOStream    AS Progress.IO.FileInputStream NO-UNDO.
        DEFINE VARIABLE BStream       AS BinaryStream                NO-UNDO.
        
        IF NOT THIS-OBJECT:CentralDirMap:ContainsKey(cFileName) THEN 
        DO:
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('File "&1" not found in the zip-file ("&2")', cFileName, THIS-OBJECT:ZipFile:AbsoluteFileName())).
        END.
               
        oCD = CAST (THIS-OBJECT:CentralDirMap:Get(cFileName), CentralDirectory).

        IF oCD:isDirectory THEN
        DO:
            RETURN.
        END.

        oFIOStream = NEW Progress.IO.FileInputStream(THIS-OBJECT:ZipFile:AbsoluteFileName()).

        oFIOStream:SkipBytes(oCD:LocalFileHeader:FileDataStartOffset - 1).

        BStream = NEW BinaryStream(oFIOStream).
        
        oDecompressor = THIS-OBJECT:CreateDecompressor(BStream, oCD, cDestPath + cFileName).

        IF oDecompressor:Decompress() <> oCD:CRC32UncompressedData THEN
            UNDO, THROW NEW ZipException(SUBSTITUTE('Decompresing error!!!')).

        oFIOStream:Close().
        BStream = ?.

        RETURN.

    END METHOD.

    METHOD PUBLIC VOID FileList(  ):
        DEFINE VARIABLE itr       AS INTEGER          NO-UNDO.
        DEFINE VARIABLE oIterator AS IIterator        NO-UNDO.
        DEFINE VARIABLE oCD       AS CentralDirectory NO-UNDO.
        DEFINE VARIABLE keyName   AS CHARACTER        NO-UNDO.
        
        ASSIGN
            oIterator = THIS-OBJECT:CentralDirMap:KeySet:Iterator()
            itr       = 0.

        DO WHILE oIterator:HasNext():
            ASSIGN
                keyName = oIterator:Next():ToString()
                oCD     = CAST(THIS-OBJECT:CentralDirMap:Get(keyName),CentralDirectory)
                itr     = itr + 1.

            DISPLAY
                itr LABEL 'Nr.'
                oCD:FileName LABEL 'File Name' FORMAT 'x(25)'
                IF oCD:isDirectory THEN 'FOLDER' ELSE 'FILE  ' FORMAT 'x(6)' LABEL '' 
                CompressionMethod:GetEnum(oCD:CompressionMethod):ToString()
                oCD:LocalFileHeader:UncompressedSize LABEL 'Size, B' 
                oCD:LocalFileHeader:CompressedSize LABEL 'C.Size, B' 
                oCD:LocalFileHeader:FileLastModificationDate LABEL 'File date' FORMAT '99/99/9999'
                STRING(oCD:LocalFileHeader:FileLastModificationTime,'HH:MM:SS') LABEL 'File time' FORMAT 'x(8)'
                WITH WIDTH 100 FRAME a.
            PAUSE.
        END.
        
        RETURN.

    END METHOD.

    METHOD PUBLIC VOID OpenFile( picFileName AS CHARACTER ):
       
        AssertFile:FileExists(picFileName).
        
        THIS-OBJECT:ZipFile = NEW File(picFileName).
        
        THIS-OBJECT:ReadZipStructures(THIS-OBJECT:ZipFile).        
       
        RETURN.

    END METHOD.

    METHOD PUBLIC IStringKeyedMap ReadCD( oZipFile AS File, oEOCD AS EOCD ):
        DEFINE VARIABLE oCentralDir      AS CentralDirectory NO-UNDO.
        DEFINE VARIABLE oLocalFileHeader AS LocalFileHeader  NO-UNDO.
        DEFINE VARIABLE oParser          AS Parser           NO-UNDO.
        DEFINE VARIABLE oParserLFH       AS Parser           NO-UNDO.
        DEFINE VARIABLE mTmpDataCD       AS MEMPTR           NO-UNDO.        
        DEFINE VARIABLE mTmpDataLFH      AS MEMPTR           NO-UNDO.
        DEFINE VARIABLE oCentralDirMap   AS IStringKeyedMap  NO-UNDO.
        DEFINE VARIABLE iter             AS INTEGER          NO-UNDO.
        
        mTmpDataCD = oZipFile:GetBlock(oEOCD:OffsetToStartOfCD + 1, oEOCD:SizeOfCDInBytes).

        ASSIGN 
            oParser        = NEW Parser(GET-POINTER-VALUE (mTmpDataCD), GET-SIZE (mTmpDataCD), 1, oEOCD:OffsetToStartOfCD + 1)
            oCentralDirMap = NEW StringKeyedMap().

        SET-SIZE (mTmpDataLFH) = 0.
                        
        DO iter = 1 TO oEOCD:NumbersOfCDRecordsOnThisDisk:
            oCentralDir = NEW CentralDirectory(oParser).
    
            oCentralDirMap:Put(oCentralDir:FileName,oCentralDir).
            
            mTmpDataLFH = oZipFile:GetBlock(oCentralDir:OffsetOfLocalFileHeader + 1, 26 + 20).
            oParserLFH = NEW Parser(GET-POINTER-VALUE (mTmpDataLFH), GET-SIZE (mTmpDataLFH), 1, oCentralDir:OffsetOfLocalFileHeader + 1). 
            
            oCentralDir:LocalFileHeader = NEW LocalFileHeader(oParserLFH).
            
            oCentralDir = ?.
            oParserLFH = ?.
            SET-SIZE (mTmpDataLFH) = 0.
        END.
            
        RETURN oCentralDirMap.

    END METHOD.

    METHOD PROTECTED EOCD ReadEOCD( oZipFile AS File ):
        DEFINE VARIABLE mByteData      AS MEMPTR  NO-UNDO.
        DEFINE VARIABLE iPos           AS INTEGER NO-UNDO.
        DEFINE VARIABLE iEOCDPos       AS INT64   NO-UNDO.
        DEFINE VARIABLE oParser        AS Parser  NO-UNDO.
        DEFINE VARIABLE oEOCD          AS EOCD    NO-UNDO.
        DEFINE VARIABLE iEOCDBlockSize AS INT64   NO-UNDO.
        
        ASSIGN 
            mByteData      = oZipFile:GetEOCDBlock()
            iEOCDBlockSize = GET-SIZE(mByteData).
        
        DO iPos = (iEOCDBlockSize - 18) TO 1 BY -1:    
            IF GET-BYTE (mByteData, iPos) + GET-BYTE (mByteData, iPos + 1) * 256 + GET-BYTE (mByteData, iPos + 2) * 65536 + GET-BYTE (mByteData, iPos + 3) * 16777216 = ZipConstants:EOCD_SIGN THEN
            DO:
                iEOCDPos = iPos.
                LEAVE.
            END.
        END.
        
        IF iEOCDPos = 0 THEN 
        DO:
            UNDO, THROW NEW InvalidArgumentException(SUBSTITUTE('"&1" NOT A ZIP FILE', oZipFile:AbsoluteFileName())).
        END.
        
        oParser = NEW Parser(GET-POINTER-VALUE (mByteData), GET-SIZE (mByteData), iEOCDPos, 0).

        oEOCD= NEW EOCD(oParser).
        
        SET-SIZE (mByteData) = 0.
        
        RETURN oEOCD.

    END METHOD.

    METHOD PROTECTED VOID ReadZipStructures( oZipFile AS File ):
        
        THIS-OBJECT:EOCD = THIS-OBJECT:ReadEOCD(oZipFile).
        
        THIS-OBJECT:CentralDirMap = THIS-OBJECT:ReadCD(oZipFile,THIS-OBJECT:EOCD).
        
        RETURN.

    END METHOD.

END CLASS.