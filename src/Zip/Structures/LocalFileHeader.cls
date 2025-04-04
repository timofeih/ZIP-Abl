 
/*------------------------------------------------------------------------
   File        : LocalFileHeader
   Purpose     : 
   Syntax      : 
   Description : Local file header
   Author(s)   : hutorovski
   Created     : Fri Mar 14 19:29:04 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert FROM PROPATH.
USING Zip.Structures.Parser FROM PROPATH.
USING Zip.ZipConstants FROM PROPATH.

CLASS Zip.Structures.LocalFileHeader FINAL SERIALIZABLE:
    
    DEFINE PUBLIC PROPERTY BitFlag AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY CompressedSize AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY CompressionMethod AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    /** CRC-32 of uncompressed data */
    DEFINE PUBLIC PROPERTY CRC32UncompressedData AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 
        
    DEFINE PUBLIC PROPERTY ExtraField AS MEMPTR NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY ExtraFieldLength AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileDataLength AS INT64 NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY FileDataStartOffset AS INT64 NO-UNDO 
        GET.
        SET.

    DEFINE PUBLIC PROPERTY FileLastModificationDate AS DATE NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileLastModificationTime AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileNameLength AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 
 
    /** Minimum version needed to extract */
    DEFINE PUBLIC PROPERTY MinimumVersion AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY UncompressedSize AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    CONSTRUCTOR PUBLIC LocalFileHeader ( INPUT oParser AS Parser ):
        Assert:Equals(oParser:popInteger(4), ZipConstants:LFH_SIGN).
        
        ASSIGN 
            THIS-OBJECT:MinimumVersion           = oParser:popInteger(2)
            THIS-OBJECT:BitFlag                  = oParser:popInteger(2)
            THIS-OBJECT:CompressionMethod        = oParser:popInteger(2)
            THIS-OBJECT:FileLastModificationTime = oParser:popTime(2)
            THIS-OBJECT:FileLastModificationDate = oParser:popDate(2)
            THIS-OBJECT:CRC32UncompressedData    = oParser:popInteger(4)
            THIS-OBJECT:CompressedSize           = oParser:popInteger(4)
            THIS-OBJECT:UncompressedSize         = oParser:popInteger(4)
            THIS-OBJECT:FileNameLength           = oParser:popInteger(2)
            THIS-OBJECT:ExtraFieldLength         = oParser:popInteger(2)
            .
            
        IF THIS-OBJECT:FileNameLength > 0 THEN
            THIS-OBJECT:FileName = oParser:popString(THIS-OBJECT:FileNameLength).
            
        IF THIS-OBJECT:ExtraFieldLength > 0 THEN
        DO:
            THIS-OBJECT:ExtraField = oParser:popMemptr(THIS-OBJECT:ExtraFieldLength).
        END.
        
        THIS-OBJECT:FileDataStartOffset = oParser:GetCurrentOffset() - 1.
        THIS-OBJECT:FileDataLength = THIS-OBJECT:CompressedSize.
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC LocalFileHeader ( ):
        IF GET-SIZE (THIS-OBJECT:ExtraField) > 0 THEN
            SET-SIZE (THIS-OBJECT:ExtraField) = 0.
    END DESTRUCTOR.
    
END CLASS.