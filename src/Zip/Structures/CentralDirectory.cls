 
/*------------------------------------------------------------------------
   File        : CentralDirectory
   Purpose     : 
   Syntax      : 
   Description : Central Directory
   Author(s)   : hutorovski
   Created     : Fri Mar 14 18:13:45 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert FROM PROPATH.
USING Zip.Structures.Parser FROM PROPATH.
USING Zip.ZipConstants FROM PROPATH.

CLASS Zip.Structures.CentralDirectory FINAL SERIALIZABLE: 
    
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

    DEFINE PUBLIC PROPERTY DiskNumberWhereFileStarts AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY ExternalFileAttributes AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY ExtraField AS MEMPTR NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY ExtraFieldLength AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileComment AS CHARACTER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FileCommentLength AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

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

    DEFINE PUBLIC PROPERTY InternalFileAttributes AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY isDirectory AS LOGICAL NO-UNDO 
        GET():
            DEFINE VARIABLE iFilleNameLength AS INTEGER NO-UNDO.
            
            iFilleNameLength = LENGTH(THIS-OBJECT:FileName).
            
            RETURN (iFilleNameLength > 0 AND SUBSTRING(THIS-OBJECT:FileName, iFilleNameLength, 1) = '/').            
        END.

    DEFINE PUBLIC PROPERTY LocalFileHeader AS Zip.Structures.LocalFileHeader NO-UNDO 
        GET.
        SET. 

    /** Minimum version needed to extract */
    DEFINE PUBLIC PROPERTY MinimumVersion AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    /** Offset of local file header (from start of disk) */
    DEFINE PUBLIC PROPERTY OffsetOfLocalFileHeader AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY UncompressedSize AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY VersionMadeBy AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    CONSTRUCTOR PUBLIC CentralDirectory ( INPUT oParser AS Parser ):
        
        Assert:Equals(oParser:popInteger(4), ZipConstants:CD_SIGN).
        
        ASSIGN
            THIS-OBJECT:VersionMadeBy             = oParser:popInteger(2)
            THIS-OBJECT:MinimumVersion            = oParser:popInteger(2)
            THIS-OBJECT:BitFlag                   = oParser:popInteger(2)
            THIS-OBJECT:CompressionMethod         = oParser:popInteger(2)
            THIS-OBJECT:FileLastModificationTime  = oParser:popTime(2)
            THIS-OBJECT:FileLastModificationDate  = oParser:popDate(2)
            THIS-OBJECT:CRC32UncompressedData     = oParser:popInteger(4)
            THIS-OBJECT:CompressedSize            = oParser:popInteger(4)
            THIS-OBJECT:UncompressedSize          = oParser:popInteger(4)
            THIS-OBJECT:FileNameLength            = oParser:popInteger(2)
            THIS-OBJECT:ExtraFieldLength          = oParser:popInteger(2)
            THIS-OBJECT:FileCommentLength         = oParser:popInteger(2)
            THIS-OBJECT:DiskNumberWhereFileStarts = oParser:popInteger(2)
            THIS-OBJECT:InternalFileAttributes    = oParser:popInteger(2)
            THIS-OBJECT:ExternalFileAttributes    = oParser:popInteger(4)
            THIS-OBJECT:OffsetOfLocalFileHeader   = oParser:popInteger(4)
            .
        
        IF THIS-OBJECT:FileNameLength > 0 THEN
            THIS-OBJECT:FileName = oParser:popString(THIS-OBJECT:FileNameLength).
            
        IF THIS-OBJECT:ExtraFieldLength > 0 THEN
        DO:
            THIS-OBJECT:ExtraField = oParser:popMemptr(THIS-OBJECT:ExtraFieldLength).
        END.
        
        IF THIS-OBJECT:FileCommentLength > 0 THEN
            THIS-OBJECT:FileComment = oParser:popString(THIS-OBJECT:FileCommentLength).
        
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC CentralDirectory ( ):
        IF GET-SIZE (THIS-OBJECT:ExtraField) > 0 THEN
            SET-SIZE (THIS-OBJECT:ExtraField) = 0.
    END DESTRUCTOR.
   
END CLASS.