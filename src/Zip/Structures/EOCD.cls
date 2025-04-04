 
/*------------------------------------------------------------------------
   File        : EOCD
   Purpose     : 
   Syntax      : 
   Description : End of Central Directory Record
   Author(s)   : hutorovski
   Created     : Fri Mar 14 16:29:50 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert FROM PROPATH.
USING Zip.Structures.Parser FROM PROPATH.
USING Zip.ZipConstants FROM PROPATH.

CLASS Zip.Structures.EOCD FINAL SERIALIZABLE: 

    &SCOPED-DEFINE SIGNATURE 0x06054b50

    DEFINE PUBLIC PROPERTY Comment AS CHARACTER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY CommentLength AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY DiskWhereCDStarts AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 
 
    DEFINE PUBLIC PROPERTY NumberOfThisDisk AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY NumbersOfCDRecordsOnThisDisk AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY OffsetToStartOfCD AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY SizeOfCDInBytes AS INT64 NO-UNDO 
        GET.
        PROTECTED SET. 

    DEFINE PUBLIC PROPERTY TotalNumberOfCDRecords AS INTEGER NO-UNDO 
        GET.
        PROTECTED SET. 

    CONSTRUCTOR PUBLIC EOCD ( INPUT oParser AS Parser ):
        
        Assert:Equals(oParser:popInteger(4), ZipConstants:EOCD_SIGN).
        
        ASSIGN 
            NumberOfThisDisk             = oParser:popInteger(2)
            DiskWhereCDStarts            = oParser:popInteger(2)
            NumbersOfCDRecordsOnThisDisk = oParser:popInteger(2)
            TotalNumberOfCDRecords       = oParser:popInteger(2)
            SizeOfCDInBytes              = oParser:popInteger(4)
            OffsetToStartOfCD            = oParser:popInteger(4)
            CommentLength                = oParser:popInteger(2)
            .
        IF CommentLength > 0 THEN
        DO:
            Comment = oParser:popString(CommentLength).
        END.
             
    END CONSTRUCTOR.

END CLASS.