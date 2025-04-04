 
/*------------------------------------------------------------------------
   File        : ZipConstants
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 17 18:29:25 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Zip.ZipConstants FINAL: 

    DEFINE PUBLIC STATIC PROPERTY CD_SIGN AS INTEGER INITIAL 0X02014B50 NO-UNDO 
        GET.
     
    DEFINE PUBLIC STATIC PROPERTY EOCD_SIGN AS INTEGER INITIAL 0X06054B50 NO-UNDO 
        GET.

    DEFINE PUBLIC STATIC PROPERTY LFH_SIGN AS INTEGER INITIAL 0X04034B50 NO-UNDO 
        GET.
          

END CLASS.