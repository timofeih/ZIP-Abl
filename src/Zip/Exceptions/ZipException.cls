 
/*------------------------------------------------------------------------
   File        : ZipException
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 31 10:46:19 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Zip.Exceptions.AbstractZipException.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Zip.Exceptions.ZipException INHERITS AbstractZipException: 
    CONSTRUCTOR PUBLIC ZipException ( INPUT errorMessage AS CHARACTER):
        SUPER (INPUT errorMessage).
        
    END CONSTRUCTOR.
END CLASS.