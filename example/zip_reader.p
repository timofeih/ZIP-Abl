
/*------------------------------------------------------------------------
    File        : zip_reader.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hutorovski
    Created     : Fri Apr 04 12:12:33 EEST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Zip FROM PROPATH.

DEFINE VARIABLE oZip AS Zip NO-UNDO.

/* ***************************  Main Block  *************************** */


oZip = NEW Zip().

oZip:OpenFile('example\test.zip').


/*oZip:FileList().*/

/*/*oZip:ExtractTo('test_folder/folder_2/testcrc32.p', '').*/*/
/*oZip:ExtractTo('gzip-1.3.12.tar.gz', '.\').*/
/*oZip:ExtractTo('test_zip_reader.p', '.\').*/
oZip:ExtractTo('test_short4.txt', '.\example\results\').

CATCH e1 AS Progress.Lang.Error:
    MESSAGE SUBSTITUTE("ERROR: [&1] '&2'",e1:GetMessageNum(1), e1:getmessage(1)) VIEW-AS ALERT-BOX TITLE 'ERROR'.
END CATCH.