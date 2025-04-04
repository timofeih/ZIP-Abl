/* *************************************************************************************************************************
Copyright (c) 2021 by Progress Software Corporation and/or one of its subsidiaries or affiliates. All rights reserved.
************************************************************************************************************************** */
/*------------------------------------------------------------------------
    File        : File
    Purpose     : Represents a filename, which may or not exist.
    Author(s)   : pjudge
    Created     : 2021-10-14
    Notes       : * The path elements (name, folder, extension, basename) are immutable once the object has been instantiated.
                  * The file and folder can be created using the CreateFile() and CreateFolder() methods
                  * The file name (incl any folder) cannot contain any of the following characters: < > : " | ? * ^
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.AssertionFailedError FROM PROPATH.
USING OpenEdge.Core.StringConstant FROM PROPATH.

CLASS Zip.Utils.File: 
    
    // the path separator used is /, since ABL deals nicely with it
    DEFINE PRIVATE VARIABLE mPathSeparator    AS CHARACTER INITIAL '/':u NO-UNDO.
    
    // On Windows in particular, these characters are illegal in file name
    DEFINE PRIVATE VARIABLE illegalChars      AS CHARACTER EXTENT 8 INITIAL ['<':u,'>':u,':':u,'"':u,'|':u,'?':u,'*':u, '^':u] NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_LEFT_ANGLE    AS INTEGER   INITIAL 1 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_RIGHT_ANGLE   AS INTEGER   INITIAL 2 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_COLON         AS INTEGER   INITIAL 3 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_DOUBLE_QUOTE  AS INTEGER   INITIAL 4 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_PIPE          AS INTEGER   INITIAL 5 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_QUESTION_MARK AS INTEGER   INITIAL 6 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_ASTERISK      AS INTEGER   INITIAL 7 NO-UNDO.
    DEFINE PRIVATE VARIABLE IDX_CARET         AS INTEGER   INITIAL 8 NO-UNDO.

    /* The file's basename, defined as the last /-separated entry, including the extension */
    DEFINE PUBLIC PROPERTY Basename AS CHARACTER NO-UNDO 
        GET.
        PRIVATE SET. 

    /* The extension, defined as the last .-separated entry of the basename */
    DEFINE PUBLIC PROPERTY Extension AS CHARACTER NO-UNDO 
        GET.
        PRIVATE SET. 

    /* Indiucates whether the file exists. Returns TRUE if the Folder exists, and is a File */
    DEFINE PUBLIC PROPERTY FileExists AS LOGICAL NO-UNDO 
        GET():
            ASSIGN 
                FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
            // this file exists on disk and it is actually a file
            RETURN (FILE-INFO:FULL-PATHNAME NE ? AND index(FILE-INFO:FILE-TYPE, 'F':u) GT 0).
        END GET.
        
    /* The path of the file.
           
           May be blank; if so, the various Can* methods assume the directory is ".", the
           pwd. The value returned by this property will be blank in that case. */     
    DEFINE PUBLIC PROPERTY Folder AS CHARACTER NO-UNDO 
        GET.
        PRIVATE SET. 

    /* Indiucates whether the folder represented by the Folder exists. Returns TRUE if the Folder exists, and is a DIRECTORY */
    DEFINE PUBLIC PROPERTY FolderExists AS LOGICAL NO-UNDO 
        GET():
            FileInfoSetToFolder().
            // this file exists on disk and it is actually a file
            RETURN (FILE-INFO:FULL-PATHNAME NE ? AND index(FILE-INFO:FILE-TYPE, 'D':u) GT 0).
        END GET.

    /* The file name, including path */
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO 
        GET.
        PRIVATE SET. 


    /* Constructor. Sanitises the filename - ie replaces illegal characters with the input replacement char(s).
       
       @param character A file name - not empty, not null, not "."
       @param character A/the replacement character that will replace the illegal characters. If ? is passed, no replacements
                        are done, and the file name cannot contain any of the following characters: <>:"|?* */

    CONSTRUCTOR PUBLIC File ( INPUT pFileName AS CHARACTER, INPUT pReplacementChar AS CHARACTER ):
        DEFINE VARIABLE pos       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE firstPath AS CHARACTER NO-UNDO.
        DEFINE VARIABLE testName  AS CHARACTER NO-UNDO.
        
        IF pFileName EQ '.':u THEN
            UNDO, THROW NEW AssertionFailedError('File name cannot be "."').
        
        // we only work with slashes as path separators
        ASSIGN 
            pFileName = REPLACE(pFileName, StringConstant:BACKSLASH, mPathSeparator).
        
        // don't allow illegal characters in the filename
        IF pReplacementChar EQ ? THEN
        DO:
            // on windows we can have C: or D: as the drive (first path-segment)
            IF OPSYS EQ 'win32':u THEN
            DO:
                ASSIGN 
                    pos = INDEX(pFileName, mPathSeparator).
                IF pos GT 0 THEN
                DO:
                    ASSIGN 
                        firstPath = SUBSTRING(pFileName, 1, pos - 1)
                        testName  = SUBSTRING(pFileName, pos + 1)
                        .
                    IF NOT LENGTH(firstPath) EQ 2 THEN
                        ASSIGN testName = pFileName.
                    ELSE
                    // still check for other illegals
                        IF INDEX(firstPath, illegalChars[IDX_ASTERISK]) GT 0
                            OR index(firstPath, illegalChars[IDX_CARET]) GT 0
                    // we allow the second character to be : not the first
                            OR index(firstPath, illegalChars[IDX_COLON]) EQ 1
                            OR index(firstPath, illegalChars[IDX_DOUBLE_QUOTE]) GT 0
                            OR index(firstPath, illegalChars[IDX_LEFT_ANGLE]) GT 0
                            OR index(firstPath, illegalChars[IDX_RIGHT_ANGLE]) GT 0
                            OR index(firstPath, illegalChars[IDX_PIPE]) GT 0
                            OR index(firstPath, illegalChars[IDX_QUESTION_MARK]) GT 0
                            THEN
                            UNDO, THROW NEW AssertionFailedError(SUBSTITUTE('File name "&1" contains illegal characters', pFileName)).
                END.
            END.    // WINDOWS-specific checks
            ELSE
                ASSIGN testName = pFileName.
            
            IF ContainsIllegalCharacters(testName) THEN
                UNDO, THROW NEW AssertionFailedError(SUBSTITUTE('File name "&1" contains illegal characters', pFileName)).
        END.
        ELSE
        DO:
            // don't allow replacement with an illegal char
            IF ContainsIllegalCharacters(pReplacementChar) THEN
                UNDO, THROW NEW AssertionFailedError(SUBSTITUTE('Replacement string "&1" contains illegal characters', pReplacementChar)).
            
            ASSIGN 
                pFileName = REPLACE(pFileName, illegalChars[1], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[2], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[3], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[4], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[5], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[6], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[7], pReplacementChar)
                pFileName = REPLACE(pFileName, illegalChars[8], pReplacementChar)
                .
        END.
        
        SplitFileName(pFileName).
    END CONSTRUCTOR.

    /* Constructor
           
           @param character A file name - not empty, not null. Cannot contain any of the following characters: <>:"|?*  */
    CONSTRUCTOR PUBLIC File ( INPUT pFileName AS CHARACTER ):
        
        THIS-OBJECT(pFileName, ?).
        
    END CONSTRUCTOR.

    /* Indicates whether a string has illegal characters anywhere in it.
       @param character A string to check
       @return logical TRUE if the string is not-null, and has at least one illegal character in it; FALSE otherwise */
    METHOD PRIVATE LOGICAL ContainsIllegalCharacters(INPUT pValue AS CHARACTER):
        IF pValue EQ ? THEN
            RETURN FALSE.
        
        IF INDEX(pValue, illegalChars[IDX_ASTERISK]) GT 0
            OR index(pValue, illegalChars[IDX_CARET]) GT 0
            OR index(pValue, illegalChars[IDX_COLON]) GT 0
            OR index(pValue, illegalChars[IDX_DOUBLE_QUOTE]) GT 0
            OR index(pValue, illegalChars[IDX_LEFT_ANGLE]) GT 0
            OR index(pValue, illegalChars[IDX_RIGHT_ANGLE]) GT 0
            OR index(pValue, illegalChars[IDX_PIPE]) GT 0
            OR index(pValue, illegalChars[IDX_QUESTION_MARK]) GT 0
            THEN
            RETURN TRUE.
        
        RETURN FALSE.
    END METHOD.

    /** Returns an array of path entries
        
        @return character[] May be indeterminate if the path is empty */
    METHOD PUBLIC CHARACTER EXTENT GetSplitPath():
        DEFINE VARIABLE split AS CHARACTER EXTENT NO-UNDO.
        DEFINE VARIABLE loop  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cnt   AS INTEGER   NO-UNDO.
        
        ASSIGN
            cnt = NUM-ENTRIES(THIS-OBJECT:Folder, mPathSeparator).
        
        // do not check for the num-entries, since '' = 1
        IF THIS-OBJECT:Folder EQ '':u THEN
            RETURN split.
        
        // the path always has a trailing /
        ASSIGN 
            EXTENT(split) = cnt - 1.
        DO loop = 1 TO cnt - 1:
            ASSIGN 
                split[loop] = ENTRY(loop, THIS-OBJECT:Folder, mPathSeparator).
        END.
        
        RETURN split.
    END METHOD.
    
    /* Splits a /-delimited file name into its component parts
       
       @param character A complete file name */
    METHOD PRIVATE VOID SplitFileName(INPUT pFileName AS CHARACTER):
        DEFINE VARIABLE pos      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
        
        ASSIGN
            filename = ENTRY(NUM-ENTRIES(pFileName, mPathSeparator), pFileName, mPathSeparator).
        
        ASSIGN 
            pos                  = R-INDEX(filename, '.':u)
            THIS-OBJECT:Basename = filename
            .
        IF pos GT 0 THEN
            ASSIGN THIS-OBJECT:Extension = SUBSTRING(filename, pos + 1).
        
        ASSIGN 
            pos = R-INDEX(pFileName, mPathSeparator).
        IF NOT pos EQ 0 THEN
            ASSIGN THIS-OBJECT:Folder = SUBSTRING(pFileName, 1, pos).
        ELSE
            ASSIGN THIS-OBJECT:Folder = '':u.
        
        ASSIGN 
            THIS-OBJECT:Name = THIS-OBJECT:Folder + THIS-OBJECT:Basename.
    END METHOD.

    /* Creates this file's Folder
        
       This method does NOT check whether the folder can be created (via CanCreateFolder), but rather simply tries
       to create the folder tree, and returns FALSE if it fails.
       
       @return logical TRUE if the Folder was created. FALSE if the Folder already exists, or if an error was raised during creation */
    METHOD PUBLIC LOGICAL CreateFolder():
        DEFINE VARIABLE loop      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cnt       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE newfolder AS CHARACTER NO-UNDO.
        DEFINE VARIABLE delim     AS CHARACTER NO-UNDO.
        
        FileInfoSetToFolder().
        
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        DO:
            ASSIGN 
                cnt = NUM-ENTRIES(THIS-OBJECT:Folder, mPathSeparator).
            DO loop = 1 TO cnt
                WHILE OS-ERROR EQ 0:
                ASSIGN 
                    newfolder = newfolder + delim + ENTRY(loop, THIS-OBJECT:Folder, mPathSeparator)
                    delim     = mPathSeparator
                    .
                OS-CREATE-DIR value(newfolder).
            END.
            
            RETURN (OS-ERROR EQ 0).
        END.
        
        RETURN FALSE.
        
        CATCH uncaught AS Progress.Lang.Error:
            RETURN FALSE.
        END CATCH.
    END METHOD.

    /** Returns a logical value indicating whether the folder can be created.
    
        If any of the parent folders are not directories or are not writable, then
        return FALSE.
        
        Otherwise return TRUE.
        
        @return logical TRUE if the folder can be created, FALSE otherwise */
    METHOD PUBLIC LOGICAL CanCreateFolder():
        FileInfoSetToFolder().
        
        // if it does not exist in any form, check the parents
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        DO:
            // we know that this folder does not exist, so we look up
            FileInfoFindExistingParent(FILE-INFO:FILE-NAME).
            // this will tell us whether we can create a folder under it.
            RETURN FileInfoIsFolderAndWritable().
        END.
        ELSE
            // this folder already exists, either as a file or directory or something else,
            // so we cannot create it
            RETURN FALSE.
    END METHOD.
    
    /** Returns a logical value indicating whether the file can be created.
       
        If the Name already exists (as any type), then FALSE is returned.
        If the Folder exists, and is either a FILE or not writable, then FALSE is returned.
        If the Folder does not exist, then return the value of the CanCreateFolder() method.
        
        Otherwise return TRUE.
        
        @return logical TRUE if the file can be created, FALSE otherwise */
    METHOD PUBLIC LOGICAL CanCreateFile():
        ASSIGN 
            FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
        // does not exist in any form
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        DO:
            FileInfoSetToFolder().
            IF FILE-INFO:FULL-PATHNAME EQ ? THEN
                RETURN CanCreateFolder().
            ELSE
                RETURN FileInfoIsFolderAndWritable().
        END.
        ELSE
            // this name already exists, either as a file or directory or something else,
            // so we cannot create it
            RETURN FALSE.
    END METHOD.
    
    /* Set the FILE-INFO FILE-NAME to the folder for this File. Uses "." if the folder is empty. */
    METHOD PRIVATE VOID FileInfoSetToFolder():
        IF THIS-OBJECT:Folder EQ '':u THEN
            ASSIGN FILE-INFO:FILE-NAME = '.':u.
        ELSE
            ASSIGN FILE-INFO:FILE-NAME = THIS-OBJECT:Folder.
    END METHOD.
    
    /* Checks if the current FILE-INFO is a FILE and has the WRITE flag
       set.
       
       @return logical TRUE if the current file-info is a file, and has the write flag set; FALSE otherwise */
    METHOD PRIVATE LOGICAL FileInfoIsFileAndWritable():
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN FALSE.
        
        // It is a file and the Write flag is set, so it is writable
        IF  INDEX(FILE-INFO:FILE-TYPE, 'F':u) GT 0
            AND index(FILE-INFO:FILE-TYPE, 'W':u) GT 0
            THEN
            RETURN TRUE.
        
        // ... otherwise not
        RETURN FALSE.
    END METHOD. 
    
    /* Checks if the current FILE-INFO is a DIRECTORY and has the WRITE flag
       set.
       
       @return logical TRUE if the current file-info is a directory, and has the write flag set; FALSE otherwise */
    METHOD PRIVATE LOGICAL FileInfoIsFolderAndWritable():
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN FALSE.
        
        // It is a Directory and the Write flag is set, so it is writable
        IF  INDEX(FILE-INFO:FILE-TYPE, 'D':u) GT 0
            AND index(FILE-INFO:FILE-TYPE, 'W':u) GT 0
            THEN
            RETURN TRUE.
        
        // ... otherwise not
        RETURN FALSE.
    END METHOD.
    
    /* Indicates whether a file can be written to.
       
       @return logical TRUE if the file exists and can be written to. FALSE otherwise. */
    METHOD PUBLIC LOGICAL CanWriteToFile():
        ASSIGN 
            FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
        
        // does not exist in any form, so depends on parent folder permissions
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN CanWriteToFolder().
        
        RETURN FileInfoIsFileAndWritable().
    END METHOD.
    
    /* Indicates whether the file's folder can be written to.
       
       If none of the folders exist then this method returns TRUE
       
       @return logical TRUE if the file exists and can be written to. FALSE otherwise. */
    METHOD PUBLIC LOGICAL CanWriteToFolder():
        DEFINE VARIABLE path AS CHARACTER NO-UNDO.
        
        FileInfoSetToFolder().
        
        // if it does not exist in any form, check the parents
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        DO:
            FileInfoFindExistingParent(FILE-INFO:FILE-NAME).
            IF NOT FILE-INFO:FULL-PATHNAME EQ ? THEN
                RETURN FileInfoIsFolderAndWritable().
            
            // At this point, none of the specified parents exist
            ASSIGN 
                path = FILE-INFO:FILE-NAME.
            
            // this is relative to something
            IF NOT path BEGINS '.':u
                AND NOT path BEGINS '/':u
                THEN
            DO:
                // Can we write relative to the current path?
                ASSIGN 
                    FILE-INFO:FILE-NAME = '.':u.
                RETURN FileInfoIsFolderAndWritable().
            END.
            
            // we are trying to create a root folder, and that's a no-go
            RETURN FALSE.
        END.
        
        RETURN FileInfoIsFolderAndWritable().
    END METHOD.
    
    /* Find the first/nearest existing folder for a path. A path may be a folder
       or a file.
       
       @param character A path. */
    METHOD PRIVATE VOID FileInfoFindExistingParent(INPUT pPath AS CHARACTER):
        DEFINE VARIABLE pos AS INTEGER NO-UNDO.
        DEFINE VARIABLE len AS INTEGER NO-UNDO.
        
        ASSIGN 
            len = LENGTH(pPath)
               // folder always has trailing /
            pos = R-INDEX(pPath, mPathSeparator, len - 1)
            .
        PATH-LOOP:
        DO WHILE pos GT 0:
            ASSIGN 
                FILE-INFO:FILE-NAME = SUBSTRING(pPath, 1, pos - 1)
                   // find the previous /
                pos                 = R-INDEX(pPath, mPathSeparator, pos - 1)
                .
            // return the first parent we find
            IF NOT FILE-INFO:FULL-PATHNAME EQ ? THEN
                RETURN.
        END.
    END METHOD.
    
    /* Creates an empty file on disk.
       
       This method does NOT check whether the file can be created (via CanCreateFile), but rather simply tries
       to create the file, and returns FALSE if it fails.
       
       @return logical TRUE if the file was created. FALSE if the file already exists, or if an error wa raised during creation. */
    METHOD PUBLIC LOGICAL CreateFile():
        ASSIGN 
            FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        DO:
            CreateFolder().
            OUTPUT to value(THIS-OBJECT:Name).
            OUTPUT close.
            
            // reset to recalculate the full path
            ASSIGN 
                FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
            RETURN (NOT FILE-INFO:FULL-PATHNAME EQ ?).
        END.
        
        RETURN FALSE.
        CATCH uncaught AS Progress.Lang.Error:
            RETURN FALSE.
        END CATCH.
    END METHOD.
    
    METHOD OVERRIDE PUBLIC LOGICAL Equals(INPUT pObj AS Progress.Lang.Object):
        IF NOT VALID-OBJECT(pObj) THEN
            RETURN FALSE.
        
        IF SUPER:Equals(pObj) THEN
            RETURN TRUE.
        
        IF NOT TYPE-OF(pObj, Zip.Utils.File) THEN
            RETURN FALSE.
        
        RETURN (THIS-OBJECT:Name EQ CAST(pObj, Zip.Utils.File):Name).
    END METHOD.
    
    /** Returns the absolute file name of this object, if the file exists. Resolved using FILE-INFO
       
        @param character An absolute path. If the file does not exist, or cannot be resolved, the unknown value is returned */
    METHOD PUBLIC CHARACTER AbsoluteFileName():
        ASSIGN 
            FILE-INFO:FILE-NAME = THIS-OBJECT:Name.
        
        RETURN FILE-INFO:FULL-PATHNAME.
    END METHOD.
    
    METHOD OVERRIDE PUBLIC CHARACTER ToString():
        RETURN THIS-OBJECT:Name.
    END METHOD.
    
END CLASS.