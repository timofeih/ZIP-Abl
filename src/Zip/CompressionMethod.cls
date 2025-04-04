
/*------------------------------------------------------------------------
    File        : CompressionMethod
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hutorovski
    Created     : Tue Mar 18 11:18:32 EET 2025
    Notes       : 
  ----------------------------------------------------------------------*/

ENUM Zip.CompressionMethod :  

DEFINE ENUM 
    Stored = 0
    Deflated = 8
    BZIP2 = 12
    LZMA = 14
    PPMd = 98
    AESEncryption = 99.

END ENUM.