
/*------------------------------------------------------------------------
    File        : IDecompressor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hutorovski
    Created     : Wed Apr 02 10:59:28 EEST 2025
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Zip.Decompressors.IDecompressor:  
    METHOD PUBLIC INT64 Decompress(  ).
END INTERFACE.