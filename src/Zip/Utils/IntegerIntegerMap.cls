 
/*------------------------------------------------------------------------
   File        : IntegerIntegerMap
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Mon Mar 31 09:29:56 EEST 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Zip.Utils.IntegerIntegerMap FROM PROPATH.

CLASS Zip.Utils.IntegerIntegerMap: 
    DEFINE PRIVATE TEMP-TABLE ttMap NO-UNDO 
        FIELD iPos   AS INTEGER 
        FIELD iKey   AS INTEGER 
        FIELD iValue AS INTEGER 
        INDEX iPos AS PRIMARY UNIQUE iPos
        INDEX iKey                   iKey
        INDEX iValue                 iValue.
        
    DEFINE PROTECTED PROPERTY iPos AS INTEGER NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC IntegerIntegerMap (  ):
        EMPTY TEMP-TABLE ttMap NO-ERROR.
        iPos = 0.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC Zip.Utils.IntegerIntegerMap SortByKey(  ):
        
        DEFINE VARIABLE oNewMap AS Zip.Utils.IntegerIntegerMap NO-UNDO.

        oNewMap = NEW IntegerIntegerMap().

        FOR EACH ttMap BY ttMap.iKey:
            oNewMap:Add(ttMap.iKey, ttMap.iValue).
        END.

        RETURN oNewMap.

    END METHOD.

    METHOD PUBLIC INTEGER GetByPosValue( iPos AS INTEGER ):
        
        FIND ttMap WHERE ttMap.iPos = iPos NO-ERROR.

        IF NOT AVAILABLE ttMap THEN 
            RETURN ?.

        RETURN ttMap.iValue.

    END METHOD.

    METHOD PUBLIC INTEGER GetByPosKey( iPos AS INTEGER ):
        
        FIND ttMap WHERE ttMap.iPos = iPos NO-ERROR.

        IF NOT AVAILABLE ttMap THEN 
            RETURN ?.

        RETURN ttMap.iKey.

    END METHOD.

    METHOD PUBLIC INTEGER Length(  ):
        
        RETURN THIS-OBJECT:iPos - 1.

    END METHOD.

    METHOD PUBLIC INTEGER Get( iKey AS INTEGER ):
        FIND ttMap WHERE ttMap.iKey = iKey NO-ERROR.
        
        IF NOT AVAILABLE ttMap THEN 
            RETURN ?.
        
        RETURN ttMap.iValue.

    END METHOD.

    METHOD PUBLIC VOID Add( iKey AS INTEGER, iValue AS INTEGER ):
        
        CREATE ttMap.
        ASSIGN 
            ttMap.iPos       = THIS-OBJECT:iPos
            ttMap.iKey       = iKey
            ttMap.iValue     = iValue
            THIS-OBJECT:iPos = THIS-OBJECT:iPos + 1.
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC IntegerIntegerMap ( ):
        EMPTY TEMP-TABLE ttMap NO-ERROR.
    END DESTRUCTOR.


END CLASS.