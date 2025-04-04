 
/*------------------------------------------------------------------------
   File        : Parser
   Purpose     : 
   Syntax      : 
   Description : Zip structures parser
   Author(s)   : hutorovski
   Created     : Fri Mar 14 22:26:32 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.Assert FROM PROPATH.

CLASS Zip.Structures.Parser: 
    DEFINE PROTECTED PROPERTY iCurrPosition AS INT64 NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY iRealStartPossition AS INT64 NO-UNDO
        GET.
        SET.

    DEFINE PROTECTED PROPERTY mmValue AS MEMPTR NO-UNDO 
        GET.
        SET.

    CONSTRUCTOR PUBLIC Parser ( INPUT piPtr AS INT64, INPUT piSize AS INT64, INPUT piStartPossitin AS INT64, INPUT piRealPossition AS INT64 ):
        Assert:IsZeroOrPositive(piSize, 'Size').
        SET-POINTER-VALUE(mmValue) = piPtr.
        SET-SIZE(mmValue) = piSize.
        
        ASSIGN 
            THIS-OBJECT:iCurrPosition       = piStartPossitin
            THIS-OBJECT:iRealStartPossition = piRealPossition.
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC INT64 GetCurrentOffset(  ):
       
        RETURN THIS-OBJECT:iRealStartPossition + THIS-OBJECT:iCurrPosition.

    END METHOD.

    METHOD PUBLIC MEMPTR popMemptr( INPUT piLength AS INTEGER ):
        DEFINE VARIABLE mData AS MEMPTR NO-UNDO.
    
        SET-SIZE (mData) = piLength.
        
        mData = GET-BYTES(THIS-OBJECT:mmValue, THIS-OBJECT:iCurrPosition, piLength).    
        THIS-OBJECT:iCurrPosition = THIS-OBJECT:iCurrPosition + piLength.
    
        RETURN mData.
        
    END METHOD.
        
    METHOD PUBLIC INT64 popInteger( INPUT piLength AS INTEGER ):
        DEFINE VARIABLE itr  AS INTEGER NO-UNDO.
        DEFINE VARIABLE res  AS INT64   INITIAL 0 NO-UNDO.
        DEFINE VARIABLE idev AS INTEGER EXTENT 4 INITIAL [1,256,65536,16777216] NO-UNDO.

        DO itr = 1 TO piLength:
            res = res + GET-BYTE (THIS-OBJECT:mmValue, THIS-OBJECT:iCurrPosition + itr - 1) * idev[itr].
        END.
        
        THIS-OBJECT:iCurrPosition = THIS-OBJECT:iCurrPosition + piLength.
        
        RETURN res.

    END METHOD.
    
    METHOD PUBLIC CHARACTER popString( INPUT piLength AS INTEGER ):
        DEFINE VARIABLE res AS CHARACTER NO-UNDO.

        res = GET-STRING(THIS-OBJECT:mmValue, THIS-OBJECT:iCurrPosition, piLength).
        
        THIS-OBJECT:iCurrPosition = THIS-OBJECT:iCurrPosition + piLength.
        
        RETURN res.

    END METHOD.
    
    METHOD PUBLIC INTEGER popTime( INPUT piLength AS INTEGER ):
        DEFINE VARIABLE iData AS INT64 NO-UNDO.

        iData = THIS-OBJECT:popInteger(piLength).
        
        RETURN THIS-OBJECT:GetTime(iData).

    END METHOD.
    
    METHOD PUBLIC DATE popDate( INPUT piLength AS INTEGER ):
        DEFINE VARIABLE iData AS INT64 NO-UNDO.

        iData = THIS-OBJECT:popInteger(piLength).
        
        RETURN THIS-OBJECT:GetDate(iData).

    END METHOD.
    
    METHOD PROTECTED DATE GetDate ( INPUT iData AS INT64 ):
        RETURN DATE(GET-BITS(iData,6,4),GET-BITS(iData,1,5),GET-BITS(iData,10,7) + 1980).
    END METHOD.

    METHOD PROTECTED INTEGER GetTime ( INPUT iData AS INT64 ):
        RETURN (60 * 60 * GET-BITS(iData,12,5)) + (60 * GET-BITS(iData,6,6)) + GET-BITS(iData,1,5) * 2.
    END METHOD.        

    DESTRUCTOR PUBLIC Parser ( ):
        SET-POINTER-VALUE(mmValue) = 0.
    END DESTRUCTOR.

END CLASS.