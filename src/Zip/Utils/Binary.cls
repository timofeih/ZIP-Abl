 
/*------------------------------------------------------------------------
   File        : Binary
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hutorovski
   Created     : Wed Mar 19 17:13:20 EET 2025
   Notes       : 
 ----------------------------------------------------------------------*/


BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Zip.Utils.Binary FINAL:
    
    DEFINE STATIC VARIABLE iexp AS INT64 EXTENT 32 INITIAL [
        1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,
        4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648
        ].
     
    METHOD PUBLIC STATIC CHARACTER BinaryNOTL( cBinaryValue AS CHARACTER ):
        DEFINE VARIABLE cRes AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBit AS CHARACTER NO-UNDO.
        DEFINE VARIABLE itr  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iLen AS INTEGER   NO-UNDO.

        ASSIGN 
            cRes = ''
            iLen = LENGTH(cBinaryValue).

        DO itr = 1 TO iLen:
            cBit= SUBSTRING (cBinaryValue,1,1).
            cRes = cRes + STRING (cBit = '1', '0/1').
            cBinaryValue= SUBSTRING (cBinaryValue,2).
        END.
        
        RETURN cRes.

    END METHOD.

    METHOD PUBLIC STATIC INTEGER BinToInt( INPUT cBinaryValue AS CHARACTER ):
        
        DEFINE VARIABLE iResult  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE itr      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cCurChar AS CHARACTER NO-UNDO.
    
        ASSIGN
            cBinaryValue = TRIM(cBinaryValue)
            iResult      = 0.
        
        IF cBinaryValue EQ '' THEN RETURN 0.

        DO itr = 0 TO LENGTH(cBinaryValue) - 1:
            ASSIGN
                cCurChar = SUBSTRING(cBinaryValue,LENGTH(cBinaryValue) - itr,1)
                iResult  = iResult + (IF cCurChar NE "0" THEN INTEGER(EXP(2,itr)) ELSE 0).
        END.

        RETURN iResult.

    END METHOD.
     
    METHOD PUBLIC STATIC CHARACTER IntToBin( INPUT ipiValue AS INT64):
        
        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        DO WHILE ipiValue > 0:
            ASSIGN
                cReturn  = STRING( ipiValue MOD 2 ) + cReturn
                ipiValue = TRUNCATE( ipiValue / 2, 0 ).
        END.
        
        IF cReturn = "" THEN cReturn = "0".
        
        RETURN cReturn.

    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER str_pad (cBinValue AS CHARACTER, iLength AS INTEGER, cFiller AS CHARACTER  ):  
        RETURN FILL (cFiller, MAXIMUM (0, iLength - LENGTH(cBinValue))) + cBinValue.
    END METHOD.
    
    METHOD PUBLIC STATIC INT64 ShiftRight(i_ivalue AS INT64,i_ishift AS INTEGER):
         
        RETURN INT64( TRUNCATE( i_ivalue / iexp[ i_ishift + 1], 0 ) ).

    END METHOD.
    
    METHOD PUBLIC STATIC INT64 ShiftLeft(i_ivalue AS INT64,i_ishift AS INTEGER):
         
        RETURN  i_ivalue * iexp[ i_ishift + 1].

    END METHOD.

    METHOD PUBLIC STATIC INT64 ShiftRight_n(i_ivalue AS INT64,i_ishift AS INTEGER):
        DEFINE VARIABLE iresult AS INT64 NO-UNDO.

        PUT-BITS( iresult, 1,64 - i_ishift) = GET-BITS( i_ivalue, i_ishift + 1 , 64 - i_ishift).
        
        RETURN iresult.
    END METHOD.

    METHOD PUBLIC STATIC INTEGER BinaryAND_n (i_ivalue_1 AS INT64):  
        DEFINE VARIABLE iresult AS INTEGER NO-UNDO.

        PUT-BITS( iresult, 1, 8) = GET-BITS(i_ivalue_1,1,8).

        RETURN iresult.

    END METHOD. /* End of METHOD BinaryAND */

    METHOD PUBLIC STATIC INT64 BinaryAND (i_ivalue_1 AS INT64,i_ivalue_2 AS INT64):  
        DEFINE VARIABLE icbit   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iresult AS INT64   NO-UNDO.

        DO icbit = 1 TO 64:
            IF LOGICAL( GET-BITS( i_ivalue_1, icbit, 1 ) ) AND
                LOGICAL( GET-BITS( i_ivalue_2, icbit, 1 ) ) 
                THEN
                PUT-BITS( iresult, icbit, 1 ) = 1.
        END.

        RETURN iresult.

    END METHOD. /* End of METHOD BinaryAND */

    METHOD PUBLIC STATIC INT64 BinaryXOR_n2 (i_ivalue_1  AS INT64, i_ivalue_2  AS INTEGER):

        DEFINE VARIABLE icbit   AS INTEGER NO-UNDO.
        DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
        DEFINE VARIABLE i2      AS INTEGER NO-UNDO.
        DEFINE VARIABLE ir      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iresult AS INT64   NO-UNDO.
   
        PUT-BITS( iresult, 33 , 32) = GET-BITS( i_ivalue_1, 33 , 32).
   
        DO icbit = 1 TO 32 BY 2:
            ASSIGN
                i1 = GET-BITS( i_ivalue_1, icbit, 2 )
                i2 = GET-BITS( i_ivalue_2, icbit, 2 ).

            IF i1 = i2 THEN
                NEXT.
            ELSE IF i1 = 0 OR i2 = 0 OR i1 + i2 = 3 THEN
                    ir = i1 + i2.
                ELSE IF i1 + i2 = 5 THEN
                        ir = 1.
                    ELSE IF i1 + i2 = 4 THEN
                            ir = 2 .

            PUT-BITS( iresult, icbit, 2 ) = ir.
        END.

        RETURN iresult.

    END METHOD. /*End of METHOD BinaryXOR */

    METHOD PUBLIC STATIC INT64 BinaryXOR_n (i_ivalue_1  AS INT64, i_ivalue_2  AS INTEGER):

        DEFINE VARIABLE icbit   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iresult AS INT64   NO-UNDO.

        DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
        DEFINE VARIABLE i2      AS INTEGER NO-UNDO.
        
        /*        DEFINE VARIABLE bb      AS Zip.Utils.Byte    NO-UNDO.*/
   
        IF i_ivalue_1 = 0 OR i_ivalue_2 = 0 THEN
            RETURN  i_ivalue_1 + i_ivalue_2.
   
        IF i_ivalue_1 = i_ivalue_2 THEN
            RETURN 0.
   
        PUT-BITS( iresult, 33 , 32) = GET-BITS( i_ivalue_1, 33 , 32).

        DO icbit = 1 TO 32 BY 2:
            ASSIGN
                i1 = GET-BITS( i_ivalue_1, icbit, 2 )
                i2 = GET-BITS( i_ivalue_2, icbit, 2 ).

            IF i1 <> i2 THEN
            DO:
                IF i1 = 0 OR i2 = 0 OR i1 + i2 = 3 THEN
                    PUT-BITS( iresult, icbit, 2 ) = i1 + i2.
                ELSE IF i1 + i2 = 5 THEN
                        PUT-BITS( iresult, icbit, 2 ) = 1.
                    ELSE IF i1 + i2 = 4 THEN
                            PUT-BITS( iresult, icbit, 2 ) = 2 .
            END.
        END.

        RETURN iresult.

    END METHOD. /*End of METHOD BinaryXOR */

    METHOD PUBLIC STATIC INT64 BinaryXOR (i_ivalue_1  AS INT64, i_ivalue_2  AS INT64):

        DEFINE VARIABLE icbit   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iresult AS INT64   NO-UNDO.
        
        DEFINE VARIABLE i1      AS INTEGER NO-UNDO.
        DEFINE VARIABLE i2      AS INTEGER NO-UNDO.
                
        /*        DEFINE VARIABLE bb AS Zip.Utils.Byte    NO-UNDO.*/

        IF i_ivalue_1= 0 OR i_ivalue_2= 0 THEN
            RETURN  i_ivalue_1 + i_ivalue_2.
        
        IF i_ivalue_1 = i_ivalue_2 THEN
            RETURN 0. 
        
        /*                                                                                 */
        /*        DEFINE VARIABLE r1 AS RAW NO-UNDO.                                       */
        /*        DEFINE VARIABLE r2 AS RAW NO-UNDO.                                       */
        /*        DEFINE VARIABLE rr AS RAW NO-UNDO.                                       */
        /*                                                                                 */
        /*        put-INT64(r1,1) = i_ivalue_1.                                            */
        /*        put-INT64(r2,1) = i_ivalue_2.                                            */
        /*                                                                                 */
        /*        DO icbit = 1 TO 4:                                                       */
        /*            ASSIGN                                                               */
        /*                i1 = GET-BYTE( r1, icbit)                                        */
        /*                i2 = GET-BYTE( r2, icbit).                                       */
        /*/*                DISPLAY i1 i2.*/                                               */
        /*                bb = (Zip.Utils.Byte:GetEnum(i1) XOR Zip.Utils.Byte:GetEnum(i2)).*/
        /*                                                                                 */
        /*            PUT-Byte( rr, icbit) = bb:GetValue().                                */
        /*        END.                                                                     */
        /*        RETURN GET-INT64(rr,1).                                                  */
        DO icbit = 1 TO 64 BY 2:
            ASSIGN
                i1 = GET-BITS( i_ivalue_1, icbit, 2 )
                i2 = GET-BITS( i_ivalue_2, icbit, 2 ).

            IF i1 <> i2 THEN
            DO:
                IF i1 = 0 OR i2 = 0 OR i1 + i2 = 3 THEN
                    PUT-BITS( iresult, icbit, 2 ) = i1 + i2.
                ELSE IF i1 + i2 = 5 THEN
                        PUT-BITS( iresult, icbit, 2 ) = 1.
                    ELSE IF i1 + i2 = 4 THEN
                            PUT-BITS( iresult, icbit, 2 ) = 2 .
            END.
        END.
   
        RETURN iresult.

    END METHOD. /*End of METHOD BinaryXOR */
        
    METHOD PUBLIC STATIC CHARACTER intToHex (i_iint AS INT64):
        DEFINE VARIABLE chex  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE rbyte AS RAW       NO-UNDO.
        
        DO WHILE i_iint > 0:
            PUT-BYTE( rbyte, 1 ) = i_iint MODULO 256.
            chex = STRING( HEX-ENCODE( rbyte ) ) + chex.
            i_iint = TRUNCATE( i_iint / 256, 0 ).
        END.
        RETURN chex.
    END METHOD.

END CLASS.