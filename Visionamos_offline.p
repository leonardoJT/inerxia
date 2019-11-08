DEFINE VARIABLE nomcontrol AS CHARACTER.
DEFINE VARIABLE nomcliente AS CHARACTER.
DEFINE VARIABLE nomcuentas AS CHARACTER.
DEFINE VARIABLE nomtarjeta AS CHARACTER.
DEFINE VARIABLE nomtarvcta AS CHARACTER.
DEFINE VARIABLE CFECHA AS CHARACTER FORMAT "X(8)".
DEFINE VARIABLE CFECHA1 AS CHARACTER FORMAT "X(8)".
DEFINE VARIABLE cupo AS DECIMAL.
DEFINE VARIABLE concli AS INTEGER.
DEFINE VARIABLE concta AS INTEGER.
DEFINE VARIABLE vcSexo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vi AS INTEGER     NO-UNDO.

DEFINE VARIABLE vcComando AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcNomFile7z AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcRaiz AS CHARACTER /* INITIAL "c:\visionamos\20110216.00000018" */  NO-UNDO.
DEFINE VARIABLE vcPwd AS CHARACTER  NO-UNDO.

ASSIGN vcRaiz = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018".

ASSIGN  vcComando = "C:\7-Zip\7z"
        vcNomFile7z = vcRaiz + ".7z"
        vcRaiz = vcRaiz + "*".

OS-COMMAND SILENT DEL VALUE(vcRaiz).


ASSIGN nomcontrol = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018.F".

ASSIGN nomcliente = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018.FCL".

ASSIGN nomcuentas = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018.FAC".

ASSIGN nomtarjeta = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018.FCA".
ASSIGN nomtarvcta = "c:\visionamos\" +
                    trim(string(YEAR(TODAY))) + 
                    trim(string(month(TODAY),"99")) +
                    trim(string(day(TODAY),"99")) + ".00000018.FCVA".


OUTPUT TO VALUE(NOMCLIENTE).
FOR EACH CLIENTES:
    FIND FIRST creditos WHERE creditos.nit = clientes.nit and
                              creditos.COD_CREDITO = 123  AND
                              creditos.sdo_capital GT 0 NO-LOCK NO-ERROR.                                   
    IF NOT AVAILABLE(creditos)  THEN NEXT.

    CFECHA  = trim(string(YEAR(FEC_NACIMIENTO))) + 
              trim(string(month(FEC_NACIMIENTO), "99")) +
              trim(string(day(FEC_NACIMIENTO), "99")).

    CFECHA1 = trim(string(YEAR(TODAY))) + 
              trim(string(month(TODAY), "99")) +
              trim(string(day(TODAY), "99")).

    IF CFECHA EQ ?  THEN ASSIGN CFECHA = "".
    IF CFECHA1 EQ ?  THEN ASSIGN CFECHA1 = "".
       
    ASSIGN vi = INDEX(email,',',1).
    IF vi GT 0 THEN
        UPDATE email = SUBSTRING(email,1,(vi - 1)).
    
    ASSIGN vcSexo = IF CLIENTES.SEXO EQ 1 THEN "M" ELSE "F".


    concli = concli + 1.
    PUT UNFORMATTED "+" ","
        TRIM(CLIENTES.NIT) ","
        "0"          ","
        TRIM(CLIENTES.NOMBRE) ","
        ""             ","
        TRIM(CLIENTES.APELLIDO1) ","
        TRIM(CLIENTES.APELLIDO2) ","
        TRIM(CLIENTES.DIR_RESIDENCIA) ","
        TRIM(CLIENTES.DIR_COMERCIAL) ","
        TRIM(CLIENTES.TEL_RESIDENCIA) ","
        TRIM(CLIENTES.TEL_COMERCIAL) ","
        TRIM(CLIENTES.CELULAR) ","
        TRIM(CFECHA) ","
        TRIM(vcSexo) ","
        TRIM(CLIENTES.EMAIL) ","
        "" /* "COLOMBIA"     */  ","
        "" /* "CUNDINAMARCA" */  ","
        "" /* "BOGOTA"       */  ","
        "" /* "COLOMBIA"     */  ","
        "" /* "CUNDINAMARCA" */  ","
        "" /* "BOGOTA"       */  ","
        "0000"          ","
        TRIM(CFECHA1) SKIP.

/*         DISPLAY "," "+" CLIENTES.NIT  "0" CLIENTES.NOMBRE "" CLIENTES.APELLIDO1 CLIENTES.APELLIDO2 CLIENTES.DIR_RESIDENCIA */
/*         CLIENTES.DIR_COMERCIAL  CLIENTES.TEL_RESIDENCIA CLIENTES.TEL_COMERCIAL  CLIENTES.CELULAR CFECHA                    */
/*         vcSexo CLIENTES.EMAIL "COLOMBIA" "CUNDINAMARCA" "BOGOTA" "COLOMBIA" "CUNDINAMARCA" "BOGOTA" "0000" CFECHA1 .       */
END.
OUTPUT CLOSE.

OUTPUT TO VALUE(NOMCUENTAS).
FOR EACH CLIENTES:
    FIND FIRST creditos WHERE creditos.nit = clientes.nit and
                              creditos.COD_CREDITO = 123  AND
                              creditos.sdo_capital GT 0 NO-LOCK NO-ERROR.                                   
    IF AVAILABLE(creditos)  THEN DO:
        cupo = (creditos.monto - creditos.sdo_capital) * 100.
        IF cupo LT 0 THEN cupo = 0.
        concta = concta + 1.
        PUT UNFORMATTED "+"  ","
            "00000018" ","
            TRIM(creditos.nit) ","
            TRIM(STRING(creditos.num_credito)) ","
            "50" ","
            "1" ","
            "170" ","
            "1000000000" ","
            cupo /* FORMAT "->>>>>>>>>>>>" */ ","
            "1000"  ","
            "1000" ","
            cupo  /* FORMAT "->>>>>>>>>>>>" */ ","
            cupo  /* FORMAT "->>>>>>>>>>>>" */ ","
            TRIM(cfecha1) SKIP.
    END.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE(NOMTARJETA).
OUTPUT CLOSE.
OUTPUT TO VALUE(NOMTARVCTA).
OUTPUT CLOSE.

OUTPUT TO VALUE(NOMCONTROL).
PUT UNFORMATTED substring(nomcliente,15) /* FORMAT "X(25)"  */ "," concli SKIP.
PUT UNFORMATTED substring(nomcuentas,15) /* FORMAT "X(25)"  */ "," concta SKIP.
PUT UNFORMATTED substring(nomtarjeta,15) /* FORMAT "X(25)"  */ "," 0 SKIP.
PUT UNFORMATTED substring(nomtarvcta,15) /* FORMAT "X(25)"  */ "," 0 SKIP.
OUTPUT CLOSE.


OS-COMMAND SILENT VALUE(vcComando) a VALUE(vcNomFile7z) VALUE(vcRaiz) -r -p0123456789ABCDEF0123456789ABCDEF.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

