DEFINE TEMP-TABLE CRDT
   FIELD CODIGO     AS CHAR FORMAT "X(10)"       
   FIELD NUMEROCREDITO  AS CHAR FORMAT "X(10)"
   FIELD NIT            AS CHAR FORMAT "X(15)"
   FIELD estado         AS CHAR FORMAT "X(2)"
   FIELD fec1 AS CHAR FORMAT    "X(20)"
   FIELD fec2 AS CHAR FORMAT "X(20)"
   FIELD fec3 AS CHAR FORMAT "X(20)"
   FIELD fec4 AS CHAR FORMAT "X(20)"
    .
DEFINE TEMP-TABLE FEBR
   FIELD NIT2            AS CHAR FORMAT "X(15)"
   FIELD NUMERO         AS CHAR FORMAT "X(10)"
   FIELD feci           AS CHAR FORMAT    "X(20)"
   FIELD fecf           AS CHAR FORMAT "X(20)"
   .


DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wfecha LIKE Creditos.Fec_Aprobacion FORMAT "99/99/9999". 
DEFINE VAR wano AS CHAR FORMAT "XXXX".
DEFINE VAR wmes AS CHAR FORMAT "XX".
DEFINE VAR wdia AS CHAR FORMAT "XX".

DEFINE VAR wfecha2 LIKE Creditos.Fec_CanceTotal FORMAT "99/99/9999". 
DEFINE VAR wano2 AS CHAR FORMAT "XXXX".
DEFINE VAR wmes2 AS CHAR FORMAT "XX".
DEFINE VAR wdia2 AS CHAR FORMAT "XX".


DEF BUFFER BCRDT FOR CRDT.

/* INPUT FROM U:\bd\CIDIN\ajuste.csv. */
/* INPUT FROM C:\CIFIN\AJUSTES\FEB2011.csv. */
INPUT FROM C:\CIFIN\AJUSTES\CIFINSEPTIEMBRE.csv.
REPEAT:    
    /* CONT = CONT + 1. */
    CREATE FEBR.    
    IMPORT DELIMITER ";" FEBR.
    wano = SUBSTRING(feci,1,4) .
    wmes = SUBSTRING(feci,5,2) .
    wdia = SUBSTRING(feci,7,2) .
    wfecha = DATE(TRIM(wdia) +  trim(wmes) + trim(wano)).

    wano2 = SUBSTRING(fecf,1,4) .
    wmes2 = SUBSTRING(fecf,5,2) .
    wdia2 = SUBSTRING(fecf,7,2) .
    wfecha2 = DATE(TRIM(wdia2) +  trim(wmes2) + trim(wano2)).

    /* DISPLAY  wfecha.  */ 
    /* wfecha  = DATE(wdia + "/" + wmes + "/" + wano). */
    FIND FIRST CREDITOS WHERE Creditos.Num_Credito = INT (FEBR.NUMERO) AND 
                              DECIMAL(Creditos.Nit ) EQ DECIMAL(FEBR.NIT2)  
                              NO-ERROR.
    /* IF AVAILABLE Creditos AND Creditos.Estado NE 2  THEN */
    IF AVAILABLE Creditos  THEN
    DO:
        IF Creditos.Fec_Desembolso EQ ? THEN  DO:
            DISPLAY Creditos.Nit 
                    Creditos.Num_Credito 
                    Creditos.Fec_Desembolso
                    wfecha 
                    Creditos.Fec_CanceTotal 
                    wfecha2
                    Creditos.Estado VIEW-AS TEXT WITH WIDTH 200.
            PAUSE 0.  
            ASSIGN Creditos.Fec_Desembolso = wfecha.  
            /* ASSIGN Creditos.Fec_CanceTotal = wfecha2.*/ 
            CONT = CONT + 1.
        END.
    END.
    ELSE DO:
/*         MESSAGE "No existe Credito, Nit:   " + FEBR.nit2 + " Nit  : " +  FEBR.NIT2 */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                     */
    END.
END.
INPUT CLOSE.

MESSAGE "Registros Encontrados "  STRING(cont)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
FOR EACH FEBR NO-LOCK:
    DISPLAY FEBR WITH WIDTH 500.
    
END.

/* 
OUTPUT TO c:\INFO_juriscoop\ctaerror.txt.
FOR EACH crdt:
    IF valdetalle <> 0 THEN
        DISPLAY
            crdt.nit
            crdt.numerocredito
            crdt.agencia.
END.


OUTPUT CLOSE.
*/ 
