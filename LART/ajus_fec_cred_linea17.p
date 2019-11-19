DEFINE TEMP-TABLE CRDT
   FIELD Agencia       AS CHAR FORMAT "X(3)"
   FIELD fec_desembolso LIKE Creditos.Fec_Desembolso
   FIELD NIT           AS CHAR FORMAT "X(20)"
   FIELD codcredito    AS CHAR FORMAT "X(4)"
   FIELD NUMEROCREDITO AS CHAR FORMAT "X(14)"
   FIELD plazo         LIKE Creditos.Plazo 
   FIELD Fec_PagAnti   LIKE Creditos.Fec_PagAnti COLUMN-LABEL "Fecha-Ubi-sal"
   FIELD fec_pago      LIKE Creditos.Fec_PagAnti COLUMN-LABEL "Fecha-Pri-descto" .

DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEF BUFFER BCRDT FOR CRDT.

INPUT FROM C:\info_fodun\Ajuste_creditos-viany\aju-plazofechas.csv.
REPEAT:                         
    CONT = CONT + 1.
    CREATE CRDT.    
    IMPORT DELIMITER ";" CRDT.
    FIND FIRST Creditos WHERE Creditos.Nit = CRDT.NIT AND  
                              Creditos.Cod_Credito = INT(CRDT.codcredito) AND
                              Creditos.Num_Credito = INT(CRDT.NUMEROCREDITO) 
             NO-ERROR.
    IF AVAILABLE Creditos THEN
    DO:
        DISPLAY Creditos.Fec_Desembolso 
                Creditos.Fec_Pago 
                Creditos.Fec_PagAnti
                WITH WIDTH 250.
        PAUSE 0.
        ASSIGN Creditos.Fec_PagAnti = CRDT.Fec_PagAnti
               Creditos.Fec_Pago = CRDT.Fec_Pago
               Creditos.plazo = CRDT.Plazo.
    END.
    ELSE DO:
        IF year(CRDT.fec_desembolso) EQ 2011 AND  MONTH(CRDT.fec_desembolso) EQ 5  THEN DO:
            NEXT.
        END.
        MESSAGE "No existe Credito Nit: " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " +
                string(CRDT.fec_desembolso)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
INPUT CLOSE.

OUTPUT TO C:\INFO_FODUN\salidax.csv.
wtitulo = "NIT;Linea;Num-Credito;cuotas-ant;cuotaa-Sfg;Estado".
DISPLAY wtitulo NO-LABEL WITH WIDTH 400.
FOR EACH CRDT NO-LOCK:
    wsalida = CRDT.NIT + ";" +
              CRDT.codcredito + ";" +
              CRDT.NUMEROCREDITO + ";" +
              trim(string(CRDT.plazo)) + ";" +
              trim(string(CRDT.Fec_PagAnti)) + ";" +
              trim(string(CRDT.Fec_Pago)).
    DISPLAY wsalida  NO-LABEL WITH WIDTH 400.
END.
OUTPUT CLOSE.
