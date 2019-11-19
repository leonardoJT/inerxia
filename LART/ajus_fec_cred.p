DEFINE TEMP-TABLE CRDT
   FIELD Agencia       AS CHAR FORMAT "X(3)"
   FIELD NIT           AS CHAR FORMAT "X(20)"
   FIELD codcredito    AS CHAR FORMAT "X(4)"
   FIELD NUMEROCREDITO AS CHAR FORMAT "X(14)"
   FIELD fec_desembolso LIKE Creditos.Fec_Desembolso 
   FIELD fec_ubi_saldo  LIKE Creditos.Fec_PagAnti COLUMN-LABEL "Fecha-Ubi-sal"
   FIELD fec_pri_pago   LIKE Creditos.Fec_PagAnti COLUMN-LABEL "Fecha-Pri-descto" 
   FIELD wfecha        LIKE  Creditos.Fec_UltPago COLUMN-LABEL "Fecha-Termina"
   FIELD wcuotas       AS INTE FORMAT ">>>>9"
   FIELD cuotaan       LIKE Creditos.Plazo 
   FIELD estado        LIKE Creditos.Estado.

DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEF BUFFER BCRDT FOR CRDT.

INPUT FROM C:\CIFIN\AJUSTES\abril2011\Abril_bta.csv.
REPEAT:                         
    CONT = CONT + 1.
    CREATE CRDT.    
    IMPORT DELIMITER ";" CRDT.
    FIND FIRST Creditos WHERE Creditos.Nit = CRDT.NIT AND  
                              Creditos.Cod_Credito = INT(CRDT.codcredito) AND
                              Creditos.Num_Credito = INT(CRDT.NUMEROCREDITO) 
            NO-LOCK NO-ERROR.
    IF AVAILABLE Creditos THEN
    DO:
        DISPLAY Creditos.Fec_Desembolso 
                Creditos.Fec_Pago 
                Creditos.Fec_PagAnti
                CRDT.wfecha
                WITH WIDTH 250.
        PAUSE 0.
        ASSIGN  CRDT.cuotaan = Creditos.Plazo.
    END.
    ELSE DO:
        IF year(CRDT.fec_desembolso) EQ 2011 AND  MONTH(CRDT.fec_desembolso) EQ 5  THEN DO:
            NEXT.
        END.
        MESSAGE "No existe Credito Nit: " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " +
                string(CRDT.fec_desembolso)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN  CRDT.cuotaan = Creditos.Plazo.
    END.
END.
INPUT CLOSE.

OUTPUT TO C:\CIFIN\AJUSTES\abril2011\salida.csv.
wtitulo = "NIT;Linea;Num-Credito;cuotas-ant;cuotaa-Sfg;Estado".
DISPLAY wtitulo NO-LABEL WITH WIDTH 400. 
FOR EACH CRDT NO-LOCK:
    wsalida = CRDT.NIT + ";" + 
              CRDT.codcredito + ";" +
              CRDT.NUMEROCREDITO + ";" + 
              trim(string(CRDT.wcuotas)) + ";" +
              trim(string(CRDT.cuotaan)) + ";" + 
              trim(string(CRDT.Estado)).
    DISPLAY wsalida  NO-LABEL WITH WIDTH 400.
END.
OUTPUT CLOSE.
