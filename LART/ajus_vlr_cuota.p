DEFINE TEMP-TABLE CRDT
   FIELD Agencia       AS CHAR FORMAT "X(3)"
   FIELD NIT           AS CHAR FORMAT "X(20)"
   FIELD codcredito    AS CHAR FORMAT "X(4)"
   FIELD NUMEROCREDITO AS CHAR FORMAT "X(14)"
   FIELD wcuotas       AS INTE FORMAT ">>>>9"
   FIELD cuotaan       LIKE Creditos.Plazo 
   FIELD estado        LIKE Creditos.Estado.

DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEF BUFFER BCRDT FOR CRDT.

INPUT FROM C:\CIFIN\Abril302011\aju-vlor-cuota.csv.
REPEAT:                         
    CONT = CONT + 1.
    CREATE CRDT.    
    IMPORT DELIMITER ";" CRDT.
    FIND FIRST Creditos WHERE Creditos.Nit = CRDT.NIT AND  
                              Creditos.Num_Credito = INT(CRDT.NUMEROCREDITO) 
            NO-LOCK NO-ERROR.
    IF AVAILABLE Creditos THEN
    DO:
        DISPLAY Creditos.Fec_Desembolso 
                Creditos.Fec_Pago 
                Creditos.Fec_PagAnti
                CRDT.wcuotas
                WITH WIDTH 250.
        PAUSE 0.
        ASSIGN  CRDT.agencia = trim(STRING(Creditos.agencia)).
        ASSIGN  CRDT.codcredito = trim(string(Creditos.Cod_Credito)).
    END.
    ELSE DO:
        MESSAGE "No existe Credito Nit: " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        /* ASSIGN  CRDT.cuotaan = Creditos.Plazo. */
    END.
END.
INPUT CLOSE.

OUTPUT TO C:\CIFIN\AJUSTES\abril2011\salida.csv.
wtitulo = "agencia;NIT;Linea;Num-Credito;cuotas-sfg;cuotaa-Feb;Estado".
DISPLAY wtitulo NO-LABEL WITH WIDTH 400. 
FOR EACH CRDT NO-LOCK:
    wsalida = CRDT.agencia + ";" +
              CRDT.NIT + ";" + 
              CRDT.codcredito + ";" +
              CRDT.NUMEROCREDITO + ";" + 
              trim(string(CRDT.wcuotas)) + ";" +
              trim(string(CRDT.cuotaan)) + ";" + 
              trim(string(CRDT.Estado)).
    DISPLAY wsalida  NO-LABEL WITH WIDTH 400.
END.
OUTPUT CLOSE.
