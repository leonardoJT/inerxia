DEFINE TEMP-TABLE CRDT
   FIELD NIT           AS CHAR FORMAT "X(20)"
   FIELD codcredito    AS CHAR FORMAT "X(4)"
   FIELD NUMEROCREDITO AS CHAR FORMAT "X(14)"
   FIELD fecha         LIKE Creditos.Fec_Pago COLUMN-LABEL "Fecha-cambio".

DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEF BUFFER BCRDT FOR CRDT.

INPUT FROM C:\info_fodun\manizalez.csv.
REPEAT:                                                       
    CONT = CONT + 1.
    CREATE CRDT.    
    IMPORT DELIMITER ";" CRDT.
    FIND FIRST Creditos WHERE Creditos.Nit = CRDT.NIT AND  
                              Creditos.agencia = INT(CRDT.codcredito) AND
                              Creditos.Num_Credito = INT(CRDT.NUMEROCREDITO) 
                              AND Creditos.Estado EQ 2 
             NO-ERROR.
    IF AVAILABLE Creditos  THEN
    DO:                                                 
      IF Creditos.Fec_Pago NE CRDT.fecha THEN DO:
          DISPLAY Creditos.nit
                  Creditos.Cod_Credito 
                  Creditos.Num_Credito
                  Creditos.Fec_Desembolso
                  Creditos.Fec_PagAnti LABEL "Fec Incia"
                  Creditos.Per_Pago   VIEW-AS TEXT
                  Creditos.Plazo 
                  Creditos.Tasa 
                  Creditos.Tip_Credito VIEW-AS TEXT
                  Creditos.Fec_Pago
                  CRDT.fecha LABEL "Fecha-cambio" 
                  Creditos.For_Pago VIEW-AS TEXT
                  Creditos.Tip_Credito
                  
                        WITH WIDTH 250.
                /* PAUSE 0. */ 
                /* ASSIGN Creditos.Fec_Pago = CRDT.fecha.     */ 
        END.
    END.
    ELSE
        MESSAGE "No existe Credito " + CRDT.NIT + " Numero:   " + CRDT.NUMEROCREDITO
            + " Agencia " + CRDT.codcredito  
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
INPUT CLOSE.

