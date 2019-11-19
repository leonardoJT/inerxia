DEFINE TEMP-TABLE CRDT
   FIELD NIT           AS CHAR FORMAT "X(20)"
   FIELD codcredito    AS CHAR FORMAT "X(4)"
   FIELD NUMEROCREDITO AS CHAR FORMAT "X(14)"
   FIELD fec_desembolso LIKE Creditos.Fec_Desembolso
   FIELD fec_pago       LIKE Creditos.fec_pago COLUMN-LABEL "Fecha-Ubi-sal"
   FIELD Fec_PagAnti    LIKE Creditos.Fec_PagAnti COLUMN-LABEL "Fecha-Pri-Des"
   FIELD Fec_CanceTotal LIKE Creditos.Fec_CanceTotal COLUMN-LABEL "Fecha-FIn"
   FIELD Fec_Fin_antes  LIKE Creditos.Fec_CanceTotal COLUMN-LABEL "Fecha-FIn-Old"  
   FIELD Estado         LIKE Creditos.Estado .

DEF VAR CONTADOR AS INT.
DEF VAR I AS INT.
DEF VAR CONT AS INT.
DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEF BUFFER BCRDT FOR CRDT.

INPUT FROM C:\info_fodun\Ajuste_creditos-viany\ajus-fect-terminacion-bta.csv.
REPEAT:                         
    CONT = CONT + 1.
    CREATE CRDT.    
    IMPORT DELIMITER ";" CRDT.
    FIND FIRST Creditos WHERE Creditos.Nit = CRDT.NIT AND  
                              Creditos.Cod_Credito = INT(CRDT.codcredito) AND
                              Creditos.Num_Credito = INT(CRDT.NUMEROCREDITO) 
                              AND Creditos.Estado EQ 2  
             NO-ERROR.
    IF AVAILABLE Creditos THEN
    DO:
        IF Creditos.Fec_Pago NE CRDT.Fec_pago THEN DO:
            DISPLAY Creditos.Fec_Pago
                    CRDT.Fec_pago
                    Creditos.Fec_PagAnti
                    CRDT.Fec_PagAnti
                    WITH WIDTH 250.
            PAUSE 0.
            ASSIGN  CRDT.Fec_CanceTotal =  Creditos.fec_pago.
            ASSIGN  CRDT.Estado =  Creditos.Estado.
            /* ASSIGN  Creditos.Fec_Pago = CRDT.Fec_pago. */ 
        END.
        IF Creditos.Estado EQ 2  THEN DO:
/*             MESSAGE "Credito No Cancelado " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " + */
/*                     string(CRDT.fec_desembolso)                                                                          */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                       */
        END.
        ELSE DO:
            MESSAGE "Credito Cancelado " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " +
                    string(CRDT.fec_desembolso)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
              /*  Actulizar Fecha Terminacion  */ 
        END.

/*         ASSIGN Creditos.Fec_PagAnti = CRDT.Fec_PagAnti */
/*                Creditos.Fec_Pago = CRDT.Fec_Pago       */
        
    END.
    ELSE DO:
        MESSAGE "No existe Credito CAncelado Nit: " + CRDT.NIT + " Codigo: " + codcredito + " Numero: " + NUMEROCREDITO +  " " +
                string(CRDT.fec_desembolso)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
INPUT CLOSE.

OUTPUT TO C:\INFO_FODUN\salidax.csv.
wtitulo = "NIT;Linea;Num-Credito;Fec_CanceTotal;Fec_Fin_antes;Estado".
DISPLAY wtitulo NO-LABEL WITH WIDTH 400.
FOR EACH CRDT NO-LOCK:
    wsalida = CRDT.NIT + ";" +
              CRDT.codcredito + ";" +
              CRDT.NUMEROCREDITO + ";" +
              trim(string(CRDT.Fec_CanceTotal)) + ";" +
              trim(string(CRDT.Fec_Fin_antes)) + ";" + trim(STRING(CRDT.estado)) .
    DISPLAY wsalida  NO-LABEL WITH WIDTH 400.
END.
OUTPUT CLOSE.
