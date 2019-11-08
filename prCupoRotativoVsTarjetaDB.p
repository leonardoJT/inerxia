DEFINE VARIABLE vlahorros AS LOGICAL  INITIAL FALSE  NO-UNDO.
DEFINE TEMP-TABLE TemCupoTarj
    FIELD TAgenCupo    LIKE mov_creditos.agencia
    FIELD TNit         LIKE mov_creditos.nit
    FIELD TNum_Credito LIKE mov_creditos.num_credito
    FIELD TMonto       AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD TAgenTarj    LIKE mov_creditos.agencia
    FIELD TCuenta      AS CHARACTER FORMAT "X(20)"
    FIELD TComenta     AS CHARACTER FORMAT "X(40)"
    INDEX idxagenit TAgenCupo TNit TNum_Credito.

EMPTY TEMP-TABLE TemCupoTarj.
FOR EACH creditos WHERE
         creditos.estado      EQ 2   AND
         creditos.cod_credito EQ 570 NO-LOCK:
    ASSIGN vlahorros = FALSE.
    FOR EACH ahorros WHERE
         ahorros.estado    EQ 1 AND
         ahorro.cod_ahorro EQ 3 AND
         ahorros.nit       EQ creditos.nit NO-LOCK:
         ASSIGN vlahorros = TRUE.
         CREATE TemCupoTarj.
         UPDATE TemCupoTarj.TAgenCupo     = creditos.agencia
                TemCupoTarj.TNit          = creditos.nit
                TemCupoTarj.TNum_Credito  = creditos.num_credito
                TemCupoTarj.TMonto        = creditos.monto
                TemCupoTarj.TAgenTarj     = ahorros.agencia
                TemCupoTarj.TCuenta       = ahorros.cue_ahorros
                TemCupoTarj.TComenta      = "".
         IF ahorros.tarjetadb = "" THEN
            UPDATE TemCupoTarj.TComenta = "No Maneja Tarjeta".
         ELSE
            IF ahorros.tarjetadb BEGINS "Tarj.db.solicit." THEN
               UPDATE TemCupoTarj.TComenta = "Tarjeta En trámite".
            ELSE
               IF ahorros.tarjetadb BEGINS "4" THEN
                  UPDATE TemCupoTarj.TComenta = "Tarjeta Asignada".
    END.
    IF NOT vlahorros THEN DO:
       CREATE TemCupoTarj.
       UPDATE TemCupoTarj.TAgenCupo     = creditos.agencia
              TemCupoTarj.TNit          = creditos.nit
              TemCupoTarj.TNum_Credito  = creditos.num_credito
              TemCupoTarj.TMonto        = creditos.monto
              TemCupoTarj.TAgenTarj     = 000
              TemCupoTarj.TCuenta       = "0-No Existe"
              TemCupoTarj.TComenta      = "No Tiene Cuenta de Ahorros".
    END.
END.


FOR EACH TemCupoTarj NO-LOCK:
    FORM
        TAgenCupo      COLUMN-LABEL "Age.Cupo"    FORMAT "999"
        TNit           COLUMN-LABEL "Nit"         FORMAT "X(14)"
        TNum_Credito   COLUMN-LABEL "Credito"     FORMAT "9999999999"
        TMonto         COLUMN-LABEL "Monto"       FORMAT  "->>>,>>>,>>>,>>9.99"
        TAgenTarj      COLUMN-LABEL "Age.Ahorro"  FORMAT "999"
        TCuenta        COLUMN-LABEL "Cue.Ahorro"  FORMAT "X(15)"
        TComenta       COLUMN-LABEL "Observacion" FORMAT "X(40)"
    WITH FRAME FTemCupo DOWN COLUMN 1 WIDTH 132
    NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY 
        TAgenCupo    
        TNit             
        TNum_Credito     
        TMonto           
        TAgenTarj        
        TCuenta          
        TComenta    
    WITH FRAME FTemCupo.
END.
/*OUTPUT TO "c:\comercial\cuporotatarjDb.txt".                                                                             
PUT "Descripcion;Cod.Age;Agencia;Nit;Nombres;Cuenta_Ahorros;Saldo_AH;Nro.TarjetaDB;Fecha_Creacion;Nro.Credito;Monto_CR;Saldo_CR;Disponible" SKIP(0). */
