/* Geenra Reporte de Documentos descuadros x mes    */ 
DEFINE TEMP-TABLE docu
   FIELD wagencia   LIKE Mov_Contable.agencia
   FIELD wcomprob   LIKE Mov_Contable.Comprobante 
   FIELD wfecha     LIKE Mov_Contable.Fec_Contable
   FIELD wdocu      LIKE Mov_Contable.Num_Documento
   FIELD wdebitos   LIKE Mov_Contable.Db 
   FIELD wcreditos  LIKE Mov_Contable.Cr
   FIELD wdife      LIKE Mov_Contable.Db
   INDEX documento IS UNIQUE wagencia wcomprob wfecha wdocu.
DISPLAY plazo.
DEFINE VAR wdb LIKE Sal_C              WISDSDuenta.Db[1] no-undo.
DEFINE VAR wcr LIKE Sal_Cuenta.Cr[1] NO-UNDO.
DEFINE VAR wdi LIKE Sal_Cuenta.sal_inicial NO-UNDO.
DEFINE VAR wre AS RECID.
DEFINE VAR wsalida AS CHAR FORMAT "X(200)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(200)".
/* OUTPUT TO c:\INFO_fodun\veri_sal_cta_marzo.csv. */
ASSIGN wdb = 0.
ASSIGN wcr = 0.
FOR EACH    Mov_Contable WHERE year(Mov_Contable.Fec_Contable) EQ 2011 
                               AND  MONTH(Mov_Contable.Fec_Contable) EQ 5 NO-LOCK:
        FIND LAST docu WHERE wagencia EQ agencia AND wcomprob EQ Comprobante AND 
                             wfecha EQ Fec_contable AND wdocu  EQ num_documento NO-ERROR.
        IF NOT AVAILABLE docu  THEN DO:
            CREATE docu.
            ASSIGN docu.wagencia = Mov_Contable.agencia  
                   docu.wcomprob = Mov_Contable.Comprobante
                   docu.wfecha   = Mov_Contable.Fec_Contable
                   docu.wdocu    = Mov_Contable.Num_Documento.
        END.
        ASSIGN docu.wdebitos  = docu.wdebitos + Mov_Contable.Db
               docu.wcreditos = docu.wcreditos + Mov_Contable.Cr.
        ASSIGN wdb = wdb + Mov_Contable.Db 
               wcr = wcr + Mov_Contable.Cr.
END.
wdi = wdb - wcr.
DISPLAY  wdb wcr wdi.
ASSIGN wtitulo = "Agencia;Comprobante;Fecha;Documento;Debitos;Creditos".
OUTPUT TO c:\INFO_fodun\documnetos_mayo2011.csv.
/* DISPLAY wtitulo no-label  WITH WIDTH 300. */ 
FOR EACH docu NO-LOCK BY wfecha BY wcomprob BY wdocu :
    ASSIGN docu.wdife = docu.wdebitos - docu.wcreditos.
    IF docu.wdife NE 0 THEN DO:
        ASSIGN wsalida = STRING(wagencia) + ";" +
               string(wcomprob) + ";" +  
               STRING(wfecha) + ";" + 
               string(wdocu,"9999999999999")  + ";" +  
               string(wdb,"-9999999999999999.99") + ";" +  
               string(wcr,"-9999999999999999.99")   . 
        DISPLAY wsalida 
            NO-LABEL WITH WIDTH  300.
    END.

END.
OUTPUT CLOSE. 
