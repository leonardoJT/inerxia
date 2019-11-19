DEFINE VAR psecuencia AS INTEGER.

DEFINE TEMP-TABLE movCont
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD comentario AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cen_costos AS INTEGER INITIAL 999.

DEFINE VAR sumaDB AS DECIMAL.
DEFINE VAR sumaCR AS DECIMAL.

INPUT FROM D:\Leonardo\DocumentoAjuste.csv.

REPEAT :
    CREATE movCont.
    IMPORT DELIMITER ";" movCont.

    /*DISPLAY movCont WITH 1 COL.*/

    sumaDB = sumaDB + movCont.db.
    sumaCR = sumaCR + movCont.cr.
END.

FIND FIRST comprobantes WHERE comprobantes.comprobante = 10
                          AND comprobantes.agencia = 1 NO-ERROR.
IF AVAILABLE comprobantes THEN DO:
    comprobantes.secuencia = comprobantes.secuencia + 1.
    pSecuencia = comprobantes.secuencia.
    /*pSecuencia = 1.*/

    FOR EACH movCont WHERE movCont.cuenta <> "":
        CREATE mov_contable.
        BUFFER-COPY movCont TO mov_contable.
    
        ASSIGN Mov_Contable.Destino = movCont.agencia
               Mov_Contable.Comprobante = comprobantes.comprobante
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Usuario = "desarrollo"
               Mov_Contable.Estacion = "005".
     END.
END.

MESSAGE "Hecho!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
