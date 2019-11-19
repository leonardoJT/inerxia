DEFINE VAR psecuencia AS INTEGER.

DEFINE TEMP-TABLE movCont
    FIELD agencia AS INTEGER
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD comentario AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cen_costos AS INTEGER.

DEFINE VAR sumaDB AS DECIMAL.
DEFINE VAR sumaCR AS DECIMAL.

INPUT FROM D:\Leonardo\DocumentoAjuste.csv.

REPEAT :
    CREATE movCont.
    IMPORT DELIMITER ";" movCont.

    DISPLAY movCont WITH 1 COL.

    sumaDB = sumaDB + movCont.db.
    sumaCR = sumaCR + movCont.cr.
END.

FOR EACH movCont:
    CREATE mov_contable.
    BUFFER-COPY movCont TO mov_contable.
    
    ASSIGN Mov_Contable.Destino = movCont.agencia
           Mov_Contable.Comprobante = movCont.comprobante
           Mov_Contable.Num_Documento = movCont.num_documento
           Mov_contable.Doc_referencia = STRING(movCont.num_documento)
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005".
END.

MESSAGE "Hecho!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
