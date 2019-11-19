DEFINE VAR psecuencia AS INTEGER.

DEFINE TEMP-TABLE movCont
    FIELD agencia AS INTEGER
    FIELD fec_contable AS DATE
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD comentario AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cen_costos AS INTEGER INITIAL 999.

DEFINE VAR sumaDB AS DECIMAL.
DEFINE VAR sumaCR AS DECIMAL.

INPUT FROM C:\Info_Crearcoop\DocumentoAjuste.csv.

REPEAT :
    CREATE movCont.
    IMPORT DELIMITER ";" movCont.

    DISPLAY movCont WITH 1 COL.

    sumaDB = sumaDB + movCont.db.
    sumaCR = sumaCR + movCont.cr.
END.

FOR EACH movCont WHERE movCont.cuenta <> "":
    CREATE mov_contable.
    BUFFER-COPY movCont TO mov_contable.

    ASSIGN Mov_Contable.Destino = movCont.agencia
           Mov_contable.Doc_referencia = "2305"
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005".
END.

MESSAGE "Hecho!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
