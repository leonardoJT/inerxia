DEFINE TEMP-TABLE movs
    FIELD cedula AS CHARACTER
    FIELD cr AS DECIMAL
    FIELD db AS DECIMAL.

DEFINE VAR vTotal AS DECIMAL.
DEFINE VAR pAgencia AS INTEGER INITIAL 1.
DEFINE VAR pSecuencia AS INTEGER.

INPUT FROM C:\INFO_FODUN\trabajo.csv.
REPEAT :
    CREATE movs.
    IMPORT DELIMITER ";" movs.
END.

FIND FIRST comprobantes WHERE comprobantes.agencia = pAgencia
                          AND comprobantes.comprobante = 4 NO-ERROR.
IF AVAILABLE comprobantes THEN DO:
    comprobantes.secuencia = comprobantes.secuencia + 1.
    pSecuencia = comprobantes.secuencia.
END.


FOR EACH movs NO-LOCK:
    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = 999
           Mov_Contable.Agencia = pAgencia
           Mov_Contable.Destino = pAgencia
           Mov_Contable.Comprobante = 4
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = 09/30/2011
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "24953001"
           Mov_Contable.Comentario = "Ajuste Contable"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           Mov_contable.db = movs.db
           Mov_contable.cr = movs.cr
           mov_contable.nit = movs.cedula.

    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = 999
           Mov_Contable.Agencia = pAgencia
           Mov_Contable.Destino = pAgencia
           Mov_Contable.Comprobante = 4
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = 09/30/2011
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "21050501"
           Mov_Contable.Comentario = "Ajuste Contable"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           Mov_contable.db = movs.cr
           Mov_contable.cr = movs.db
           mov_contable.nit = movs.cedula.
END.
