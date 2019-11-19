DEFINE VAR pAgencia AS INTEGER.
DEFINE VAR pSecuencia AS INTEGER.

pAgencia = 1.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FIND FIRST comprobantes WHERE comprobantes.agencia = pAgencia
                          AND comprobantes.comprobante = 4 NO-ERROR.
IF AVAILABLE comprobantes THEN DO:
    comprobantes.secuencia = comprobantes.secuencia + 1.
    pSecuencia = comprobantes.secuencia.
END.

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = pAgencia
       Mov_Contable.Destino = pAgencia
       Mov_Contable.Comprobante = 4
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = STRING(pSecuencia)
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "24051001"
       Mov_Contable.Comentario = "Ajuste Ahorros vs. Cuentas"
       Mov_Contable.Usuario = "2305"
       Mov_Contable.Estacion = "005"
       Mov_contable.db = 16949595.87.

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = pAgencia
       Mov_Contable.Destino = pAgencia
       Mov_Contable.Comprobante = 4
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = STRING(pSecuencia)
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "24050501"
       Mov_Contable.Comentario = "Ajuste Ahorros vs. Cuentas"
       Mov_Contable.Usuario = "2305"
       Mov_Contable.Estacion = "005"
       Mov_contable.db = 1252.

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = pAgencia
       Mov_Contable.Destino = pAgencia
       Mov_Contable.Comprobante = 4
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = STRING(pSecuencia)
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "21101001"
       Mov_Contable.Comentario = "Ajuste Ahorros vs. Cuentas"
       Mov_Contable.Usuario = "2305"
       Mov_Contable.Estacion = "005"
       Mov_contable.cr = 16950847.87.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
