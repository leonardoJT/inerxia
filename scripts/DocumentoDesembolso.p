
DEFINE VAR pAgencia AS INTEGER.
DEFINE VAR pSecuencia AS INTEGER.

pAgencia = 3.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FIND FIRST comprobantes WHERE comprobantes.agencia = pAgencia
                          AND comprobantes.comprobante = 13 NO-ERROR.
IF AVAILABLE comprobantes THEN DO:
    comprobantes.secuencia = comprobantes.secuencia + 1.
    pSecuencia = comprobantes.secuencia.
END.

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = 3
       Mov_Contable.Destino = 3
       Mov_Contable.Comprobante = 13
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = "2791"
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "91250501"
       Mov_Contable.Comentario = "Reversión Monto Soli"
       Mov_Contable.Usuario = "14"
       Mov_Contable.Estacion = "005"
       Mov_contable.cr = 4000000
       mov_contable.nit = "10286414"
       enlace = "2791".


CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = 3
       Mov_Contable.Destino = 3
       Mov_Contable.Comprobante = 13
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = "2791"
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "91250501"
       Mov_Contable.Comentario = "Reversión Monto Soli"
       Mov_Contable.Usuario = "14"
       Mov_Contable.Estacion = "005"
       Mov_contable.db = 4000000
       mov_contable.nit = "10286414"
       enlace = "2791".

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = 3
       Mov_Contable.Destino = 3
       Mov_Contable.Comprobante = 13
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = "2791"
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "11100534"
       Mov_Contable.Comentario = "Desemb.en cheque"
       Mov_Contable.Usuario = "14"
       Mov_Contable.Estacion = "005"
       Mov_contable.cr = 4000000
       mov_contable.nit = "10286414"
       enlace = "2791".

CREATE mov_contable.
ASSIGN mov_contable.cen_costos = 999
       Mov_Contable.Agencia = 3
       Mov_Contable.Destino = 3
       Mov_Contable.Comprobante = 13
       Mov_Contable.Num_Documento = pSecuencia
       Mov_contable.Doc_referencia = "2791"
       Mov_Contable.Fec_Contable = TODAY
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = "14420503"
       Mov_Contable.Comentario = "Desembolso del credi"
       Mov_Contable.Usuario = "14"
       Mov_Contable.Estacion = "005"
       Mov_contable.db = 4000000
       mov_contable.nit = "10286414"
       enlace = "2791".
       
       
/*FOR EACH mov_contable WHERE fec_contable = TODAY
                        AND comprobante = 13
                        AND usuario = "10"
                        AND comentario = "Desemb.en cheque":
    UPDATE mov_contable WITH WIDTH 200 1 COL.

END.*/
