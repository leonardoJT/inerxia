DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF mov_contable2.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vFecha AS DATE FORMAT "99/99/9999" INITIAL 04/29/2011.

FOR EACH mov_contable WHERE mov_contable.agencia >= 0
                        AND mov_contable.cuenta <> ""
                        AND mov_contable.fec_contable = vFecha:
    cont = cont + 1.

    CREATE mov_contable2.

    mov_contable2.agencia = mov_contable.agencia.
    mov_contable2.cen_costos = mov_contable.cen_costos.
    mov_contable2.cliente_id = mov_contable.nit.
    mov_contable2.comentario = mov_contable.comentario.
    mov_contable2.comprobante = mov_contable.comprobante.
    mov_contable2.cr = mov_contable.cr.
    mov_contable2.created_at = NOW.
    mov_contable2.cuenta = mov_contable.cuenta.
    mov_contable2.db = mov_contable.db.
    mov_contable2.doc_referencia = mov_contable.doc_referencia.
    mov_contable2.fec_contable = mov_contable.fec_contable.
    mov_contable2.num_documento = mov_contable.num_documento.
    mov_contable2.usuario = mov_contable.usuario.

    DELETE mov_contable.
END.

MESSAGE STRING(vFecha,"99/99/9999") "-" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
