FOR EACH MOV_CONTABLE WHERE COMPROBANTE = 22 AND DB GT 0 NO-LOCK BY fec_contable:
    DISP  CUENTA NIT DOC_REFERENCIA DB .
   /* FIND FIRST CREDITOS WHERE CREDITOS.NIT = MOV_CONTABLE.NIT AND
                              CREDITOS.NUM_CREDITO = INTEGER(MOV_CONTABLE.DOC_REFERENCIA) NO-LOCK NO-ERROR.
    IF AVAILABLE(CREDITOS) THEN DO:
        
        CREATE Mov_Creditos.
         ASSIGN Mov_Creditos.Agencia       = Creditos.Agencia
                Mov_Creditos.Cod_Credito   = Creditos.Cod_Credito
                Mov_Creditos.Nit           = Creditos.Nit
                Mov_Creditos.Num_Credito   = Creditos.Num_Credito
                Mov_Creditos.Cod_Operacion = 020102001
                Mov_Creditos.Ofi_Destino   = Creditos.Agencia
                Mov_Creditos.Ofi_Fuente    = Creditos.Agencia
                Mov_Creditos.Pagare        = Creditos.Pagare
                Mov_Creditos.Fecha         = MOV_CONTABLE.FEC_CONTABLE
                Mov_Creditos.Hora          = TIME
                Mov_Creditos.Num_Documento = STRING (MOV_CONTABLE.NUM_DOCUMENTO)
                Mov_Creditos.Usuario       = MOV_contable.usuario
                Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
                Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                Mov_Creditos.Sdo_Capital   = Creditos.Sdo_Capital
                Mov_Creditos.Val_Efectivo  = mov_contable.db
                Mov_Creditos.Cpte          = 22
                MOV_creditos.Descrip       = mov_contable.comentario.
    END.
    ELSE DISP CREDITOS WITH 1 COL.*/
END.
