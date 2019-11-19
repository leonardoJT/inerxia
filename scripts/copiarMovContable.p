DISABLE TRIGGERS FOR LOAD OF bdcentral.mov_contable.
DISABLE TRIGGERS FOR LOAD OF bdcentral.mov_ahorros.
DISABLE TRIGGERS FOR LOAD OF bdcentral.mov_creditos.

DEFINE VAR vFecha AS DATE INITIAL 01/01/2011.
DEFINE VAR cont AS INTEGER.

FOR EACH bdcentral.agencias NO-LOCK:
    FOR EACH copia.mov_contable WHERE copia.mov_contable.agencia = bdcentral.agencias.agencia
                                  AND copia.mov_contable.fec_contable = vFecha NO-LOCK:
        cont = cont + 1.
        CREATE bdcentral.mov_contable.
        BUFFER-COPY copia.mov_contable TO bdcentral.mov_contable.
    END.

    FOR EACH copia.mov_ahorros WHERE copia.mov_ahorros.agencia = bdcentral.agencias.agencia
                                 AND copia.mov_ahorros.fecha = vFecha NO-LOCK:
        /*AND bdcentral.Mov_Ahorros.Cedula_Transac = copia.Mov_Ahorros.Cedula_Transac
                                           AND bdcentral.Mov_Ahorros.Cod_Ahorro = copia.Mov_Ahorros.Cod_Ahorro
                                           AND bdcentral.Mov_Ahorros.Cod_Operacion = copia.Mov_Ahorros.Cod_Operacion
                                           AND bdcentral.Mov_Ahorros.Cpte = copia.Mov_Ahorros.Cpte
                                           AND bdcentral.Mov_Ahorros.Cue_Ahorros = copia.Mov_Ahorros.Cue_Ahorros
                                           AND bdcentral.Mov_Ahorros.Descrip = copia.Mov_Ahorros.Descrip
                                           AND bdcentral.Mov_Ahorros.Fecha = copia.Mov_Ahorros.Fecha
                                           AND bdcentral.Mov_Ahorros.Hora = copia.Mov_Ahorros.Hora
                                           AND bdcentral.Mov_Ahorros.Nit = copia.Mov_Ahorros.Nit
                                           AND bdcentral.Mov_Ahorros.NomApell_Trans = copia.Mov_Ahorros.NomApell_Trans
                                           AND bdcentral.Mov_Ahorros.Nro_Auditoria = copia.Mov_Ahorros.Nro_Auditoria
                                           AND bdcentral.Mov_Ahorros.Num_Documento = copia.Mov_Ahorros.Num_Documento
                                           AND bdcentral.Mov_Ahorros.Sdo_Disponible = copia.Mov_Ahorros.Sdo_Disponible
                                           AND bdcentral.Mov_Ahorros.Usuario = copia.Mov_Ahorros.Usuario
                                           AND bdcentral.Mov_Ahorros.Val_Cheque = copia.Mov_Ahorros.Val_Cheque
                                           AND bdcentral.Mov_Ahorros.Val_Efectivo = copia.Mov_Ahorros.Val_Efectivo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bdcentral.mov_ahorros THEN DO:*/
            CREATE bdcentral.mov_ahorros.
            BUFFER-COPY copia.mov_ahorros TO bdcentral.mov_ahorros.
        /*END.*/
    END.

    FOR EACH copia.mov_creditos WHERE copia.mov_creditos.agencia = bdcentral.agencias.agencia
                                  AND copia.mov_creditos.fecha = vFecha NO-LOCK:
        /*FIND FIRST bdcentral.mov_creditos WHERE bdcentral.Mov_Creditos.agencia = copia.Mov_Creditos.agencia
                                            AND bdcentral.Mov_Creditos.Cod_Credito = copia.Mov_Creditos.Cod_Credito
                                            AND bdcentral.Mov_Creditos.Cod_Operacion = copia.Mov_Creditos.Cod_Operacion
                                            AND bdcentral.Mov_Creditos.Com_Adicional = copia.Mov_Creditos.Com_Adicional
                                            AND bdcentral.Mov_Creditos.Com_Bruta = copia.Mov_Creditos.Com_Bruta
                                            AND bdcentral.Mov_Creditos.Cpte = copia.Mov_Creditos.Cpte
                                            AND bdcentral.Mov_Creditos.Descrip = copia.Mov_Creditos.Descrip
                                            AND bdcentral.Mov_Creditos.Fecha = copia.Mov_Creditos.Fecha
                                            AND bdcentral.Mov_Creditos.GmfxC = copia.Mov_Creditos.GmfxC
                                            AND bdcentral.Mov_Creditos.Hora = copia.Mov_Creditos.Hora
                                            AND bdcentral.Mov_Creditos.Int_Corrientes = copia.Mov_Creditos.Int_Corrientes
                                            AND bdcentral.Mov_Creditos.Int_MorCobrar = copia.Mov_Creditos.Int_MorCobrar
                                            AND bdcentral.Mov_Creditos.Nit = copia.Mov_Creditos.Nit
                                            AND bdcentral.Mov_Creditos.Nro_Auditoria = copia.Mov_Creditos.Nro_Auditoria
                                            AND bdcentral.Mov_Creditos.Num_Credito = copia.Mov_Creditos.Num_Credito
                                            AND bdcentral.Mov_Creditos.Num_Documento = copia.Mov_Creditos.Num_Documento
                                            AND bdcentral.Mov_Creditos.Ofi_Destino = copia.Mov_Creditos.Ofi_Destino
                                            AND bdcentral.Mov_Creditos.Ofi_Fuente = copia.Mov_Creditos.Ofi_Fuente
                                            AND bdcentral.Mov_Creditos.Pagare = copia.Mov_Creditos.Pagare
                                            AND bdcentral.Mov_Creditos.Sdo_Capital = copia.Mov_Creditos.Sdo_Capital
                                            AND bdcentral.Mov_Creditos.Seg_Cartera = copia.Mov_Creditos.Seg_Cartera
                                            AND bdcentral.Mov_Creditos.Usuario = copia.Mov_Creditos.Usuario
                                            AND bdcentral.Mov_Creditos.Val_Cheque = copia.Mov_Creditos.Val_Cheque
                                            AND bdcentral.Mov_Creditos.Val_Efectivo = copia.Mov_Creditos.Val_Efectivo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bdcentral.mov_creditos THEN DO:*/
            CREATE bdcentral.mov_creditos.
            BUFFER-COPY copia.mov_creditos TO bdcentral.mov_creditos.
        /*END.*/
    END.
END.

MESSAGE "Fin" vFecha cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
