DEFINE VAR valor AS DECIMAL.
DEFINE VAR pSecuencia AS INTEGER.

FIND FIRST comprobantes WHERE comprobantes.comprobante = 21
                          AND comprobantes.agencia = 4 NO-ERROR.
comprobantes.secuencia = comprobantes.secuencia + 1.
pSecuencia = comprobantes.secuencia.
 
FOR EACH ahorros WHERE agencia = 4
                   AND ahorros.cod_ahorro = 9
                   AND sdo_disponible < 0:
    valor = ahorros.sdo_disponible * -1.

    ahorros.sdo_disponible = 0.

    CREATE Mov_Ahorros.
    ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
           Mov_Ahorros.Age_Destino = Ahorros.Agencia
           Mov_Ahorros.Age_Fuente = ahorros.agencia
           Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
           Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
           Mov_Ahorros.Fecha = TODAY
           Mov_Ahorros.Hora = TIME
           Mov_Ahorros.Nit = Ahorros.Nit
           Mov_Ahorros.Num_Documento = STRING(pSecuencia)
           Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
           Mov_Ahorros.Usuario = "2305"
           Mov_Ahorros.Val_Efectivo = valor
           Mov_Ahorros.Cod_Operacion = 010101001
           Mov_Ahorros.Descrip = "Ajuste x Saldo Negativo"
           Mov_Ahorros.Cpte = 21.

    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = 999
           mov_contable.nit = ahorros.nit
           Mov_Contable.Agencia = ahorros.agencia
           Mov_Contable.Destino = ahorros.agencia
           Mov_Contable.Comprobante = 21
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "21050502"
           Mov_Contable.Comentario = "Ajuste x Saldo Negativo"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           Mov_contable.cr = valor.

    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = 999
           mov_contable.nit = ahorros.nit
           Mov_Contable.Agencia = ahorros.agencia
           Mov_Contable.Destino = ahorros.agencia
           Mov_Contable.Comprobante = 21
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "16909502"
           Mov_Contable.Comentario = "Ajuste x Saldo Negativo"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           Mov_contable.db = valor.
END.
