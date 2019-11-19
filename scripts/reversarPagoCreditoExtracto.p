FOR EACH creditos WHERE creditos.nit = "41641598"
                    AND creditos.num_credito = 2877
                    AND estado = 2:
    creditos.sdo_capital = creditos.sdo_capital + 269152.

    CREATE mov_creditos.
    ASSIGN Mov_Creditos.agencia = creditos.agencia
           Mov_Creditos.Cod_Credito = creditos.cod_credito
           Mov_Creditos.Cod_Operacion = 020102001
           Mov_Creditos.Cpte = 15
           Mov_Creditos.Descrip = "ReversiónPagosDuplicadosPSE"
           Mov_Creditos.Fecha = TODAY
           Mov_Creditos.Hora = TIME
           Mov_Creditos.Nit = creditos.nit
           Mov_Creditos.Num_Credito = creditos.num_credito
           mov_Creditos.Num_Documento = "99999"
           Mov_Creditos.Ofi_Destino = creditos.agencia
           Mov_Creditos.Ofi_Fuente = creditos.agencia
           mov_Creditos.Pagare = creditos.pagare
           Mov_Creditos.Sdo_Capital = creditos.sdo_capital
           Mov_Creditos.Usuario = "desarrollo"
           Mov_Creditos.Val_Efectivo = 269152.
END.
