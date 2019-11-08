  FIELD agencia LIKE Mov_Creditos.agencia VALIDATE ~
  FIELD Cod_Credito LIKE Mov_Creditos.Cod_Credito VALIDATE ~
  FIELD Cod_Operacion LIKE Mov_Creditos.Cod_Operacion VALIDATE ~
  FIELD Cpte LIKE Mov_Creditos.Cpte VALIDATE ~
  FIELD Descrip LIKE Mov_Creditos.Descrip VALIDATE ~
  FIELD Fecha LIKE Mov_Creditos.Fecha VALIDATE  LABEL "Fec. Pago" COLUMN-LABEL "Fec. Pago"~
  FIELD Hora LIKE Mov_Creditos.Hora VALIDATE ~
  FIELD Int_Corrientes LIKE Mov_Creditos.Int_Corrientes VALIDATE ~
  FIELD Int_MorCobrar LIKE Mov_Creditos.Int_MorCobrar VALIDATE ~
  FIELD Nit LIKE Mov_Creditos.Nit VALIDATE ~
  FIELD Num_Credito LIKE Mov_Creditos.Num_Credito VALIDATE ~
  FIELD Num_Documento LIKE Mov_Creditos.Num_Documento VALIDATE ~
  FIELD Ofi_Destino LIKE Mov_Creditos.Ofi_Destino VALIDATE ~
  FIELD Ofi_Fuente LIKE Mov_Creditos.Ofi_Fuente VALIDATE ~
  FIELD Pagare LIKE Mov_Creditos.Pagare VALIDATE ~
  FIELD Sdo_Capital LIKE Mov_Creditos.Sdo_Capital VALIDATE ~
  FIELD Usuario LIKE Mov_Creditos.Usuario VALIDATE ~
  FIELD Val_Cheque LIKE Mov_Creditos.Val_Cheque VALIDATE ~
  FIELD Val_Efectivo LIKE Mov_Creditos.Val_Efectivo VALIDATE ~
  FIELD FDeposito AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Deposito" COLUMN-LABEL "Deposito"~
  FIELD FEfeChe AS CHARACTER FORMAT "X(8)" LABEL "Efec/Cheq" COLUMN-LABEL "Efe/Che"
