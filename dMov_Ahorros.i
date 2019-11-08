  FIELD Agencia LIKE Mov_Ahorros.Agencia VALIDATE ~
  FIELD Age_Destino LIKE Mov_Ahorros.Age_Destino VALIDATE ~
  FIELD Age_Fuente LIKE Mov_Ahorros.Age_Fuente VALIDATE ~
  FIELD Cedula_Transac LIKE Mov_Ahorros.Cedula_Transac VALIDATE ~
  FIELD Cod_Ahorro LIKE Mov_Ahorros.Cod_Ahorro VALIDATE  LABEL "Cod. Prod." COLUMN-LABEL "Cod. Prod."~
  FIELD Cod_Operacion LIKE Mov_Ahorros.Cod_Operacion VALIDATE ~
  FIELD Cpte LIKE Mov_Ahorros.Cpte VALIDATE ~
  FIELD Cue_Ahorros LIKE Mov_Ahorros.Cue_Ahorros VALIDATE ~
  FIELD Descrip LIKE Mov_Ahorros.Descrip VALIDATE ~
  FIELD Fecha LIKE Mov_Ahorros.Fecha VALIDATE  LABEL "Fec. Mov." COLUMN-LABEL "Fec. Mov."~
  FIELD Hora LIKE Mov_Ahorros.Hora VALIDATE ~
  FIELD Nit LIKE Mov_Ahorros.Nit VALIDATE ~
  FIELD NomApell_Trans LIKE Mov_Ahorros.NomApell_Trans VALIDATE ~
  FIELD Nro_Auditoria LIKE Mov_Ahorros.Nro_Auditoria VALIDATE ~
  FIELD Num_Documento LIKE Mov_Ahorros.Num_Documento VALIDATE ~
  FIELD Sdo_Disponible LIKE Mov_Ahorros.Sdo_Disponible VALIDATE ~
  FIELD Usuario LIKE Mov_Ahorros.Usuario VALIDATE ~
  FIELD Val_Cheque LIKE Mov_Ahorros.Val_Cheque VALIDATE ~
  FIELD Val_Efectivo LIKE Mov_Ahorros.Val_Efectivo VALIDATE ~
  FIELD FEfeChe AS CHARACTER FORMAT "x(8)" LABEL "Efec/Cheq" COLUMN-LABEL "Efec/Cheq"~
  FIELD FValConsignacion AS DECIMAL FORMAT "->>>,>>>,>>9.99" LABEL "Val. Consig." COLUMN-LABEL "Val. Consig."~
  FIELD FValRetiro AS DECIMAL FORMAT "->>>,>>>,>>9.99" LABEL "Val. Retiro" COLUMN-LABEL "Val. Retiro"
