  FIELD Agencia LIKE Taquilla.Agencia VALIDATE  COLUMN-LABEL "Agencia"~
  FIELD Age_Destino LIKE Taquilla.Age_Destino VALIDATE  COLUMN-LABEL "Ofc. Destino"~
  FIELD Age_Fuente LIKE Taquilla.Age_Fuente VALIDATE  COLUMN-LABEL "Ofc. Fuente"~
  FIELD Autorizo LIKE Taquilla.Autorizo VALIDATE  COLUMN-LABEL "Autorizo"~
  FIELD Cod_Compensa LIKE Taquilla.Cod_Compensa VALIDATE ~
  FIELD Cod_Operacion LIKE Taquilla.Cod_Operacion VALIDATE ~
  FIELD Cod_Producto LIKE Taquilla.Cod_Producto VALIDATE ~
  FIELD Cod_Segmento LIKE Taquilla.Cod_Segmento VALIDATE ~
  FIELD Contabilizar LIKE Taquilla.Contabilizar VALIDATE ~
  FIELD Cta_Contra LIKE Taquilla.Cta_Contra VALIDATE ~
  FIELD Cuenta LIKE Taquilla.Cuenta VALIDATE ~
  FIELD Descripcion LIKE Taquilla.Descripcion VALIDATE ~
  FIELD Duracion LIKE Taquilla.Duracion VALIDATE ~
  FIELD Estacion LIKE Taquilla.Estacion VALIDATE ~
  FIELD Estado LIKE Taquilla.Estado VALIDATE ~
  FIELD Est_Linea LIKE Taquilla.Est_Linea VALIDATE ~
  FIELD Fec_Exonerado LIKE Taquilla.Fec_Exonerado VALIDATE ~
  FIELD Fec_GNU LIKE Taquilla.Fec_GNU VALIDATE ~
  FIELD Fec_GNUM LIKE Taquilla.Fec_GNUM VALIDATE ~
  FIELD Fec_Reportada LIKE Taquilla.Fec_Reportada VALIDATE ~
  FIELD Fec_Sospechosa LIKE Taquilla.Fec_Sospechosa VALIDATE ~
  FIELD Fec_Transaccion LIKE Taquilla.Fec_Transaccion VALIDATE ~
  FIELD Hora_Transaccion LIKE Taquilla.Hora_Transaccion VALIDATE ~
  FIELD Id_Exonerada LIKE Taquilla.Id_Exonerada VALIDATE ~
  FIELD Id_NUD LIKE Taquilla.Id_NUD VALIDATE ~
  FIELD Id_NUM LIKE Taquilla.Id_NUM VALIDATE ~
  FIELD Id_NUS LIKE Taquilla.Id_NUS VALIDATE ~
  FIELD Id_RepFiscal LIKE Taquilla.Id_RepFiscal VALIDATE  FORMAT "Enviada/No Enviada"~
  FIELD Id_Sospechosa LIKE Taquilla.Id_Sospechosa VALIDATE  FORMAT "Si/No"~
  FIELD Naturaleza LIKE Taquilla.Naturaleza VALIDATE ~
  FIELD Nit LIKE Taquilla.Nit VALIDATE ~
  FIELD Nro_Cuenta LIKE Taquilla.Nro_Cuenta VALIDATE ~
  FIELD Nro_Transaccion LIKE Taquilla.Nro_Transaccion VALIDATE ~
  FIELD Num_Documento LIKE Taquilla.Num_Documento VALIDATE ~
  FIELD Num_Retcheque LIKE Taquilla.Num_Retcheque VALIDATE ~
  FIELD Tip_Producto LIKE Taquilla.Tip_Producto VALIDATE ~
  FIELD Ult_Instancia LIKE Taquilla.Ult_Instancia VALIDATE ~
  FIELD Usuario LIKE Taquilla.Usuario VALIDATE ~
  FIELD Usu_Exonerado LIKE Taquilla.Usu_Exonerado VALIDATE ~
  FIELD Usu_GNU LIKE Taquilla.Usu_GNU VALIDATE ~
  FIELD Usu_GNUM LIKE Taquilla.Usu_GNUM VALIDATE ~
  FIELD Usu_Reportada LIKE Taquilla.Usu_Reportada VALIDATE ~
  FIELD Usu_Sospechosa LIKE Taquilla.Usu_Sospechosa VALIDATE ~
  FIELD Val_Cheque LIKE Taquilla.Val_Cheque VALIDATE ~
  FIELD Val_Efectivo LIKE Taquilla.Val_Efectivo VALIDATE ~
  FIELD FEfeChe AS CHARACTER FORMAT "x(8)" LABEL "Efe/Che" COLUMN-LABEL "Efec/Cheq"~
  FIELD FValConsignacion AS DECIMAL FORMAT "->>>,>>>,>>9.99" LABEL "Val. Consig." COLUMN-LABEL "Val. Consig."~
  FIELD FValRetiro AS DECIMAL FORMAT "->>>,>>>,>>9.99" LABEL "Val. Retiro" COLUMN-LABEL "Val. Retiro"
