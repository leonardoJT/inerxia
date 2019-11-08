  FIELD Agencia LIKE PQR.Agencia VALIDATE  COLUMN-LABEL "Agencia"~
  FIELD Area_Resp LIKE PQR.Area_Resp VALIDATE  COLUMN-LABEL "Responsable"~
  FIELD Canal_Servicio LIKE PQR.Canal_Servicio VALIDATE  COLUMN-LABEL "Canal"~
  FIELD Clase_Producto LIKE PQR.Clase_Producto VALIDATE  COLUMN-LABEL "Clase!Producto"~
  FIELD Cod_Producto LIKE PQR.Cod_Producto VALIDATE  COLUMN-LABEL "Producto"~
  FIELD Descrip_Anulado LIKE PQR.Descrip_Anulado VALIDATE ~
  FIELD Descrip_PQR LIKE PQR.Descrip_PQR VALIDATE  FORMAT "X(50)" COLUMN-LABEL "Descripcion"~
  FIELD Descrip_Resp LIKE PQR.Descrip_Resp VALIDATE  FORMAT "X(50)" COLUMN-LABEL "Respuesta"~
  FIELD Fec_Anu LIKE PQR.Fec_Anu VALIDATE  COLUMN-LABEL "Fecha Anulacion"~
  FIELD Fec_Fin_PQR LIKE PQR.Fec_Fin_PQR VALIDATE  COLUMN-LABEL "Fecha Fin"~
  FIELD Fec_Grabacion LIKE PQR.Fec_Grabacion VALIDATE  COLUMN-LABEL "Fecha de Grabacion"~
  FIELD Fec_Resp LIKE PQR.Fec_Resp VALIDATE  COLUMN-LABEL "Fecha!Respuesta"~
  FIELD Fec_Solicitud LIKE PQR.Fec_Solicitud VALIDATE  COLUMN-LABEL "Fecha!Solicitud"~
  FIELD Hora_Anu LIKE PQR.Hora_Anu VALIDATE  COLUMN-LABEL "Hora Anulacion"~
  FIELD Hora_Fin_PQR LIKE PQR.Hora_Fin_PQR VALIDATE  COLUMN-LABEL "Hora Fin"~
  FIELD Hora_Grabacion LIKE PQR.Hora_Grabacion VALIDATE  COLUMN-LABEL "Hora Grabacion"~
  FIELD Hora_Resp LIKE PQR.Hora_Resp VALIDATE  COLUMN-LABEL "Hora Respuesta"~
  FIELD Hora_Solicitud LIKE PQR.Hora_Solicitud VALIDATE ~
  FIELD Motivo LIKE PQR.Motivo VALIDATE ~
  FIELD Nit LIKE PQR.Nit VALIDATE  COLUMN-LABEL "Nit Cliente"~
  FIELD Num_PQR LIKE PQR.Num_PQR VALIDATE  COLUMN-LABEL "Número"~
  FIELD Per_Resp LIKE PQR.Per_Resp VALIDATE ~
  FIELD Tip_Credito LIKE PQR.Tip_Credito VALIDATE  COLUMN-LABEL "Tipo Credito"~
  FIELD Tip_PQR LIKE PQR.Tip_PQR VALIDATE  COLUMN-LABEL "Tipo Solcitud"~
  FIELD Usuario LIKE PQR.Usuario VALIDATE ~
  FIELD Usu_Anulacion LIKE PQR.Usu_Anulacion VALIDATE ~
  FIELD Usu_Resp LIKE PQR.Usu_Resp VALIDATE ~
  FIELD Estado LIKE PQR.Estado VALIDATE  COLUMN-LABEL "Estado"~
  FIELD Cod_Proceso LIKE PQR.Cod_Proceso VALIDATE ~
  FIELD Cod_Req LIKE PQR.Cod_Req VALIDATE ~
  FIELD Fnom_producto AS CHARACTER FORMAT "x(15)" LABEL "Producto" COLUMN-LABEL "Producto"~
  FIELD FAgencia AS CHARACTER FORMAT "x(45)" LABEL "Agencia"
