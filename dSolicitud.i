  FIELD Agencia LIKE Solicitud.Agencia VALIDATE ~
  FIELD Fle AS CHARACTER FORMAT "x(255)" LABEL "Vínculo" COLUMN-LABEL "Vínculo"~
  FIELD Age_DebAutomatico LIKE Solicitud.Age_DebAutomatico VALIDATE ~
  FIELD Age_Desembolso LIKE Solicitud.Age_Desembolso VALIDATE ~
  FIELD Capacidad_Pago LIKE Solicitud.Capacidad_Pago VALIDATE ~
  FIELD Cod_Credito LIKE Solicitud.Cod_Credito VALIDATE ~
  FIELD Cod_DebAutomatico LIKE Solicitud.Cod_DebAutomatico VALIDATE ~
  FIELD Cod_Desembolso LIKE Solicitud.Cod_Desembolso VALIDATE ~
  FIELD Cod_Formato LIKE Solicitud.Cod_Formato VALIDATE ~
  FIELD Cod_Negacion LIKE Solicitud.Cod_Negacion VALIDATE ~
  FIELD Concepto LIKE Solicitud.Concepto VALIDATE ~
  FIELD Conocimiento_Cliente LIKE Solicitud.Conocimiento_Cliente VALIDATE ~
  FIELD Cue_DebAutomatico LIKE Solicitud.Cue_DebAutomatico VALIDATE ~
  FIELD Cue_Desembolso LIKE Solicitud.Cue_Desembolso VALIDATE ~
  FIELD Cuota LIKE Solicitud.Cuota VALIDATE ~
  FIELD Deducible LIKE Solicitud.Deducible VALIDATE ~
  FIELD Desembolso LIKE Solicitud.Desembolso VALIDATE ~
  FIELD Destino LIKE Solicitud.Destino VALIDATE ~
  FIELD DestinoF LIKE Solicitud.DestinoF VALIDATE ~
  FIELD Endeud_Indirecto LIKE Solicitud.Endeud_Indirecto VALIDATE ~
  FIELD Estado LIKE Solicitud.Estado VALIDATE ~
  FIELD Fec_Aprobacion LIKE Solicitud.Fec_Aprobacion VALIDATE ~
  FIELD Fec_Retiro LIKE Solicitud.Fec_Retiro VALIDATE ~
  FIELD Fec_Solicitud LIKE Solicitud.Fec_Solicitud VALIDATE  COLUMN-LABEL "Fecha!Solitud"~
  FIELD For_Interes LIKE Solicitud.For_Interes VALIDATE ~
  FIELD For_Pago LIKE Solicitud.For_Pago VALIDATE ~
  FIELD Garantia LIKE Solicitud.Garantia VALIDATE ~
  FIELD Id_Adicionales LIKE Solicitud.Id_Adicionales VALIDATE ~
  FIELD Incremento LIKE Solicitud.Incremento VALIDATE ~
  FIELD Lin_Ahorro LIKE Solicitud.Lin_Ahorro VALIDATE ~
  FIELD Monto LIKE Solicitud.Monto VALIDATE ~
  FIELD Mora_Comercial LIKE Solicitud.Mora_Comercial VALIDATE ~
  FIELD Nit LIKE Solicitud.Nit VALIDATE ~
  FIELD Num_Asesoria LIKE Solicitud.Num_Asesoria VALIDATE ~
  FIELD Num_CredACanc1 LIKE Solicitud.Num_CredACanc[1] VALIDATE ~
  FIELD Num_CredACanc2 LIKE Solicitud.Num_CredACanc[2] VALIDATE ~
  FIELD Num_CredACanc3 LIKE Solicitud.Num_CredACanc[3] VALIDATE ~
  FIELD Num_CredACanc4 LIKE Solicitud.Num_CredACanc[4] VALIDATE ~
  FIELD Num_CredACanc5 LIKE Solicitud.Num_CredACanc[5] VALIDATE ~
  FIELD Num_Solicitud LIKE Solicitud.Num_Solicitud VALIDATE  COLUMN-LABEL "Número!Solicitud"~
  FIELD Observaciones LIKE Solicitud.Observaciones VALIDATE ~
  FIELD Pagare LIKE Solicitud.Pagare VALIDATE ~
  FIELD Per_Gracia LIKE Solicitud.Per_Gracia VALIDATE ~
  FIELD Per_Pago LIKE Solicitud.Per_Pago VALIDATE ~
  FIELD Plazo LIKE Solicitud.Plazo VALIDATE ~
  FIELD Puntaje LIKE Solicitud.Puntaje VALIDATE ~
  FIELD Pun_Negociables LIKE Solicitud.Pun_Negociables VALIDATE ~
  FIELD Respaldo_Patrim LIKE Solicitud.Respaldo_Patrim VALIDATE ~
  FIELD Sistema LIKE Solicitud.Sistema VALIDATE ~
  FIELD Tasa LIKE Solicitud.Tasa VALIDATE ~
  FIELD Tipo_Actividad LIKE Solicitud.Tipo_Actividad VALIDATE ~
  FIELD Tip_Credito LIKE Solicitud.Tip_Credito VALIDATE ~
  FIELD Total_Prestamo LIKE Solicitud.Total_Prestamo VALIDATE ~
  FIELD Usuario LIKE Solicitud.Usuario VALIDATE  COLUMN-LABEL "Usuario"~
  FIELD Verificacion LIKE Solicitud.Verificacion VALIDATE ~
  FIELD Nombre LIKE Usuarios.Nombre VALIDATE  LABEL "Nombre Usuario" COLUMN-LABEL "Nombre Usuario"~
  FIELD Apellido1 LIKE Clientes.Apellido1 VALIDATE  COLUMN-LABEL "Primer!Apellido"~
  FIELD Apellido2 LIKE Clientes.Apellido2 VALIDATE  COLUMN-LABEL "Segundo!Apellido"~
  FIELD Nombre-2 LIKE Clientes.Nombre VALIDATE  LABEL "Nombres" COLUMN-LABEL "Nombres"~
  FIELD Nombre-3 LIKE Agencias.Nombre VALIDATE  LABEL "Agencia" COLUMN-LABEL "Agencia"
