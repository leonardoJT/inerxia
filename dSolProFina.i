  FIELD Agencia LIKE Creditos.Agencia COLUMN-LABEL "Agencia" HELP "Agencia"~
  FIELD NombreAgencia LIKE Agencias.Nombre VALIDATE  LABEL "Nombre Agencia" COLUMN-LABEL "Nombre!Agencia" HELP "Nombre De La Agencia"~
  FIELD Ciudad LIKE Agencias.Ciudad VALIDATE  HELP "Ciudad"~
  FIELD NombreCiudad LIKE Ubicacion.Nombre VALIDATE  LABEL "Nombre Ciudad" COLUMN-LABEL "Nombre!Ciudad"~
  FIELD Fec_UltActualiza LIKE Clientes.Fec_UltActualiza VALIDATE  LABEL "Ultima Actualizaci�n" COLUMN-LABEL "Ultima!Actualizaci�n"~
  FIELD TpoCliente LIKE Creditos.Cod_Credito LABEL "Tipo Cliente" COLUMN-LABEL "Tipo!Cliente" HELP "Tipo De Cliente"~
  FIELD OtroTpoCliente LIKE Creditos.Observaciones LABEL "Otro Tipo Cliente" COLUMN-LABEL "Otro Tipo Cliente"~
  FIELD CptcionClccion LIKE Creditos.Categoria LABEL "Clase Producto" COLUMN-LABEL "Clase!Producto"~
  FIELD PrdctoSlctar LIKE Creditos.Per_Pago LABEL "Producto A Solicitar" COLUMN-LABEL "Producto!A Solicitar" HELP "Producto A Solicitar"~
  FIELD OtroPrdctoSlctar LIKE Creditos.Pagare LABEL "Otro Producto A Solicitar" COLUMN-LABEL "Otro Producto!A Solicitar" HELP "Cual Otro Producto?"~
  FIELD Monto LIKE Creditos.Monto LABEL "Monto" COLUMN-LABEL "Monto" HELP "Monto"~
  FIELD Plazo LIKE Creditos.Plazo COLUMN-LABEL "Plazo" HELP "Producto A Solicitar"~
  FIELD Grntia LIKE Creditos.Cue_Desembolso LABEL "Garant�a" COLUMN-LABEL "Garant�a"~
  FIELD Linea LIKE Creditos.Cue_DebAutomatico LABEL "L�nea" COLUMN-LABEL "L�nea"~
  FIELD reestrctrcion LIKE Creditos.Abogado FORMAT "S/N" LABEL "Reestructuraci�n" COLUMN-LABEL "Reestructuraci�n"~
  FIELD FrmaPgo LIKE Creditos.CategoriaMes LABEL "Forma De Pago" COLUMN-LABEL "Forma De Pago"~
  FIELD dstncion LIKE Creditos.Nit_Juzgado LABEL "Destinaci�n" COLUMN-LABEL "Destinaci�n"~
  FIELD Cuota LIKE Creditos.Cuota LABEL "Cuota" COLUMN-LABEL "Cuota" HELP "Cuota"
