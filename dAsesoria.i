  FIELD Agencia LIKE Asesoria.Agencia VALIDATE ~
  FIELD Clase_Producto LIKE Asesoria.Clase_Producto VALIDATE ~
  FIELD Cod_Producto LIKE Asesoria.Cod_Producto VALIDATE ~
  FIELD Cuota LIKE Asesoria.Cuota VALIDATE ~
  FIELD Estado LIKE Asesoria.Estado VALIDATE ~
  FIELD Fec_Apertura LIKE Asesoria.Fec_Apertura VALIDATE ~
  FIELD Fec_Asesoria LIKE Asesoria.Fec_Asesoria VALIDATE ~
  FIELD For_Liquidacion LIKE Asesoria.For_Liquidacion VALIDATE ~
  FIELD Id_Resultado LIKE Asesoria.Id_Resultado VALIDATE ~
  FIELD Monto LIKE Asesoria.Monto VALIDATE ~
  FIELD Nit LIKE Asesoria.Nit VALIDATE ~
  FIELD Num_Asesoria LIKE Asesoria.Num_Asesoria VALIDATE ~
  FIELD Per_Deduccion LIKE Asesoria.Per_Deduccion VALIDATE ~
  FIELD Per_Liquidacion LIKE Asesoria.Per_Liquidacion VALIDATE ~
  FIELD Plazo LIKE Asesoria.Plazo VALIDATE ~
  FIELD Tasa LIKE Asesoria.Tasa VALIDATE ~
  FIELD Usuario LIKE Asesoria.Usuario VALIDATE ~
  FIELD Val_EgresosMes LIKE Asesoria.Val_EgresosMes VALIDATE ~
  FIELD Val_IngresosMes LIKE Asesoria.Val_IngresosMes VALIDATE ~
  FIELD FAgencia AS CHARACTER FORMAT "x(40)" LABEL "Agencia" COLUMN-LABEL "Agencia"~
  FIELD FCliente AS CHARACTER FORMAT "x(50)" LABEL "Cliente" COLUMN-LABEL "Cliente"~
  FIELD FClase_Producto AS CHARACTER FORMAT "x(12)" LABEL "Clase Prod." COLUMN-LABEL "Clase Prod."~
  FIELD FProducto AS CHARACTER FORMAT "x(40)" LABEL "Producto" COLUMN-LABEL "Producto"~
  FIELD FUsuario AS CHARACTER FORMAT "x(45)" LABEL "Usuario" COLUMN-LABEL "Usuario"~
  FIELD FPorcEndeud AS DECIMAL FORMAT ">>9.99" LABEL "%Endeudam." COLUMN-LABEL "%Endeudam."
