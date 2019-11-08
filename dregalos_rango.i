  FIELD regalo LIKE regalos_rango.regalo VALIDATE ~
  FIELD agencia LIKE regalos_rango.agencia VALIDATE ~
  FIELD edad_final LIKE regalos_rango.edad_final VALIDATE ~
  FIELD edad_inicial LIKE regalos_rango.edad_inicial VALIDATE ~
  FIELD fecha_final LIKE regalos_rango.fecha_final VALIDATE ~
  FIELD fecha_inicial LIKE regalos_rango.fecha_inicial VALIDATE ~
  FIELD usuario LIKE regalos_rango.usuario VALIDATE ~
  FIELD FAgencia AS CHARACTER FORMAT "x(30)" LABEL "Agencia" COLUMN-LABEL "Agencia"~
  FIELD FUsuario AS CHARACTER FORMAT "x(40)" LABEL "Usuario" COLUMN-LABEL "Usuario"
