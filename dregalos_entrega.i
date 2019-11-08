  FIELD regalo LIKE regalos_entrega.regalo VALIDATE ~
  FIELD agencia LIKE regalos_entrega.agencia VALIDATE ~
  FIELD fecha_entrega LIKE regalos_entrega.fecha_entrega VALIDATE ~
  FIELD hora LIKE regalos_entrega.hora VALIDATE ~
  FIELD nit LIKE regalos_entrega.nit VALIDATE ~
  FIELD nit_relacion LIKE regalos_entrega.nit_relacion VALIDATE ~
  FIELD usuario LIKE regalos_entrega.usuario VALIDATE ~
  FIELD FAgencia AS CHARACTER FORMAT "x(30)" LABEL "Agencia" COLUMN-LABEL "Agencia"~
  FIELD FCliente AS CHARACTER FORMAT "x(40)" LABEL "Cliente" COLUMN-LABEL "Cliente"~
  FIELD FRelacion AS CHARACTER FORMAT "x(40)" LABEL "Beneficiario" COLUMN-LABEL "Beneficiario"~
  FIELD FRegalo AS CHARACTER FORMAT "x(40)" LABEL "Regalo" COLUMN-LABEL "Regalo"~
  FIELD FHora AS CHARACTER FORMAT "x(10)" LABEL "Hora" COLUMN-LABEL "Hora"
