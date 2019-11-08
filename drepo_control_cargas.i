  FIELD periodo LIKE control_carga.periodo VALIDATE ~
  FIELD cHra AS CHARACTER FORMAT "x(11)" LABEL "Hora" COLUMN-LABEL "Hora"~
  FIELD fechaCarga LIKE control_carga.fechaCarga VALIDATE ~
  FIELD horaCarga LIKE control_carga.horaCarga VALIDATE ~
  FIELD usuario LIKE control_carga.usuario VALIDATE ~
  FIELD Nombre LIKE Usuarios.Nombre VALIDATE ~
  FIELD agencia LIKE control_carga.agencia VALIDATE ~
  FIELD Nombre-2 LIKE Agencias.Nombre VALIDATE  LABEL "Agencia"~
  FIELD TotalRegistros LIKE control_carga.TotalRegistros VALIDATE 
