  FIELD fecha LIKE repositorio.resultados.fecha VALIDATE ~
  FIELD Nit LIKE repositorio.resultados.Nit VALIDATE ~
  FIELD Apellido1 LIKE bdcentral.Clientes.Apellido1 VALIDATE ~
  FIELD Apellido2 LIKE bdcentral.Clientes.Apellido2 VALIDATE ~
  FIELD Nombre LIKE bdcentral.Clientes.Nombre VALIDATE ~
  FIELD prestamo LIKE repositorio.resultados.prestamo VALIDATE ~
  FIELD int002 LIKE repositorio.repositorio.int002 VALIDATE  LABEL "Tipo Crédito" COLUMN-LABEL "Tipo Crédito"~
  FIELD int003 LIKE repositorio.repositorio.int003 VALIDATE  LABEL "Clase Crédito" COLUMN-LABEL "Clase Crédito"~
  FIELD Nom_Producto LIKE bdcentral.Pro_Creditos.Nom_Producto VALIDATE ~
  FIELD PerdidaEsperada LIKE repositorio.resultados.PerdidaEsperada VALIDATE ~
  FIELD ProbabilidadIncumplimiento LIKE repositorio.resultados.ProbabilidadIncumplimiento VALIDATE ~
  FIELD calificacion LIKE repositorio.resultados.calificacion VALIDATE ~
  FIELD CalificacionEquivalente LIKE repositorio.resultados.CalificacionEquivalente VALIDATE ~
  FIELD ExposicionActivo LIKE repositorio.resultados.ExposicionActivo VALIDATE ~
  FIELD PerdidaEsperada% LIKE repositorio.resultados.PerdidaEsperada% VALIDATE ~
  FIELD AnoContable LIKE repositorio.resultados.AnoContable VALIDATE ~
  FIELD MesContable LIKE repositorio.resultados.MesContable VALIDATE ~
  FIELD int001 LIKE repositorio.repositorio.int001 VALIDATE  FORMAT "9999" LABEL "Agencia" COLUMN-LABEL "Agencia"
