  FIELD Codigo LIKE SuperFormatoC.Codigo VALIDATE  COLUMN-LABEL "Código!Formato"~
  FIELD DocumentoTecnico LIKE SuperFormatoC.DocumentoTecnico VALIDATE  FORMAT "x(30)" COLUMN-LABEL "Documento!Técnico"~
  FIELD FormatoExcel LIKE SuperFormatoC.FormatoExcel VALIDATE  FORMAT "x(30)" COLUMN-LABEL "Formato!Excel"~
  FIELD Nombre LIKE SuperFormatoC.Nombre VALIDATE  FORMAT "x(60)"~
  FIELD Observaciones LIKE SuperFormatoC.Observaciones VALIDATE  FORMAT "x(256)"~
  FIELD Periodicidad LIKE SuperFormatoC.Periodicidad VALIDATE  FORMAT "x(30)"~
  FIELD Proforma LIKE SuperFormatoC.Proforma VALIDATE  FORMAT "x(30)"~
  FIELD circular LIKE SuperFormatoC.circular VALIDATE  FORMAT "x(30)"~
  FIELD TipoYNumero LIKE SuperFormatoC.TipoYNumero VALIDATE  COLUMN-LABEL "Tipo!Número!Informe"~
  FIELD AreaInformacion LIKE SuperFormatoC.AreaInformacion VALIDATE  COLUMN-LABEL "Area!Información"~
  FIELD EnDolares LIKE SuperFormatoC.EnDolares VALIDATE ~
  FIELD EnMiles LIKE SuperFormatoC.EnMiles VALIDATE  COLUMN-LABEL "En Miles"~
  FIELD NombreArchivoSalida LIKE SuperFormatoC.NombreArchivoSalida VALIDATE  FORMAT "x(60)"
