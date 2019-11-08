  FIELD Agencia LIKE Mov_InsSipla.Agencia COLUMN-LABEL "Agencia"~
  FIELD Instancia LIKE Mov_InsSipla.Instancia~
  FIELD UsuReporta LIKE Mov_InsSipla.UsuReporta~
  FIELD IAgencia AS CHARACTER FORMAT "x(40)" LABEL "Agencia" COLUMN-LABEL "Agencia"~
  FIELD ICant AS INTEGER FORMAT ">,>>>,>>9" LABEL "Cant. Usu." COLUMN-LABEL "Cant. Usu."~
  FIELD IDist AS DECIMAL FORMAT ">>9.99" LABEL "%Dist." COLUMN-LABEL "%Dist"
