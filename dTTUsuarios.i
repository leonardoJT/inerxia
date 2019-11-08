  FIELD Agencia LIKE Mov_InsSipla.Agencia~
  FIELD Instancia LIKE Mov_InsSipla.Instancia~
  FIELD UsuGestiona LIKE Mov_InsSipla.UsuGestiona~
  FIELD UsuReporta LIKE Mov_InsSipla.UsuReporta~
  FIELD IUsuGest AS CHARACTER FORMAT "x(40)" LABEL "Usu. Gestiona" COLUMN-LABEL "Usu. Gestiona"~
  FIELD ICant AS INTEGER FORMAT ">>,>>>,>>9" LABEL "Cant. Inst."~
  FIELD IDist AS DECIMAL FORMAT ">>9.99" LABEL "%Dist." COLUMN-LABEL "%Dist."
