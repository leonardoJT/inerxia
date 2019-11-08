  FIELD Agencia LIKE Mov_InsSipla.Agencia~
  FIELD CodAutoriza LIKE Mov_InsSipla.CodAutoriza~
  FIELD Descripcion LIKE Mov_InsSipla.Descripcion FORMAT "X(100)"~
  FIELD Estado LIKE Mov_InsSipla.Estado FORMAT "Gestionada/No Gestionada"~
  FIELD Fecha_Gestion LIKE Mov_InsSipla.Fecha_Gestion FORMAT "99/99/9999" LABEL "Fec. Gestión" COLUMN-LABEL "Fec. Gestión"~
  FIELD Fecha_Transaccion LIKE Mov_InsSipla.Fecha_Transaccion FORMAT "99/99/9999" LABEL "Fec. Trans." COLUMN-LABEL "Fec. Trans."~
  FIELD Fec_RepROSS LIKE Mov_InsSipla.Fec_RepROSS LABEL "Fec. Rep. ROSS" COLUMN-LABEL "Fec. Rep. ROSS"~
  FIELD Fec_RepUIAF LIKE Mov_InsSipla.Fec_RepUIAF LABEL "Fec. Rep. UIAF" COLUMN-LABEL "Fec. Rep. UIAF"~
  FIELD Hora_Gestion LIKE Mov_InsSipla.Hora_Gestion~
  FIELD Hora_Transaccion LIKE Mov_InsSipla.Hora_Transaccion~
  FIELD Id_Exonerada LIKE Mov_InsSipla.Id_Exonerada~
  FIELD Id_NUD LIKE Mov_InsSipla.Id_NUD~
  FIELD Id_NUM LIKE Mov_InsSipla.Id_NUM~
  FIELD Id_RepROSS LIKE Mov_InsSipla.Id_RepROSS~
  FIELD Id_RepUIAF LIKE Mov_InsSipla.Id_RepUIAF~
  FIELD Id_Sospechosa LIKE Mov_InsSipla.Id_Sospechosa~
  FIELD Id_Traslado LIKE Mov_InsSipla.Id_Traslado~
  FIELD Instancia LIKE Mov_InsSipla.Instancia~
  FIELD Instancia_Anterior LIKE Mov_InsSipla.Instancia_Anterior LABEL "Inst. Anterior" COLUMN-LABEL "Inst. Anterior"~
  FIELD Nit LIKE Mov_InsSipla.Nit~
  FIELD Tipo_Registro LIKE Mov_InsSipla.Tipo_Registro LABEL "Tp. Registro" COLUMN-LABEL "Tp. Registro"~
  FIELD UsuCajero LIKE Mov_InsSipla.UsuCajero~
  FIELD UsuGestiona LIKE Mov_InsSipla.UsuGestiona LABEL "Usu. Gestiona" COLUMN-LABEL "Usu. Gestiona"~
  FIELD UsuReporta LIKE Mov_InsSipla.UsuReporta LABEL "Usu. Reporta" COLUMN-LABEL "Usu. Reporta"~
  FIELD Valor_RegManual LIKE Mov_InsSipla.Valor_RegManual LABEL "Val. RegManual" COLUMN-LABEL "Val. RegManual"~
  FIELD FUsu_reporta AS CHARACTER FORMAT "x(40)" LABEL "Usu. Reporta" COLUMN-LABEL "Usu. Reporta"~
  FIELD FUsu_Gestiona AS CHARACTER FORMAT "x(40)" LABEL "Usu. Gestiona" COLUMN-LABEL "Usu. Gestiona"~
  FIELD FAgencia AS CHARACTER FORMAT "x(40)" LABEL "Agencia" COLUMN-LABEL "Agencia"~
  FIELD FCliente AS CHARACTER FORMAT "x(40)" LABEL "Cliente" COLUMN-LABEL "Cliente"~
  FIELD FVNUD AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Val. N.U. Día" COLUMN-LABEL "Val. N.U. Día"~
  FIELD FVNUM AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Val. N.U. Mes" COLUMN-LABEL "Val. N.U. Mes"~
  FIELD FInstancia AS CHARACTER FORMAT "x(35)" LABEL "Instancia" COLUMN-LABEL "Instancia"~
  FIELD FHorTrans AS CHARACTER FORMAT "x(8)" LABEL "HoraTrans"~
  FIELD FHorGestion AS CHARACTER FORMAT "x(8)" LABEL "HoraGest."
