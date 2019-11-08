  FIELD Cfg_Cod LIKE Cfg_Sarlaft.Cfg_Cod VALIDATE ~
  FIELD Cod_Analisis LIKE Cfg_Sarlaft.Cod_Analisis VALIDATE ~
  FIELD Cod_Nivel LIKE Cfg_Sarlaft.Cod_Nivel VALIDATE  FORMAT ">>9"~
  FIELD Cod_Probabilidad LIKE Cfg_Sarlaft.Cod_Probabilidad VALIDATE ~
  FIELD Cod_Severidad LIKE Cfg_Sarlaft.Cod_Severidad VALIDATE ~
  FIELD Cod_RASoc LIKE Cfg_Sarlaft.Cod_RASoc VALIDATE ~
  FIELD Cta_Cr LIKE Cfg_Sarlaft.Cta_Cr VALIDATE ~
  FIELD Cta_Db LIKE Cfg_Sarlaft.Cta_Db VALIDATE ~
  FIELD Descripcion LIKE Cfg_Sarlaft.Descripcion VALIDATE  FORMAT "X(50)" LABEL "Descripción Detallada" COLUMN-LABEL "Descripción Detallada"~
  FIELD Estado LIKE Cfg_Sarlaft.Estado VALIDATE ~
  FIELD Fec_Creacion LIKE Cfg_Sarlaft.Fec_Creacion VALIDATE ~
  FIELD Fec_Retiro LIKE Cfg_Sarlaft.Fec_Retiro VALIDATE ~
  FIELD Nombre LIKE Cfg_Sarlaft.Nombre VALIDATE  FORMAT "X(20)"~
  FIELD Tipo LIKE Cfg_Sarlaft.Tipo VALIDATE ~
  FIELD Util_Neta LIKE Cfg_Sarlaft.Util_Neta VALIDATE  FORMAT ">>9,99"~
  FIELD Val_Final LIKE Cfg_Sarlaft.Val_Final VALIDATE  FORMAT ">>>,>>>,>>9.99"~
  FIELD Val_Inicial LIKE Cfg_Sarlaft.Val_Inicial VALIDATE  FORMAT ">>>,>>>,>>9.99"~
  FIELD Por_dos LIKE Cfg_Sarlaft.Por_dos VALIDATE ~
  FIELD Por_uno LIKE Cfg_Sarlaft.Por_uno VALIDATE ~
  FIELD Val_Detallada AS CHARACTER FORMAT "x(47)" LABEL "Valoración Detallada" COLUMN-LABEL "Valoración Detallada"~
  FIELD Val_DetalladaP AS CHARACTER FORMAT "x(45)" LABEL "Valoración Detallada" COLUMN-LABEL "Valoración Detallada"~
  FIELD Cla_Imp AS CHARACTER FORMAT "x(60)" LABEL "Clasificación Impacto del Riesgo Operativo" COLUMN-LABEL "Clasificación Impacto del Riesgo Operativo"~
  FIELD FSeveridad AS CHARACTER FORMAT "x(30)" LABEL "Severidad" COLUMN-LABEL "Severidad"~
  FIELD FProbabilidad AS CHARACTER FORMAT "x(30)" LABEL "Probabilidad" COLUMN-LABEL "Probabilidad"~
  FIELD Cfg_Nombre AS CHARACTER FORMAT "x(12)" LABEL "Nombre" COLUMN-LABEL "Nombre"
