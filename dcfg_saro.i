  FIELD Cfg_Cod LIKE Cfg_Saro.Cfg_Cod VALIDATE ~
  FIELD Cod_Analisis LIKE Cfg_Saro.Cod_Analisis VALIDATE ~
  FIELD Cod_Nivel LIKE Cfg_Saro.Cod_Nivel VALIDATE  FORMAT ">>9"~
  FIELD Cod_Probabilidad LIKE Cfg_Saro.Cod_Probabilidad VALIDATE ~
  FIELD Cod_Severidad LIKE Cfg_Saro.Cod_Severidad VALIDATE ~
  FIELD Cta_Cr LIKE Cfg_Saro.Cta_Cr VALIDATE ~
  FIELD Cta_Db LIKE Cfg_Saro.Cta_Db VALIDATE ~
  FIELD Descripcion LIKE Cfg_Saro.Descripcion VALIDATE  FORMAT "X(50)" LABEL "Descripción Detallada" COLUMN-LABEL "Descripción Detallada"~
  FIELD Estado LIKE Cfg_Saro.Estado VALIDATE ~
  FIELD Fec_Creacion LIKE Cfg_Saro.Fec_Creacion VALIDATE ~
  FIELD Fec_Retiro LIKE Cfg_Saro.Fec_Retiro VALIDATE ~
  FIELD Nombre LIKE Cfg_Saro.Nombre VALIDATE  FORMAT "X(20)"~
  FIELD Tipo LIKE Cfg_Saro.Tipo VALIDATE ~
  FIELD Util_Neta LIKE Cfg_Saro.Util_Neta VALIDATE  FORMAT ">>9,99"~
  FIELD Val_Final LIKE Cfg_Saro.Val_Final VALIDATE  FORMAT ">>>,>>>,>>9.99"~
  FIELD Val_Inicial LIKE Cfg_Saro.Val_Inicial VALIDATE  FORMAT ">>>,>>>,>>9.99"~
  FIELD Por_dos LIKE Cfg_Saro.Por_dos VALIDATE ~
  FIELD Por_uno LIKE Cfg_Saro.Por_uno VALIDATE ~
  FIELD Val_Detallada AS CHARACTER FORMAT "x(47)" LABEL "Valoración Detallada" COLUMN-LABEL "Valoración Detallada"~
  FIELD Val_DetalladaP AS CHARACTER FORMAT "x(45)" LABEL "Valoración Detallada" COLUMN-LABEL "Valoración Detallada"~
  FIELD Cla_Imp AS CHARACTER FORMAT "x(60)" LABEL "Clasificación Impacto del Riesgo Operativo" COLUMN-LABEL "Clasificación Impacto del Riesgo Operativo"~
  FIELD FSeveridad AS CHARACTER FORMAT "x(30)" LABEL "Severidad" COLUMN-LABEL "Severidad"~
  FIELD FProbabilidad AS CHARACTER FORMAT "x(30)" LABEL "Probabilidad" COLUMN-LABEL "Probabilidad"~
  FIELD Cfg_Nombre AS CHARACTER FORMAT "x(12)" LABEL "Nombre" COLUMN-LABEL "Nombre"
