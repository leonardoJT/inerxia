DEFINE VAR wcodigo LIKE Creditos.Cod_Credito NO-UNDO.
DEFINE VAR wsalida AS CHAR FORMAT "X(255)"   NO-UNDO.
DEFINE VAR wtitulo AS CHAR FORMAT "X(255)"   NO-UNDO.


wtitulo = "Agencia;Comprobante;Linea" + ";" .
wtitulo = wtitulo + "Nom_Producto;Cta_AsoAd;".
wtitulo = wtitulo + "Cta_AsoNa;" .
wtitulo = wtitulo + "Cta_Castigo;" .
wtitulo = wtitulo + "Cta_ContingenteCr;". 
wtitulo = wtitulo + "Cta_ContingenteDb; ".     
wtitulo = wtitulo + "Cta_ContrapartidaGar;". 
/*wtitulo = wtitulo + "Cta_ContrapartidaGar;" . */ 
wtitulo = wtitulo + "Cta_CostasCR;" .
wtitulo = wtitulo + "Cta_CostasDB;" .
wtitulo = wtitulo + "Cta_FutGantia;" .
wtitulo = wtitulo + "Cta_GarPenCancel;".
wtitulo = wtitulo + "Cta_HonorariosCR;" .
wtitulo = wtitulo + "Cta_HonorariosDB;" .
wtitulo = wtitulo + "Cta_NoaAd;" .
wtitulo = wtitulo + "Cta_NoaNa;"  .
wtitulo = wtitulo + "Cta_OrdCasCR;". 
wtitulo = wtitulo + "Cta_OrdCasDB;" .
wtitulo = wtitulo + "Cta_PolizasCR;" .
wtitulo = wtitulo + "Cta_PolizasDB;"  .
wtitulo = wtitulo + "Cta_SYA;"        .
wtitulo = wtitulo + "Cta_VigGarAd;"    .
wtitulo = wtitulo + "Cta_VigGarNa;"     .
wtitulo = wtitulo + "Plazo_Inicial;"     .
wtitulo = wtitulo + "Plazo_Final"        .


OUTPUT TO c:\info_fodun\rtpcnfcr.txt.
PUT UNFORMATTED wtitulo . 
FOR EACH Pro_Creditos   NO-LOCK BY Pro_Creditos.Cod_Credito :
    /* CortoLargo.Clase_Producto = 2  Creditos */ 
    FIND FIRST CortoLargo  WHERE CortoLargo.Clase_Producto EQ 2 AND CortoLargo.Cod_Producto EQ Pro_Creditos.Cod_Credito   NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CortoLargo  THEN DO:
        MESSAGE "No exixte Configuracion CortoLargo "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT. 
    END.
    wsalida = trim(string(agencia)) + ";" + STRING(CortoLargo.Comprobante) + ";" + STRING(CortoLargo.Cod_Producto) + ";"  .
    wsalida = wsalida + Pro_Creditos.Nom_Producto + trim(CortoLargo.Cta_AsoAd) + ";".
    wsalida = wsalida + trim(CortoLargo.Cta_AsoNa) + ";" +
    TRIM(CortoLargo.Cta_Castigo) + ";" +
    trim(CortoLargo.Cta_ContingenteCr) + ";" + trim(CortoLargo.Cta_ContingenteDb) + ";" +
    trim(CortoLargo.Cta_ContrapartidaGar) + ";" + TRIM(CortoLargo.Cta_ContrapartidaGar) + ";" +
    trim(CortoLargo.Cta_CostasCR) + ";" + TRIM(CortoLargo.Cta_CostasDB) + ";" + 
    trim(CortoLargo.Cta_FutGantia) + ";" + trim(CortoLargo.Cta_GarPenCancel) + ";" +
    trim(CortoLargo.Cta_HonorariosCR) + ";" + trim(CortoLargo.Cta_HonorariosDB) + ";"  +
    TRIM(CortoLargo.Cta_NoaAd) + ";" +  trim(CortoLargo.Cta_NoaNa) + ";" + TRIM(CortoLargo.Cta_OrdCasCR) + ";" + 
    trim(CortoLargo.Cta_OrdCasDB) + ";" + trim(CortoLargo.Cta_PolizasCR) + ";" +  trim(CortoLargo.Cta_PolizasDB) + ";" +  
    TRIM(CortoLargo.Cta_SYA) + ";" + TRIM(CortoLargo.Cta_VigGarAd) + ";" +  trim(CortoLargo.Cta_VigGarNa)  + ";" +
    TRIM(STRING(CortoLargo.Plazo_Inicial))  + ";" + TRIM(STRING(CortoLargo.Plazo_Final))
        .
     DISPLAY wsalida NO-LABEL WITH   WIDTH  500. 
END.

OUTPUT CLOSE.


