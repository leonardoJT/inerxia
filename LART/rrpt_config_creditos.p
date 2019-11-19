DEFINE VAR wcodigo LIKE Creditos.Cod_Credito NO-UNDO.
DEFINE VAR wsalida AS CHAR FORMAT "X(255)"   NO-UNDO.
DEFINE VAR wtitulo AS CHAR FORMAT "X(255)"   NO-UNDO.
/* wtitulo = "Linea;Nom_Producto;Cta_AsoAd;Cta_AsoAd;Cta_AsoNa;Cta_ContingenteCr;".                 */
/* wtitulo = wtitulo + "Cta_Castigo;Cta_ContingenteCr;Cta_ContingenteDb;Cta_ContrapartidaGar;".     */
/* wtitulo = wtitulo + "Cta_CostasCR;Cta_CostasDB;Cta_FutGantia;Cta_GarPenCancel;Cta_HonorariosCR;" */
/* wtitulo = wtitulo + "Cta_HonorariosDB;Cta_NoaAd;Cta_NoaNa;Cta_OrdCasCR;Cta_OrdCasDB;"            */
/* wtitulo = wtitulo + "Cta_PolizasCR;Cta_PolizasDB;Cta_SYA;Cta_VigGarAd;Cta_VigGarNa".             */

wtitulo = "Linea" + ";" .
wtitulo = wtitulo + "Nom_Producto;Cta_AsoAd;".
wtitulo = wtitulo + "Cta_AsoNa;". 
wtitulo = wtitulo + "Cta_ContingenteCr;Cta_Castigo;". 
wtitulo = wtitulo + "Cta_ContingenteCr;Cta_ContingenteDb;". 
wtitulo = wtitulo + "Cta_ContrapartidaGar;Cta_CostasCR;Cta_CostasDB;"  .
wtitulo = wtitulo + "Cta_FutGantia;Cta_GarPenCancel;".
wtitulo = wtitulo + "Cta_HonorariosCR;Cta_HonorariosDB;".
wtitulo = wtitulo + "Cta_NoaAd;Cta_NoaNa;Cta_OrdCasCR;". 
wtitulo = wtitulo + "Cta_OrdCasDB;Cta_PolizasCR;Cta_PolizasDB;" . 
wtitulo = wtitulo + "Cta_SYA;Cta_VigGarAd;Cta_VigGarNa".

OUTPUT TO c:\info_fodun\rtpcnfcr.txt.
DISPLAY wtitulo NO-LABEL WITH WIDTH 500. 
/*
DISPLAY "codigo;Cta_AsoAd;Nom_Producto;Cta_AsoAd;Cta_AsoNa;Cta_ContingenteCr;Cta_Castigo;Cta_ContingenteCr;Cta_ContingenteDb;Cta_ContrapartidaGar;Cta_CostasCR;Cta_CostasDB;Cta_FutGantia;Cta_GarPenCancel;Cta_HonorariosCR;Cta_HonorariosDB;Cta_NoaAd;Cta_NoaNa;Cta_OrdCasCR;Cta_OrdCasDB;Cta_PolizasCR;Cta_PolizasDB;Cta_SYA;Cta_VigGarAd;Cta_VigGarNa" .    
*/ 
FOR EACH Pro_Creditos   NO-LOCK BY Pro_Creditos.Cod_Credito :
    /* CortoLargo.Clase_Producto = 2  Creditos */ 
    FIND FIRST CortoLargo  WHERE CortoLargo.Clase_Producto EQ 2 AND CortoLargo.Cod_Producto EQ Pro_Creditos.Cod_Credito   NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CortoLargo  THEN DO:
        MESSAGE "No exixte Configuracion CortoLargo "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT. 
    END.
    wsalida = trim( STRING (Pro_Creditos.Cod_Credito)) + ";" .
    wsalida = wsalida + Pro_Creditos.Nom_Producto + trim(CortoLargo.Cta_AsoAd) + ";".
    wsalida = wsalida + trim(CortoLargo.Cta_AsoNa) + ";" +
    trim(CortoLargo.Cta_ContingenteCr) + ";" + trim(CortoLargo.Cta_Castigo) + ";" +
    trim(CortoLargo.Cta_ContingenteCr) + ";" + TRIM(CortoLargo.Cta_ContingenteDb) + ";" +
    trim(CortoLargo.Cta_ContrapartidaGar) + ";" + TRIM(CortoLargo.Cta_CostasCR) + ";" + TRIM(CortoLargo.Cta_CostasDB) + ";" + 
    trim(CortoLargo.Cta_FutGantia) + ";" + trim(CortoLargo.Cta_GarPenCancel) + ";" +
    trim(CortoLargo.Cta_HonorariosCR) + ";" + trim(CortoLargo.Cta_HonorariosDB) + ";"  +
    TRIM(CortoLargo.Cta_NoaAd) + ";" +  trim(CortoLargo.Cta_NoaNa) + ";" + TRIM(CortoLargo.Cta_OrdCasCR) + ";" + 
    trim(CortoLargo.Cta_OrdCasDB) + ";" + trim(CortoLargo.Cta_PolizasCR) + ";" +  trim(CortoLargo.Cta_PolizasDB) + ";" +  
    TRIM(CortoLargo.Cta_SYA) + ";" + TRIM(CortoLargo.Cta_VigGarAd) + ";" +  trim(CortoLargo.Cta_VigGarNa)
        .
    DISPLAY wsalida NO-LABEL WITH   WIDTH  500.
    /*                                                      
    DISPLAY
        Pro_Creditos.Cod_Credito    
        Pro_Creditos.Nom_Producto   FORMAT "XXXXXXXXXXXXXXXXXXXXXXX"
        Pro_Creditos.Cod_Tasa
        CortoLargo.Cta_AsoAd 
        CortoLargo.Cta_AsoNa 
        CortoLargo.Cta_CostasCR 
        CortoLargo.Cta_CostasDB 
        CortoLargo.Cta_ContrapartidaGar
        Pro_Creditos.Estado         VIEW-AS TEXT 
        NO-LABEL
     WITH   WIDTH  200.
     */
END.

OUTPUT CLOSE.
