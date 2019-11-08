   DEFINE VARIABLE W_ValProvision LIKE creditos.sdo_capital. 
   DEFINE VARIABLE W_CalMayor     LIKE Creditos.Cod_Califica.
   DEFINE VARIABLE W_ValGarAdm    AS DECIMAL.
   DEFINE VARIABLE W_ValHipoteca   AS DECIMAL.
   DEFINE VARIABLE W_ValNoHipoteca AS DECIMAL.
   DEFINE VARIABLE W_ValDefecto    AS DECIMAL.
   DEFINE VARIABLE W_PorAporte     AS DECIMAL.
   DEFINE VARIABLE W_TotCredito    AS DECIMAL.
   DEFINE VARIABLE W_ApoDesc       AS DECIMAL.
   DEFINE VARIABLE W_Aportes       AS DECIMAL.

   DEFINE TEMP-TABLE TotCre
    FIELD Nit LIKE Creditos.Nit
    FIELD Tot LIKE Creditos.Sdo_Capital
    FIELD Totapo LIKE ahorros.sdo_disponible
    INDEX nit IS PRIMARY nit .

FOR EACH Creditos WHERE creditos.agencia GE 0 BREAK BY Creditos.Nit:

    IF Creditos.Sdo_Capital LE 0 THEN NEXT.
    IF tip_credito GT 4 THEN NEXT. 
    IF estado NE 2 THEN NEXT.
    
 
    ASSIGN Creditos.Provision = 0
           Creditos.Provision_Interes = 0
           Creditos.Provision_Otros = 0.
    IF FIRST-OF(Creditos.Nit) THEN DO: 
       CREATE TotCre.
       ASSIGN TotCre.Nit = Creditos.Nit.
       W_Aportes = 0.                            
        FOR EACH Ahorros WHERE Ahorros.Nit EQ Creditos.Nit NO-LOCK: /*calcula el total de los aportes del cliente*/
            IF Ahorros.Tip_Ahorro EQ 4 AND Ahorros.Estado EQ 1 THEN
               W_Aportes = W_Aportes + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).
        END.
        totcre.totapo = totcre.totapo + W_aportes.
    END.
    TotCre.Tot = TotCre.Tot + Creditos.Sdo_Capital.
END.



FOR EACH Creditos WHERE creditos.agencia GE 0  
               BREAK BY Creditos.Nit BY Creditos.Tip_Credito
                                     BY Creditos.Cod_Califica DESCENDING:

    IF Creditos.Sdo_Capital LE 0 THEN next.
    IF tip_credito          GT 4          THEN next.
    IF estado               NE 2 THEN NEXT.
    W_ValProvision = 0.

    IF FIRST-OF(Creditos.Nit) THEN DO:
        FIND TotCre WHERE TotCre.Nit EQ Creditos.Nit NO-ERROR.
        IF AVAILABLE TotCre THEN ASSIGN W_TotCredito = TotCre.Tot
                                        w_apodesc    = totcre.totapo.
    END.

    IF FIRST-OF(Creditos.Tip_Credito) THEN 
       W_CalMayor = Creditos.Cod_Califica.
    
    /*busqueda de cartera vencida*/
    /*IF W_CalCliente GT Creditos.Cod_Califica THEN
       W_CalMayor = W_CalCliente.*/

    FIND FIRST CarteraVencida WHERE                                                       
         CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND                              
         CarteraVencida.Cod_Califica EQ W_CalMayor NO-LOCK NO-ERROR.                          
    IF NOT AVAILABLE CarteraVencida  THEN DO:                                                 
       MESSAGE "La Configuración para la Clasificación de Cartera" SKIP                       
               "no existe en la Tabla CARTERAVENCIDA para el" SKIP(1)                         
               "Producto: " Creditos.Cod_Credito  SKIP                                        
               "Cliente:  " Creditos.Nit SKIP                                                 
               "Con un Rango valido que contenga el Número: " Creditos.Dias_Atraso SKIP(1)    
               "Se cancela el proceso de Calificación" SKIP                                   
               "Conmuniquese con el Administrador del Sistema"                                
               VIEW-AS ALERT-BOX ERROR.                                                       
       RETURN ERROR.                                                                          
    END.                                                                                      
        
     ASSIGN W_ValGarAdm     = 0
           W_ValHipoteca   = 0
           W_ValNoHipoteca = 0
           W_ValDefecto    = 0.

    FOR EACH Garantias WHERE /*Garantias.Agencia EQ Creditos.Agencia AND*/
             Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
             Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
             Garantias.Tipo_Garantia LT 4                      AND
             Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
             Garantias.Num_Credito   EQ Creditos.Num_Credito   AND
             Garantias.Estado      EQ 1 NO-LOCK:
        W_ValGarAdm = W_ValGarAdm + Garantias.Val_Bien.
        IF Garantias.Tipo_Garantia EQ 1 THEN
           W_ValHipoteca = W_ValHipoteca + Garantias.Val_Bien.
        ELSE
           W_ValNoHipoteca = W_ValNoHipoteca + Garantias.Val_Bien.

    END.

    /*calculo  aportes descontados*/
    IF W_ValGarAdm GT 0 THEN DO:
       IF W_ValHipoteca GT 0 THEN
          W_ValDefecto   = (W_ValHipoteca * CarteraVencida.Porc_DefGarantia).
       IF W_ValNoHipoteca GT 0 THEN
          W_ValDefecto   = W_ValDefecto + (W_ValNoHipoteca * CarteraVencida.Porc_DefNoHipoteca).
       
       IF (Creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) GT 0 THEN 
          W_ValProvision = (Creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) * CarteraVencida.Porc_Admisible.
    END.
    ELSE DO: 
       IF (Creditos.Sdo_Capital - W_ApoDesc) GT 0 THEN
          W_ValProvision = (Creditos.Sdo_Capital - W_ApoDesc) * CarteraVencida.Por_ProvNoAdm.
    END.

    IF W_ValProvision GT 0 THEN
       ASSIGN Creditos.Provision       = ROUND(W_ValProvision,0).

END.
  
