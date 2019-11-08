DEFINE VAR W_CalMayor   LIKE Clientes.Calificacion.
DEFINE VAR W_Aportes    LIKE Ahorros.Sdo_Disponible.
DEFINE VAR W_CalCliente LIKE Clientes.Calificacion.
DEFINE VAR W_PorAporte  AS DECIMAL FORMAT "99.999999".
DEFINE VAR W_TotCredito LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ApoDesc    LIKE Creditos.Sdo_Capital.

DEFINE VAR W_ValGarAdm    LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValHipoteca  LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValNoHipoteca  LIKE Creditos.Sdo_Capital.
DEFINE VAR W_Valdefecto   LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValProvision LIKE Creditos.Sdo_Capital.

DEFINE TEMP-TABLE Pro
    FIELD Agencia     LIKE Creditos.Agencia
    FIELD Nit         LIKE Creditos.Nit
    FIELD Cod_Credito LIKE Creditos.Cod_Credito
    FIELD Pagare      LIKE Creditos.Pagare
    FIELD Num_Credito LIKE Creditos.Num_Credito
    FIELD Sdo_Capital LIKE Creditos.Sdo_Capital
    FIELD Provision   LIKE Creditos.Provision
    FIELD Cal_Credito LIKE Creditos.Cod_Califica
    FIELD Cal_Cliente LIKE Clientes.Calificacion
    FIELD ApoDistribu LIKE Ahorros.Sdo_Disponible
    FIELD ValDefecto  LIKE Ahorros.Sdo_Disponible
    FIELD TotAportes  LIKE Ahorros.Sdo_Disponible
    FIELD Reestru     LIKE Clientes.Reestructurado
    FIELD ValGarant   LIKE Ahorros.Sdo_Disponible
    FIELD diamora     LIKE Creditos.Dias_atraso.

DISABLE TRIGGERS FOR LOAD OF creditos.

DEFINE TEMP-TABLE TotCre
    FIELD Nit LIKE Creditos.Nit
    FIELD Tot LIKE Creditos.Sdo_Capital.
/*MESSAGE "comienza" VIEW-AS ALERT-BOX.*/
/*recorre la tabla de creditos para hallar el total de credito para la busqueda
  del porcentaje que le corresponde de descuento de los aportes*/
FOR EACH Creditos WHERE Creditos.Sdo_Capital GT 0 
    /*AND Creditos.Nit EQ "71627544"*/
    BREAK BY Creditos.Nit:
    IF FIRST-OF(Creditos.Nit) THEN DO: 

       CREATE TotCre.
       ASSIGN TotCre.Nit = Creditos.Nit.
    END.
    TotCre.Tot = TotCre.Tot + Creditos.Sdo_Capital.
END.
/*MESSAGE "termino calculo total creditos" VIEW-AS ALERT-BOX.*/

DO TRANSACTION:
FOR EACH Creditos WHERE Creditos.Sdo_Capital GT 0 
    /*AND Creditos.Nit EQ "71627544"*/
    BREAK BY Creditos.Nit 
          BY Creditos.Tip_Credito
          BY Creditos.Cod_Califica DESCENDING:
    ASSIGN W_ValProvision = 0
           Creditos.Provision = 0.
   /* IF Creditos.Nit EQ "8290773" THEN
       MESSAGE "Hola"  VIEW-AS ALERT-BOX.*/
    IF FIRST-OF(Creditos.Nit) THEN DO:
       /*busca el total de creditos del nit para el calculo
        l porcentaje de aportes que le corresponde*/
        FIND TotCre WHERE TotCre.Nit EQ Creditos.Nit.
        IF AVAILABLE TotCre THEN W_TotCredito = TotCre.Tot.
       /*busca calificacion en clientes si ha sido reestructurado*/
        FIND Clientes WHERE 
             Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           W_CalCliente = Clientes.Calificacion.
       /*calcula el total de los aportes del cliente*/
        W_Aportes = 0.
        FOR EACH Ahorros WHERE 
                 Ahorros.Tip_Ahorro EQ 4 AND
                 Ahorros.Nit        EQ Creditos.Nit NO-LOCK:
            W_Aportes = W_Aportes + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).
        END.
    END.
    IF FIRST-OF(Creditos.Tip_Credito) THEN DO:
       W_CalMayor = Creditos.Cod_Califica.
    END.
    /*busqueda de cartera vencida*/
    IF W_CalCliente GT Creditos.Cod_Califica THEN
       W_CalMayor = W_CalCliente.

    IF W_CalMayor GE 6 AND Creditos.Dias_Atraso LT 361 THEN DO:
        FIND CarteraVencida WHERE 
             CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND
             CarteraVencida.Cod_Califica EQ 6 NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND CarteraVencida WHERE 
             CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND
             CarteraVencida.Cod_Califica EQ W_CalMayor NO-LOCK NO-ERROR.
    END.
    IF NOT AVAILABLE CarteraVencida THEN DO:
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
    CREATE Pro.
    ASSIGN Pro.Agencia     = Creditos.Agencia
           Pro.Nit         = Creditos.Nit
           Pro.Cod_Credito = Creditos.Cod_Credito
           Pro.Num_Credito = Creditos.Num_Credito
           Pro.Sdo_Capital = Creditos.Sdo_Capital
           Pro.Cal_Credito = Creditos.Cod_Califica
           Pro.TotAportes  = W_Aportes
           Pro.Cal_Cliente = W_CalCliente
           Pro.Pagare      = Creditos.Pagare
           Pro.diamora     = Creditos.Dias_atraso
           Pro.ValGarant   = 0.

    /*busca las garantias del credito*/
    ASSIGN W_ValGarAdm     = 0
           W_ValHipoteca   = 0
           W_ValNoHipoteca = 0
           W_ValDefecto    = 0.
    FOR EACH Garantias WHERE /*Garantias.Agencia EQ Creditos.Agencia AND*/
             Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
             Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
             Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
             Garantias.Num_Credito   EQ Creditos.Num_Credito   AND
             Garantias.Estado      EQ 1 NO-LOCK:
        W_ValGarAdm = W_ValGarAdm + Garantias.Val_Bien.
        IF Garantias.Tipo_Garantia EQ 1 THEN
           W_ValHipoteca = W_ValHipoteca + Garantias.Val_Bien.
        ELSE
           W_ValNoHipoteca = W_ValNoHipoteca + Garantias.Val_Bien.

        ASSIGN Pro.ValGarant = Pro.ValGarant + Garantias.Val_Bien.
    END.

    /*calculo  aportes descontados*/
    ASSIGN W_PorAporte     = (Pro.Sdo_Capital * 100) / W_TotCredito
           W_ApoDesc       = W_Aportes * (W_PorAporte / 100)
           Pro.ApoDistribu = W_ApoDesc.

    /*Calculo de La Provision*/ 
    IF W_ValGarAdm GT 0 THEN DO:
       IF W_ValHipoteca GT 0 THEN
          W_ValDefecto   = (W_ValHipoteca * CarteraVencida.Porc_DefGarantia).
       IF W_ValNoHipoteca GT 0 THEN
          W_ValDefecto   = W_ValDefecto + (W_ValNoHipoteca * CarteraVencida.Porc_DefNoHipoteca).
       ASSIGN Pro.ValDEfecto = W_ValDefecto.
       IF (Creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) GT 0 THEN
          W_ValProvision = (Creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) * CarteraVencida.Porc_Admisible.
    END.
    ELSE DO: 
      IF (Creditos.Sdo_Capital - W_ApoDesc) GT 0 THEN
        W_ValProvision = (Creditos.Sdo_Capital - W_ApoDesc) * CarteraVencida.Por_ProvNoAdm.
    END.
    ASSIGN Pro.Provision      = W_ValProvision
           Creditos.Provision = W_ValProvision
           Creditos.Cod_Califica = W_CalMayor.
    CASE W_CalMayor:
        WHEN 1 THEN Creditos.Categoria = "A".
        WHEN 2 THEN Creditos.Categoria = "A".
        WHEN 3 THEN Creditos.Categoria = "B".
        WHEN 4 THEN Creditos.Categoria = "C".
        WHEN 5 THEN Creditos.Categoria = "D".
        OTHERWISE  Creditos.Categoria = "E".
    END CASE.
END.
END. /* fin de la transaccion */

DEFINE VAR TotProvision AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotProAge    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".

/*MESSAGE "termino creacion temporal de provision" VIEW-AS ALERT-BOX.*/

OUTPUT TO "c:\SICOBEL\provision_J.txt".
/*          1         2         3         4         5         6         7         8         9         10        1         2         3         4         5
 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
DISPLAY 
  "AGE NIT          CRE NUMCREDIT           SDO.CAPITAL   PROVISION CACRE CACLI        APORTESDISTRIB            VALDEFECTO         TOTAL APORTES  REESTRUCT   PAGARE   DIAMORA   GARANTIA"
WITH FRAME FTI WIDTH 300.

FOR EACH Pro BREAK BY Pro.Agencia BY Pro.Nit:
    IF FIRST-OF(Pro.Agencia) THEN TotProAge = 0.
    DISPLAY 
       Pro.Agencia     
       Pro.Nit         FORMAT "x(11)"
       Pro.Cod_Credito 
       Pro.Num_Credito 
       Pro.Sdo_Capital 
       Pro.Provision   
       Pro.Cal_Credito 
       Pro.Cal_Cliente 
       Pro.ApoDistribu 
       Pro.ValDefecto  
       Pro.TotAportes  
       Pro.Reestru     
       Pro.Pagare     
       Pro.diamora    
       Pro.ValGarant  
    WITH FRAME Fp WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE.
    ASSIGN TotProAge    = TotProAge + Pro.Provision.
    IF LAST-OF(Pro.Agencia) THEN DO:
       DISPLAY "Total Provision Agencia : " Pro.Agencia " - $ " TotProaGE WITH NO-LABELS.
       ASSIGN  TotProvision = TotProvision  + TotProAge
               TotProAge = 0.
    END.
END.
DISPLAY "Total Provision: " TotProvision.
OUTPUT CLOSE.


