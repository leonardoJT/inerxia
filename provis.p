 DEFINE VAR W_ValGarAdm    LIKE Creditos.Sdo_Capital.
 DEFINE VAR W_ValHipoteca  LIKE Creditos.Sdo_Capital.
 DEFINE VAR W_ValNoHipoteca  LIKE Creditos.Sdo_Capital.
 DEFINE VAR W_Valdefecto   LIKE Creditos.Sdo_Capital.
 DEFINE VAR W_ValProvision LIKE Creditos.Sdo_Capital.
 DEFINE VAR W_aporte       LIKE ahorros.sdo_disponible.

 DEFINE VAR WK_CtaProDeb   LIKE Cuentas.Cuenta.
 DEFINE VAR WK_CtaProCre   LIKE Cuentas.Cuenta.
  
FOR EACH creditos WHERE sdo_capital > 0:
    W_ValGarAdm = 0.
 /*FIND FIRST CarteraVencida WHERE 
      CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND
      CarteraVencida.Per_Inicial  LE Creditos.Dias_Atraso AND
      CarteraVencida.Per_Final    GE Creditos.Dias_Atraso NO-LOCK NO-ERROR.*/
  FIND FIRST CarteraVencida WHERE 
      CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND
      CarteraVencida.categoria    EQ creditos.categoria NO-LOCK NO-ERROR.
 
 IF NOT AVAILABLE CarteraVencida THEN DO:
     DISPLAY creditos WITH 1 COL.
    MESSAGE "La Configuración para la Clasificación de Cartera" SKIP
            "no existe en la Tabla CARTERAVENCIDA para el" SKIP(1)
            "Producto: " Creditos.Cod_Credito  SKIP 
            "Con un Rango valido que contenga el Número: " Creditos.Dias_Atraso SKIP(1)
            "Se cancela el proceso de Calificación" SKIP
            "Conmuniquese con el Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
 END.
        
 FOR EACH Garantias WHERE 
          Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
          Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
          Garantias.Num_Credito   EQ Creditos.Num_Credito   AND
          Garantias.Estado      EQ 1 NO-LOCK:
     IF garantias.tipo_garantia = 1 THEN
        W_ValGarAdm = W_ValGarAdm + Garantias.Val_Bien.
 END.
 
 ASSIGN W_aporte = 0.
 FOR EACH ahorros WHERE ahorros.nit = creditos.nit NO-LOCK:
     W_aporte = w_aporte + ahorros.sdo_disponible + ahorros.sdo_canje.
 END.



 IF W_ValGarAdm GT 0 OR W_aporte GE creditos.sdo_capital THEN DO: /*cuando tiene garantia admisible*/
    IF Creditos.FOR_Pago EQ 2 THEN 
       ASSIGN creditos.cta_contable = CarteraVencida.Cta_AsoAdDB.
    ELSE 
       ASSIGN creditos.cta_contable = CarteraVencida.Cta_NoaAdDB.
 END.
 ELSE DO: /*Garantia personal*/
    IF Creditos.FOR_Pago EQ 2 THEN 
       ASSIGN creditos.cta_contable = CarteraVencida.Cta_AsoNaDB.
    ELSE 
       ASSIGN creditos.cta_contable = CarteraVencida.Cta_NoANaDB.
 END.
 END.

