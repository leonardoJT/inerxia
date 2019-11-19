IF Inserted.Estado EQ 2 THEN DO:
   FIND FIRST Pro_Especiales WHERE Pro_Especiales.Cta_Cargos   EQ Inserted.Cuenta
                                OR Pro_Especiales.Cta_Recaudos EQ Inserted.Cuenta
                                OR Pro_Especiales.Cta_SucyAge  EQ Inserted.Cuenta
                             NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Especiales THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada" SKIP(0)
              "Tiene Productos Especiales Configurados."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.
   
   FIND FIRST Causa_Inte WHERE Causa_Inte.CtaCredito     EQ Inserted.Cuenta
                            OR Causa_Inte.CtaCredito_Cau EQ Inserted.Cuenta
                            OR Causa_Inte.CtaDebito      EQ Inserted.Cuenta
                            OR Causa_Inte.CtaDebito_Cau  EQ Inserted.Cuenta
                         NO-LOCK NO-ERROR.
   IF AVAILABLE Causa_Inte THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada" SKIP(0)
              "Tiene Causaciones Configuradas."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.
   
   FIND FIRST AjInfLineas WHERE AjInfLineas.Cta_Ajuste     EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_CorrAjuste EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_AxIAju     EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_CorrAxIAju EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_OrigenAj EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_Sobra    EQ Inserted.Cuenta
                             OR AjInfLineas.Cta_SucAge   EQ Inserted.Cuenta
                          NO-LOCK NO-ERROR.
   IF AVAILABLE AjInfLineas THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada" SKIP(0)
              "Tiene Ajustes a los Aportes Configurados."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.
   
   FIND FIRST CortoLargo WHERE CortoLargo.Cta_AsoAd            EQ Inserted.Cuenta
                            OR CortoLargo.Cta_AsoNa            EQ Inserted.Cuenta
                            OR CortoLargo.Cta_ContingenteCr    EQ Inserted.Cuenta
                            OR CortoLargo.Cta_ContingenteDb    EQ Inserted.Cuenta 
                            OR CortoLargo.Cta_ContrapartidaGar EQ Inserted.Cuenta
                            OR CortoLargo.Cta_FutGantia        EQ Inserted.Cuenta
                            OR CortoLargo.Cta_GarPenCancel     EQ Inserted.Cuenta
                            OR CortoLargo.Cta_NoaAd            EQ Inserted.Cuenta
                            OR CortoLargo.Cta_NoaNa            EQ Inserted.Cuenta
                            OR CortoLargo.Cta_VigGarAd         EQ Inserted.Cuenta
                            OR CortoLargo.Cta_VigGarNa         EQ Inserted.Cuenta
                         NO-LOCK NO-ERROR.
   IF AVAILABLE CortoLargo THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada Tiene" SKIP(0)
              "Configuraciones de Corto y Largo Asignadas."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.
   
   FIND FIRST Liqui_Int WHERE Liqui_Int.CtaCr_DifCob    EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_DifCobAso EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_Liq       EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_LiqAso    EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_Mora      EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_MoraAso   EQ Inserted.Cuenta
                           OR Liqui_Int.CtaCr_Ret       EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_DifCob    EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_DifCobAso EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_Liq       EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_LiqAso    EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_Mora      EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_MoraAso   EQ Inserted.Cuenta
                           OR Liqui_Int.CtaDb_Ret       EQ Inserted.Cuenta
                           OR Liqui_Int.CtaInt_Ant      EQ Inserted.Cuenta
                           OR Liqui_Int.CtaInt_AntAso   EQ Inserted.Cuenta
                           OR Liqui_Int.Cta_SucyAge     EQ Inserted.Cuenta
                        NO-LOCK NO-ERROR.
   IF AVAILABLE Liqui_Int THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada Tiene" SKIP(0)
              "Configuración Liquidación Intereses Asignados."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.
   
   FIND FIRST CarteraVencida WHERE CarteraVencida.Cta_AsoAdDb    EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoIntAdCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoIntAdDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoIntNaCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoIntNaDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoNaDb    EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoPrvAdCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoPrvAdDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoPrvNaCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_AsoPrvNaDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaAdDb    EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaIntAdCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaIntAdDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaIntNaCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaIntNaDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaNaDb    EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaPrvAdCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaPrvAdDb EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaPrvNaCr EQ Inserted.Cuenta
                                OR CarteraVencida.Cta_NoaPrvNaDb EQ Inserted.Cuenta
                             NO-LOCK NO-ERROR.
   IF AVAILABLE Liqui_Int THEN DO:
      MESSAGE "Cuenta no puede Ser Inactivada Tiene" SKIP(0)
              "Configuración Cartera Vencida Asignada."
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.   
   END.   
END.
