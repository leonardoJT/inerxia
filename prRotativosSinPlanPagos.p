DEFINE VAR wfec_proxpago AS DATE.                
DEFINE VAR wfec_inic     AS DATE.                
DEFINE VAR wfec_vcto     AS DATE.                
DEFINE VAR wplazo        LIKE creditos.plazo.
DEFINE VAR wtasa         LIKE creditos.tasa.
DEFINE VAR wperiodo      AS INTEGER.
DEFINE VAR vdsdoproyec   AS DECIMAL.
DEFINE VAR listado AS CHARACTER FORMAT "X(80)".

Listado = "c:\info_juriscoop\PlanPagosCreados-" + STRING(TODAY,"99999999") + "-" + STRING(TIME) + ".csv".
OUTPUT TO value(Listado).
PUT "agencia;Nit;Num_Credito;Cod_Credito;Fec_Desembolso;Plazo;Cuota;Monto;Sdo_Capital;nro_cuota(PP);Fec_ProxPago(PP)" SKIP. 

FOR EACH creditos WHERE 
/*     creditos.estado EQ 2 AND */
    (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870)
    EXCLUSIVE:
    FIND FIRST planpagos WHERE
        planpagos.agencia     = creditos.agencia     AND   
        planpagos.nit         = creditos.nit         AND   
        planpagos.cod_credito = creditos.Cod_credito AND   
        planpagos.num_credito = creditos.num_credito
        NO-ERROR.
    IF NOT AVAILABLE(planpagos) THEN DO:
       FIND FIRST pro_creditos WHERE
            pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
       IF AVAILABLE(pro_creditos) THEN
           ASSIGN  wplazo   = pro_creditos.Pla_Maximo.
           FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
           IF AVAILABLE(indicadores) THEN DO:
              ASSIGN wtasa = (((EXP( (indicadores.tasa / 100) + 1,1 / 12)) - 1 )  * 100) * 12.
           END.
       ELSE Do:
           MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN.
       END.

       ASSIGN creditos.tasa = wtasa   creditos.plazo = wplazo  creditos.Per_pago = 4.
/*        FOR EACH planpagos WHERE                                   */
/*                 planpagos.agencia     = creditos.agencia     AND  */
/*                 planpagos.nit         = creditos.nit         AND  */
/*                 planpagos.cod_credito = creditos.Cod_credito AND  */
/*                 planpagos.num_credito = creditos.num_credito:     */
/*            DELETE planpagos.                                      */
/*        END.                                                       */
/*                                                                                                           */
       IF Creditos.Sdo_Capital       LT 0
          OR Creditos.Nit            LE "0"
          OR Creditos.Fec_Desembolso EQ ? OR STRING(Creditos.Fec_Desembolso) LE " "
          OR Creditos.Monto          LE 0
          OR Creditos.Plazo          LE 0 THEN DO:
/*              PUT    creditos.agencia            FORMAT "zz9"           " "      */
/*                     creditos.Nit                FORMAT "X(12)"         " "      */
/*                     creditos.num_credito        FORMAT "zzzzzzz9"      " "      */
/*                     creditos.Monto              FORMAT "zzz,zzz,zz9"   " "      */
/*                     creditos.Fec_Desembolso     FORMAT "99/99/9999"    " "      */
/*                     creditos.Plazo              FORMAT "zzz9"          " "      */
/*                     creditos.Sdo_Capital        FORMAT "zzz,zzz,zz9"   SKIP(0). */
          NEXT.
       END.

       ASSIGN Per_pago = 4
              Creditos.Fec_Desembolso = Creditos.Fec_Aprobac
              Creditos.Fec_PagAnt     = Creditos.Fec_Aprobac.

       IF Creditos.Monto LT Creditos.Sdo_Capital THEN
          Creditos.Monto = Creditos.Sdo_Capital.

       IF Creditos.Sistema NE 1 AND (Creditos.Cuota LT Creditos.Monto) THEN
          Creditos.Sistema = 1.

       ASSIGN Creditos.Sdo_CapPag     = Creditos.Monto - Creditos.Sdo_Capital
              Creditos.Val_Desembolso = Creditos.Monto
              Creditos.Sdo_Proyectado = Creditos.Monto
              Creditos.Capital_Acum   = 0
              Creditos.Int_LiqAcum    = 0
              Creditos.Sdo_Intpag     = 0
              Creditos.Cuo_Pagadas    = 0
              Creditos.Cuo_Atraso     = 0
              Creditos.Dias_Atraso    = 0
              Creditos.Val_Atraso     = 0
              Creditos.Estado         = 2
              Creditos.For_Interes    = 1.

       /* Busqueda de periodo siguiente para facturacion */
       FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(per_facturacion) THEN DO:
           wperiodo = per_facturacion.per_factura +  1.
           FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.
           IF AVAILABLE(per_facturacion) THEN
              ASSIGN wfec_proxpago  = per_facturacion.fec_limpago
                     wfec_inic      = per_facturacion.fec_inicial
                     wfec_vcto      = per_facturacion.fec_final.

       END.
       ELSE DO:
           MESSAGE "no se encontro Periodo Vigente en la facturacion actual" SKIP
                   "Valide este datos con el Administrador del Sistema "
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.

       ASSIGN creditos.fec_pago = wfec_proxpago
              Creditos.Cuo_Pagadas = 0.

       CREATE PlanPagos.
       ASSIGN PlanPagos.Agencia                 = Creditos.Agencia
              PlanPagos.Cod_Credito             = Creditos.Cod_Credito
              PlanPAgos.Nit                     = Creditos.Nit
              PlanPagos.Num_Credito             = Creditos.Num_credito
              PlanPagos.Pagare                  = Creditos.Pagare
              PlanPagos.Tip_Credito             = Creditos.Tip_Credito
              PlanPagos.Nro_Cuota               = 1
              PlanPagos.Fec_Inic                = TODAY
              PlanPagos.Fec_Vcto                = wfec_proxpago
              PlanPagos.Cuota                   = Creditos.Cuota
              PlanPagos.Tasa                    = Creditos.Tasa
              PlanPagos.Plazo                   = Creditos.Plazo
              PlanPagos.Int_LiqPdo              = 0
              PlanPagos.Int_LiqAcum             = 0
              PlanPagos.Int_MoraPdo             = 0
              PlanPagos.Int_MoraAcum            = 0
              PlanPagos.Capital_Pdo             = 0
              PlanPagos.Capital_Acum            = 0
              PlanPagos.Pagos_IntPdo            = 0
              PlanPagos.Pagos_IntAcum           = 0
              PlanPagos.Pagos_MoraPdo           = 0
              PlanPagos.Pagos_MoraAcum          = 0
              PlanPagos.Pagos_CapitalAcum       = 0
              PlanPagos.Pagos_CapitalPdo        = 0
              PlanPagos.Cargos_Pdo              = 0
              PlanPagos.Cargos_Acum             = 0
              PlanPagos.Pagos_OtrosPdo          = 0
              PlanPagos.Pagos_OtrosAcum         = 0
              PlanPagos.Cuo_Pagas               = 0
              PlanPagos.Fec_ProxPago            = wfec_proxpago
              PlanPagos.Id_PdoMes               = 1
              PlanPagos.Monto_Actual            = Creditos.Monto
              PlanPagos.Provision               = 0
              PlanPagos.Categoria               = ""
              PlanPagos.ITasa_Mora              = 0.

        PUT creditos.agencia        FORMAT "zz9"             ";"     
            creditos.Nit            FORMAT "X(12)"           ";" 
            creditos.num_credito    FORMAT "zzzzzzz9"        ";" 
            creditos.cod_credito    FORMAT "999"             ";"
            creditos.Fec_Desembolso FORMAT "99/99/9999"      ";"
            creditos.Plazo          FORMAT "zzz9"            ";"
            creditos.cuota          FORMAT "zzz,zzz,zzz,zz9" ";"
            creditos.Monto          FORMAT "zzz,zzz,zzz,zz9" ";"
            creditos.Sdo_Capital    FORMAT "zzz,zzz,zzz,zz9" ";"  
            planpagos.nro_cuota     FORMAT "9"               ";"
            PlanPagos.Fec_ProxPago  FORMAT "99/99/9999"
            SKIP(0). 
      END.
END.
