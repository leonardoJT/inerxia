DEFINE INPUT PARAMETER wcrage AS INTEGER.
DEFINE INPUT PARAMETER wcrnit AS CHARACTER.
DEFINE INPUT PARAMETER wcrcodcre AS INTEGER.
DEFINE INPUT PARAMETER wcrnumcre AS INTEGER.

DEFINE VAR wfec_proxpago AS DATE.

/* oakley */

DEFINE VAR wfec_inic AS DATE.
DEFINE VAR wfec_vcto AS DATE.
DEFINE VAR listado AS CHARACTER FORMAT "X(80)".

{INCLUIDO\VARIABLE.I "SHARED"}

RELEASE creditos.

DISABLE TRIGGERS FOR LOAD OF creditos.
DISABLE TRIGGERS FOR LOAD OF planpagos.

DEFI TEMP-TABLE CopPP LIKE PlanPagos.

DEFINE VAR ROWID_mov AS ROWID.
DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaCorAho LIKE Cuentas.Cuenta.

/*guarda las partidas de contabilizacion*/
DEFINE TEMP-TABLE TPartidas
    FIELD Tage LIKE Creditos.Agencia
    FIELD TCta LIKE Cuentas.Cuenta
    FIELD TCed LIKE Clientes.Nit
    FIELD TDoc LIKE Mov_Contable.Doc_Referencia

    /* oakley */

    FIELD TDsc LIKE Mov_Contable.Comentario
    FIELD TTip AS INTEGER FORMAT 9 /*1-Ahorro, 2-Credito*/
    FIELD TOpe LIKE Operacion.Cod_Operacion
    FIELD TDeb LIKE Ahorros.Sdo_Disponible
    FIELD TCre LIKE Ahorros.Sdo_Disponible.

/* Para la creacion de cuentas de orden */
DEFINE VAR W_NumCbt LIKE Comprobantes.Secuencia.
DEFINE VAR W_Cbte LIKE Comprobantes.Comprobante.
DEFI VAR W_CtaSyA LIKE CortoLargo.Cta_SYA.
DEFI VAR TotReg AS INTEG FORM "9999999".
DEFI VAR wplazo LIKE creditos.plazo.
DEFI VAR wtasa LIKE creditos.tasa.
DEFI VAR wperiodo AS INTEGER.
DEFINE VAR vdsdoproyec AS DECIMAL.

Listado = W_PathSpl + "PlanPagosCupoR-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

OUTPUT TO value(Listado).
FIND FIRST Creditos WHERE creditos.agencia = wcrage
                      AND creditos.nit = wcrnit
                      AND (Creditos.Cod_credito = 123 OR Creditos.Cod_credito = 870)
                      AND Creditos.Num_credito EQ wcrnumcre
                      AND creditos.estado = 2 NO-ERROR.
IF AVAILABLE(creditos) THEN DO:
    FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
    IF AVAILABLE(pro_creditos) THEN
        ASSIGN wplazo = pro_creditos.Pla_Maximo.

    FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
    IF AVAILABLE(indicadores) THEN
        wtasa = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1)  * 100) * 12.
    ELSE DO:
        MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    ASSIGN creditos.tasa = wtasa 
           creditos.plazo = wplazo
           creditos.Per_pago = 4.

    FOR EACH planpagos WHERE planpagos.agencia = creditos.agencia
                         AND planpagos.nit = creditos.nit
                         AND planpagos.cod_credito = creditos.Cod_credito
                         AND planpagos.num_credito = creditos.num_credito:
        DELETE planpagos.
    END.

    IF Creditos.Sdo_Capital LT 0 OR
       Creditos.Nit LE "0" OR
       Creditos.Fec_Desembolso EQ ? OR
       STRING(Creditos.Fec_Desembolso) LE " " OR
       Creditos.Monto LE 0 OR
       Creditos.Plazo LE 0 THEN DO:
        PUT creditos.agencia FORMAT "zz9" " "
            creditos.Nit FORMAT "X(12)" " "
            creditos.num_credito FORMAT "zzzzzzz9" " "
            creditos.Monto FORMAT "zzz,zzz,zz9" " "
            creditos.Fec_Desembolso FORMAT "99/99/9999" " "
            creditos.Plazo FORMAT "zzz9" " "
            creditos.Sdo_Capital FORMAT "zzz,zzz,zz9" SKIP(0).

        NEXT.
    END.

    ASSIGN Creditos.Fec_Desembolso = Creditos.Fec_Aprobac
           Creditos.Fec_PagAnt = Creditos.Fec_Aprobac.

    IF Creditos.Monto LT Creditos.Sdo_Capital THEN
        Creditos.Monto = Creditos.Sdo_Capital.

    IF Creditos.Sistema NE 1 AND (Creditos.Cuota LT Creditos.Monto) THEN
        Creditos.Sistema = 1.

    ASSIGN Creditos.Sdo_CapPag = Creditos.Monto - Creditos.Sdo_Capital
           Creditos.Val_Desembolso = Creditos.Monto
           Creditos.Sdo_Proyectado = Creditos.Monto
           Creditos.Capital_Acum = 0
           Creditos.Int_LiqAcum = 0
           Creditos.Sdo_Intpag = 0
           Creditos.Cuo_Pagadas = 0
           Creditos.Cuo_Atraso = 0
           Creditos.Dias_Atraso = 0
           Creditos.Val_Atraso = 0
           Creditos.Estado = 2
           Creditos.For_Interes = 1.

    /* Busqueda de periodo siguiente para facturacion */
    FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(per_facturacion) THEN DO:
        wperiodo = per_facturacion.per_factura +  1.

        FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.
        IF AVAILABLE(per_facturacion) THEN
            ASSIGN wfec_proxpago = per_facturacion.fec_limpago
                   wfec_inic = per_facturacion.fec_inicial
                   wfec_vcto = per_facturacion.fec_final.
    END.
    ELSE DO:
        MESSAGE "no se encontro Periodo Vigente en la facturacion actual" SKIP
                "Valide este datos con el Administrador del Sistema "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    ASSIGN creditos.fec_pago = wfec_proxpago
           Creditos.Cuo_Pagadas = 0.

    CREATE PlanPagos.
    ASSIGN PlanPagos.Agencia = Creditos.Agencia
           PlanPagos.Cod_Credito = Creditos.Cod_Credito
           PlanPAgos.Nit = Creditos.Nit
           PlanPagos.Num_Credito = Creditos.Num_credito
           PlanPagos.Pagare = Creditos.Pagare
           PlanPagos.Tip_Credito = Creditos.Tip_Credito
           PlanPagos.Nro_Cuota = 1
           PlanPagos.Fec_Inic = TODAY
           PlanPagos.Fec_Vcto = wfec_proxpago
           PlanPagos.Cuota = Creditos.Cuota
           PlanPagos.Tasa = Creditos.Tasa
           PlanPagos.Plazo = Creditos.Plazo
           PlanPagos.Int_LiqPdo = 0
           PlanPagos.Int_LiqAcum = 0
           PlanPagos.Int_MoraPdo = 0
           PlanPagos.Int_MoraAcum = 0
           PlanPagos.Capital_Pdo = 0
           PlanPagos.Capital_Acum = 0
           PlanPagos.Pagos_IntPdo = 0
           PlanPagos.Pagos_IntAcum = 0
           PlanPagos.Pagos_MoraPdo = 0
           PlanPagos.Pagos_MoraAcum = 0
           PlanPagos.Pagos_CapitalAcum = 0
           PlanPagos.Pagos_CapitalPdo = 0
           PlanPagos.Cargos_Pdo = 0
           PlanPagos.Cargos_Acum = 0
           PlanPagos.Pagos_OtrosPdo = 0
           PlanPagos.Pagos_OtrosAcum = 0
           PlanPagos.Cuo_Pagas = 0
           PlanPagos.Fec_ProxPago = wfec_proxpago
           PlanPagos.Id_PdoMes = 1
           PlanPagos.Monto_Actual = Creditos.Monto
           PlanPagos.Provision = 0
           PlanPagos.Categoria = ""
           PlanPagos.ITasa_Mora = 0
           TotReg = TotReg + 1.
END.
ELSE
    MESSAGE "No encontro la cedula"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN cargar_diferidos.

SESSION:SET-WAIT-STATE("").


PROCEDURE cargar_diferidos.
    FOR EACH TPartidas:
        DELETE TPartidas.
    END.

    TransAnular:
    DO TRANS ON ERROR UNDO TransAnular:
        RUN CortoLargoCreditos NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.

        FIND Comprobantes WHERE Comprobantes.Agencia EQ Creditos.Agencia
                            AND Comprobantes.Comprobante EQ 4 NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
                    "de la reversa del crédito. Configurado en corto y largo" SKIP
                    "Rectifique con el Administrador!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        ASSIGN W_Cbte = Comprobantes.Comprobante
               W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

        RUN Contabilizar_Partidas NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.
END PROCEDURE.


PROCEDURE cortolargocreditos:
    FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ creditos.Agencia
                            AND CortoLargo.Clase_Producto EQ 2
                            AND CortoLargo.Cod_Producto EQ creditos.cod_credito
                            AND CortoLargo.Cta_ContingenteDB ne ""
                            AND CortoLargo.Cta_ContingenteCR ne ""
                            AND CortoLargo.Comprobante ne 0 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CortoLargo THEN DO:
        MESSAGE "No se ha encontrado la configuración de Corto y largo" SKIP
                "o existe algun tipo de inconsistencia en su configuración" SKIP
                "Comunique esta inconsistencia al Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.

        MESSAGE INTEGER(creditos.agencia)
            VIEW-AS ALERT-BOX.

        MESSAGE INTEGER(creditos.cod_credito)
            VIEW-AS ALERT-BOX.

        RETURN ERROR.
    END.
    ELSE DO:
        W_CtaSyA = CortoLargo.Cta_SYA.

        CREATE TPartidas.
        ASSIGN Tpartidas.Tage = Creditos.Agencia
               Tpartidas.TCta = CortoLargo.Cta_ContingenteCR
               TPartidas.TDsc = "Aprobación de Solicitud"
               TPartidas.TDoc = STRING(Creditos.Num_Solicitud)
               TPartidas.TTip = 2
               TPartidas.TOpe = 0
               TPartidas.TCre = Creditos.Monto.

        CREATE TPartidas.
        ASSIGN Tpartidas.Tage = Creditos.Agencia
               Tpartidas.TCta = CortoLargo.Cta_ContingenteDB
               TPartidas.TDsc = "Aprobación de Solicitud"
               TPartidas.TDoc = STRING(Creditos.Num_Solicitud)
               TPartidas.TTip = 2
               TPartidas.TOpe = 0
               TPartidas.TDeb = Creditos.Monto.

        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
        CASE Clientes.Tipo_Vinculo:
            WHEN 1 THEN W_CtaCorCre = CortoLargo.Cta_AsoAd.
            OTHERWISE W_CtaCorCre = CortoLargo.Cta_NoAAd.
        END CASE.

        IF W_CtaCorCre EQ "" THEN DO:
            MESSAGE "La cuenta de corto y largo no esta configurada" SKIP
                    "para el producto de crédito. se cancela la operación" SKIP
                    "de desembolso!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.
END PROCEDURE.

PROCEDURE Contabilizar_partidas.
    IF W_Cbte EQ 0 OR W_NumCbt EQ 0 THEN DO:
        MESSAGE "No se ha encontrado el Cpte o el número consecutivo" SKIP
                "se cancela la operación de desembolso!"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    FOR EACH TPartidas:
        IF TPartidas.TAge LE 0 THEN DO:
            MESSAGE "Existe una Contrapartida sin Agencia, No se acepta la Operacion."
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = TPartidas.TAge
               Mov_Contable.Comprobante = W_Cbte
               Mov_Contable.Cuenta = TPartidas.TCta
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = TPartidas.TDsc
               Mov_Contable.Usuario = W_Usuario
               Mov_contable.Nit = Creditos.Nit
               Mov_Contable.Cen_Costos = 999
               Mov_Contable.Destino = W_Agencia
               Mov_Contable.Num_Documento = INTEGER(W_NumCbt)
               Mov_Contable.Doc_Referencia = Creditos.Pagare
               Mov_Contable.Enlace = string(Creditos.Num_Credito)
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion NO-ERROR.

        IF TPartidas.TCta EQ W_CtaSyA THEN
            Mov_contable.Nit = TPartidas.TCed.

        IF TPartidas.TDeb GT 0 THEN
            ASSIGN Mov_Contable.DB = TPartidas.TDeb NO-ERROR.
        ELSE
            ASSIGN Mov_Contable.CR = TPArtidas.TCre NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error Grabando Mov_Contable, en Proc.Contabilizar_Partidas."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
        ELSE
            ASSIGN ROWID_mov = ROWID(mov_contable).
    END.

END PROCEDURE.

