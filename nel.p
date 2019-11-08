{d:\sps\soportes\fodun\prog\INCLUIDO\VARIABLE.I "NEW GLOBAL SHARED"}
{d:\sps\soportes\fodun\prog\INCLUIDO\CLIENTES.I "NEW GLOBAL SHARED"}
{d:\sps\soportes\fodun\prog\Incluido\VARCON.I   "NEW GLOBAL SHARED"}

DEFINE VARIABLE W_Programa      AS WIDGET-HANDLE. 
DEFINE VARIABLE W_NomArc   AS CHARACTER FORMAT "X(60)" INITIAL "".
DEFINE NEW SHARED VARIABLE FCODIGO   AS CHARACTER FORMAT "X(14)".
/*DEFINE NEW GLOBAL SHARED VAR W_SGiro      LIKE Pro_Creditos.. */
DEFINE NEW GLOBAL SHARED VAR W_SiModifica AS    LOGICAL.
DEFINE NEW GLOBAL SHARED VAR W_TotPorD    AS DECIMAL FORMAT ">>9.9999".
DEFINE NEW GLOBAL SHARED VAR W_Estado     LIKE Solicitud.Estado.
DEFINE NEW GLOBAL SHARED VAR W_TotValD    LIKE Asesoria.monto.
DEFINE NEW GLOBAL SHARED VAR W_ID         AS INTEGER FORMAT "9".  
DEFINE NEW GLOBAL SHARED VAR W_NitGtia    like solicitud.Nit.
DEFINE NEW GLOBAL SHARED VAR W_Asesoria   like solicitud.Num_Asesoria.
DEFINE NEW GLOBAL SHARED VAR W_Solicitud  like solicitud.Num_Solicitud.
DEFINE NEW GLOBAL SHARED VAR W_PagareS    LIKE Creditos.Pagare.
DEFINE NEW GLOBAL SHARED VAR W_SuperUsu   AS LOGICAL.
DEFINE NEW GLOBAL SHARED VAR W_Monto      LIKE Asesoria.monto.
DEFINE NEW GLOBAL SHARED VAR Codigo       LIKE Pro_Credito.Cod_credito.
DEFINE NEW GLOBAL SHARED VAR W_ClaPro     AS INTEGER.
DEFINE NEW GLOBAL SHARED VAR W_Procredito AS INTEGER.
DEFINE NEW GLOBAL SHARED VAR W_CodproA    AS INTEGER.
DEFINE NEW GLOBAL SHARED VAR W_ClaProC    AS INTEGER.
DEFINE NEW GLOBAL SHARED VAR W_ControlDisp AS logical.    
DEFINE NEW GLOBAL SHARED VAR W_CodProC    LIKE Pro_ahorros.Cod_ahorro.
DEFINE NEW GLOBAL SHARED VAR W_Ofitbajo   LIKE Agencias.Agencia. 
DEFINE NEW GLOBAL SHARED VAR W_RetParc    LIKE Pro_Ahorros.Id_RetParcial.
DEFINE NEW GLOBAL SHARED VAR W_Sobregi    LIKE Pro_Ahorros.ID_Sobregiro.
DEFINE NEW GLOBAL SHARED VAR W_SinoQ      as logical.
DEFINE NEW GLOBAL SHARED VAR W_Talonar    LIKE Pro_Ahorros.ID_Talonario.
DEFINE NEW GLOBAL SHARED VAR W_Ind        LIKE Indicadores.Indicador.
DEFINE NEW GLOBAL SHARED VAR W_Rango      LIKE Indicadores.Rango.
DEFINE NEW GLOBAL SHARED VAR W_Est        LIKE Indicadores.Estado.
DEFINE NEW GLOBAL SHARED VAR W_FecVcto    LIKE Indicadores.FecVcto.
DEFINE NEW GLOBAL SHARED VAR W_Fec        LIKE Indicadores.Fecha.
DEFINE NEW GLOBAL SHARED VAR W_Estacion   LIKE Estaciones.Estacion.
DEFINE NEW GLOBAL SHARED VAR W_CpdProe    LIKE Pro_Especiales.Cod_Producto.
DEFINE NEW GLOBAL SHARED VAR W_codoperacion LIKE Pro_Credito.Cod_credito.
    /*DEFINE NEW GLOBAL SHARED VAR W_TPd LIKE Operacion.Tip_Producto.*/
DEFINE NEW GLOBAL SHARED VAR W_Indi1 LIKE Pro_Creditos.Cod_Tasa.
DEFINE NEW GLOBAL SHARED VAR W_Indi2 LIKE Pro_Creditos.Cod_TasaMora.
W_Nom_Agencia = "Agencia PRUEBA".
W_Entidad     = 1.
W_Estacion    = "010".
W_Nom_Entidad = "COOPERATIVA PRUEBA".
W_PathSpl     = "C:\sicobel".
W_Path        = "c:\DESARROLLO\BELEN\PRG\".
W_Agencia     = 1.
w_cencosgral  = 999.
W_Usuario     = "205".
W_Fecha       = TODAY.
W_prioridad   = 4.

RUN RUTINAS.P  PERSISTENT SET W_MANIJA. 
RUN RUTITAQ.P  PERSISTENT SET W_MANTAQ.
RUN RUTFINAN.P PERSISTENT SET W_MANFIN.
RUN RUTICON.P  PERSISTENT SET W_MANCON. 
 
DEFINE VARIABLE p_vrinteres LIKE creditos.sdo_capital.
DEFINE VARIABLE p_vrcapital LIKE creditos.sdo_capital.



DEFINE VARIABLE xfec AS DATE.
DEFINE VARIABLE xpro AS DECIMAL.
DEFINE VARIABLE xatr AS DECIMAL.
DEFINE VARIABLE xubica AS date.
DEFINE VARIABLE xdiasatr AS DECIMAL.
/*

FOR EACH creditos WHERE nit = "50891307" AND num_credito = 45654:
    xfec = Creditos.Fec_Desembolso.
    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
      xfec = Creditos.Fec_PagAnti.

RUN HSP IN W_manfin (INPUT xfec,
                         INPUT creditos.monto,
                         INPUT creditos.cuota,
                         INPUT creditos.monto,
                         INPUT creditos.tasa,
                         INPUT creditos.plazo,
                         INPUT creditos.sdo_capital,
                         OUTPUT xpro,
                         OUTPUT xatr,
                         OUTPUT xfpro,
                         OUTPUT xUbica,
                         OUTPUT xDiasatr).
   
    DISPLAY xpro FORMAT "999,999,999"
            xatr FORMAT "999,999,999"
            xubica 
            xdiasatr.

END.

  */
ON RETURN RETURN.
