{INCLUIDO\VARIABLE.I "NEW GLOBAL SHARED"}
{INCLUIDO\CLIENTES.I "NEW GLOBAL SHARED"}
{Incluido\VARCON.I   "NEW GLOBAL SHARED"}

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


DEFINE VARIABLE xfec AS DATE NO-UNDO.
DEFINE VARIABLE xpro AS DECIMAL NO-UNDO.
DEFINE VARIABLE xatr AS DECIMAL NO-UNDO.
DEFINE VARIABLE xubica AS DATE NO-UNDO.
DEFINE VARIABLE xfpro AS DATE NO-UNDO.
DEFINE VARIABLE xdiasatr AS DECIMAL NO-UNDO.
DEFINE VARIABLE W_Frec AS INTEGER NO-UNDO.
DEFINE VARIABLE W_NroPer AS INTEGER     NO-UNDO.
DEFINE VARIABLE W_N AS DECIMAL     NO-UNDO.

DEFINE VARIABLE extrasXpagar AS DECIMAL     NO-UNDO.


FOR EACH creditos WHERE estado = 2:
    IF cod_CREDITO = 570 THEN NEXT.
    IF COD_CREDITO = 870 THEN NEXT.
    xfec = Creditos.Fec_Desembolso .
    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
      xfec = Creditos.Fec_PagAnti .
    
    extrasXpagar = 0.
    FOR EACH extras WHERE  Num_Solicitud = creditos.num_solicitud AND Fec_Vcto GE TODAY: 
        extrasXpagar = extrasxpagar + Vr_CuoExtra.
    END.
/* Halla la frecuencia */
ASSIGN W_Frec = 15
        W_NroPer = 24.
 
 CASE creditos.Per_Pago:
    WHEN 1 THEN DO:
        ASSIGN W_Frec = 7
               W_NroPer = 52.
        END.
    WHEN 2 THEN DO: 
        ASSIGN W_Frec = 10
               W_NroPer = 36.
    END.
    WHEN 3 THEN DO: 
        ASSIGN W_Frec = 15
               W_NroPer = 24.
    END.
    WHEN 4 THEN DO: 
        ASSIGN W_Frec = 30
               W_NroPer = 12.
    END.
    WHEN 5 THEN DO: 
        ASSIGN W_Frec =     60
               W_NroPer =   6.
    END.
    WHEN 6 THEN DO: 
        ASSIGN W_Frec = 90
               W_NroPer = 4.
    END.
    WHEN 7 THEN DO: 
        ASSIGN W_Frec = 120
               W_NroPer = 3.
    END.
    WHEN 8 THEN DO: 
        ASSIGN W_Frec = 180
               W_NroPer = 2.
    END.
    WHEN 9 THEN DO: 
        ASSIGN W_Frec = 360
               W_NroPer = 1.
    END.
END CASE.



ASSIGN W_N = decimal(creditos.tasa / W_NroPer) / 100.

/* HALLA CAPITAL MORA */
RUN HSM IN W_manfin (INPUT xfec,
                     INPUT creditos.cuota,
                     INPUT W_N,
                     INPUT creditos.plazo,
                     INPUT W_Frec,
                     OUTPUT xpro).


 IF xpro +  extrasXpagar GT creditos.monto THEN
        ASSIGN  creditos.sdo_proyectado = creditos.monto
                creditos.val_atraso    = 0
                creditos.dias_atraso = 0.
 ELSE 
      ASSIGN  creditos.sdo_proyectado = xpro +  extrasXpagar
             creditos.val_atraso    = creditos.Sdo_Capital - creditos.sdo_proyectado
             creditos.dias_atraso = (creditos.val_atraso / creditos.cuota) * W_Frec.


IF creditos.val_atraso LE 0 THEN
    ASSIGN creditos.val_atraso = 0
    creditos.dias_atraso = 0
    Creditos.Cod_Califica = 1.




END.


ON RETURN RETURN.
