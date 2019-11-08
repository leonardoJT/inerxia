&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL.
    /*para archivo de excel*/
    DEFINE VAR NomInforme AS CHARACTER FORMAT "X(30)".
    DEFINE VAR LisEx AS CHARACTER.
    DEFINE VAR Ct AS DECIMAL.
    DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
    DEFINE TEMP-TABLE IEx
        FIELD NLinea AS INTEGER FORMAT "999999"
        FIELD Linea  AS CHARACTER FORMAT "X(250)".
    /**/
  DEFINE VAR W_Pan AS INTEGER INITIAL 1. /*1-pantalla 2 pantalla2*/
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  DEFINE VAR NomInf AS CHARACTER FORMAT "X(30)".
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.
  DEFINE VAR NomAho AS CHARACTER FORMAT "X(40)".
  DEFI VAR W_Age  LIKE Agencias.Agencia.
  DEFI VAR W_Pro  LIKE Ahorros.Cod_ahorro.
  DEFI VAR W_NitW LIKE Clientes.Nit.
  DEFI VAR W_Cue  LIKE Creditos.Num_Credito.
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".
  DEFINE VAR FP1 LIKE Creditos.FOR_Pago.
  DEFINE VAR FP2 LIKE Creditos.FOR_Pago.
  DEFINE VAR FPN AS CHARACTER FORMAT "X(15)".

  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR WListado AS CHARACTER INITIAL "". 

 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

DEFINE VAR W_Nin AS CHARACTER FORMAT "X(61)".
DEFINE VAR W_Vig AS INTEGER FORMAT "9999".
DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".

DEFINE VAR W_Compromiso AS CHARACTER FORMAT "X(2)".
DEFINE VAR W_Interes AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR W_IntMora AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR W_CobJuri AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".
DEFINE VARIABLE W_FecRetRad AS DATE    NO-UNDO.
DEFINE VARIABLE W_Edad      AS INTEGER NO-UNDO.
DEFINE VARIABLE wtotahoapo  AS DECIMAL NO-UNDO FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VARIABLE Wtieahor    AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE Wtiecre     AS INTEGER NO-UNDO INITIAL 0.

/************** Definiciones de InfClientesSoloAso y de InfClientesOtros ****/
DEFINE VAR wnomcli    AS CHARACTER FORMAT "X(60)"    INITIAL "". 
DEFINE VAR wnomemp    LIKE empresas.alias_empresa    INITIAL "".
DEFINE VAR wnomcargo  LIKE varios.descripcion        INITIAL "".
DEFINE VAR wnomprofe  LIKE varios.descripcion        INITIAL "".
DEFINE VAR wtipoclie  AS CHARACTER FORMAT "X(25)"    INITIAL "".
DEFINE VAR wtipoiden  AS CHARACTER FORMAT "X(30)"    INITIAL "".
DEFINE VAR wtipocont  AS CHARACTER FORMAT "X(30)"    INITIAL "".
DEFINE VAR wtipovivi  AS CHARACTER FORMAT "X(12)"    INITIAL "".
DEFINE VAR wtipovinc  AS CHARACTER FORMAT "X(25)"    INITIAL "".
DEFINE VAR Wlugresid  AS CHARACTER FORMAT "X(60)"    INITIAL "".
DEFINE VAR Wlugcomer  AS CHARACTER FORMAT "X(60)"    INITIAL "".
DEFINE VAR wcodagei   LIKE agencias.agencia          INITIAL 0.
DEFINE VAR wnomagecli LIKE agencias.nombre           INITIAL "".
DEFINE VAR wtoting    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"  INITIAL 0.
DEFINE VAR wtotegr    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"  INITIAL 0.
DEFINE VAR wnomestado AS CHARACTER FORMAT "X(12)"  INITIAL "".
DEFINE VAR wsexo      AS CHARACTER FORMAT "X(12)"  INITIAL "".
DEFINE VAR wnompro    LIKE pro_ahorros.nom_producto  INITIAL "".
DEFINE VAR wnomtipro  AS CHARACTER FORMAT "X(20)"    INITIAL "".
DEFINE VAR wcodigo    AS INTEGER                     INITIAL 0.
DEFINE VAR wsalcli    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"  INITIAL 0.
FIND LAST agencias NO-LOCK NO-ERROR.
/*****************************************************************************/
DEFI TEMP-TABLE TPdtos 
     FIELD      Nom       LIKE Clientes.Nombre
     FIELD      Agenci    LIKE creditos.Agencia      
     FIELD      Nit       LIKE creditos.Nit  
     FIELD      Tip       AS CHAR FORM "X"
     FIELD      Cod_Cred  LIKE creditos.Cod_credito  
     FIELD      Cuota     LIKE creditos.Cuota        
     FIELD      Pagare    LIKE creditos.Pagare       
     FIELD      Monto     LIKE creditos.Monto        
     FIELD      Sdo_Capit LIKE creditos.Sdo_Capital  
     FIELD      CapVdo    LIKE creditos.Val_Atraso
     FIELD      Plazo     LIKE creditos.Plazo        
     FIELD      Per_Pago  AS INTEG FORM "999"     
     FIELD      Fec_Desemb  LIKE creditos.Fec_Desemb 
     FIELD      Fec_UltPago LIKE creditos.Fec_UltPago.

DEFI TEMP-TABLE TTCreditos LIKE Creditos
     FIELD DM  AS INTEG FORM "-9999" INIT 0
     FIELD NM  AS CHAR  FORM "X(40)" INIT "".

DEFI TEMP-TABLE TCCtas
     FIELD Age   LIKE Ahorros.Agencia
     FIELD Cta  LIKE Cuentas.Cuenta
     FIELD Vlr  LIKE Creditos.Sdo_Capital.

DEFINE TEMP-TABLE TC
    FIELD T_Tpd        LIKE Ahorros.Tip_Ahorro
    FIELD T_Cod        LIKE Ahorros.Cod_Ahorro
    FIELD T_NomPdt     AS CHARACTER FORMAT "X(15)"
    FIELD T_SInicial   AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 100
    FIELD T_SActual    AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 100
    FIELD T_Diferencia AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 100.

DEFINE TEMP-TABLE TT
    FIELD To_Tpd        LIKE Ahorros.Tip_Ahorro
    FIELD To_Cod        LIKE Ahorros.Cod_Ahorro
    FIELD To_NomPdt     AS CHARACTER FORMAT "X(15)"
    FIELD To_Total      AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD To_Fondo      AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

DEFINE TEMP-TABLE TxLinea
    FIELD TxLId AS INTEGER
    FIELD TXLin AS CHARACTER FORMAT "X(240)".

/**************************************************/
/* Datos Demograficos*/

DEFINE TEMP-TABLE TTCreditosDemog LIKE creditos
    FIELDS NomAge           AS CHARACTER COLUMN-LABEL "Agencia"         FORMAT "X(20)"
    FIELDS Cliente          AS CHARACTER COLUMN-LABEL "Cliente"         FORMAT "X(40)" 
    FIELDS UbcResCli        AS CHARACTER COLUMN-LABEL "Lug. Resi."      FORMAT "X(20)" 
    FIELDS UbcComCli        AS CHARACTER COLUMN-LABEL "Lug. Comer."     FORMAT "X(20)"
    FIELDS Dir_Residencia   AS CHARACTER COLUMN-LABEL "Dir. Resid."     FORMAT "X(40)"
    FIELDS Tel_Residencia   AS CHARACTER COLUMN-LABEL "Tel. Resid."     FORMAT "X(10)"
    FIELDS Dir_comercial    AS CHARACTER COLUMN-LABEL "Dir. Comer."     FORMAT "X(40)"
    FIELDS Tel_comercial    AS CHARACTER COLUMN-LABEL "Tel. Comer."     FORMAT "X(10)"
    /*Datos Codeudor A*/
    FIELDS nitCodA          AS CHARACTER COLUMN-LABEL "Nit Cod.A"       FORMAT "X(12)"
    FIELDS nomCodA          AS CHARACTER COLUMN-LABEL "Codeudor A"      FORMAT "X(30)"
    FIELDS dirResCodA       AS CHARACTER COLUMN-LABEL "Dir.Res. Cod. A" FORMAT "X(40)"
    FIELDS TelResCodA       AS CHARACTER COLUMN-LABEL "Tel.Res.Cod.A"   FORMAT "X(10)"
    FIELDS UbcResCodA       AS CHARACTER COLUMN-LABEL "Lug.Res.Cos.A"   FORMAT "X(20)"
    FIELDS dirComCodA       AS CHARACTER COLUMN-LABEL "Dir.Com. Cod. A" FORMAT "X(40)"
    FIELDS TelComCodA       AS CHARACTER COLUMN-LABEL "Tel.Com.Cod.A"   FORMAT "X(10)"
    FIELDS UbcComCodA       AS CHARACTER COLUMN-LABEL "Lug.Comer.Cos.A" FORMAT "X(20)"
    /*Datos Codeudor B*/
    FIELDS nitCodB          AS CHARACTER COLUMN-LABEL "Nit Cod.B"       FORMAT "X(12)"
    FIELDS nomCodB          AS CHARACTER COLUMN-LABEL "Codeudor B"      FORMAT "X(30)"
    FIELDS dirResCodB       AS CHARACTER COLUMN-LABEL "Dir.Res. Cod. B" FORMAT "X(40)"
    FIELDS TelResCodB       AS CHARACTER COLUMN-LABEL "Tel.Res.Cod.B"   FORMAT "X(10)"
    FIELDS UbcResCodB       AS CHARACTER COLUMN-LABEL "Lug.Res.Cos.B"   FORMAT "X(20)"
    FIELDS dirComCodB       AS CHARACTER COLUMN-LABEL "Dir.Com. Cod. B" FORMAT "X(40)"
    FIELDS TelComCodB       AS CHARACTER COLUMN-LABEL "Tel.Com.Cod.B"   FORMAT "X(10)"
    FIELDS UbcComCodB       AS CHARACTER COLUMN-LABEL "Lug.Comer.Cos.B" FORMAT "X(20)"
    /*Datos Codeudor C*/
    FIELDS nitCodC          AS CHARACTER COLUMN-LABEL "Nit Cod.C"       FORMAT "X(12)"
    FIELDS nomCodC          AS CHARACTER COLUMN-LABEL "Codeudor C"      FORMAT "X(30)"
    FIELDS dirResCodC       AS CHARACTER COLUMN-LABEL "Dir.Res. Cod. C" FORMAT "X(40)"
    FIELDS TelResCodC       AS CHARACTER COLUMN-LABEL "Tel.Res.Cod.C"   FORMAT "X(10)"
    FIELDS UbcResCodC       AS CHARACTER COLUMN-LABEL "Lug.Res.Cos.C"   FORMAT "X(20)"
    FIELDS dirComCodC       AS CHARACTER COLUMN-LABEL "Dir.Com. Cod. C" FORMAT "X(40)"
    FIELDS TelComCodC       AS CHARACTER COLUMN-LABEL "Tel.Com.Cod.C"   FORMAT "X(10)"
    FIELDS UbcComCodC       AS CHARACTER COLUMN-LABEL "Lug.Comer.Cos.C" FORMAT "X(20)".
/***********************************************/


DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(90)". 
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(380)". 
DEFINE VAR W_NomTpd AS CHARACTER FORMAT "X(19)".
DEFINE VAR TotAvis    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAter    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCont    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotApor    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCons    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCome    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotHipo    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotMicr    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotDMay    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotDMen    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotDjsl    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotDjcl    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCMay    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCMen    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCjsl    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCjcl    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge1    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge2    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge3    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge4    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge5    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge6    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge7    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge8    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge9    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotAge10   AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd1    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd2    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd3    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd4    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd5    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd6    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd7    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd8    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd9    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotTpd10   AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR w_pl  AS CHARACTER FORMAT "X(14)" INITIAL "".
DEFINE VAR W_NomPdt AS CHARACTER FORMAT "X(15)".
DEFINE VAR TotAhoEnt AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotFdoEnt AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

DEFINE VAR W_NomCtas AS CHARACTER EXTENT 4 FORMAT "X(6)".
DEFINE VAR W_Saldo AS DECIMAL FORMAT "->>>>>>>>>>9.99".
DEFINE VAR W_SdoAge AS DECIMAL EXTENT 4 FORMAT "->>>>>>>>>>9.99".
DEFINE VAR W_SdoLib AS DECIMAL EXTENT 4 FORMAT "->>>>>>>>>>9.99".
/*DEFINE VAR i AS INTEGER.*/
/*DEFINE VAR j AS INTEGER.*/

DEFINE TEMP-TABLE TemAsociados
    FIELD TAgecli      AS INTEGER
    FIELD TNit         AS CHARACTER FORMAT "X(12)" 
    FIELD TNombres     AS CHARACTER FORMAT "X(80)" 
    FIELD TFecIngre    AS DATE
    FIELD TDetalle     AS CHARACTER
    FIELD TTipVincu    AS CHARACTER
    FIELD TTipClien    AS CHARACTER
    FIELD TDirCorres   AS CHARACTER
    FIELD TDirRes      AS CHARACTER FORMAT "X(40)"
    FIELD TTelRes      AS CHARACTER FORMAT "X(20)"
    FIELD TDirCor      AS CHARACTER FORMAT "X(40)"
    FIELD TTelCor      AS CHARACTER FORMAT "X(20)"
    FIELD TAportes     AS DECIMAL
    FIELD TAtrapor     AS CHARACTER
    FIELD TCreditos    AS DECIMAL
    FIELD TFondos      AS DECIMAL
    INDEX IdxAsociado TAgecli TNit.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Basicos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Tipo Cmb_BUsuarios Cmb_BCreditos ~
Cmb_BClientes BUTTON-143 BUTTON-153 BUTTON-144 Btn_Imp E1 w-BUTTON-148 ~
BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo Cmb_BUsuarios Cmb_BCreditos ~
Cmb_BClientes Cmb_BEmpresas E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_BUsuarios Cmb_BCreditos Cmb_BClientes ~
Cmb_BEmpresas Cmb_BGarantias Cmb_BAhorros Cmb_BSolicitudes 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62
     FONT 8.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-144 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

DEFINE BUTTON BUTTON-150 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-152 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 152" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65
     FONT 8.

DEFINE BUTTON w-BUTTON-148 
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Cmb_BAhorros AS CHARACTER FORMAT "X(50)":U INITIAL "Básico" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "Básico","Fechas de Apertura","Fechas de Cancelación","Fechas de Vencimiento","Canje","Depositos","Clasificacion CDATS","TAC Vencidos","A Termino","Aportes","Contractual","Contractuales 3 Vdas","Vctos-Liquidac.Proximas A Termino","Cuentas de Empresas Asociadas","Tarjetas Debito" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BClientes AS CHARACTER FORMAT "X(256)":U INITIAL "Básico" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "Básico","Actualización de Información","Cumpleaños","Afiliaciones","Retiros","Proveedores","Asociados Informe General","Asociados Sin Productos","Solo Asociados","Solo Clientes No Asociados","Habiles","No Habiles","Motivos de Vinculación","Actividades","Profesiones","Empresas Asociadas","Inconsistencias en Ubicación" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BCreditos AS CHARACTER FORMAT "X(50)":U INITIAL "Cartera" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Cartera","Resumen Creditos","Detallado-Dias Mora","Desembolsos","Por Instancias","Vencimiento","Créditos en Abogado","Cancelados","Fondo Mutual","Fondo Mutual General","Por Empresa - Canc. y Desembolsos","Datos Demograficos" 
     DROP-DOWN-LIST
     SIZE 40 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BEmpresas AS CHARACTER FORMAT "X(256)":U INITIAL "Pagos" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Pagos","No Pagos","Asociados de Empresa" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BGarantias AS CHARACTER FORMAT "X(256)":U INITIAL "Garantias Admisibles" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "Garantias Admisibles","Vencimientos Poliza","Vencimiento Impuestos","Cupos" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BSolicitudes AS CHARACTER FORMAT "X(256)":U INITIAL "Básico" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Básico","Fechas de Ingreso","Aprobadas","Negadas","Condicionadas","Por Tipo de Producto","Forma de Pago" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BUsuarios AS CHARACTER FORMAT "X(256)":U INITIAL "Pendientes de Cobro" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Pendientes de Cobro" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(30)":U INITIAL "Clientes" 
     LABEL "Tipo de Informe" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "Clientes","Ahorros","Créditos","Solicitudes","Garantias","Especiales","Usuarios","Empresas","Pdctos X Empresa","Crecimiento Agencias","Crecimiento Totales" 
     DROP-DOWN-LIST
     SIZE 21.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP MAX-CHARS 999999999 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 16.42
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_anterior 
     LABEL "Anterior" 
     SIZE 15 BY 1.12
     FONT 4.

DEFINE BUTTON Btn_siguiente 
     LABEL "Siguiente" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-154 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 154" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-145 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_EstCre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado del Crédito" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Aprobado No Desembolsado","2 - Desembolsado Normal","3 - Cancelado","4 - Retirado Sin Aprobar","5 - Castigado" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todos" 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "000 - Todos" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos de Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE P_Nom AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuFin AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Final" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuIni AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValFin AS DECIMAL FORMAT "->>>,>>>,>>9.999":U INITIAL 999999999 
     LABEL "Valor Final" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValIni AS DECIMAL FORMAT "->>>,>>>,>>9.999":U INITIAL 0 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-DiaVdo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Dias Cuota Vda" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 TOOLTIP "Digite el Número de días que lleva la cuota vencida"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CodEmp AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cód.Empresa" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .81 TOOLTIP "Con doble click O Enter Consulta las Empresas"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE VARIABLE RFPago AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Caja", 1,
"Nómina", 2,
"Todos", 3
     SIZE 32 BY .69 NO-UNDO.

DEFINE VARIABLE Rs_Pto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorros", 1,
"Creditos", 2,
"Todos", 3
     SIZE 37.43 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 1.35.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.72 BY 1.42.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 1.23.

DEFINE VARIABLE TCupoRota AS LOGICAL INITIAL no 
     LABEL "Todos los Cupos Rotativos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .77 NO-UNDO.

DEFINE VARIABLE TTotales AS LOGICAL INITIAL no 
     LABEL "Solo Totales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .77 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Basicos
     Cmb_Tipo AT ROW 1.54 COL 13.29 COLON-ALIGNED
     Cmb_BUsuarios AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BCreditos AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BClientes AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BEmpresas AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BGarantias AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BAhorros AT ROW 1.54 COL 57 COLON-ALIGNED
     Cmb_BSolicitudes AT ROW 1.54 COL 57 COLON-ALIGNED
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.62 COL 3
     BUTTON-144 AT ROW 2.88 COL 56
     Btn_Imp AT ROW 3.15 COL 102
     E1 AT ROW 4.23 COL 3 NO-LABEL
     w-BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.86 BY 21.04
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 4.57
     R1 AT ROW 6.92 COL 3.57
     R2 AT ROW 6.38 COL 3.57
     R3 AT ROW 5.85 COL 3.57
     R4 AT ROW 5.31 COL 3.57
     R5 AT ROW 4.77 COL 3.57
     R6 AT ROW 4.23 COL 3.57
     R7 AT ROW 3.69 COL 3.57
     R8 AT ROW 3.15 COL 3.57
     R9 AT ROW 2.62 COL 3.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 101 ROW 6.65
         SIZE 12 BY 6.73
         BGCOLOR 17 .

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10 ROW 2.88
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

DEFINE FRAME F_Filtros
     Cmb_Agencias AT ROW 1.27 COL 9 COLON-ALIGNED
     W_DiaIni AT ROW 3.27 COL 11 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 3.27 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 3.27 COL 34
     W_DiaFin AT ROW 3.27 COL 39 NO-LABEL
     AnoFin AT ROW 3.27 COL 52 COLON-ALIGNED NO-LABEL
     BUTTON-121 AT ROW 3.31 COL 59.86
     Cmb_TProducto AT ROW 4.5 COL 25 COLON-ALIGNED
     Cmb_Productos AT ROW 5.58 COL 25 COLON-ALIGNED
     Cmb_EstCre AT ROW 6.65 COL 25 COLON-ALIGNED
     ValIni AT ROW 7.73 COL 25 COLON-ALIGNED
     TTotales AT ROW 7.73 COL 48
     TCupoRota AT ROW 8.65 COL 48 WIDGET-ID 2
     ValFin AT ROW 8.77 COL 25 COLON-ALIGNED
     UsuIni AT ROW 9.85 COL 25 COLON-ALIGNED
     UsuFin AT ROW 10.92 COL 25 COLON-ALIGNED
     W-DiaVdo AT ROW 12.85 COL 16.29 COLON-ALIGNED
     RFPago AT ROW 14.19 COL 4 NO-LABEL
     Rs_Pto AT ROW 17 COL 4.72 NO-LABEL
     BUTTON-145 AT ROW 17.31 COL 59.14
     W_CodEmp AT ROW 18.23 COL 14 COLON-ALIGNED
     P_Nom AT ROW 18.27 COL 20 COLON-ALIGNED NO-LABEL
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 2.46 COL 17
          BGCOLOR 17 FGCOLOR 7 
     "Productos X Empresa" VIEW-AS TEXT
          SIZE 19.29 BY .62 AT ROW 15.92 COL 3.29
          BGCOLOR 7 FGCOLOR 15 
     "Contractuales" VIEW-AS TEXT
          SIZE 12.57 BY 1.08 AT ROW 12.04 COL 4.43
          FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.5 COL 43
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 3.27 COL 43
     Img_MesI AT ROW 3.27 COL 17
     RECT-282 AT ROW 2.35 COL 12
     RECT-283 AT ROW 2.35 COL 38
     RECT-284 AT ROW 12.58 COL 3
     RECT-302 AT ROW 16.58 COL 3.29
     RECT-303 AT ROW 13.92 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 15 ROW 2.08
         SIZE 84 BY 19.38
         BGCOLOR 17 FONT 5
         TITLE "Filtros de Información".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Modulo de Informes Básicos"
         HEIGHT             = 21.04
         WIDTH              = 112.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Basicos:HANDLE.

/* SETTINGS FOR FRAME F_Basicos
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (BUTTON-143:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (BUTTON-153:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-AFTER-TAB-ITEM (BUTTON-153:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (BUTTON-144:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (w-BUTTON-148:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Basicos)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR COMBO-BOX Cmb_BAhorros IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       Cmb_BAhorros:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BClientes IN FRAME F_Basicos
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_BCreditos IN FRAME F_Basicos
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_BEmpresas IN FRAME F_Basicos
   NO-ENABLE 1                                                          */
ASSIGN 
       Cmb_BEmpresas:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BGarantias IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       Cmb_BGarantias:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BSolicitudes IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       Cmb_BSolicitudes:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BUsuarios IN FRAME F_Basicos
   1                                                                    */
/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AnoFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_EstCre IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_EstCre:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_TProducto IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN P_Nom IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN UsuFin IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       UsuFin:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN UsuIni IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       UsuIni:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN ValFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ValIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
ASSIGN 
       W-DiaVdo:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME F_Filtros
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Basicos
/* Query rebuild information for FRAME F_Basicos
     _Query            is NOT OPENED
*/  /* FRAME F_Basicos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Filtros
/* Query rebuild information for FRAME F_Filtros
     _Query            is NOT OPENED
*/  /* FRAME F_Filtros */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Modulo de Informes Básicos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes Básicos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wWin
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME F_Basicos /* Button 149 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  /*MESSAGE listado VIEW-AS ALERT-BOX.*/
/*    IF W_Dispositivo = "" THEN
      RETURN.*/
    IF W_Dispositivo = "P" THEN DO: 
        IF W_Pan EQ 1 THEN RUN Pantalla IN W_Manija (INPUT Listado).
        IF W_Pan EQ 2 THEN RUN Pantalla2 IN W_Manija (INPUT Listado).
    END.
      
    ELSE                                                  
      IF W_Dispositivo = "I" THEN DO:
        IF W_Pan EQ 1 THEN 
            RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  2,INPUT  1,INPUT  1,
                                           INPUT  99999,OUTPUT W_sw).
         IF W_Pan EQ 2 THEN
             RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  8,INPUT  1,INPUT  1,
                                            INPUT  99999,OUTPUT W_sw).
      END.
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(Listado).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Filtros /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         W_DiaIni:SCREEN-VALUE = STRING(DAY(WFec))
         AnoIni = WAno
         MesIni = WMes
         FecIni = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Filtros /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(WFec))
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F_Basicos /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 wWin
ON CHOOSE OF BUTTON-144 IN FRAME F_Basicos /* Cambiar los Rangos de Filtro de Información */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_Filtros /* Button 145 */
DO:
  HIDE FRAME F_Filtros.
  ASSIGN TCupoRota.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_Basicos /* Salir */
DO:
  /*OS-DELETE VALUE(Listado).*/
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Basicos /* Button 153 */
DO:
  IF E1:SCREEN-VALUE EQ "" THEN
     MESSAGE "No se ha ejecutado ningún Informe" SKIP
             "Escoja el informe y presione clic" SKIP
             "en el boton ejecutar!." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
    Buscar:SCREEN-VALUE IN FRAME F_Buscar = E1:SELECTION-TEXT.
    ASSIGN FRAME F_Buscar Buscar.
    W_Ok = E1:SEARCH(Buscar,32).
    VIEW FRAME F_Buscar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Buscar /* Button 154 */
DO:
  HIDE FRAME F_Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME F_Filtros /* Agencias */
DO:
  ASSIGN FRAME F_Filtros Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_BAhorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BAhorros wWin
ON VALUE-CHANGED OF Cmb_BAhorros IN FRAME F_Basicos /* Informes Disponibles */
DO:
  W_Pan = 1.
  IF SELF:SCREEN-VALUE EQ "Fechas de Apertura" THEN DO:
     ASSIGN UsuIni:SENSITIVE IN FRAME F_Filtros = YES
            UsuIni:HIDDEN = NO
            UsuFin:SENSITIVE = YES
            UsuFin:HIDDEN = NO.
  END.
  ELSE
     IF SELF:SCREEN-VALUE EQ "Tarjetas Debito"  THEN
        ENABLE Cmb_Agencias Button-120 Button-121 BUTTON-145 ValIni ValFin WITH FRAME F_Filtros.
     ELSE DO:
     ASSIGN UsuIni:SENSITIVE IN FRAME F_Filtros = NO
            UsuIni:HIDDEN = YES
            UsuFin:SENSITIVE = NO
            UsuFin:HIDDEN = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BClientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BClientes wWin
ON VALUE-CHANGED OF Cmb_BClientes IN FRAME F_Basicos /* Informes Disponibles */
DO:
  W_Pan = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BCreditos wWin
ON VALUE-CHANGED OF Cmb_BCreditos IN FRAME F_Basicos /* Informes Disponibles */
DO:

  CASE SELF:SCREEN-VALUE:
    WHEN "Por Instancias" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = YES
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = NO   
                Cmb_EstCre:LIST-ITEMS IN FRAME F_Filtros = ""
                Cmb_EstCre:LABEL IN FRAME F_Filtros = "Instancias"
                W_Ok = Cmb_EstCre:ADD-LAST("2 - Proceso de Cobros")
                W_Ok = Cmb_EstCre:ADD-LAST("5 - Proceso de Abogados")
                Cmb_EstCre:SCREEN-VALUE = Cmb_EstCre:ENTRY(1)
                TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
                TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
                ValIni:LABEL = "Valor Inicial"
                ValFin:LABEL = "Valor Final".
     END.
    WHEN "Por Estados" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = YES
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = NO
                Cmb_EstCre:LIST-ITEMS IN FRAME F_Filtros = ""
                Cmb_EstCre:LABEL IN FRAME F_Filtros = "Estados"
                W_Ok = Cmb_EstCre:ADD-LAST("1 - Aprobado No Desembolsado")
                W_Ok = Cmb_EstCre:ADD-LAST("2 - Normal Desembolsado")
                W_Ok = Cmb_EstCre:ADD-LAST("3 - Cancelado")
                W_Ok = Cmb_EstCre:ADD-LAST("4 - Retirado Sin Aprobar")
                W_Ok = Cmb_EstCre:ADD-LAST("5 - Castigado")
                Cmb_EstCre:SCREEN-VALUE = Cmb_EstCre:ENTRY(1)
                TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
                TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
                ValIni:LABEL = "Valor Inicial"
                ValFin:LABEL = "Valor Final".
    END.
    WHEN "Cartera" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = NO
                ValIni:LABEL = "Días de Mora Inicial"
                ValFin:LABEL = "Días de Mora Final".
    END.


      WHEN "Datos Demograficos" THEN DO:
           ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                  Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                  TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                  TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = NO
                  ValIni:LABEL = "Días de Mora Inicial"
                  ValFin:LABEL = "Días de Mora Final".
      END.

    WHEN "Cartera Complemento" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                TTotales:SENSITIVE IN FRAME F_Filtros  = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = NO
                ValIni:LABEL = "Días de Mora Inicial"
                ValFin:LABEL = "Días de Mora Final".
    END.
    WHEN "Vencimiento" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = NO
                ValIni:LABEL = "Días de Mora Inicial"
                ValFin:LABEL = "Días de Mora Final".
    END.
    WHEN "Vctos para Débitar" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros  = YES
                TTotales:SENSITIVE IN FRAME F_Filtros = FALSE TTotales:HIDDEN IN FRAME F_Filtros = NO
                TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = NO
                ValIni:LABEL                            = "No necesario"
                ValFin:LABEL                            = "No necesario".
    END.
    WHEN "Fondo Mutual General" THEN DO:
          ASSIGN Cmb_Tproducto:SCREEN-VALUE = Cmb_Tproducto:ENTRY(8)  Cmb_Tproducto.
          ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                 Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                 TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                 TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = YES.
    END.
    WHEN "Fondo Mutual" THEN DO:
        ASSIGN Cmb_Tproducto:SCREEN-VALUE = Cmb_Tproducto:ENTRY(8)  Cmb_Tproducto.
        ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
               Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
               TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
               TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES.
    END.
    WHEN "Detallado-Dias Mora" THEN W_Pan = 2.
    OTHERWISE W_Pan = 1.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BEmpresas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BEmpresas wWin
ON VALUE-CHANGED OF Cmb_BEmpresas IN FRAME F_Basicos /* Informes Disponibles */
DO:
  W_Pan = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BGarantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BGarantias wWin
ON VALUE-CHANGED OF Cmb_BGarantias IN FRAME F_Basicos /* Informes Disponibles */
DO:
  W_Pan = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BSolicitudes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BSolicitudes wWin
ON VALUE-CHANGED OF Cmb_BSolicitudes IN FRAME F_Basicos /* Informes Disponibles */
DO:
  IF SELF:SCREEN-VALUE EQ "Por Tipo de Producto" THEN DO:
     VIEW FRAME F_Filtros.
     APPLY "entry" TO Cmb_TProducto IN FRAME F_Filtros.
     RETURN NO-APPLY.
  END.
  W_Pan = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BUsuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BUsuarios wWin
ON VALUE-CHANGED OF Cmb_BUsuarios IN FRAME F_Basicos /* Informes Disponibles */
DO:
  W_Pan = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Filtros /* Productos Disponibles */
DO:
  ASSIGN FRAME F_Filtros Cmb_Productos.
  IF Cmb_Productos EQ "000 - Todos" THEN ASSIGN ProIni = 0 ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) ProFin = ProIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME F_Basicos /* Tipo de Informe */
DO:
  W_Pan = 1.
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  DISABLE RFPago WITH FRAME F_Filtros.
  ENABLE BUTTON-144 WITH FRAME F_basicos.
  ASSIGN Cmb_TProducto:LIST-ITEMS IN FRAME F_Filtros = ""
         Cmb_Productos:LIST-ITEMS IN FRAME F_Filtros = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1)
         ValIni:SCREEN-VALUE = "0"
         ValFin:SCREEN-VALUE = "999999999".

  CASE Cmb_Tipo:
    WHEN "Clientes" THEN DO:
      HIDE {&List-1} IN FRAME F_Basicos.
      Cmb_BClientes:SCREEN-VALUE = Cmb_BClientes:ENTRY(1).
      ENABLE Cmb_BClientes WITH FRAME F_Basicos.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
             TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             ValIni:SENSITIVE = NO
             ValFin:SENSITIVE = NO
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
    END.
    WHEN "Ahorros" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BAhorros:SCREEN-VALUE = Cmb_BAhorros:ENTRY(1).
      ENABLE Cmb_BAhorros WITH FRAME F_Basicos.
      ASSIGN Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos los Productos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - A la Vista")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Contractual")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - A Término")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Aportes")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro   EQ 1 AND
                                 Pro_Ahorros.Estado       EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_producto).
      END.
    END.
    WHEN "Solicitudes" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BSolicitudes:SCREEN-VALUE = Cmb_BSolicitudes:ENTRY(1).
      ENABLE Cmb_BSolicitudes WITH FRAME F_Basicos.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN    = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN    = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Garantias" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BGarantias:SCREEN-VALUE = Cmb_BGarantias:ENTRY(1).
      ENABLE Cmb_BGarantias WITH FRAME F_Basicos.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Usuarios" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BUsuarios:SCREEN-VALUE = Cmb_BUsuarios:ENTRY(1).
      ENABLE Cmb_BUsuarios WITH FRAME F_Basicos.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             UsuIni:SENSITIVE = YES
             UsuIni:HIDDEN = NO
             UsuFin:SENSITIVE = YES
             UsuFin:HIDDEN = NO
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Créditos" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BCreditos:SCREEN-VALUE = Cmb_BCreditos:ENTRY(1).
      ENABLE Cmb_BCreditos WITH FRAME F_Basicos.
      ENABLE RFPago WITH FRAME F_Filtros.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             W_Ok = Cmb_TProducto:ADD-LAST("5 - Bienes y Ser.")
             W_Ok = Cmb_TProducto:ADD-LAST("6 - Empleados")
             W_Ok = Cmb_TProducto:ADD-LAST("7 - Fondos")
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
             TCupoRota:SENSITIVE IN FRAME F_Filtros = YES TCupoRota:HIDDEN IN FRAME F_Filtros = NO
             ValIni:LABEL IN FRAME F_Filtros = "Dias Mora Inicial"
             ValFin:LABEL IN FRAME F_Filtros = "Dias Mora Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Pdctos X Empresa" THEN DO:
        HIDE  {&List-1} IN FRAME F_Basicos.                                                                            
        ASSIGN Cmb_BEmpresas:SCREEN-VALUE = Cmb_BEmpresas:ENTRY(3)
               Cmb_BEmpresas.
        ENABLE Cmb_BEmpresas WITH FRAME F_Basicos.                                                           
        APPLY "Mouse-select-Click" TO Rs_Pto IN FRAME F_Filtros.             
    END.
    WHEN "Empresas" THEN DO:
      HIDE  {&List-1} IN FRAME F_Basicos.
      Cmb_BEmpresas:SCREEN-VALUE = Cmb_BEmpresas:ENTRY(1).
      ENABLE Cmb_BEmpresas WITH FRAME F_Basicos.
      ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = NO
             ValIni:SENSITIVE = NO
             ValFin:SENSITIVE = NO
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             TCupoRota:SENSITIVE IN FRAME F_Filtros = NO TCupoRota:HIDDEN IN FRAME F_Filtros = YES.
    END.
    WHEN "Especiales" THEN DO:
      MESSAGE "No implementados" VIEW-AS ALERT-BOX.
    END.
    WHEN "Crecimiento Agencias"  OR WHEN "Crecimiento Totales" THEN DO:
         DISABLE BUTTON-144 WITH FRAME F_basicos.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_TProducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TProducto wWin
ON VALUE-CHANGED OF Cmb_TProducto IN FRAME F_Filtros /* Tipos de Producto */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN FRAME F_Filtros Cmb_TProducto.

  IF Cmb_Tipo:SCREEN-VALUE EQ "Pdctos X Empresa" THEN
     RETURN.

  ASSIGN Cmb_Productos:LIST-ITEMS = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
  
  IF Cmb_Tipo EQ "Ahorros" THEN DO:
     FOR EACH Pro_Ahorros WHERE 
         Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Ahorros.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).      
     END.
  END.
  IF Cmb_Tipo EQ "Créditos" THEN DO:
     FOR EACH Pro_Creditos WHERE 
         Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Creditos.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Credito.Cod_Credito,"999") + " - " + Pro_Credito.Nom_Producto).      
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Pto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Pto wWin
ON MOUSE-SELECT-CLICK OF Rs_Pto IN FRAME F_Filtros
DO:
   ASSIGN Rs_Pto
          Cmb_TProducto:LIST-ITEMS IN FRAME F_Filtros = ""
          Cmb_Productos:LIST-ITEMS IN FRAME F_Filtros = ""
          W_Ok = Cmb_Productos:ADD-LAST("000 - Todos").

   IF Rs_Pto EQ 3 THEN DO:
       ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES                                                        
                Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES                                                                                                                                   
                ValIni:SENSITIVE = YES                                                                                  
                ValFin:SENSITIVE = YES                                                                                  
                W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos los Productos")                                                              
                W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")                                                            
                W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")                                                          
                W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")                                                        
                W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")  
                W_Ok = Cmb_TProducto:ADD-LAST("1 - A la Vista")
                W_Ok = Cmb_TProducto:ADD-LAST("2 - Contractual")
                W_Ok = Cmb_TProducto:ADD-LAST("3 - A Término")
                W_Ok = Cmb_TProducto:ADD-LAST("4 - Aportes")
                UsuIni:SENSITIVE = NO                                                                                   
                UsuIni:HIDDEN = YES                                                                                     
                UsuFin:SENSITIVE = NO                                                                                   
                UsuFin:HIDDEN = YES                                                                                     
                Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)                                                                                                                                     
                ValIni:LABEL IN FRAME F_Filtros = "Dias Mora Inicial"                                                   
                ValFin:LABEL IN FRAME F_Filtros = "Dias Mora Final".                                                    
         FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK:                                           
             W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto). 
         END. 
         FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK:
             W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_producto).
         END.
   END.
   ELSE IF Rs_Pto EQ 1 THEN DO:
       ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES                                                        
                Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES                                                                                                                                      
                ValIni:SENSITIVE = YES                                                                                  
                ValFin:SENSITIVE = YES                                                                                  
                W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos los Productos")                                                                               
                W_Ok = Cmb_TProducto:ADD-LAST("1 - A la Vista")
                W_Ok = Cmb_TProducto:ADD-LAST("2 - Contractual")
                W_Ok = Cmb_TProducto:ADD-LAST("3 - A Término")
                W_Ok = Cmb_TProducto:ADD-LAST("4 - Aportes")
                UsuIni:SENSITIVE = NO                                                                                   
                UsuIni:HIDDEN = YES                                                                                     
                UsuFin:SENSITIVE = NO                                                                                   
                UsuFin:HIDDEN = YES                                                                                     
                Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)                                                                                                                                     
                ValIni:LABEL IN FRAME F_Filtros = "Dias Mora Inicial"                                                   
                ValFin:LABEL IN FRAME F_Filtros = "Dias Mora Final".                                                    
         
         FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK:
             W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_producto).
         END.
   END.
   ELSE IF Rs_Pto EQ 2 THEN DO:
       ASSIGN Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES                                                        
                Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES                                                        
                ValIni:SENSITIVE = YES                                                                                  
                ValFin:SENSITIVE = YES                                                                                  
                W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos los Productos")                                                                               
                W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")                                                            
                W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")                                                          
                W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")                                                        
                W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
                UsuIni:SENSITIVE = NO                                                                                   
                UsuIni:HIDDEN = YES                                                                                     
                UsuFin:SENSITIVE = NO                                                                                   
                UsuFin:HIDDEN = YES                                                                                     
                Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)                                                                                                                                      
                ValIni:LABEL IN FRAME F_Filtros = "Dias Mora Inicial"                                                   
                ValFin:LABEL IN FRAME F_Filtros = "Dias Mora Final".                                                    
         
         FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK:                                           
             W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto). 
         END. 
   END.

   ASSIGN FRAME F_Basicos Cmb_Tipo.
   ASSIGN FRAME F_Filtros Cmb_TProducto Cmb_Productos.
   IF Cmb_Productos EQ "000 - Todos" OR Cmb_Productos EQ " " THEN 
      ASSIGN ProIni = 0 
             ProFin = 999.
   ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) 
               ProFin = ProIni.

   IF SUBSTRING(Cmb_TProducto,1,1) EQ "0" OR Cmb_TProducto EQ " " THEN 
      ASSIGN TpdIni = 0 
             TpdFin = 9.
   ELSE ASSIGN TpdIni = INTEGER(SUBSTRING(Cmb_TProducto,1,1)) 
               TpdFin = TpdIni.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Pto wWin
ON VALUE-CHANGED OF Rs_Pto IN FRAME F_Filtros
DO:
   ASSIGN Rs_Pto.
   APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME w-BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-BUTTON-148 wWin
ON CHOOSE OF w-BUTTON-148 IN FRAME F_Basicos /* Ejecutar */
DO:
  FOR EACH IEx: DELETE IEx. END.
    ASSIGN FRAME F_Basicos Cmb_Tipo Cmb_BClientes Cmb_BAhorros Cmb_BEmpresas.

  ASSIGN FRAME F_Filtros RFPago TTotales AnoIni AnoFin Cmb_Agencias W_DiaIni W_DiaFin UsuIni UsuFin
               Cmb_TProducto Cmb_Productos valini valfin.
  NomInf = Cmb_TProducto + " - " + Cmb_Productos.
  ASSIGN EstadoC = INTEGER(SUBSTRING(Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros,1,1)).
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  IF SUBSTRING(Cmb_TProducto,1,1) EQ "0" OR Cmb_TProducto EQ " " THEN 
     ASSIGN TpdIni = 0 
            TpdFin = 9.
  ELSE ASSIGN TpdIni = INTEGER(SUBSTRING(Cmb_TProducto,1,1)) 
              TpdFin = TpdIni.
  
  IF SUBSTRING(Cmb_Productos,1,3) EQ "000" OR Cmb_Productos EQ " " THEN 
     ASSIGN ProIni = 0 
            ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) 
              ProFin = ProIni.
  
  IF W_DiaIni NE DAY(FecIni) OR MesIni NE MONTH(FecIni) THEN DO:
     FecIni = DATE(STRING(W_DiaIni) + "/" + STRING(MesIni) + "/" + STRING(AnoIni)).
  END.
  IF W_DiaFin NE DAY(FecFin) OR MesFin NE MONTH(FecFin) THEN DO:
     FecFin = DATE(STRING(W_DiaFin) + "/" + STRING(MesFin) + "/" + STRING(AnoFin)).
  END.
  VIEW FRAME F_Progreso.
  CASE Cmb_Tipo:
    WHEN "Clientes" THEN RUN Informes_Clientes.

    WHEN "Ahorros"  THEN DO:
        IF Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos EQ "Contractual" AND (ProIni EQ 24 OR ProIni EQ 25) THEN
           RUN Inf_AhoCod2425.  /*Feb.5/07 Inf.Aux.funerario*/
        ELSE
           RUN Informes_Ahorros.
    END.

    WHEN "Solicitudes" THEN RUN Informes_Solicitudes.
    WHEN "Garantias" THEN RUN Informes_Garantias.
    WHEN "Usuarios" THEN RUN Informes_Usuarios.
    WHEN "Créditos" THEN DO:
      IF Cmb_BCreditos:SCREEN-VALUE IN FRAME F_Basicos EQ "Simular Refinanciación" THEN DO:
         ASSIGN WWin:SENSITIVE = FALSE.
   
         RUN C-Creditos.R (INPUT "", OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_NitW, OUTPUT W_Cue).

         FIND FIRST Creditos WHERE Creditos.Nit  EQ W_NitW AND
                            Creditos.Cod_Credito EQ W_Pro AND
                            Creditos.Num_Credito EQ W_Cue AND 
                            Creditos.Estado      EQ 2 NO-LOCK NO-ERROR.
         IF AVAIL(Creditos) THEN 
            RUN W-Prorec_Refinancia.R
                 (INPUT "Créditos",    
                  INPUT W_NitW,       
                  INPUT W_Pro,        
                  INPUT Creditos.Tip_Credito,            
                  INPUT W_Cue,        
                  INPUT 010101001,    
                  INPUT 9).           

         ASSIGN WWin:SENSITIVE = TRUE.
         WWin:MOVE-TO-TOP().
         RETURN.
      END. 
        
      RUN Informes_Creditos.
    END.
    WHEN "Empresas" OR WHEN "Pdctos X Empresa" THEN RUN Informes_Empresas.
    /* WHEN "Crecimiento Entidad" THEN RUN Informe_Crecimiento. */
    WHEN "Crecimiento Agencias" THEN RUN  inf_CrecAgencias.
    WHEN "Crecimiento Totales"  THEN RUN  inf_CrecTotales.      
    
  END CASE.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Basicos.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME W_CodEmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodEmp wWin
ON LEAVE OF W_CodEmp IN FRAME F_Filtros /* Cód.Empresa */
DO:
   ASSIGN W_CodEmp
          P_Nom:SCREEN-VALUE = "".

   FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ W_CodEmp NO-LOCK NO-ERROR.
   IF AVAIL(Empresas) THEN
      ASSIGN P_Nom:SCREEN-VALUE = Empresas.ALIAS_Empresa.
   ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodEmp wWin
ON MOUSE-SELECT-DBLCLICK OF W_CodEmp IN FRAME F_Filtros /* Cód.Empresa */
DO:
    DEFI VAR P_Nit AS CHAR FORM "X(12)".
    DEFI VAR P_AgeEmp LIKE Agencias.Agencia.

    RUN C-Empresas.R (INPUT  W_Agencia,
                      OUTPUT W_CodEmp,OUTPUT P_Nit,OUTPUT P_AgeEmp,OUTPUT P_Nom). 

    ASSIGN W_CodEmp:SCREEN-VALUE = STRING(W_CodEmp).

    APPLY "LEAVE" TO W_CodEmp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_Encabezados wWin 
PROCEDURE Aho_Encabezados :
DEFINE OUTPUT PARAMETER Encabezado AS CHARACTER FORMAT "X(150)".
DEFINE OUTPUT PARAMETER Reporte AS CHARACTER FORMAT "X(150)".
  CASE Cmb_BAhorros:
    WHEN "Básico" THEN DO: 
        Encabezado = "AGE NIT          NOMBRE                                   CUENTA          TASA PLAZ FEC.APERTURA   SDO.CUENTA".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_Basico" 
               IEx.NLinea = Ct 
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "CUENTA" + Cma + "TASA" + Cma + "PLAZO" + Cma + "FEC.APERTURA" + Cma + "SALDO CUENTA".
    END.
    WHEN "Fechas de Apertura" THEN DO: 
          Encabezado = "AGE NIT            NOMBRE                                   CUENTA          TASA  PLAZO FEC.APERTURA   SDO.CUENTA".
          CREATE IEx.
          ASSIGN Ct = Ct + 1
                 NomInforme = "Aho_FecApertura"
                 IEx.NLinea = Ct
                 IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "CUENTA" + Cma + "TASA" + Cma + "PLAZO" + Cma + "FEC.APERTURA" + Cma + "SALDO CUENTA".
    END.
    WHEN "Fechas de Cancelacion" THEN DO:
        Encabezado = "AGE NIT            NOMBRE                                   CUENTA          TASA  PLAZO  FEC.APE   FEC.CANC      SDO.CUENTA ".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_FecCancelacion"
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "CUENTA" + Cma + "TASA" + Cma + "PLAZO" + Cma + "FEC.APERTURA" + Cma + "FEC.CANCELACION" + Cma + "SALDO CUENTA".
    END.
    WHEN "Fechas de Vencimiento" THEN DO: 
        Encabezado = "AGE NIT            NOMBRE                                   CUENTA          TASA  FEC.APERTR PLAZ FEC.VENCIM  SDO.DISPONIBLE".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_FecVencimiento"
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "CUENTA" + Cma + "TASA" + Cma + "PLAZO" + Cma + "FEC.APERTURA" + Cma + "FEC.VENCIMIENTO" + Cma + "SALDO CUENTA".
    END.
    WHEN "TAC Vencidos" THEN DO:
          ASSIGN Encabezado = "Ag.   Nit      Numero Cuenta        Sdo.Disponible      Int.Pagad   Int.Causad    D.Vdo"
                 ProIni = 3
                 ProFin = 3.
          CREATE IEx.
          ASSIGN Ct = Ct + 1
                 NomInforme = "Aho_TACVencidos"
                 IEx.NLinea = Ct
                 IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NUM.CUENTA" + Cma + "SDO.DISPONIBLE" + Cma + 
                              "INTERES PAGADO" + Cma + "INTERES CAUSADO" + Cma + "DIAS VENCIDOS".
    END.
    WHEN "Canje" THEN DO: 
        Encabezado = "AGE NIT            NOMBRE                                   CUENTA           TASA      FEC.APERTR  SDO.DISPONIBLE     CANJE".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_Canje"
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "CUENTA" + Cma + 
                            "TASA" + Cma + "PLAZO" + Cma + "FEC.APERTURA" + Cma + 
                            "SALDO DISPONIBLE" + Cma + "SALDO EN CANJE". 
    END.
    WHEN "Depositos" THEN DO: 
        Encabezado = "Ag. Nit       Cod.A  NroCuenta        Sdo.Disponible    Sdo. Canje   Saldo Minimo    Int_Causado".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_Depositos"
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "COD.AHORRO" + Cma + "NUM.CUENTA" + Cma + 
                            "SDO.DISPONIBLE" + Cma + "SDO.CANJE" + Cma + "SDO.MINIMO" + Cma + "INT.CAUSADO". 
    END.
      WHEN "Clasificacion CDATS" THEN DO:
          ASSIGN Encabezado = "Ag. Cuenta    SaldoAgencia    SaldoLibros"
                 Tpdini = 3
                 TpdFin = 3
                 NomInforme = "Aho_CDATS".
          CREATE IEx.
          ASSIGN Ct = Ct + 1
                 IEx.NLinea = Ct
                 IEx.Linea  = "AGENCIA" + Cma + "CUENTA" + Cma + "SALDOAGENCIA" + Cma + "SALDOLIBROS".
      END.

    WHEN "A Termino" THEN DO:
        ASSIGN Encabezado = "Ag. Nit            Numero Titulo   Valor Titulo Fec.Const. Fec.Vcto    Plazo     Tasa   Int.Causado    Int.X Pagar Ult.Liq    Prox.Liq   Per.Pago"
               Tpdini = 3
               TpdFin = 3.
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               NomInforme = "Aho_ATermino"
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NUM.TITULO" + Cma + "VALOR TITULO" + Cma + 
                            "FEC.CONSTITUCION" + Cma + "FEC.VENCIMIENTO" + Cma + "PLAZO" + Cma + 
                            "TASA" + Cma + "INT.CAUSADO" + Cma + "INT.X.PAGAR" + Cma + "FEC.ULT.PAGO" + Cma + "FEC.PROX.LIQ" + Cma + "PER.PAGO".
    END.
    WHEN "Contractual" /*OR WHEN "Aportes"*/ THEN DO:
        ASSIGN Encabezado = "Ag.   Nit      Numero Titulo       Valor Titulo  Vlr. Cuota  Fec.Const  Fec.Vencmto Sdo.Disponb   Plazo  Tasa Int.Causado Fec_Ulttran  Estado"
               Tpdini = 2
               TpdFin = 2.
        IF Cmb_BAhorros EQ "Contractual" THEN NomInforme = "Aho_Contractual".
        ELSE NomInforme = "Aho_Aportes".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NUM.TITULO" + Cma + "VALOR TITULO" + Cma + "VAL.CUOTA" + Cma +
                            "FEC.CONSTITUCION" + Cma + "FEC.VENCIMIENTO" + Cma + "SDO.DISPONIBLE" + Cma + "PLAZO" + Cma + "TASA" + Cma + 
                            "INT.CAUSADO" + Cma + "FEC.ULTTRANSACCION" + Cma + "ESTADO".
    END.
    /**Crea reporte de Aportes**/
    /**Adicionado por Alan Gordon**/
    /**05 de octubre de 2007**/
    WHEN "Aportes" THEN DO:
        ASSIGN Encabezado = "Ag. Nit            Numero Titulo  Fec. Afil  Vlr.Cuota  Estado   Forma Pago Per.Pago   Sdo.Disponible Ult. Pago"
               Tpdini = 4
               TpdFin = 4
               NomInforme = "Aho_Aportes".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NUM.TITULO" + Cma + "FEC. AFILIACION" + 
                            Cma + "VAL.CUOTA" + Cma + "ESTADO" + Cma + "FORMA PAGO" + Cma + 
                            "PERIODO PAGO" + Cma + "SDO.DISPONIBLE" + Cma + "FEC.ULT.PAGO".
    END.
    /**fin de la adicion**/
    WHEN "Contractuales 3 Vdas" THEN DO:
       ASSIGN Encabezado = "Ag. Ced/Nit    Pdcto Numero-Titulo  F-Apertura Fec.Vencmto  Vlr. Cuota Sdo.Disponble Plaz-Vdas Int.Causado Fec_Ulttran"
               Tpdini = 2
               TpdFin = 2
               NomInforme = "Aho_Contrac3Vdas".
       CREATE IEx.
       ASSIGN Ct = Ct + 1
              IEx.NLinea = Ct
              IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "CODAHORRO" + Cma + "CUEAHORROS" + Cma +
                           "FEC.APERTURA" + Cma + "FEC.VENCIMIENTO" + Cma +
                           "CUOTA" + Cma + "SDO.DISPONIBLE" + Cma + "PLAZO" + Cma +
                           "FALTAN" + Cma + "INT.CAUSADO" + Cma + "FEC.ULTTRANSACCION".
    END.
    WHEN "Vctos-Liquidac.Proximas A Termino" THEN DO:
        Encabezado = "Ag. Ced/Nit      Nombre                    Telefono Pdcto Numero-Titulo  F-Apertura Fec.Vecmto Fec.Liqd Sdo.Disponb Plazo Int.Causado".
        IF fecini NE fecfin THEN
           ASSIGN Reporte = "REPORTE Ahorros : Vctos-Liquidac.Proximas: " + " - FECHA: " + 
                              STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + 
                              " DEL " + STRING(FECINI) + " AL " + STRING(FECFIN).
        ELSE
           ASSIGN Reporte = "REPORTE Ahorros : Vctos-Liquidac.Proximas: " + " - FECHA: " + 
                              STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + " AL " + STRING(FECFIN).
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             NomInforme = "Aho_VctosLiquiAT"
             IEx.NLinea = Ct
             IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "TELEFONO" + Cma +
                          "COD.AHORRO" + Cma + "CUENTA" + Cma + "FEC.APERTURA" + Cma +
                          "FEC.VENCIMIENTO" + Cma + "FEC.PROX.LIQUIDACION" + Cma + 
                          "SDO.DISPONIBLE" + Cma + "PLAZO" + Cma + "INT.CAUSADO".
    END.  
    WHEN "Cuentas de Empresas Asociadas" THEN DO:
        Encabezado = "AGE NIT            NOMBRE                  TELEFONO             DIR.COMERCIAL             CUENTA                 SALDO".
        ASSIGN Reporte = "REPORTE Ahorros : Cuentas de Empresas Asociadas: " + " - FECHA: " + 
                              STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + " AL " + STRING(FECFIN).
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             NomInforme = "Aho_CtasEmpresas"
             IEx.NLinea = Ct
             IEx.Linea  = "AGENCIA" + cma + "NIT" + cma + "NOMBRE" + cma + "TELEFONO" + cma + "DIR.COMERCIAL" + cma + "CUENTA" + cma + "SALDO".
    END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ah_FecVencimiento wWin 
PROCEDURE Ah_FecVencimiento :
Listado = W_PathSpl + "FecVencimiento-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin W-DiaVdo.  
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCje    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCjeAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotIntAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCauAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotInt    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCau    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR w_pl  AS CHARACTER FORMAT "X(14)" INITIAL "".
ASSIGN J = 0 k = 0.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND
           Ahorros.Agencia      LE AgeFin AND
           Ahorros.Tip_Ahorro   GE TpdIni AND
           Ahorros.Tip_Ahorro   LE TpdFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND
           Ahorros.Cod_Ahorro   LE ProFin AND
           Ahorros.Sdo_Disponible GT 0    AND 
           Ahorros.Sdo_Disponible  GE ValIni AND
           Ahorros.Sdo_Disponible  LE ValFin AND 
           ((Ahorros.Fec_Vencimiento GE FecIni AND 
           Ahorros.Fec_Vencimiento LE FecFin) OR 
           (Ahorros.Fec_Prorroga GE FecIni AND 
           Ahorros.Fec_Prorroga LE FecFin))
           NO-LOCK
           BREAK BY Ahorros.Agencia
                 BY Ahorros.Nit 
                 BY Ahorros.Fec_Vencimiento
                 BY Ahorros.Sdo_Disponible DESCENDING:  
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN NomAho = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
      END.
      j = j + 1.
      RUN Progreso.
      DISPLAY Ahorros.Agencia          FORMAT "999"
              Ahorros.Nit              FORMAT "X(14)"
              NomAho
              Ahorros.Cue_Ahorros      FORMAT "X(14)"
              Ahorros.Tasa             FORMAT ">9.999"
              Ahorros.Plazo            FORMAT "9999"
              Ahorros.Fec_Apertura     FORMAT "99/99/99"    
              Ahorros.Fec_Vencimiento  FORMAT "99/99/99" 
              Ahorros.Sdo_Disponible   FORMAT ">,>>>,>>>,>>9"
      WITH FRAME F4 WIDTH 142 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             IEx.NLinea = Ct
             IEx.Linea  = STRING(Ahorros.Agencia) + Cma + 
                          Ahorros.Nit + Cma + NomAho + Cma +
                          Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                          STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                          STRING(Ahorros.Fec_Vencimiento) + Cma + 
                          STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje,">>>>>>>>>>>9").

      ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
             TotRegAge = TotReg + 1
             TotSdo    = TotSdo + Ahorros.Sdo_Disponible
             TotReg    = TotReg + 1.
      IF LAST-OF(Ahorros.Agencia) THEN
        IF Cmb_BAhorros EQ "Depositos" OR Cmb_BAhorros EQ "TAC Vencidos" OR  Cmb_BAhorros EQ "A Termino" OR   Cmb_BAhorros EQ "Contractual" THEN  
         DISPLAY SKIP(1)
                "Tot Reg Ag: "            AT 1
                 Ahorros.Agencia          AT 13 FORMAT "999"
                ": "                      AT 16
                 TotRegAge                AT 18 FORMAT ">>>,>>9"
                 "  Saldo Disponible Inteses Pagados  Intereses Causados" AT 27 SKIP(1)
                 TotSdoAge                AT 27 FORMAT ">>>,>>>,>>>,>>9"
                 TotInt                   AT 44 FORMAT ">>>,>>>,>>>,>>9"
                 TotCau                   AT 62 FORMAT ">>>,>>>,>>>,>>9"

          WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
       ELSE
           DISPLAY SKIP(1)
                  "Tot Reg Ag: "    AT 1
                   Ahorros.Agencia          AT 15 FORMAT "999"
                  ":"                       AT 20
                   TotRegAge                AT 22 FORMAT ">>>,>>9"
                  "  Saldo Disponible "     AT 33 SKIP(1)
                   TotSdoAge                AT 52 FORMAT ">>>,>>>,>>>,>>9"
            WITH FRAME F10 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Total Disponible   : "          AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ah_VctosLiquidac wWin 
PROCEDURE Ah_VctosLiquidac :
DEFI VAR W_TotSdo  LIKE Ahorros.Sdo_Disponible INIT 0 EXTENT 3.
  DEFI VAR W_TotInt  LIKE Ahorros.Sdo_Disponible INIT 0 EXTENT 3.
  DEFI VAR W_Nom AS CHARACTER FORMAT "X(25)".
  DEFI VAR W_Tel AS CHARACTER FORMAT "X(10)".
  SESSION:SET-WAIT-STATE("GENERAL").
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni                           AND
           Ahorros.Agencia      LE AgeFin                           AND
          (Ahorros.Tip_Ahorro   EQ 2 OR Ahorros.Tip_Ahorro EQ 3)    AND
           Ahorros.Cod_Ahorro   GE ProIni                           AND 
           Ahorros.Cod_Ahorro   LE ProFin                           AND
           Ahorros.Sdo_Dispon   GT 0                                AND
          ((Ahorros.Fec_Vencimi GE FecIni AND Ahorros.Fec_Vencimie LE FecFin) OR
           (Ahorros.Fec_ProLiqu GE FecIni AND Ahorros.Fec_ProLiqui LE FecFin)) NO-LOCK
            BREAK BY Ahorros.Agencia    BY Ahorros.Tip_Ahorro 
                  BY Ahorros.Cod_Ahorro BY Ahorros.Nit:
      FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
      ASSIGN W_Nom = "" W_Tel = "".
      IF AVAILABLE Clientes THEN
         ASSIGN W_Nom = Clientes.Nombre + " " + Clientes.Apellido1
                W_tel = Clientes.Tel_comercial.
      DISPLAY Ahorros.Agencia            FORMAT "999"                          
              Ahorros.Nit                FORMAT "X(12)" 
              W_Nom                      FORMAT "X(25)"
              W_Tel                      FORMAT "X(10)"
              Ahorros.Cod_Ahorro                                      
              Ahorros.Cue_Ahorros        FORMAT "X(14)"                                   
              Ahorros.Fec_Apertura                                                        
              Ahorros.Fec_Vencimiento                                                     
              STRING(Ahorros.Fec_ProLiquidacion) 
              Ahorros.Sdo_disponible     FORMAT ">>>,>>>,>>9"                            
              Ahorros.Plazo / 30         FORMAT "9999"                                
              Ahorros.Int_Causado        FORMAT ">>,>>>,>>9"                             
/*              Ahorros.Fec_Ulttransaccion                              */
           WITH DOWN WIDTH 180 FRAME IAho NO-LABELS NO-BOX USE-TEXT STREAM-IO.
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             IEx.NLinea = Ct
             IEx.Linea  = STRING(Ahorros.Agencia,"999") + Cma + Ahorros.Nit + Cma + W_Nom + Cma + W_Tel + Cma +
                          STRING(Ahorros.Cod_Ahorro) + Cma + STRING(Ahorros.Cue_Ahorros) + Cma +
                          STRING(Ahorros.Fec_Apertura) + Cma + STRING(Ahorros.Fec_Vencimiento) + Cma +
                          STRING(Ahorros.Fec_ProLiquidacion) + Cma + STRING(Ahorros.Sdo_Disponible) + Cma +
                          STRING(Ahorros.Plazo / 30,"9999") + Cma + STRING(Ahorros.INt_Causado).
      IF Ahorros.Fec_Vencimi GE FecIni AND Ahorros.Fec_Vencimie LE FecFin THEN
         ASSIGN W_TotSdo [1] = W_TotSdo [1] + Ahorros.Sdo_disponible
                W_TotInt [1] = W_TotInt [1] + Ahorros.INT_Causado
                W_TotSdo [2] = W_TotSdo [2] + Ahorros.Sdo_disponible
                W_TotInt [2] = W_TotInt [2] + Ahorros.INT_Causado
                W_TotSdo [3] = W_TotSdo [3] + Ahorros.Sdo_disponible
                W_TotInt [3] = W_TotInt [3] + Ahorros.INT_Causado.
      ELSE
         ASSIGN W_TotInt [1] = W_TotInt [1] + Ahorros.INT_Causado
                W_TotInt [2] = W_TotInt [2] + Ahorros.INT_Causado
                W_TotInt [3] = W_TotInt [3] + Ahorros.INT_Causado.
      IF LAST-OF(Ahorros.Cod_Ahorro) THEN DO:
         DISPLAY SKIP(1)
                 "                        Totales del Pdcto x Vencer ------->"
                 W_TotSdo [1]
                 W_TotInt [1] SKIP(1)
             WITH DOWN WIDTH 180 FRAME IAhoT1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN W_TotSdo [1] = 0 
                W_TotInt [1] = 0.
      END.
      IF LAST-OF(Ahorros.Agencia) THEN DO:
         DISPLAY SKIP(1)
                 "                         Totales Agencia x Vencer -------->"
                 W_TotSdo [2]
                 W_TotInt [2] SKIP(2)
             WITH DOWN WIDTH 180 FRAME IAhoT2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN W_TotSdo [2] = 0 W_TotInt [2] = 0.
      END.
  END.
  DISPLAY SKIP(1)
          "                       Totales Generales x Vencer -------->"
          W_TotSdo [3]
          W_TotInt [3] 
       WITH DOWN WIDTH 180 FRAME IAhoT3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN W_TotSdo [3] = 0 W_TotInt [3] = 0.
  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aprob_YaDesemb wWin 
PROCEDURE Aprob_YaDesemb :
/*------------------------------------------------------------------------------
  Purpose:  Informe Solicitudes aprobadas ya Desembolsadas.     
  Notes:    Marzo 17/05 GAER   
 ------------------------------------------------------------------------------*/
     ASSIGN TotRegAge = 0
            TotSdoAge = 0
            TotReg    = 0
            TotSdo    = 0.

     FOR EACH Creditos WHERE Creditos.Agencia      GE AgeIni AND
                             Creditos.Agencia      LE AgeFin AND
                             Creditos.Cod_Credito  GE ProIni AND
                             Creditos.Cod_Credito  LE ProFin AND
                             Creditos.Fec_Desemb   GE FecIni AND
                             Creditos.Fec_Desemb   LE FecFin AND
                             Creditos.Estado       EQ 2 NO-LOCK
                BREAK BY Creditos.Agencia BY Creditos.Fec_Desemb BY Creditos.Fec_Aprob:

         /******* Fecha de Radicacion de Solicitudes ****/
         ASSIGN W_FecRetRad = DATE ("  /  /  ").
         FIND FIRST Solicitud WHERE Solicitud.Nit           EQ Creditos.Nit AND
                                    Solicitud.Num_Solicitud EQ Creditos.Num_Solicitud AND
                                    Solicitud.Estado        EQ 2
                                    NO-ERROR.
         IF AVAILABLE Solicitud THEN
            ASSIGN W_FecRetRad =  Solicitud.Fec_Solicitud. 
         /************************************************/
         DISPLAY Creditos.Agencia        LABEL "Ag."           FORMAT "999"
                 Creditos.Nit            LABEL "Ced./Nit"      FORMAT "X(12)"
                 Creditos.Cod_Credito    LABEL "Pdto"          FORMAT "999"
                 Creditos.Num_Solicitud  LABEL "Num-Solicit"   FORMAT "999999999"
                 Creditos.Tasa           LABEL "Tasa"          FORMAT ">9.9999"
                 Creditos.Plazo          LABEL "Plazo"         FORMAT "9999"
                 W_FecRetRad             LABEL "Fec-Radica."   FORMAT "99/99/9999"
                 Creditos.Fec_Aprobacion LABEL "Fec-Aprobac"   FORMAT "99/99/9999"
                 Creditos.Fec_Desemb     LABEL "Fec-Desemb."   FORMAT "99/99/9999" WHEN Creditos.Estado EQ 2
                 Creditos.Monto          LABEL "Monto Crédito" FORMAT ">,>>>,>>>,>>9"
                 Creditos.Usuario        LABEL "Usuario"
              WITH DOWN FRAME F_AprCred WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

         ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
                TotRegAge = TotRegAge + 1
                TotSdo    = TotSdo    + Creditos.Monto
                TotReg    = TotReg    + 1.

         IF LAST-OF(Creditos.Agencia) THEN DO:
            DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Creditos.Agencia                AT 30 FORMAT "999"
                 ": "                            AT 35
                 TotRegAge                       AT 40
                "Tot.Sdo.Prestado Agencia :"     AT 60
                 TotSdoAge                       AT 89 SKIP(1)
                WITH FRAME tApro1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

            ASSIGN TotRegAge = 0
                   TotSdoAge = 0.
         END.
    END.

    DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Total Prestado Entidad  : "     AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

  ASSIGN TotReg = 0
         TotSdo = 0.

  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BtnImp_Ahorros wWin 
PROCEDURE BtnImp_Ahorros :
Listado = W_PathSpl + "ImpAhorros-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" 
  INITIAL "AGE NIT           CODPDT CUENTA            TASA   PLAZO   ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
ASSIGN J = 0 k = 0.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Ahorros: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BClientes:
    WHEN "Básico" THEN  
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE".
    WHEN "Fecha de Cancelacion" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   FEC.CANCELA".
    WHEN "Fecha de Vencimiento" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   FEC.VENCIMIEN".
    WHEN "Canje" THEN
                             /* 6         7         8         9         1
                                012345678901234567890123456789012345678901234567890*/
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   CANJE".
  END CASE.
  VIEW FRAME F-Encabezado.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND
           Ahorros.Agencia      LE AgeFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND
           Ahorros.Cod_Ahorro   LE ProFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit BY Ahorros.Cod_Ahorro:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BAhorros:
        WHEN "Básico" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible  AT 89 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Apertura" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin AND 
              Ahorros.Fec_Apertura   GE FecIni AND 
              Ahorros.Fec_Apertura   LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible  AT 89 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Cancelacion" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Cancelacion GE FecIni AND 
              Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible  AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Fec_Cancelacion AT 105 FORMAT "99/99/9999" 
              WITH FRAME F3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Vencimiento" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Cancelacion GE FecIni AND 
              Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible  AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Fec_Vencimiento AT 105 FORMAT "99/99/9999" 
              WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Canje" THEN DO:
           IF Ahorros.Sdo_Canje  GE ValIni AND
              Ahorros.Sdo_Canje  LE ValFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible  AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Canje       AT 105 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Ahorros.Agencia) THEN
         DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Ahorros.Agencia                 AT 30 FORMAT "999"
                ": "                             AT 35
                 TotRegAge                       AT 40
                "Tot.Sdo.Disponible : "          AT 60
                 TotSdoAge                       AT 89
        WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Total Disponible   : "          AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BtnImp_Clientes wWin 
PROCEDURE BtnImp_Clientes :
Listado = W_PathSpl + "Clientes-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Clientes: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BClientes:
    WHEN "Básico" THEN  
        W_EncColumna = WEnc1 + "TEL.RESIDENCIA   TEL.COMERCIAL".
    WHEN "Actualización de Información" THEN
        W_EncColumna = "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        FEC.ULT.ACT      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Cumpleaños" THEN
        W_EncColumna = WEnc1 + "FEC.NACIMIENTO   TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Afiliaciones" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Retiros" THEN
        W_EncColumna = WEnc1 + "FEC.RETIRO       TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Proveedores" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Solo Asociados" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Solo Clientes No Asociados" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Habiles" THEN
        W_EncColumna = WEnc1 + "FEC.ULT.ACTUALIZ TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "No Habiles" THEN
        W_EncColumna = WEnc1 + "FEC.ULT.ACTUALIZ TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Inconsistencias en Ubicación" THEN
        W_EncColumna = "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        UBICACION        TEL.REDIDENCIA   TEL.COMERCIAL ".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Clientes WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Nit:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BClientes:
        WHEN "Básico" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 THEN DO:
              DISPLAY Clientes.Agencia         AT 1  FORMAT "999"
                      Clientes.Nit             AT 5  FORMAT "X(14)"
                      Clientes.Nombre          AT 20 FORMAT "X(20)"
                      Clientes.Apellido1       AT 42 FORMAT "X(15)"
                      Clientes.Apellido2       AT 59 FORMAT "X(15)"
                      Clientes.Tel_Residencia  AT 76 FORMAT "X(15)"
                      Clientes.Tel_Comercial   AT 93 FORMAT "X(15)"
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Actualización de Información" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_UltActualiza GE FecIni AND
              Clientes.Fec_UltActualiza LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Cumpleaños" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Nacimiento GE FecIni AND
              Clientes.Fec_Nacimiento LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Nacimiento   AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Afiliaciones" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Ingreso GE FecIni AND
              Clientes.Fec_Ingreso LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Retiros" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Retiro GE FecIni AND
              Clientes.Fec_Retiro LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Retiro       AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Proveedores" THEN DO:
           IF Clientes.Tipo_Vinculo GT 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F6 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Solo Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F7 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Solo Clientes No Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F8 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) EQ YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F9 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "No Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) NE YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F10 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
          WHEN "Inconsistencias en Ubicación" THEN DO:
              FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_resi NO-LOCK NO-ERROR.
              IF NOT AVAILABLE ubicacion THEN DO:
                  DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                          Clientes.Nit              AT 5  FORMAT "X(14)"
                          Clientes.Nombre           AT 20 FORMAT "X(20)"
                          Clientes.Apellido1        AT 42 FORMAT "X(15)"
                          Clientes.Apellido2        AT 59 FORMAT "X(15)"
                          Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                          Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                          Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                  WITH FRAME FIU WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                  TotReg = TotReg + 1.
              END.
          END.
      END CASE.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_Actividades wWin 
PROCEDURE Cli_Actividades :
DEFINE VAR W_Act AS CHARACTER.
DEFINE VAR TotAct AS DECIMAL.
DEFINE VAR W_nombres AS CHARACTER.

CREATE IEx.
ASSIGN Ct = Ct + 1
       IEx.NLinea = Ct
       IEx.Linea = "AGENCIA;COD.CIIU;CIIU;NIT;NOMBRES Y APELLIDOS;FEC.ULT.ACTUALIZACION;TEL.RESIDENCIA;TEL.COMERCIAL;". 

FOR EACH Clientes WHERE Clientes.Tipo_Cliente LE 2 AND SUBSTRING(Clientes.Nit,1,2) NE "AF" AND
                        Clientes.Agencia GE AgeIni AND Clientes.Agencia LE AgeFin AND 
                        Clientes.Fec_UltActualiza GE FecIni AND Clientes.Fec_UltActualiza LE FecFin
                        NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Grupo BY Clientes.Fec_UltActualiza:
    j = j + 1.
    RUN Progreso.

    FIND FIRST Ahorros WHERE Clientes.Nit EQ Ahorros.Nit AND 
                             Ahorros.Cod_ahorro EQ 5 AND
                             Ahorros.Tip_ahorro EQ 4
                             NO-LOCK NO-ERROR.
    IF AVAILABLE (Ahorros) THEN DO:
        IF (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) GT 0 THEN DO:
            ASSIGN W_nombres = STRING(Clientes.Nombre) + " " +
                               STRING(Clientes.Apellido1) + " " +
                               STRING(Clientes.Apellido2).
            FIND FIRST Ciiu WHERE Ciiu.Tipo EQ 1 AND
                                  Ciiu.Grupo EQ Clientes.Grupo
                                  NO-LOCK NO-ERROR.
            IF AVAILABLE Ciiu THEN
                ASSIGN W_Act = STRING(Ciiu.Descripcion).
            ELSE
                ASSIGN W_Act = "Actividad NO Alimentada en Registro de Cliente".

            PUT Clientes.Agencia FORMAT "999" " "
                Clientes.Grupo FORMAT "999" ":"
                W_Act FORMAT "X(16)" " "
                Clientes.Nit FORMAT "X(14)" " "
                W_nombres FORMAT "X(40)" " "
                Clientes.Fec_UltActualiza FORMAT "99/99/9999" " "
                Clientes.Tel_Residencia FORMAT "X(15)" " "
                Clientes.Tel_Comercial FORMAT "X(15)" SKIP(0).

            CREATE IEx.
            ASSIGN Ct = Ct + 1
                   IEx.NLinea = Ct
                   IEx.Linea = STRING(Clientes.Agencia) + Cma +
                               STRING(Clientes.Grupo) + Cma + W_Act + Cma + 
                               STRING(Clientes.Nit) + Cma + W_nombres + Cma +
                               STRING(Clientes.Fec_UltActualiza) + Cma + 
                               STRING(Clientes.Tel_Residencia) + Cma + 
                               STRING(Clientes.Tel_Comercial) + Cma.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_Afiliaciones wWin 
PROCEDURE Cli_Afiliaciones :
FOR EACH Clientes WHERE Clientes.Agencia GE AgeIni AND Clientes.Agencia LE AgeFin 
                    AND Clientes.Tipo_Vinculo EQ 1 AND
                        Clientes.Fec_Ingreso GE FecIni AND Clientes.Fec_Ingreso LE FecFin NO-LOCK
                      BY Clientes.Agencia BY Clientes.Fec_Ingreso:

    /*EACH Ahorros  WHERE ahorros.nit = clientes.nit AND  ahorros.cod_ahorro EQ 5
                        AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0
      NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Fec_Ingreso:  */
      j = j + 1.
      RUN Progreso.
      FIND FIRST Ahorros  WHERE ahorros.nit = clientes.nit AND
                               (ahorros.cod_ahorro EQ 5 OR ahorros.cod_ahorro EQ 9)
                        AND Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
      IF AVAIL(Ahorros) THEN
                    DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                            Clientes.Nit              AT 5  FORMAT "X(14)"
                            Clientes.Nombre           AT 20 FORMAT "X(20)"
                            Clientes.Apellido1        AT 42 FORMAT "X(15)"
                            Clientes.Apellido2        AT 59 FORMAT "X(15)"
                            Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                            Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                            Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                    WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                    TotReg = TotReg + 1.
      
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_Habiles wWin 
PROCEDURE Cli_Habiles :
DEFINE OUTPUT PARAMETER TotR AS DECIMAL.
    DEFINE VAR nomcli AS CHARACTER FORMAT "X(30)".
    DEFINE VARIABLE TotRA AS DECIMAL.
    DEFINE VAR pc40 AS DECIMAL.
    DEFINE VAR SdoVdoCre AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR CtaVda AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR meseswk AS DECIMAL.
    DEFINE VAR SdoCre  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR Bd AS LOGICAL INITIAL YES.


    DEFINE VARIABLE vlsihabil AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vlnohabil AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vlfondos  AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vctipcli  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vctipvin  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vctipenv  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vdtotapor AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vdsdocre  AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE vndias    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vdfecha   AS DATE      NO-UNDO.
    DEFINE VARIABLE vdaFecIng AS DATE        NO-UNDO. /*fecha de ingreso como cliente o asociado*/

    EMPTY TEMP-TABLE TemAsociados.

    ASSIGN TotR = 0
        j = 0.

    FOR EACH Clientes WHERE 
        (Clientes.Agencia EQ AgeIni OR AgeIni = 0) AND
        Clientes.Estado       EQ 1 AND
        Clientes.Tipo_Vinculo EQ 1 AND
        TRUE NO-LOCK BY Clientes.Agencia BY Clientes.Nit:

        ASSIGN vctipcli = ""
            vdaFecIng = IF Clientes.Fec_Ingreso EQ ? THEN Clientes.Fec_Asociacion ELSE Clientes.Fec_Ingreso.
        RUN Progreso.

        IF vdaFecIng LE FecFin THEN  DO:
            ASSIGN j = j + 1.
            CASE Clientes.Tipo_Cliente:                                                                                               
                 WHEN 1 THEN
                     ASSIGN vctipcli = "Natural Mayor De Edad".
                 WHEN 2 THEN
                     ASSIGN vctipcli = "Natural Menor De Edad".
                 WHEN 3 THEN
                     ASSIGN vctipcli = "Juridicas S.A.".
                 WHEN 4 THEN
                     ASSIGN vctipcli = "Juridicas C.A.".
            END CASE.     
            ASSIGN vctipvin = "".
            CASE Clientes.Tipo_Vinculo:   
                WHEN 1 THEN
                    ASSIGN vctipvin = "1-Asociado".
                WHEN 2 THEN
                    ASSIGN vctipvin = "2-Cliente No Asociado".
                WHEN 3 THEN
                    ASSIGN vctipvin = "3-Tercero".
                WHEN 4 THEN
                    ASSIGN vctipvin = "4-Proveedor".
            END CASE.  
            ASSIGN vctipenv = "".
            CASE Clientes.Dir_Correspondencia:   
                 WHEN YES THEN
                     ASSIGN vctipenv = "1-Residencia".
                 WHEN FALSE THEN
                     ASSIGN vctipenv = "2-Trabajo".
            END CASE.  

            CREATE TemAsociados.
            UPDATE TemAsociados.TAgecli     = Clientes.Agencia                                                                       
                   TemAsociados.TNit        = Clientes.Nit                                                                           
                   TemAsociados.TNombres    = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2)
                   TemAsociados.TFecIngre   = IF Clientes.Fec_Ingreso EQ ? THEN Clientes.Fec_Asociacion ELSE Clientes.Fec_Ingreso
                   TemAsociados.TDetalle    = "No-Habil"
                   TemAsociados.TTipVincu   = vctipvin                                                                               
                   TemAsociados.TTipClien   = vctipcli                                                                               
                   TemAsociados.TDirCorres  = vctipenv                                                                               
                   TemAsociados.TDirRes     = TRIM(Clientes.Dir_Residencia)                                                          
                   TemAsociados.TTelRes     = TRIM(Clientes.Tel_Residencia)                                                          
                   TemAsociados.TDirCor     = TRIM(Clientes.Dir_Comercial)                                                           
                   TemAsociados.TTelCor     = TRIM(Clientes.Tel_Comercial)                                                          
                   TemAsociados.TAportes    = 0
                   TemAsociados.TAtrapor    = "No"
                   TemAsociados.TCreditos   = 0
                   TemAsociados.TFondos     = 0.

            ASSIGN vdtotapor = 0.
            FOR EACH Ahorros WHERE 
               ahorros.nit        EQ clientes.nit AND 
               ahorros.tip_ahorro EQ 4 AND
               ahorros.estado     EQ 1 AND
               ahorros.sdo_disponible + ahorros.sdo_canje GT 0 NO-LOCK BREAK BY Ahorros.Nit:

               ASSIGN vdtotapor = vdtotapor + ahorros.sdo_disponible + ahorros.sdo_canje.
               IF LAST-OF (ahorros.nit)THEN DO:
                   UPDATE TemAsociados.TAportes    = vdtotapor.
                   IF vdtotapor GE 0 THEN 
                       UPDATE TemAsociados.TDetalle    = "Habil".
               END.
            END. /*FOR EACH Ahorros */
            /* Valida atraso en Aportes*/
            FOR EACH Ahorros WHERE 
                ahorros.nit         EQ Clientes.Nit AND
                ahorros.cod_ahorro  EQ 5 AND
                ahorros.estado      EQ 1 AND /*AND  nit = "93363496"  - 12972259 Aportes Obligatorios*/
                ahorros.sdo_disponible + ahorros.sdo_canje GT 0 /*AND
                ahorros.nit BEGINS "93"*/    NO-LOCK:
                FIND LAST mov_ahorros WHERE
                     mov_ahorros.cod_ahorro EQ ahorros.cod_ahorro AND
                     mov_ahorros.nit        EQ ahorros.nit        AND
                     mov_ahorros.fecha      LE fecfin             AND                          
                    (mov_ahorros.val_efectivo + mov_ahorros.val_cheque) GT 0 NO-LOCK NO-ERROR.
                IF AVAILABLE mov_ahorros THEN DO:
                   ASSIGN vndias  = TODAY - fecfin
                          vdfecha = TODAY - vndias.
                  IF (vdfecha - mov_ahorros.fecha) GT 30 THEN DO:
        /*           IF (TODAY - mov_ahorros.Fecha) GT 61 THEN DO: */
                     UPDATE TemAsociados.TDetalle = "No-Habil"
                            TemAsociados.TAtrapor = "Si".
                  END.
                END. /*IF AVAILABLE mov_ahorros */
                ELSE DO:
                  /* No Tiene Movimiento*/
                  ASSIGN vndias  = TODAY - fecfin
                         vdfecha = TODAY - vndias.
                  IF (vdfecha - ahorros.fec_apertura) GT 30 THEN DO:
        /*           IF (TODAY - ahorros.fec_apertura) GT 61 THEN DO:  */
                      UPDATE TemAsociados.TDetalle = "No-Habil"
                             TemAsociados.TAtrapor = "Si".
                  END.
                END. /*ELSE DO:*/
            END. /*FOR EACH Ahorros */
            /* Valida Creditos en Mora Sin Fondos Mutuales*/
            FOR EACH creditos WHERE 
                 creditos.Nit         EQ clientes.nit AND
                 creditos.cod_credito NE 545 AND creditos.cod_credito NE 546 AND
                 creditos.estado      EQ 2   AND creditos.sdo_capital GT 0   AND
                 ((creditos.dias_atraso - (TODAY - fecfin)) GT 30            AND
                 creditos.val_atraso  GT 0)  NO-LOCK BREAK BY creditos.nit:
    
                ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                  Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                  Creditos.Int_Anticipado.
                IF LAST-OF (creditos.nit)THEN DO:
                   IF ROUND(vdsdocre,0) GT 0 THEN DO:
                       UPDATE TemAsociados.TDetalle  = "No-Habil"
                              TemAsociados.TCreditos = vdsdocre.
                   END.
                   ASSIGN  vdsdocre = 0.
                END.
            END. /*FOR EACH creditos */
    
            /* Valida Fondos Mutuales - Funerario (545) - Educativo (546) */
            ASSIGN  vdsdocre = 0.
            FOR EACH creditos WHERE
                 creditos.nit EQ clientes.nit AND
                (creditos.cod_credito EQ 545 OR  creditos.cod_credito EQ 546) AND
                 creditos.estado      EQ 2   AND creditos.sdo_capital GT 0    AND 
                ((creditos.dias_atraso - (W_fecha - fecfin))            GE 0    OR
                 (creditos.dias_atraso - (W_fecha - fecfin))            LE 30)  NO-LOCK BREAK BY creditos.nit:
        /* /*         (creditos.dias_atraso GT 30  AND creditos.val_atraso  GT 0)*/   NO-LOCK BREAK BY creditos.nit: */ 
            /*     IF creditos.monto - creditos.sdo_capital THEN DO: */
                IF FIRST-OF (creditos.nit) THEN
                   ASSIGN vlfondos = FALSE.
    
                IF creditos.cod_credito EQ 545 THEN
                   IF ROUND(creditos.monto / 2 , 2) LT ROUND(creditos.sdo_capital, 2) THEN DO:
                      ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                        Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                        Creditos.Int_Anticipado.
                      ASSIGN vlfondos = TRUE.
                   END. /*IF ROUND(creditos.monto / 2 , 2) */
    
                IF creditos.cod_credito EQ 546 THEN DO:
                   ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                     Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                     Creditos.Int_Anticipado.
                   ASSIGN vlfondos = TRUE.
                END.
    
                IF vlfondos AND ROUND(vdsdocre,0) GT 0 THEN DO:
                   IF LAST-OF (creditos.nit)THEN DO:
                      FIND CURRENT TemAsociados.
                      UPDATE TemAsociados.TDetalle = "No-Habil"
                             TemAsociados.TFondos  = vdsdocre.
                      ASSIGN vlfondos = FALSE.
                   END.
                END. /*IF vlfondos AND */
            END. /*FOR EACH creditos */
            IF AVAILABLE TemAsociados THEN DO:
                IF TemAsociados.TDetalle EQ "Habil" THEN DO:
                    FORM
                        TemAsociados.TAgecli        COLUMN-LABEL "Age"              FORMAT "999"   
                        TemAsociados.TNit           COLUMN-LABEL "Nit"              FORMAT "X(12)"
                        TemAsociados.TNombres       COLUMN-LABEL "Nombres"          FORMAT "X(30)"
                /*         TemAsociados.TFecIngre      COLUMN-LABEL "Fec_ing"          FORMAT "99/99/9999" */
                        TemAsociados.TDetalle       COLUMN-LABEL "Detalle"          FORMAT "X(8)"
                        TemAsociados.TTipVincu      COLUMN-LABEL "Vinculo"          FORMAT "X(10)"
                        TemAsociados.TTipClien      COLUMN-LABEL "Tip_Cliente"      FORMAT "X(10)"
                        TemAsociados.TDirCorres     COLUMN-LABEL "Corresp."         FORMAT "X(8)"
                        TemAsociados.TDirRes        COLUMN-LABEL "Dir. Residencia"  FORMAT "X(30)"
                        TemAsociados.TTelRes        COLUMN-LABEL "Tel.Res."         FORMAT "X(10)"
                        TemAsociados.TDirCor        COLUMN-LABEL "Dir. Corresp"     FORMAT "X(30)"
                        TemAsociados.TTelCor        COLUMN-LABEL "Tel.Corr. "       FORMAT "X(10)"
                        TemAsociados.TAportes       COLUMN-LABEL "T. Aportes"       FORMAT "->>,>>>,>>9"
                        TemAsociados.TAtrapor       COLUMN-LABEL "Atraso?"          FORMAT "X(3)"
                        TemAsociados.TCreditos      COLUMN-LABEL "Vencid Cred"      FORMAT "->>,>>>,>>9"
                        TemAsociados.TFondos        COLUMN-LABEL "Vencid Fondos"    FORMAT "->>,>>>,>>9"
                        WITH FRAME Cli_Habil DOWN COLUMN 1 WIDTH 240
                            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
                    DISPLAY
                        TemAsociados.TAgecli        
                        TemAsociados.TNit           
                        TemAsociados.TNombres       
    /*                     TemAsociados.TFecIngre */
                        TemAsociados.TDetalle       
                        TemAsociados.TTipVincu      
                        TemAsociados.TTipClien      
                        TemAsociados.TDirCorres     
                        TemAsociados.TDirRes        
                        TemAsociados.TTelRes        
                        TemAsociados.TDirCor        
                        TemAsociados.TTelCor        
                        TemAsociados.TAportes       
                        TemAsociados.TAtrapor       
                        TemAsociados.TCreditos      
                        TemAsociados.TFondos        
                        WITH FRAME Cli_Habil.
                    DOWN WITH FRAME Cli_Habil.
                    ASSIGN TotR = TotR + 1.
                END.
            END. /*IF TemAsociados.TDetalle */
        END. /*IF vdaFecIng */
    END. /*FOR EACH Clientes */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_MotivosVinculacion wWin 
PROCEDURE Cli_MotivosVinculacion :
DEFINE VAR Motivo AS CHARACTER FORMAT "X(75)".
DEFINE VAR TotMot AS DECIMAL.     
DEFINE VAR W_nombres AS CHARACTER.

CREATE IEx.
ASSIGN Ct = Ct + 1
       IEx.NLinea = Ct
       IEx.Linea  = "AGENCIA;COD.VINCULACION;NOMBRE VINCULACION;NIT;NOMBRES Y APELLIDOS;FEC.AFILIACION;TEL.RESIDENCIA;TEL.COMERCIAL;".


     FOR EACH Clientes WHERE Tipo_Vinculo LE 2 AND SUBSTRING(Clientes.Nit,1,2) NE "AF"AND
                             Clientes.Agencia GE AgeIni AND
                             Clientes.Agencia LE AgeFin AND 
                             Clientes.Fec_Asociacion GE FecIni AND 
                             Clientes.Fec_Asociacion LE FecFin NO-LOCK
                             BREAK BY Clientes.Agencia BY Clientes.Cod_Ingreso BY Clientes.Fec_Asociacion:
         j = j + 1.
         RUN Progreso.
         
         FIND FIRST Ahorros WHERE Clientes.Nit EQ Ahorros.Nit AND
                                  Clientes.Agencia EQ Ahorros.Agencia AND
                                  Ahorros.Cod_ahorro EQ 5 AND Ahorros.Tip_ahorro EQ 4
                                  AND (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) GT 0
                                  NO-LOCK NO-ERROR.
         IF AVAILABLE (Ahorros) THEN DO:
             ASSIGN W_nombres = STRING(Clientes.Nombre) + " " +
                                STRING(Clientes.Apellido1) + " " +
                                STRING(Clientes.Apellido2).
             IF FIRST-OF (Clientes.Cod_Ingreso) THEN DO:
                 FIND FIRST Varios WHERE Varios.Tipo EQ 4 AND 
                                         Varios.Codigo EQ Clientes.Cod_Ingreso 
                                         NO-LOCK NO-ERROR.
                 IF AVAILABLE Varios THEN
                     ASSIGN Motivo = "Motivo " +
                                     STRING(Varios.Codigo) + ": " +
                                     STRING(Varios.Descripcion).
                 ELSE
                     ASSIGN Motivo = "Motivo NO Alimentado en Registro de Cliente".
             END.
             ASSIGN TotReg = TotReg + 1
                    TotMot = TotMot + 1.
             IF NOT Ttotales THEN DO:
                 PUT Clientes.Agencia FORMAT "999" " "
                     Clientes.Cod_Ingreso FORMAT "999" " "
                     Clientes.Nit FORMAT "X(14)" " "
                     W_nombres FORMAT "X(40)" " "
                     Clientes.Fec_Asociacion FORMAT "99/99/9999" " "
                     Clientes.Tel_Residencia FORMAT "X(15)" " "
                     Clientes.Tel_Comercial FORMAT "X(15)" SKIP(0).

                 CREATE IEx.
                 ASSIGN Ct = Ct + 1
                        IEx.NLinea = Ct
                        IEx.Linea  = STRING(Clientes.Agencia) + Cma +
                                     STRING(Clientes.Cod_Ingreso) + Cma + Motivo + Cma +
                                     STRING(Clientes.Nit) + Cma + W_nombres + Cma +
                                     STRING(Clientes.Fec_Asociacion) + Cma + 
                                     STRING(Clientes.Tel_Residencia) + Cma + 
                                     STRING(Clientes.Tel_Comercial) + Cma. 
             END.
             IF LAST-OF(Clientes.Cod_Ingreso) THEN DO:
                 IF NOT Ttotales THEN DO:
                     Motivo = "Total " + STRING(Motivo,"X(43)") + ": " + STRING(TotMot,">>>,>>9").
                     DISPLAY SKIP(1) Motivo AT 1 WITH FRAME TotMot3 WIDTH 132 
                         USE-TEXT NO-LABELS NO-BOX STREAM-IO.
                     DISPLAY SKIP(1) " " AT 1 WITH FRAME TotMot3 WIDTH 132 
                         USE-TEXT NO-LABELS NO-BOX STREAM-IO.
                 END.
                 ELSE DO:
                     Motivo = "Agencia " + 
                              STRING(Clientes.Agencia,"999") + ": " + 
                              STRING(Motivo,"X(43)") + ": " + STRING(TotMot,">>>,>>9").
                     DISPLAY Motivo  AT 1 WITH FRAME TotMot3 WIDTH 146 
                         USE-TEXT NO-LABELS NO-BOX STREAM-IO.
                 END.
                 TotMot = 0.
             END.
         END.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_NoHabiles wWin 
PROCEDURE Cli_NoHabiles :
DEFINE OUTPUT PARAMETER TotR AS DECIMAL.
    DEFINE VAR nomcli AS CHARACTER FORMAT "X(30)".
    DEFINE VARIABLE TotRA AS DECIMAL.
    DEFINE VAR pc40 AS DECIMAL.
    DEFINE VAR SdoVdoCre AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR CtaVda AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR meseswk AS DECIMAL.
    DEFINE VAR SdoCre  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR Bd AS LOGICAL INITIAL YES.


    DEFINE VARIABLE vlsihabil AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vlnohabil AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vlfondos  AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE vctipcli  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vctipvin  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vctipenv  AS CHARACTER FORMAT "X(30)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vdtotapor AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vdsdocre  AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE vndias    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vdfecha   AS DATE      NO-UNDO.
    DEFINE VARIABLE vdaFecIng AS DATE        NO-UNDO. /*fecha de ingreso como cliente o asociado*/

    EMPTY TEMP-TABLE TemAsociados.

    ASSIGN TotR = 0
        j = 0.

    FOR EACH Clientes WHERE 
        (Clientes.Agencia EQ AgeIni OR AgeIni = 0) AND
        Clientes.Estado       EQ 1 AND
        Clientes.Tipo_Vinculo EQ 1 AND
        TRUE NO-LOCK BY Clientes.Agencia BY Clientes.Nit:

        ASSIGN vctipcli = ""
            vdaFecIng = IF Clientes.Fec_Ingreso EQ ? THEN Clientes.Fec_Asociacion ELSE Clientes.Fec_Ingreso.
        RUN Progreso.

        IF vdaFecIng LE FecFin THEN  DO:
            ASSIGN j = j + 1.
            CASE Clientes.Tipo_Cliente:                                                                                               
                 WHEN 1 THEN
                     ASSIGN vctipcli = "Natural Mayor De Edad".
                 WHEN 2 THEN
                     ASSIGN vctipcli = "Natural Menor De Edad".
                 WHEN 3 THEN
                     ASSIGN vctipcli = "Juridicas S.A.".
                 WHEN 4 THEN
                     ASSIGN vctipcli = "Juridicas C.A.".
            END CASE.     
            ASSIGN vctipvin = "".
            CASE Clientes.Tipo_Vinculo:   
                WHEN 1 THEN
                    ASSIGN vctipvin = "1-Asociado".
                WHEN 2 THEN
                    ASSIGN vctipvin = "2-Cliente No Asociado".
                WHEN 3 THEN
                    ASSIGN vctipvin = "3-Tercero".
                WHEN 4 THEN
                    ASSIGN vctipvin = "4-Proveedor".
            END CASE.  
            ASSIGN vctipenv = "".
            CASE Clientes.Dir_Correspondencia:   
                 WHEN YES THEN
                     ASSIGN vctipenv = "1-Residencia".
                 WHEN FALSE THEN
                     ASSIGN vctipenv = "2-Trabajo".
            END CASE.  

            CREATE TemAsociados.
            UPDATE TemAsociados.TAgecli     = Clientes.Agencia                                                                       
                   TemAsociados.TNit        = Clientes.Nit                                                                           
                   TemAsociados.TNombres    = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2)
                   TemAsociados.TFecIngre   = IF Clientes.Fec_Ingreso EQ ? THEN Clientes.Fec_Asociacion ELSE Clientes.Fec_Ingreso
                   TemAsociados.TDetalle    = "No-Habil"
                   TemAsociados.TTipVincu   = vctipvin                                                                               
                   TemAsociados.TTipClien   = vctipcli                                                                               
                   TemAsociados.TDirCorres  = vctipenv                                                                               
                   TemAsociados.TDirRes     = TRIM(Clientes.Dir_Residencia)                                                          
                   TemAsociados.TTelRes     = TRIM(Clientes.Tel_Residencia)                                                          
                   TemAsociados.TDirCor     = TRIM(Clientes.Dir_Comercial)                                                           
                   TemAsociados.TTelCor     = TRIM(Clientes.Tel_Comercial)                                                          
                   TemAsociados.TAportes    = 0
                   TemAsociados.TAtrapor    = "No"
                   TemAsociados.TCreditos   = 0
                   TemAsociados.TFondos     = 0.

            ASSIGN vdtotapor = 0.
            FOR EACH Ahorros WHERE 
               ahorros.nit        EQ clientes.nit AND 
               ahorros.tip_ahorro EQ 4 AND
               ahorros.estado     EQ 1 AND
               ahorros.sdo_disponible + ahorros.sdo_canje GT 0 NO-LOCK BREAK BY Ahorros.Nit:

               ASSIGN vdtotapor = vdtotapor + ahorros.sdo_disponible + ahorros.sdo_canje.
               IF LAST-OF (ahorros.nit)THEN DO:
                   UPDATE TemAsociados.TAportes    = vdtotapor.
                   IF vdtotapor GE 0 THEN 
                       UPDATE TemAsociados.TDetalle    = "Habil".
               END.
            END. /*FOR EACH Ahorros */
            /* Valida atraso en Aportes*/
            FOR EACH Ahorros WHERE 
                ahorros.nit         EQ Clientes.Nit AND
                ahorros.cod_ahorro  EQ 5 AND
                ahorros.estado      EQ 1 AND /*AND  nit = "93363496"  - 12972259 Aportes Obligatorios*/
                ahorros.sdo_disponible + ahorros.sdo_canje GT 0 /*AND
                ahorros.nit BEGINS "93"*/    NO-LOCK:
                FIND LAST mov_ahorros WHERE
                     mov_ahorros.cod_ahorro EQ ahorros.cod_ahorro AND
                     mov_ahorros.nit        EQ ahorros.nit        AND
                     mov_ahorros.fecha      LE fecfin             AND                          
                    (mov_ahorros.val_efectivo + mov_ahorros.val_cheque) GT 0 NO-LOCK NO-ERROR.
                IF AVAILABLE mov_ahorros THEN DO:
                   ASSIGN vndias  = TODAY - fecfin
                          vdfecha = TODAY - vndias.
                  IF (vdfecha - mov_ahorros.fecha) GT 30 THEN DO:
        /*           IF (TODAY - mov_ahorros.Fecha) GT 61 THEN DO: */
                     UPDATE TemAsociados.TDetalle = "No-Habil"
                            TemAsociados.TAtrapor = "Si".
                  END.
                END. /*IF AVAILABLE mov_ahorros */
                ELSE DO:
                  /* No Tiene Movimiento*/
                  ASSIGN vndias  = TODAY - fecfin
                         vdfecha = TODAY - vndias.
                  IF (vdfecha - ahorros.fec_apertura) GT 30 THEN DO:
        /*           IF (TODAY - ahorros.fec_apertura) GT 61 THEN DO:  */
                      UPDATE TemAsociados.TDetalle = "No-Habil"
                             TemAsociados.TAtrapor = "Si".
                  END.
                END. /*ELSE DO:*/
            END. /*FOR EACH Ahorros */
            /* Valida Creditos en Mora Sin Fondos Mutuales*/
            FOR EACH creditos WHERE 
                 creditos.Nit         EQ clientes.nit AND
                 creditos.cod_credito NE 545 AND creditos.cod_credito NE 546 AND
                 creditos.estado      EQ 2   AND creditos.sdo_capital GT 0   AND
                 ((creditos.dias_atraso - (TODAY - fecfin)) GT 30            AND
                 creditos.val_atraso  GT 0)  NO-LOCK BREAK BY creditos.nit:
    
                ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                  Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                  Creditos.Int_Anticipado.
                IF LAST-OF (creditos.nit)THEN DO:
                   IF ROUND(vdsdocre,0) GT 0 THEN DO:
                       UPDATE TemAsociados.TDetalle  = "No-Habil"
                              TemAsociados.TCreditos = vdsdocre.
                   END.
                   ASSIGN  vdsdocre = 0.
                END.
            END. /*FOR EACH creditos */
    
            /* Valida Fondos Mutuales - Funerario (545) - Educativo (546) */
            ASSIGN  vdsdocre = 0.
            FOR EACH creditos WHERE
                 creditos.nit EQ clientes.nit AND
                (creditos.cod_credito EQ 545 OR  creditos.cod_credito EQ 546) AND
                 creditos.estado      EQ 2   AND creditos.sdo_capital GT 0    AND 
                ((creditos.dias_atraso - (W_fecha - fecfin))            GE 0    OR
                 (creditos.dias_atraso - (W_fecha - fecfin))            LE 30)  NO-LOCK BREAK BY creditos.nit:
        /* /*         (creditos.dias_atraso GT 30  AND creditos.val_atraso  GT 0)*/   NO-LOCK BREAK BY creditos.nit: */ 
            /*     IF creditos.monto - creditos.sdo_capital THEN DO: */
                IF FIRST-OF (creditos.nit) THEN
                   ASSIGN vlfondos = FALSE.
    
                IF creditos.cod_credito EQ 545 THEN
                   IF ROUND(creditos.monto / 2 , 2) LT ROUND(creditos.sdo_capital, 2) THEN DO:
                      ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                        Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                        Creditos.Int_Anticipado.
                      ASSIGN vlfondos = TRUE.
                   END. /*IF ROUND(creditos.monto / 2 , 2) */
    
                IF creditos.cod_credito EQ 546 THEN DO:
                   ASSIGN vdsdocre = vdsdocre + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                     Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Sdo_Capital -
                                     Creditos.Int_Anticipado.
                   ASSIGN vlfondos = TRUE.
                END.
    
                IF vlfondos AND ROUND(vdsdocre,0) GT 0 THEN DO:
                   IF LAST-OF (creditos.nit)THEN DO:
                      FIND CURRENT TemAsociados.
                      UPDATE TemAsociados.TDetalle = "No-Habil"
                             TemAsociados.TFondos  = vdsdocre.
                      ASSIGN vlfondos = FALSE.
                   END.
                END. /*IF vlfondos AND */
            END. /*FOR EACH creditos */
            IF AVAILABLE TemAsociados THEN DO:
/*                 IF TemAsociados.TDetalle EQ "No-Habil" THEN DO: */
                IF TemAsociados.TDetalle EQ "No-Habil" THEN DO:
                    FORM
                        TemAsociados.TAgecli        COLUMN-LABEL "Age"              FORMAT "999"   
                        TemAsociados.TNit           COLUMN-LABEL "Nit"              FORMAT "X(12)"
                        TemAsociados.TNombres       COLUMN-LABEL "Nombres"          FORMAT "X(30)"
                /*         TemAsociados.TFecIngre      COLUMN-LABEL "Fec_ing"          FORMAT "99/99/9999" */
                        TemAsociados.TDetalle       COLUMN-LABEL "Detalle"          FORMAT "X(8)"
                        TemAsociados.TTipVincu      COLUMN-LABEL "Vinculo"          FORMAT "X(10)"
                        TemAsociados.TTipClien      COLUMN-LABEL "Tip_Cliente"      FORMAT "X(10)"
                        TemAsociados.TDirCorres     COLUMN-LABEL "Corresp."         FORMAT "X(8)"
                        TemAsociados.TDirRes        COLUMN-LABEL "Dir. Residencia"  FORMAT "X(30)"
                        TemAsociados.TTelRes        COLUMN-LABEL "Tel.Res."         FORMAT "X(10)"
                        TemAsociados.TDirCor        COLUMN-LABEL "Dir. Corresp"     FORMAT "X(30)"
                        TemAsociados.TTelCor        COLUMN-LABEL "Tel.Corr. "       FORMAT "X(10)"
                        TemAsociados.TAportes       COLUMN-LABEL "T. Aportes"       FORMAT "->>,>>>,>>9"
                        TemAsociados.TAtrapor       COLUMN-LABEL "Atraso?"          FORMAT "X(3)"
                        TemAsociados.TCreditos      COLUMN-LABEL "Vencid Cred"      FORMAT "->>,>>>,>>9"
                        TemAsociados.TFondos        COLUMN-LABEL "Vencid Fondos"    FORMAT "->>,>>>,>>9"
                        WITH FRAME Cli_Habil DOWN COLUMN 1 WIDTH 240
                            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
                    DISPLAY
                        TemAsociados.TAgecli        
                        TemAsociados.TNit           
                        TemAsociados.TNombres       
    /*                     TemAsociados.TFecIngre */
                        TemAsociados.TDetalle       
                        TemAsociados.TTipVincu      
                        TemAsociados.TTipClien      
                        TemAsociados.TDirCorres     
                        TemAsociados.TDirRes        
                        TemAsociados.TTelRes        
                        TemAsociados.TDirCor        
                        TemAsociados.TTelCor        
                        TemAsociados.TAportes       
                        TemAsociados.TAtrapor       
                        TemAsociados.TCreditos      
                        TemAsociados.TFondos        
                        WITH FRAME Cli_Habil.
                    DOWN WITH FRAME Cli_Habil.
                    ASSIGN TotR = TotR + 1.
                END.
            END. /*IF TemAsociados.TDetalle */
        END. /*IF vdaFecIng */
    END. /*FOR EACH Clientes */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cli_Profesiones wWin 
PROCEDURE Cli_Profesiones :
DEFINE VAR Nomcli AS CHARACTER FORMAT "x(40)".
DEFINE VAR NomPro AS CHARACTER FORMAT "x(20)".
DEFINE VAR NomCar AS CHARACTER FORMAT "x(20)".
DEFINE VAR Motivo AS CHARACTER FORMAT "X(60)".
DEFINE VAR TotMot AS DECIMAL.  
DEFINE VAR Wsex   AS CHARACTER.
DEFINE VAR Wedad  AS DECIMAL.

     FOR EACH Clientes WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin AND
           Clientes.Tipo_Vinculo LT 3 AND 
           Clientes.Fec_Ingreso GE FecIni AND
           Clientes.Fec_Ingreso LE FecFin NO-LOCK BREAK BY Clientes.Fec_Ingreso:  
         FIND FIRST ahorros WHERE ahorros.nit = clientes.nit AND
                                  ahorros.tip_ahorro = 4 AND 
                                  ahorros.Fec_apertura  GE FecIni AND
                                  ahorros.Fec_apertura LE FecFin NO-ERROR.
         IF AVAILABLE(ahorros) THEN DO:
         
           j = j + 1.
           RUN Progreso.
           ASSIGN TotReg = TotReg + 1
                  TotMot = TotMot + 1.
           ASSIGN NomPro = "" NomCar = "".
           
           IF clientes.sexo = 1 THEN Wsex = "Hombre  " .
              ELSE Wsex = "Mujer   ".
           
           ASSIGN wedad = (TODAY - clientes.fec_nacimiento) / 365.

           FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
           IF AVAILABLE Varios THEN
              NomPro = Varios.Descripcion.
           FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
           IF AVAILABLE Varios THEN
              NomCar = Varios.Descripcion.

           NomCli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
           DISPLAY Clientes.Fec_Ingreso
                   Clientes.Nit
                   NomCli
                   wsex
                   wedad
                   Clientes.Salario
                   NomCar
                   NomPro
           WITH FRAME Car WIDTH 160 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         END.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contract_3Vdas wWin 
PROCEDURE Contract_3Vdas :
/*------------------------------------------------------------------------------
  Purpose: Informe Contractuales activos con 3 o mas cuotas sin abonar.
  Feb.25/05 GAER.
------------------------------------------------------------------------------*/
  DEFI VAR W_CuoTrans AS INTEG FORM "9999" INIT 0.
  DEFI VAR W_CuoPagas AS INTEG FORM "9999" INIT 0.

  SESSION:SET-WAIT-STATE("GENERAL").

  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND
           Ahorros.Agencia      LE AgeFin AND
           Ahorros.Tip_Ahorro   EQ 2      AND
           Ahorros.Sdo_Dispon   GT 0      AND
           Ahorros.Fec_Vencimie GT W_Fecha NO-LOCK
            BREAK BY Ahorros.Agencia BY Ahorros.Nit:
      IF (W_Fecha - Ahorros.Fec_Apertura) GT 90 THEN DO:
         ASSIGN W_CuoTrans = TRUNCATE((W_Fecha + 1 - Ahorros.Fec_Apertura) / 30,0)
                W_CuoPagas = TRUNCATE(Ahorros.Sdo_Dispon / Ahorros.Cuota,0).

         IF W_CuoTrans GT W_CuoPagas AND (W_CuoTrans - W_CuoPagas) GE 3 THEN DO:
            DISPLAY Ahorros.Agencia            FORMAT "999"                          
                    Ahorros.Nit                FORMAT "X(12)"   
                    Ahorros.Cod_Ahorro
                    Ahorros.Cue_Ahorros        FORMAT "X(14)"                             
                    Ahorros.Fec_Apertura                                                  
                    Ahorros.Fec_Vencimiento                                               
                    Ahorros.Cuota              FORMAT ">>>>,>>>,>>9"
                    Ahorros.Sdo_disponible     FORMAT "->>>>,>>>,>>9"                      
                    Ahorros.Plazo / 30         FORMAT "zzz9"                          
                    (W_CuoTrans - W_CuoPagas)  FORMAT "zzz9"                           
                    Ahorros.Int_Causado        FORMAT ">>>>,>>>,>>9"                       
                    Ahorros.Fec_Ulttransaccion 
                WITH DOWN WIDTH 180 FRAME IAho NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            CREATE IEx.
            ASSIGN Ct = Ct + 1
                   IEx.NLinea = Ct
                   IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + 
                                STRING(Ahorros.Cod_Ahorro) + Cma + STRING(Ahorros.Cue_Ahorros) + Cma +
                                STRING(Ahorros.Fec_Apertura) + Cma + STRING(Ahorros.Fec_Vencimiento) + Cma +
                                STRING(Ahorros.Cuota,">>>>>>>9.99") + Cma + STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma + 
                                STRING(Ahorros.Plazo / 30,"9999") + Cma + STRING((W_CuoTrans - W_CuoPagas),"99999") + Cma +
                                STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99") + Cma +
                                STRING(Ahorros.Fec_UltTransaccion).
            IF LAST-OF(Ahorros.Agencia) THEN
               DISPLAY SKIP(2).
         END.
      END.
  END.

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Empresa_Pdctos wWin 
PROCEDURE Empresa_Pdctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Listado = W_PathSpl + "EmpPro-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
  OS-DELETE VALUE(Listado).

  DEFI VAR Nro AS INTEG FORM "999999" EXTENT 2 INIT 0.
  DEFI VAR TVr LIKE Creditos.Monto    EXTENT 3 INIT 0.
  DEFI VAR TVd LIKE Creditos.Monto    EXTENT 3 INIT 0.
  DEFI VAR W_Rel AS CHAR FORM "X(2)".
  DEFI VAR W_Adm AS CHAR FORM "X(2)".
  DEFI VAR Usu1  LIKE Mov_instancias.Usuario EXTENT 3.
  DEFI VAR NomUsu AS CHAR FORM "X(19)".
  
  OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.

  {Incluido\RepEncabezado.I}

   FOR EACH TPdtos: DELETE TPdtos. END.

   FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ W_CodEmp NO-LOCK NO-ERROR.
   IF NOT AVAIL(EMpresas) THEN DO:
      MESSAGE "Empresa Solicitada para el Informe, Inexistente."
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   W_Reporte   = "REPORTE   :              PRODUCTOS DE ASOCIADOS DE EMPRESA "
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
   W_EncColumna = "                  Empresa : " +
                   STRING(W_CodEmp,"99999")      + " - " +
                   STRING(Empresas.Alias_Empresa,"X(40)").

   VIEW FRAME F-Encabezado.
   VIEW FRAME F-Ftr.
   
 /* DISPLAY "                  Empresa : " +
          STRING(W_CodEmp,"99999")       + " - " +
          STRING(Empresas.Alias_Empresa,"X(40)") FORM "X(145)" WITH WIDTH 150 NO-BOX NO-LABELS.*/

  IF Rs_Pto EQ 1 THEN      
     RUN Ahorro.              /*<---------Procedures al final de este mismo Procedure*/
  ELSE IF Rs_Pto EQ 2 THEN 
     RUN Credito.
  ELSE DO:
     RUN Ahorro.              
     RUN Credito.             /*<---------Procedures al final de este mismo Procedure*/
  END.

  FOR EACH TPdtos BY TPdtos.Nit:
      ASSIGN Nro[2] = Nro[2] + 1
             TVr[2] = TVr[2] + TPdtos.Sdo_Capit
             TVr[3] = TVr[3] + TPdtos.Cuota
             TVd[2] = TVd[2] + TPdtos.CapVdo.
             
      DISPLAY TPdtos.Agen         LABEL "Ag."
              TPdtos.Nit          LABEL "Ced./Nit"
              TPdtos.Nom          FORM "X(30)"               LABEL "Nombre del Asociado"
              TPdtos.Tip                                     LABEL "T"
              TPdtos.Cod_Cred                                LABEL "Pdcto"
              TPdtos.Pagare       FORM "X(12)"               LABEL "Cta-Producto"
              TPdtos.Monto        FORM "->>>>,>>>,>>9.99"    LABEL "Monto Inicial"
              TPdtos.Sdo_Capit    FORM "->>>>,>>>,>>9.99"    LABEL "Sdo-Actual Capital"
              TPdtos.CapVdo       FORM "->>>>,>>>,>>9.99"    LABEL "S.Capital Vencido"
              TPdtos.Plazo                                   LABEL "Plazo"
              TPdtos.Per_Pago     FORM ">>9"                 LABEL "PP"
              TPdtos.Cuota        FORM "->>>>,>>>,>>9.99"    LABEL "Valor de Cuota"
              TPdtos.Fec_Desemb   LABEL "Fec-Apertura"
              TPdtos.Fec_UltPago  LABEL "Fec-UltPago"                           
          WITH DOWN WIDTH 220 FRAME Fc1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.      
  END.

  DISPLAY SKIP(1)
          "---------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
          "To.Número :" Nro[2]     FORM "999999"                                                       
          "            Valor Saldos :" TVr[2]    FORM "->>>>>>,>>>,>>9.99"
          "                      Capital Vencido:" TVd[2]      FORM "->>>>>>,>>>,>>9.99"
          "                Vlr.Cuotas :" TVr[3]        FORM "->>>>>>,>>>,>>9.99"
       WITH DOWN WIDTH 250 FRAME Fc13 NO-BOX NO-LABELS STREAM-IO USE-TEXT.     

  ASSIGN Nro[2] = 0                                                                                  
         TVr[2] = 0
         TVr[3] = 0
         TVd[2] = 0.  
END PROCEDURE.

PROCEDURE Ahorro:
    FOR EACH Clientes WHERE Clientes.Cod_Empresa EQ W_CodEmp NO-LOCK:
        FOR EACH Ahorros WHERE Ahorros.Nit          EQ Clientes.Nit AND
                               Ahorros.FOR_pago     EQ 2  AND
                               Ahorros.Estado       EQ 1  AND
                               Ahorros.Tip_Ahorro   GE TpdIni AND 
                               Ahorros.Tip_Ahorro   LE TpdFin AND 
                               Ahorros.Cod_Ahorro   GE ProIni AND 
                               Ahorros.Cod_Ahorro   LE ProFin  NO-LOCK:  
            CREATE TPdtos.
            ASSIGN TPdtos.Nom       = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   TPdtos.Agenci    = Ahorros.Agencia
                   TPdtos.Nit       = Ahorros.Nit
                   TPdtos.Cod_Cred  = Ahorros.Cod_Ahorro
                   TPdtos.Tip       = "A"
                   TPdtos.Cuota     = Ahorros.Cuota
                   TPdtos.Pagare    = Ahorros.Cue_ahorros
                   TPdtos.Monto     = Ahorros.Monto_Apertura
                   TPdtos.Sdo_Capit = Ahorros.Sdo_Dispon
                   TPdtos.Plazo     = Ahorros.Plazo
                   TPdtos.Per_Pago  = Ahorros.Per_Deduc
                   TPdtos.Fec_Desemb  = Ahorros.Fec_Apert
                   TPdtos.Fec_UltPago = Ahorros.Fec_UltTran.
        END.
   END.

END PROCE.

PROCEDURE Credito:
    FOR EACH Clientes WHERE Clientes.Cod_Empresa EQ W_CodEmp NO-LOCK:
        FOR EACH creditos WHERE creditos.Nit         EQ Clientes.Nit AND
                               creditos.FOR_pago     EQ 2  AND
                               creditos.Estado       EQ 2  AND
                               creditos.Tip_credito   GE TpdIni AND 
                               creditos.Tip_credito   LE TpdFin AND 
                               creditos.Cod_credito   GE ProIni AND 
                               creditos.Cod_credito   LE ProFin  NO-LOCK:  
            CREATE TPdtos.
            ASSIGN TPdtos.Nom       = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   TPdtos.Agenci    = creditos.Agencia
                   TPdtos.Nit       = creditos.Nit
                   TPdtos.Cod_Cred  = creditos.Cod_credito
                   TPdtos.Tip       = "C"
                   TPdtos.Cuota     = creditos.Cuota
                   TPdtos.Pagare    = creditos.Pagare
                   TPdtos.Monto     = creditos.Monto
                   TPdtos.CapVdo    = Creditos.Val_Atraso
                   TPdtos.Sdo_Capit = creditos.Sdo_Capital
                   TPdtos.Plazo     = creditos.Plazo
                   TPdtos.Per_Pago  = creditos.Per_Pago
                   TPdtos.Fec_Desemb  = creditos.Fec_Desemb
                   TPdtos.Fec_UltPago = creditos.Fec_UltPago.
        END.
   END.

END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Cmb_Tipo Cmb_BUsuarios Cmb_BCreditos Cmb_BClientes Cmb_BEmpresas E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE Cmb_Tipo Cmb_BUsuarios Cmb_BCreditos Cmb_BClientes BUTTON-143 
         BUTTON-153 BUTTON-144 Btn_Imp E1 w-BUTTON-148 BUTTON-150 BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin Cmb_TProducto 
          Cmb_Productos ValIni TTotales TCupoRota ValFin W-DiaVdo RFPago Rs_Pto 
          W_CodEmp P_Nom 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 RECT-284 RECT-302 RECT-303 
         Cmb_Agencias BUTTON-120 BUTTON-121 TTotales TCupoRota W-DiaVdo RFPago 
         Rs_Pto BUTTON-145 W_CodEmp 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaSdo1203 wWin 
PROCEDURE HallaSdo1203 :
DEFINE INPUT  PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    FIND Cuentas WHERE Cuentas.Cuenta EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN DO:
        SFin = Sal_Cuenta.Sal_Inicial.
        DO i = 1 TO Smes BY 1:
            IF Cuentas.Naturaleza EQ "DB" THEN
               ASSIGN SFin  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i]
                      SIni  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
            ELSE
               ASSIGN SFin  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i]
                      SIni  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
DEFINE VAR Procname AS CHARACTER.
DEFINE VAR W_Tecla AS LOGICAL INITIAL TRUE.
procname =  w_Pathspl + "\" + NomInforme + ".csv".
IF NomInforme EQ "NA" THEN DO:
       MESSAGE "Este tipo de informe No Aplica para EXCEL" VIEW-AS ALERT-BOX INFORMATION.
    END.
    ELSE DO:
        /* Seleccionar ruta a donde escribir */
           SYSTEM-DIALOG GET-FILE Procname
           TITLE "Digite el Nombre del archivo"
           FILTERS "Todos los Archivos (*.*)" W_PathSpl + "\*.txt"
           INITIAL-DIR W_PathSpl
           RETURN-TO-START-DIR
           UPDATE W_Tecla.
        /* Termina Seleccion de ruta */

        LisEx = Procname. /* w_Pathspl + "\" + NomInforme + ".csv". */
        OUTPUT TO VALUE(LisEx).
        FOR EACH IEx BY IEx.NLinea:
            PUT IEx.Linea SKIP.
        END.
        OUTPUT CLOSE.
        /* MESSAGE "el informe generado se encuentra en:" SKIP
                LisEx VIEW-AS ALERT-BOX INFORMATION. */
        FOR EACH IEx: DELETE IEx. END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAhorros_Aportes wWin 
PROCEDURE InfAhorros_Aportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/**Crea reporte de Aportes**/
/**Adicionado por Alan Gordon**/
/**05 de octubre de 2007**/

DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR nom_est   AS CHARACTER.
DEFINE VAR nom_pago  AS CHARACTER.
DEFINE VAR nom_per   AS CHARACTER.
DEFINE VAR fec_ult   AS CHARACTER INITIAL "".

    IF Ahorros.Sdo_Disponible GE ValIni AND
       Ahorros.Sdo_Disponible LE ValFin THEN DO:

        IF ahorros.estado = 1 THEN
            ASSIGN nom_est = "Activo".
        ELSE
            ASSIGN nom_est = "Inactivo".

        IF ahorros.FOR_pago = 1 THEN
            ASSIGN nom_pago = "Caja".
        IF ahorros.FOR_pago = 2 THEN
            ASSIGN nom_pago = "Nomina".
        IF ahorros.FOR_pago = 3 THEN
            ASSIGN nom_pago = "Debito Aut.".

        IF ahorros.per_deduccion = 1 THEN
            ASSIGN nom_per = "Semanal".
        IF ahorros.per_deduccion = 2 THEN
            ASSIGN nom_per = "Decanal".
        IF ahorros.per_deduccion = 3 THEN
            ASSIGN nom_per = "Quincenal".
        IF ahorros.per_deduccion = 4 THEN
            ASSIGN nom_per = "Mensual".

        IF ahorros.Fec_Ulttransaccion NE ? THEN
            ASSIGN fec_ult = STRING(ahorros.Fec_Ulttransaccion,"99/99/9999").

        PUT Ahorros.Agencia FORMAT "999" " "
            Ahorros.Nit FORMAT "X(14)" " "
            Ahorros.Cue_Ahorros FORMAT "X(14)" " "
            Ahorros.Fec_Apertura FORMAT "99/99/9999" " "
            Ahorros.Cuota FORMAT ">>,>>>,>>9" " "
            nom_est FORMAT "X(8)" " "
            nom_pago FORMAT "X(10)" " "
            nom_per FORMAT "X(10)" " "
            Ahorros.Sdo_disponible FORMAT ">>,>>>,>>9" "     "
            fec_ult FORMAT "X(10)" SKIP(0).
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma +
                            STRING(Ahorros.Cue_Ahorros) + Cma + 
                            STRING(Ahorros.Fec_Apertura) + Cma + 
                            STRING(Ahorros.Cuota) + Cma + nom_est + Cma + nom_pago + Cma + nom_per + Cma + 
                            STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje) + Cma + 
                            STRING(fec_ult,"X(10)").
        ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
               TotReg    = TotReg + 1.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAhorros_ContraAportes wWin 
PROCEDURE InfAhorros_ContraAportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCje    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCjeAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotIntAge AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCauAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotInt    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCau    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR w_pl  AS CHARACTER FORMAT "X(14)" INITIAL "".
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(150)".
DEFINE VAR Reporte    AS CHARACTER FORMAT "X(150)".
DEFINE VAR w_estado   AS CHARACTER FORMAT "X(9)" INITIAL "".

IF Ahorros.Sdo_Disponible GE ValIni AND
   Ahorros.Sdo_Disponible LE ValFin THEN DO:
   IF Ahorros.estado EQ 1 THEN
      ASSIGN w_estado = "Activo".
   ELSE
      ASSIGN w_estado = "Cancelado".
   DISPLAY Ahorros.Agencia            AT 1   FORMAT "999"   Ahorros.Nit                AT 5   FORMAT "X(14)"                        
           Ahorros.Cue_Ahorros        AT 22  FORMAT "X(14)" Ahorros.cuota * ROUND(Ahorros.Plazo / 30,0) AT 38 FORMAT ">>,>>>,>>9"
           Ahorros.Cuota              AT 50  FORMAT ">>,>>>,>>9" Ahorros.Fec_Apertura       AT 62 
           Ahorros.Fec_Vencimiento    AT 74                      Ahorros.Sdo_disponible     AT 86  FORMAT ">>,>>>,>>9"
           Ahorros.Plazo              AT 98  FORMAT ">>,>>9"     Ahorros.Tasa               AT 106 FORMAT ">9.99"
           Ahorros.Int_Causado        AT 113 FORMAT ">,>>>,>>9"  Ahorros.Fec_Ulttransaccion AT 124 FORMAT "99/99/9999"
           w_estado                   AT 136 FORMAT "X(9)"
   WITH FRAME F8 WIDTH 150 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
   CREATE IEx.
   ASSIGN Ct = Ct + 1
          IEx.NLinea = Ct
          IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + STRING(Ahorros.Cue_Ahorros) + Cma + 
                       STRING((Ahorros.Cuota * ROUND(Ahorros.Plazo / 30,0)),"->>>>>>>>>>>9.99") + Cma + 
                       STRING(Ahorros.Cuota,"->>>>>>>>>>>9.99") + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                       STRING(Ahorros.Fec_Vencimiento) + Cma + STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma +
                       STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Tasa) + Cma + STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99") + Cma + 
                       STRING(Ahorros.Fec_UltTransaccion) + Cma + w_estado.
   ASSIGN TotCauAge = TotCauAge + Ahorros.Int_causado
          TotSdo    = TotSdo + Ahorros.Sdo_Disponible
          TotCau    = TotCau + Ahorros.Int_Causado.
   ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
          TotRegAge = TotReg    + 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCre_Clientes wWin 
PROCEDURE InfCre_Clientes :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Ahorros wWin 
PROCEDURE Informes_Ahorros :
Listado = W_PathSpl + "InfAho-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin W-DiaVdo.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT           CODPDT CUENTA            TASA   PLAZO ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99".
DEFINE VAR TotCje    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99".
DEFINE VAR TotCjeAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotIntAge AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCauAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotInt    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCau    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR w_pl  AS CHARACTER FORMAT "X(14)" INITIAL "".
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(150)".
DEFINE VAR Reporte    AS CHARACTER FORMAT "X(150)".
ASSIGN J = 0 k = 0 Ct = 0 Reporte = "" Encabezado = "".
RUN Aho_Encabezados(OUTPUT Encabezado, OUTPUT Reporte).
{Incluido\RepEncabezado.i}
  W_EncColumna = Encabezado.
  W_Reporte   = "REPORTE   : Ahorros: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") + " - Filtro: " + NomInf.
  IF Reporte NE "" THEN
     W_Reporte = Reporte.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.    
IF Cmb_Bahorros:SCREEN-VALUE IN FRAME F_Basicos EQ "Tarjetas Debito" THEN DO: RUN Tarjetas_Debito. RETURN. END.
  IF Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos EQ "Contractuales 3 Vdas" THEN DO: RUN Contract_3Vdas.     RETURN. END.
  IF Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos EQ "Vctos-Liquidac.Proximas A Termino" THEN DO: RUN Ah_VctosLiquidac.   RETURN. END.
  IF Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos EQ "Fechas de Vencimiento" THEN DO: RUN Ah_FecVencimiento.  RETURN. END.
  FOR EACH Ahorros WHERE Ahorros.Agencia  GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Tip_Ahorro   GE TpdIni AND Ahorros.Tip_Ahorro   LE TpdFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND Ahorros.Cod_Ahorro   LE ProFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit BY Ahorros.Cod_Ahorro:  
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN NomAho = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
      END.
      IF FIRST-OF(Ahorros.Agencia) THEN TotRegAge = 0.
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BAhorros:
        WHEN "Cuentas de Empresas Asociadas" THEN DO:
           IF AVAILABLE Clientes AND Clientes.Tipo_Vinculo EQ 1 AND Clientes.Tipo_Cliente GE 3 THEN DO:
               DISPLAY Ahorros.Agencia Ahorros.Nit NomAho FORMAT "X(25)"
                       Clientes.Tel_Comercial Clientes.Dir_Comercial FORMAT "X(25)" 
                       Ahorros.Cue_Ahorros Ahorros.Sdo_Disponible + Ahorros.Sdo_canje FORMAT ">,>>>,>>>,>>9"
               WITH FRAME Fea WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
               CREATE IEx.
               ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                      IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + NomAho + Cma +
                                   Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                                   STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                                   STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje,">>>>>>>>>>>9").
               ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                      TotRegAge = TotReg    + 1.
           END.
        END.
        WHEN "Básico" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND Ahorros.Sdo_Disponible LE ValFin AND 
              Ahorros.Sdo_Disponible  GT 0     AND Ahorros.estado EQ 1 THEN DO:
              DISPLAY Ahorros.Agencia Ahorros.Nit NomAho Ahorros.Cue_Ahorros Ahorros.Tasa FORMAT ">9.99"       
                      Ahorros.Plazo Ahorros.Fec_Apertura Ahorros.Sdo_Disponible + Ahorros.Sdo_canje FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              CREATE IEx.
              ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                     IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + NomAho + Cma +
                                  Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                                  STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                                  STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje,">>>>>>>>>>>9").
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotReg = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Apertura" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND Ahorros.Sdo_Disponible LE ValFin AND 
              Ahorros.Fec_Apertura   GE FecIni AND Ahorros.Fec_Apertura   LE FecFin AND 
              Ahorros.Sdo_Disponible GT 0      AND Ahorros.Usu_Creacion GE UsuIni AND 
              Ahorros.Usu_Creacion   LE UsuFin THEN DO:
              DISPLAY Ahorros.Agencia        FORMAT "999"      Ahorros.Nit            FORMAT "X(14)"
                      NomAho                                   Ahorros.Cue_Ahorros    FORMAT "X(14)"
                      Ahorros.Tasa           FORMAT ">9.999"   Ahorros.Plazo          FORMAT "zzzz"
                      Ahorros.Fec_Apertura   FORMAT "99/99/99" Ahorros.Sdo_Disponible + Ahorros.Sdo_canje FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Usu_Creacion
              WITH FRAME F2 WIDTH 150 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              CREATE IEx.
              ASSIGN Ct = Ct + 1 
                     IEx.NLinea = Ct
                     IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + NomAho + Cma +
                                  Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                                  STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                                  STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje,">>>>>>>>>>>9").
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg    + 1.
           END.
        END.
        WHEN "Fechas de Cancelacion" THEN DO:
           IF  Ahorros.Sdo_Disponible  EQ 0 AND Ahorros.Sdo_Canje       EQ 0
           AND Ahorros.Fec_Cancelacion NE ? AND Ahorros.Fec_Cancelacion GE FecIni 
           AND Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia          FORMAT "999" Ahorros.Nit              FORMAT "X(14)" NomAho Ahorros.Cue_Ahorros      FORMAT "X(14)"
                      Ahorros.Tasa             FORMAT ">9.999" Ahorros.Plazo            FORMAT "zzzz" Ahorros.Fec_Apertura     FORMAT "99/99/99"
                      Ahorros.Fec_Cancelacion  FORMAT "99/99/99" Ahorros.Sdo_Disponible   FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              CREATE IEx.
              ASSIGN Ct = Ct + 1
                     IEx.NLinea = Ct
                     IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + NomAho + Cma +
                                  Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                                  STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                                  STRING(Ahorros.Fec_Cancelacion) + Cma + 
                                  STRING(Ahorros.Sdo_Disponible + Ahorros.Sdo_canje,">>>>>>>>>>>9").
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg    + 1.
           END.
        END.
        WHEN "Canje" THEN DO:
           IF Ahorros.Sdo_Canje  GE ValIni AND Ahorros.Sdo_Canje  LE ValFin AND 
              Ahorros.Sdo_Canje GT 0 THEN DO:
              DISPLAY Ahorros.Agencia         FORMAT "999" Ahorros.Nit             FORMAT "X(14)"
                      Nomaho                  Ahorros.Cue_Ahorros     FORMAT "X(14)"
                      Ahorros.Tasa            FORMAT ">9.999" Ahorros.Plazo           FORMAT "zzzz"
                      Ahorros.Fec_Apertura    FORMAT "99/99/99" Ahorros.Sdo_Disponible  FORMAT ">>>,>>>,>>9"
                      Ahorros.Sdo_Canje       FORMAT ">>>,>>>,>>9"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              CREATE IEx.
              ASSIGN Ct = Ct + 1
                     IEx.NLinea = Ct
                     IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + NomAho + Cma +
                                  Ahorros.Cue_Ahorros + Cma + STRING(Ahorros.Tasa) + Cma +
                                  STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Fec_Apertura) + Cma +
                                  STRING(Ahorros.Sdo_Disponible,">>>>>>>>>>>9") + Cma + STRING(Ahorros.Sdo_canje,">>>>>>>>>>>9").
              ASSIGN TotCjeAge = TotCjeAge + Ahorros.Sdo_canje
                     TotCje    = TotCje + Ahorros.Sdo_canje.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg    + 1.
           END.
        END.
        WHEN "Depositos" THEN DO:
              IF Ahorros.Sdo_Disponible GE ValIni AND Ahorros.Sdo_Disponible LE ValFin THEN DO:
                 DISPLAY Ahorros.Agencia         AT 1  FORMAT "999" Ahorros.Nit             AT 5  FORMAT "X(14)"
                         Ahorros.Cod_Ahorro      AT 19 FORMAT "999" Ahorros.Cue_Ahorros     AT 23 FORMAT "X(14)"
                         Ahorros.Sdo_Disponible  AT 39 FORMAT ">,>>>,>>>,>>9" Ahorros.Sdo_Canje       AT 54 FORMAT ">,>>>,>>>,>>9"
                         Ahorros.Sdo_minimo      AT 69 FORMAT ">,>>>,>>>,>>9" Ahorros.Int_Causado     AT 84 FORMAT ">,>>>,>>>,>>9"
                 WITH FRAME F6 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                 CREATE IEx.                                                
                 ASSIGN Ct = Ct + 1
                        IEx.NLinea = Ct
                        IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + 
                                     STRING(Ahorros.Cod_Ahorro) + Cma + STRING(Ahorros.Cue_Ahorros) + Cma + STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma + 
                                     STRING(Ahorros.Sdo_Canje,"->>>>>>>>>>>9.99") + Cma + STRING(Ahorros.Sdo_Minimo,"->>>>>>>>>>>9.99") + Cma +
                                     STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99").
                 ASSIGN TotCjeAge = TotCjeAge + Ahorros.Sdo_canje
                        TotCauAge = TotCauAge + Ahorros.Int_Causad
                        TotCje    = TotCje + Ahorros.Sdo_canje
                        TotCau    = TotCau + Ahorros.Int_Causado.
                 ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                        TotRegAge = TotReg    + 1.
              END.
          END.
          /**++**/
          WHEN "Clasificacion CDATS" THEN DO:
                  ASSIGN W_NomCtas[1] = "211005" W_NomCtas[2] = "211010" 
                         W_NomCtas[3] = "211015" W_NomCtas[4] = "211020" 
                         W_Saldo = (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).
                  IF Ahorros.Plazo LT 180 THEN ASSIGN W_SdoAge[1] = W_SdoAge[1] + W_Saldo.
                  ELSE IF Ahorros.Plazo LE 360 THEN ASSIGN W_SdoAge[2] = W_SdoAge[2] + W_Saldo.
                  ELSE IF Ahorros.plazo LT 540 THEN ASSIGN W_SdoAge[3] = W_SdoAge[3] + W_Saldo.
                  ELSE ASSIGN W_SdoAge[4] = W_SdoAge[4] + W_Saldo.
                  IF LAST-OF (Ahorros.Agencia) THEN DO:
                      DO i = 1 TO 4:
                          FIND Sal_Cuenta WHERE Sal_Cuenta.Cuenta = W_NomCtas[i] AND Sal_Cuenta.agencia = Ahorros.Agencia NO-LOCK NO-ERROR.
                          IF AVAILABLE (Sal_Cuenta) THEN DO:
                              ASSIGN W_SdoLib[i] = Sal_Cuenta.sal_inicial.
                              DO k=1 TO 12:
                                  ASSIGN W_SdoLib[i] = W_SdoLib[i] - Sal_Cuenta.Db[k] + Sal_Cuenta.Cr[k].
                              END.
                          END.
                          PUT Ahorros.Agencia " " W_NomCtas[i] " " W_SdoAge[i] " " W_SdoLib[i] SKIP.
                          CREATE IEx.
                          ASSIGN Ct = Ct + 1
                                 IEx.NLinea = Ct
                                 IEx.Linea = STRING(Ahorros.Agencia) + Cma + STRING(W_NomCtas[i]) + Cma + STRING(W_SdoAge[i]) + Cma + STRING(W_SdoLib[i]).
                          ASSIGN W_SdoAge[i] = 0 W_SdoLib[i] = 0.
                      END.
                  END.
              END. /*++*/
          WHEN "TAC Vencidos"  THEN DO:
          IF (W_Fecha - Ahorros.Fec_Ulttransaccion) > W-DiaVdo  THEN DO:
              /* Ahorros.Fec_Ulttransaccion Equivalente a ultimo pago? */
              IF Ahorros.Sdo_Disponible GE ValIni AND Ahorros.Sdo_Disponible LE ValFin THEN DO:
                 DISPLAY Ahorros.Agencia                      AT 1  FORMAT "999" Ahorros.Nit                          AT 5  FORMAT "X(14)"
                         Ahorros.Cue_Ahorros                  AT 20 FORMAT "X(14)"  Ahorros.Sdo_Disponible               AT 35 FORMAT ">,>>>,>>>,>>9"
                         Ahorros.Sal_Intpagados               AT 50 FORMAT ">,>>>,>>>,>>9" Ahorros.Int_Causado                  AT 65 FORMAT ">,>>>,>>>,>>9"
                         W_Fecha - Ahorros.Fec_Ulttransaccion AT 80 FORMAT ">>>,>>9"
                 WITH FRAME F7 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                 CREATE IEx.
                 ASSIGN Ct = Ct + 1
                        IEx.NLinea = Ct
                        IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + STRING(Ahorros.Cue_Ahorros) + Cma + 
                                     STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma + 
                                     STRING(Ahorros.Sal_IntPagados,"->>>>>>>>>>>9.99") + Cma +
                                     STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99") + Cma +
                                     STRING(W_Fecha - Ahorros.Fec_Ulttransaccion,">>>>9").
                 ASSIGN TotIntAge = TotIntAge + Ahorros.Sal_Intpagados
                        TotCauAge = TotCauAge + Ahorros.Int_Causad
                        TotInt    = TotInt + Ahorros.Sal_Intpagados
                        TotCau    = TotCau + Ahorros.Int_Causado.
                 ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                        TotRegAge = TotReg    + 1.
              END.
           END.
          END.
          WHEN "Contractual" /*OR WHEN "Aportes"*/ THEN 
              RUN InfAhorros_ContraAportes.
          /**Crea reporte de Aportes**/
          /**Alan Gordon 05 de octubre de 2007**/
          WHEN "Aportes" THEN
              RUN InfAhorros_Aportes.
          /**fin de la Adicion**/
          WHEN "A Termino" THEN DO:
              IF Ahorros.Sdo_Disponible GE ValIni AND Ahorros.Sdo_Disponible LE ValFin THEN DO:
                  CASE Ahorros.Per_Liquidacion:
                      WHEN 1 THEN w_pl = 'Diario        '.
                      WHEN 2 THEN w_pl = 'Mensual       '.
                      WHEN 3 THEN w_pl = 'Trimestral    '.
                      WHEN 4 THEN w_pl = 'Semestral     '.
                      WHEN 5 THEN w_pl = 'Anual         '.
                      WHEN 6 THEN w_pl = 'Al Vencimiento'.
                  END CASE.
                 PUT Ahorros.Agencia FORMAT "999" " " Ahorros.Nit FORMAT "X(14)" " " Ahorros.Cue_Ahorros FORMAT "X(14)" " "
                     Ahorros.Sdo_disponible FORMAT ">,>>>,>>>,>>9" " " Ahorros.Fec_Apertura " "
                     Ahorros.Fec_Vencimiento " " Ahorros.Plazo FORMAT ">>,>>9" " " Ahorros.Tasa FORMAT ">>9.9999" " "
                     Ahorros.Int_Causado FORMAT ">,>>>,>>>,>>9" " " Ahorros.Int_Pagar FORMAT "->,>>>,>>>,>>9" " "
                     Ahorros.Fec_UltLiquidacion " " Ahorros.Fec_ProLiquidacion " " W_pl SKIP(0).
                 
                 CREATE IEx.
                 ASSIGN Ct = Ct + 1
                        IEx.NLinea = Ct
                        IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + STRING(Ahorros.Cue_Ahorros) + Cma + 
                                     STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma +
                                     STRING(Ahorros.Fec_Apertura) + Cma + STRING(Ahorros.Fec_Vencimiento) + Cma +
                                     STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Tasa) + Cma +
                                     STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99") + Cma + 
                                     STRING(Ahorros.INT_Pagar,"->>>>>>>>>>>9.99") + Cma +
                                     STRING(Ahorros.Fec_UltLiquidacion) + Cma +
                                     STRING(Ahorros.Fec_ProLiquidacion) + Cma +
                                     STRING(W_pl).
                 ASSIGN TotIntAge = TotIntAge + Ahorros.Int_Pagar
                        TotCauAge = TotCauAge + Ahorros.Int_Causado
                        TotInt    = TotInt + Ahorros.Int_Pagar
                        TotCau    = TotCau + Ahorros.Int_Causado.
                 ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                        TotRegAge = TotReg    + 1.
              END.
          END.
      END CASE.
      ASSIGN TotSdo    = TotSdo    + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje.
             /*TotReg    = TotReg    + 1.*/
      IF LAST-OF(Ahorros.Agencia) THEN DO:
        ASSIGN TotRegAge = TotReg.
        IF Cmb_BAhorros EQ "Depositos" OR Cmb_BAhorros EQ "TAC Vencidos" OR  Cmb_BAhorros EQ "A Termino" OR   Cmb_BAhorros EQ "Contractual" THEN  
         DISPLAY SKIP(1)
                "Tot Reg Ag: "            AT 1
                 Ahorros.Agencia          AT 13 FORMAT "999"
                ": "                      AT 16
                 TotRegAge                AT 18 FORMAT ">>>,>>9"
                 "  Saldo Disponible Inteses Pagados  Intereses Causados" AT 27 SKIP(1)
                 TotSdoAge                AT 27 FORMAT ">>>,>>>,>>>,>>9"
                 TotInt                   AT 44 FORMAT "->>>,>>>,>>>,>>9"
                 TotCau                   AT 62 FORMAT ">>>,>>>,>>>,>>9"
          WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
       ELSE
           IF TotSdoAge GT 0 THEN
              DISPLAY SKIP(1) "Tot Reg Ag: " Ahorros.Agencia FORMAT "999"
                     ":" TotRegAge FORMAT ">>>,>>9" 
                     "  Saldo Disponible " TotSdoAge  FORMAT ">>>,>>>,>>>,>>9" SKIP(1)
              WITH FRAME F10 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
           ASSIGN TotSdoAge = 0 TotRegAge = 0.
      END.
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1 TotReg                          AT 40
    "Total Disponible   : "          AT 60 TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Clientes wWin 
PROCEDURE Informes_Clientes :
Listado = W_PathSpl + "Clientes-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)"  INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(540)" INITIAL "Cod.Age;Agencia;Fec.Ult_Actualiza;Tipo_Cliente;Tipo_Vinculo;Nit;Nombres_Clientes;Tipo_Ident.;Fec_Ingreso;Estado;Fec_Naci.;Sexo;Est_Civil;Perso.Acargo;Num_Hijos;Niv_Educativo;Profesion;Dir_Residencia;Estrato;Ciudad_Residencia.;Tel_Residencia;Celular;Email;Tipo_Vivienda;Nom_Arrendatario;Tel_Arrendatario;Tipo_Actividad;Empresa;Tiempo_Laboral;Cargo;Fec_IngresoEmpresa;Tipo_Contrato;Dir_Comercial/Empresa;Ciudad_Empresa;Tel_Comercial/Empresa;Salario;Total_Ingresos;Sdo_Obligaciones;Gasto_Familiar;Gasto_Obligacion;Gasto_Arriendo;Total_Egresos". 
DEFINE VAR WEnc3 AS CHARACTER FORMAT "X(220)" INITIAL "Cod.Age;Agencia;Nit;Nombres;Empresa;Tipo_Contrato;Estado;Fec_Ingreso;Salario;Aportes;Dir_Comercial;Tel_Comercial".
DEFINE VAR TotReg AS DECIMAL INITIAL 0.
DEFINE VAR Motivo AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotMot AS DECIMAL.
DEFINE VAR W_nombres AS CHARACTER.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Clientes: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

  CASE Cmb_BClientes:
    WHEN "Básico" OR WHEN "Empresas Asociadas" THEN  
        W_EncColumna = WEnc1 + "TEL.RESIDENCIA   TEL.COMERCIAL      EMAIL".
    WHEN "Actualización de Información" THEN                                        
        W_EncColumna = "AGE NIT            NOMBRE                                   FEC.ULTACT TEL.RESID  TEL.COMER  E-MAIL".
    WHEN "Cumpleaños" THEN
        W_EncColumna = WEnc1 + "FEC.NACIMI. EDAD TEL.REDIDENCIA   TEL.COMERCIAL      EMAIL".
    WHEN "Afiliaciones" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Retiros" THEN
        W_EncColumna = WEnc1 + "FEC.RETIRO       TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Proveedores" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL      EMAIL".
    WHEN "Asociados Informe General" THEN DO:
        W_EncColumna = WEnc2.
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea = "AG.;FEC.ULT.ACTUALIZA;CLIENTE;NIT;NOMBRE;ESTADO;CUOTA APORTE;EMPRESA;CARGO;SALARIO;".
    END.
    WHEN "Solo Asociados" THEN DO:
        W_EncColumna = "AG. NIT            NOMBRES Y APELLIDOS                      SEXO   FEC.NACI.  EDAD FEC.INGRES TEL.RESID  DIRECCION RESIDENCIA                     EMPRESA            TEL.COMER  DIRECCION COMERICAL                      E-MAIL".
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea = "AG;NIT;NOMBRES Y APELLIDOS;SEXO;FEC.NACIMIENTO;EDAD;FEC.INGRESO;TEL.RESIDENCIA;DIR.RESIDENCIA;EMPRESA;TEL.COMERCIAL;DIR.COMERCIAL;E-MAIL".
    END.
    WHEN "Solo Clientes No Asociados" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL      EMAIL".
    WHEN "Profesiones" THEN
        W_EncColumna = "FecIngreso Nit           Nombre                                        Salario      Profesion            Cargo".
/*     WHEN "Habiles" THEN                                                                                                                          */
/*         W_EncColumna = "AGE NIT          NOMBRE                         TEL.RESIDE  TEL.COMERC.      SDO.APORTE       SDO.CREDITO  FEC.INGRESO". */
/*     WHEN "No Habiles" THEN                                                                                                             */
/*         W_EncColumna = "AGE NIT          NOMBRE                         TEL.RESIDE  TEL.COMERC.      SDO.APORTE       SDOVdo.CREDITO". */
    WHEN "Motivos de Vinculación" THEN
        W_EncColumna = "AG. MOT NIT            NOMBRES Y APELLIDOS                      FEC.AFILIA TEL.REDIDENCIA TEL.COMERCIAL ".
    WHEN "Actividades" THEN
         W_EncColumna = "AG. Ciiu                 NIT            NOMBRES Y APELLIDOS                      FEC.ULT.AC TEL.REDIDENCIA TEL.COMERCIAL ".
    WHEN "Asociados Sin Productos" THEN
         W_EncColumna = WEnc3.
    WHEN "Inconsistencias en Ubicación" THEN
         W_EncColumna = "AGE NIT            NOMBRE                                   UBICACION  TEL.RESID  TEL.COMER  E-MAIL".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.

IF Cmb_BClientes:SCREEN-VALUE NE "Motivos de Vinculación" AND 
   Cmb_BClientes:SCREEN-VALUE NE "Actividades" AND 
   Cmb_BClientes:SCREEN-VALUE NE "Afiliaciones" AND
   Cmb_BClientes:SCREEN-VALUE NE "Actualización de Información" AND 
   Cmb_BClientes:SCREEN-VALUE NE "Habiles" AND 
   Cmb_BClientes:SCREEN-VALUE NE "No Habiles" AND 
   Cmb_BClientes:SCREEN-VALUE NE "Inconsistencias en Ubicación" THEN DO:
  FOR EACH Clientes WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Nit:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BClientes:
         WHEN "Empresas Asociadas" THEN DO:
            IF Clientes.Tipo_Vinculo EQ 1 AND Clientes.Tipo_Cliente GE 3 THEN DO:
                DISPLAY Clientes.Agencia         AT 1  FORMAT "999"
                        Clientes.Nit             AT 5  FORMAT "X(14)"
                        Clientes.Nombre          AT 20 FORMAT "X(20)"
                        Clientes.Apellido1       AT 42 FORMAT "X(15)"
                        Clientes.Apellido2       AT 59 FORMAT "X(15)"
                        Clientes.Tel_Residencia  AT 76 FORMAT "X(15)"
                        Clientes.Tel_Comercial   AT 93 FORMAT "X(15)"
                WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                TotReg = TotReg + 1.
            END.
        END.
        WHEN "Básico" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 THEN DO:
              DISPLAY Clientes.Agencia         AT 1   FORMAT "999"
                      Clientes.Nit             AT 5   FORMAT "X(14)"
                      Clientes.Nombre          AT 20  FORMAT "X(20)"
                      Clientes.Apellido1       AT 42  FORMAT "X(15)"
                      Clientes.Apellido2       AT 59  FORMAT "X(15)"
                      Clientes.Tel_Residencia  AT 76  FORMAT "X(15)"
                      Clientes.Tel_Comercial   AT 93  FORMAT "X(15)"
                      Clientes.Email           AT 110 FORMAT "X(50)"
              WITH FRAME F1 WIDTH 170 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Cumpleaños" THEN DO:
/*            IF Clientes.Tipo_Vinculo LT 3 AND              */
/*               Clientes.Fec_Nacimiento GE FecIni AND       */
/*               Clientes.Fec_Nacimiento LE FecFin THEN DO:  */
          IF Clientes.Tipo_Vinculo EQ 1                      AND 
             MONTH(Clientes.Fec_Nacimiento) GE MONTH(FecIni) AND
             MONTH(Clientes.Fec_Nacimiento) LE MONTH(FecFin) AND 
             DAY(Clientes.Fec_Nacimiento)   GE DAY(FecIni)   AND
             DAY(Clientes.Fec_Nacimiento)   LE DAY(FecFin)   THEN DO:
               IF DAY(fec_Nacimiento) GT DAY(TODAY) AND MONTH(Fec_Nacimiento) GE MONTH(TODAY) THEN
                  ASSIGN W_Edad = TRUNCATE((TODAY - Fec_Nacimiento) / 366,0).
               ELSE
                  ASSIGN W_Edad = TRUNCATE((TODAY - Fec_Nacimiento) / 365,0).
               DISPLAY Clientes.Agencia          AT 1   FORMAT "999"
                       Clientes.Nit              AT 5   FORMAT "X(14)"
                       Clientes.Nombre           AT 20  FORMAT "X(20)"
                       Clientes.Apellido1        AT 42  FORMAT "X(15)"
                       Clientes.Apellido2        AT 59  FORMAT "X(15)"
                       Clientes.Fec_Nacimiento   AT 76  FORMAT "99/99/9999"
                       W_Edad                    AT 88  FORMAT "ZZZ"
                       Clientes.Tel_Residencia   AT 93  FORMAT "X(15)"
                       Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                       Clientes.Email            AT 127 FORMAT "X(50)"
               WITH FRAME F3 WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
               TotReg = TotReg + 1.
           END.
        END.     
        WHEN "Afiliaciones" THEN DO:
           FIND FIRST ahorros WHERE  ahorros.nit = clientes.nit AND  ahorros.cod_ahorro EQ 5 
                 AND ahorros.agencia = clientes.agencia  AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
           IF AVAIL(ahorros) THEN DO:
                IF Clientes.Tipo_Vinculo EQ 1 AND 
                    Clientes.Fec_Ingreso GE FecIni AND
                    Clientes.Fec_Ingreso LE FecFin THEN DO:
                    DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                            Clientes.Nit              AT 5  FORMAT "X(14)"
                            Clientes.Nombre           AT 20 FORMAT "X(20)"
                            Clientes.Apellido1        AT 42 FORMAT "X(15)"
                            Clientes.Apellido2        AT 59 FORMAT "X(15)"
                            Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                            Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                            Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                    WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                    TotReg = TotReg + 1.
                END.
           END.
        END.
        WHEN "Retiros" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Retiro GE FecIni AND
              Clientes.Fec_Retiro LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Retiro       AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Proveedores" THEN DO:
           IF Clientes.Tipo_Vinculo GT 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1   FORMAT "999"
                      Clientes.Nit              AT 5   FORMAT "X(14)"
                      Clientes.Nombre           AT 20  FORMAT "X(20)"
                      Clientes.Apellido1        AT 42  FORMAT "X(15)"
                      Clientes.Apellido2        AT 59  FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76  FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93  FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                      Clientes.Email            AT 127 FORMAT "X(50)"
              WITH FRAME F6 WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Asociados Informe General" THEN DO:
/*           IF Clientes.Tipo_Vinculo EQ 1 THEN DO:  */
            /*IF AgeIni EQ 0 AND AgeFin EQ 999 THEN
                RUN Exp_Clientes_SoloAso.
            ELSE*/
                RUN Inf_Clientes_SoloAso.
         END.
        WHEN "Solo Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
               ASSIGN W_nombres = STRING(Clientes.Nombre) + " " +
                                  STRING(Clientes.Apellido1) + " " +
                                  STRING(Clientes.Apellido2).
               FIND FIRST empresas WHERE empresas.cod_empresa=clientes.cod_empresa NO-LOCK NO-ERROR.
               IF AVAILABLE(empresas)THEN
                  ASSIGN wnomemp = empresas.alias_empresa.
               ELSE
                  ASSIGN wnomemp = "".
               CASE clientes.sexo:
                    WHEN 1 THEN ASSIGN wsexo = "Hombre".
                    WHEN 2 THEN ASSIGN wsexo = "Mujer".
                    OTHERWISE 
                       ASSIGN wsexo = "No Asignado".
               END CASE.
               IF DAY(fec_Nacimiento) GT DAY(TODAY) AND MONTH(Fec_Nacimiento) GE MONTH(TODAY) THEN
                  ASSIGN W_Edad = TRUNCATE((TODAY - Fec_Nacimiento) / 366,0).
               ELSE
                  ASSIGN W_Edad = TRUNCATE((TODAY - Fec_Nacimiento) / 365,0).
               PUT Clientes.Agencia FORMAT "999" " "
                   Clientes.Nit FORMAT "X(14)" " "
                   W_nombres FORMAT "X(40)" " "
                   wsexo FORMAT "X(6)" " "
                   Clientes.Fec_Nacimiento FORMAT "99/99/9999" " "
                   W_Edad FORMAT "ZZZ" "  "
                   Clientes.Fec_Ingreso FORMAT "99/99/9999" " "
                   Clientes.Tel_Residencia FORMAT "X(10)" " "
                   Clientes.Dir_Residencia FORMAT "X(40)" " "
                   wnomemp FORMAT "X(18)" " "
                   Clientes.Tel_Comercial FORMAT "X(10)" " "
                   Clientes.Dir_comercial FORMAT "X(40)" " "
                   Clientes.Email FORMAT "X(50)" SKIP(0).
               CREATE IEx.
               ASSIGN Ct = Ct + 1
                      IEx.NLinea = Ct
                      IEx.Linea = STRING(Clientes.Agencia) + Cma + 
                                  STRING(Clientes.Nit) + Cma + W_nombres + Cma + wsexo + Cma + 
                                  STRING(Clientes.Fec_Nacimiento,"99/99/9999") + Cma + 
                                  STRING(W_Edad) + Cma + 
                                  STRING(Clientes.Fec_Ingreso,"99/99/9999") + Cma +
                                  STRING(Clientes.Tel_Residencia) + Cma + 
                                  STRING(Clientes.Dir_Residencia) + Cma + wnomemp + Cma + 
                                  STRING(Clientes.Tel_Comercial) + Cma + 
                                  STRING(Clientes.Dir_comercial) + Cma + 
                                  STRING(Clientes.Email) + Cma.
               TotReg = TotReg + 1.                                             
/*              WITH FRAME F7 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.*/
            END.  
        END. 
        WHEN "Asociados Sin Productos" THEN 
           IF Clientes.Tipo_Vinculo EQ 1 THEN DO:  
              RUN Inf_Clientes_AsoSinPdto.
           END.
        WHEN "Solo Clientes No Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
                      Clientes.Email            AT 130 FORMAT "X(50)"
              WITH FRAME F8 WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
/*               WITH FRAME F8 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.  */
              TotReg = TotReg + 1.
           END.
        END.
          WHEN "Inconsistencias en Ubicación" THEN DO:
          END.
      END CASE.
  END.
END.
IF Cmb_BClientes:SCREEN-VALUE EQ "Actualización de Información" THEN RUN Inf_ClientesActualizados.
IF Cmb_BClientes:SCREEN-VALUE EQ "Inconsistencias en Ubicación" THEN RUN Inf_UbicInconsistente.
IF Cmb_BClientes:SCREEN-VALUE EQ "Motivos de Vinculación" THEN RUN Cli_MotivosVinculacion.
IF Cmb_BClientes:SCREEN-VALUE EQ "Afiliaciones" THEN RUN Cli_Afiliaciones.
IF Cmb_BClientes:SCREEN-VALUE EQ "Actividades"  THEN RUN Cli_Actividades.
IF Cmb_BClientes:SCREEN-VALUE EQ "Habiles"      THEN RUN Cli_Habiles(OUTPUT TotReg).
IF Cmb_BClientes:SCREEN-VALUE EQ "No Habiles"   THEN RUN Cli_NoHabiles(OUTPUT TotReg).
IF Cmb_BClientes:SCREEN-VALUE EQ "Profesiones"  THEN RUN Cli_Profesiones.


DISPLAY SKIP "Total de Registros Reportados: " TotReg FORMAT ">>,>>>,>>9" WITH FRAME FT WIDTH 132 NO-LABELS.
VIEW FRAME F-Ftr.
PAGE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos wWin 
PROCEDURE Informes_Creditos :
Listado = W_PathSpl + "InfCre-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

OS-DELETE VALUE(Listado).

ASSIGN J = 0 k = 0.

CASE RFPago:
    WHEN 1 THEN ASSIGN FP1 = 1 FP2 = 1 FPN = "Caja".
    WHEN 2 THEN ASSIGN FP1 = 2 FP2 = 2 FPN = "Nómina".
    WHEN 3 THEN ASSIGN FP1 = 1 FP2 = 2 FPN = "Todos".
END CASE.

IF W_Pan EQ 1 THEN
   OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
IF W_Pan EQ 2 THEN
   OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 57.
/*{Incluido\RepEncabezado.i}*/

/**/
   DEFINE VAR W_NomUsu                LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomUsuEnc             LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomEstEnc             LIKE Estacion.Descripcion FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_CodUsu                LIKE Usuarios.Usuario.
   DEFINE VAR W_FechaHora AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Reporte   AS CHARACTER FORMAT "X(120)".
   DEFINE VAR W_IdReporta AS CHARACTER FORMAT "X(120)".
   DEFINE VAR W_IdEstacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Ubicacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_PiePagina AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_UsuEncabe AS CHARACTER FORMAT "X(60)".


  DEFINE VAR W_Linea     AS CHARACTER FORMAT "X(130)" INITIAL "".
  DEFINE VAR W_Linea2    AS CHARACTER FORMAT "X(130)" INITIAL "".

  DEFINE VAR W_Raya      AS CHARACTER INITIAL "_".
  DEFINE VAR W_Raya2     AS CHARACTER INITIAL "-".
  DEFINE VAR W_UsuTra    AS CHAR FORMAT "X(4)" INITIAL "".
  DEFINE VAR HoraEnvio   AS CHARACTER FORMAT "X(8)".
  DEFINE VAR HoraLee     AS CHARACTER FORMAT "X(8)".
  DEFINE VAR FechaLee    AS CHARACTER FORMAT "X(10)".
  DEFINE VAR W_EncColumna  AS CHARACTER FORMAT "X(200)".
  DEFINE FRAME F-Encabezado
      HEADER
        W_Nom_Entidad AT 2
        "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
        W_Ubicacion   AT 2
        W_IdEstacion  AT 2
        W_IdReporta   AT 2
        W_Reporte     AT 2
        W_Linea       AT 1 FORMAT "X(120)"
        W_EncColumna  
        W_Linea2      AT  1  FORMAT "X(120)"
    WITH DOWN WIDTH 220 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

    DEFINE FRAME f-ftr
      HEADER 
        "________________________________________" AT 2
        W_PiePagina AT 2
      WITH DOWN WIDTH 132 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN W_NomUsuEnc = Usuarios.Nombre.
    ELSE W_NomUsuEnc = "Usuario No Encontrado".
    FIND Estacion WHERE Estacion.Estacion EQ W_Estacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Estacion) THEN W_NomEstEnc = Estacion.Descripcion.
    ELSE W_NomEstEnc = "Estacion No Encontrada".

    ASSIGN W_Ubicacion = "UBICACION : " + TRIM(W_Nom_Agencia)
           W_IdReporta = "USUARIO   : " + W_Usuario + " - " + TRIM(W_NomUsuEnc)
           W_IdEstacion = "ESTACION  : " + STRING(INTEGER(W_Estacion)) + " - " + W_NomEstEnc
           W_PiePagina = TRIM(W_Nom_Agencia) + " / " + STRING(TODAY) + " / " + STRING(TIME,"hh:mm am").
    IF W_Pan EQ 2 THEN
        ASSIGN W_Linea     = FILL(W_Raya,104)
               W_Linea2    = FILL(W_Raya2,104).
    IF W_Pan EQ 1 THEN
        ASSIGN W_Linea     = FILL(W_Raya,130)
               W_Linea2    = FILL(W_Raya2,130).


/**/

  CASE Cmb_BCreditos:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Por Estados" THEN
        ASSIGN   W_Reporte   = "REPORTE   : Creditos Por Estado: " + Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros
                               + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "AGE NIT            PDT TPPDT NUM.CREDIT TASA  PLAZO  FEC.APROBA    MONTO      SDO.CAPITAL      INTERESES     COD.USU".
    WHEN "Por Instancias" THEN
        ASSIGN   W_Reporte   = "REPORTE   : Creditos Instancias: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "AGE NIT            PDT TPPDT   PAGARE   TASA  PLAZO  FEC.APROBA    MONTO      SDO.CAPITAL      INTERESES     USUARIO     VIGENCIA".
    WHEN "Cartera" THEN DO:
       ASSIGN   W_Reporte   = "REPORTE   : Cartera de Créditos: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                W_EncColumna = "AGE NIT         PDT  ASOCIADO                 #CREDI    TASA  PLAZO F-DESEMB     MONTO   SDO.CAPITAL    INTERESES     VR.CUOTA   FEC-NAC".
       CREATE IEx.
       ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
              NomInforme = "Cre_Cartera"
/*                AGENCIA;NIT;COD.CREDITO;NOMBRE;#CREDI;TASA;PLAZO;FEC.DESEMBOLSO;MONTO;SDO.CAPITAL;INTERESES;CUOTA  */
              IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "TIP_CRED" + Cma + "COD.CRED" + Cma + "NOMBRE" + Cma +
                           "#CREDI" + Cma + "TASA" + Cma + "PLAZO" + Cma + "FEC.DES" + Cma + "MONTO" + Cma + "SDO.CAPIT" + Cma +
                           "INTERES" + Cma + "INT.MORA" + Cma + "COB.JUR" + Cma + "CUOTA" + Cma + "PROVI.CAP" + Cma + "PROVI.INT" + Cma + 
                           "#PAG" + Cma + /* "PER_PAG" + Cma + */ "MORA" + Cma + "CALIF" + Cma + "CALIF MES" + Cma + "CAPITALenMORA" + Cma + 
                           "Pagad" + Cma + "For_Pag" + Cma + "FEC.1ra.CUOTA" + Cma + "FEC.UBI.SDO" + Cma + "FEC.INI.CRED" + Cma + 
                           "INT.DIF.COBRO" + Cma + "INT.MORAxCOB" + cma + "Negocio".
    END.
      WHEN "Cartera Complemento" THEN
          ASSIGN   W_Reporte   = "REPORTE   : Cartera Complemento: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                   W_EncColumna = "AGE NIT            PDT TIP NUM.CREDIT       COSTAS      POLIZAS D.AT   PROVISION CPA COMPR  sdo ahorros".
      WHEN "Vencimiento" THEN DO:
          IF fecini NE fecfin THEN
             ASSIGN   W_Reporte   = "REPORTE   : Cartera Vencimiento: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") + " DEL " + STRING(FECINI) + " AL " + STRING(FECFIN)
                      W_EncColumna = "AGE NIT             TIP NUM.CREDIT TASA PLAZO FEC.Vencmto  SDO.CAPITAL    D.Atras  Saldo Ahorros    Cuota".
          ELSE
             ASSIGN   W_Reporte   = "REPORTE   : Cartera Vencimiento: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") + " AL " + STRING(FECFIN) 
                      W_EncColumna = "AGE NIT             TIP NUM.CREDIT TASA PLAZO FEC.Vencmto  SDO.CAPITAL    D.Atras  Saldo Ahorros    Cuota".
      END.

      WHEN "Resumen Creditos" THEN DO:
          ASSIGN W_Reporte    = "REPORTE   : Resumen de Créditos: " + " - FECHA: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") 
                 W_EncColumna = "          AGE PDTO CALIF NUMERO    SALDOS-CAPITAL      INT-CORRIENTES     INT-ANTICIPADOS      INT-MORATORIOS    INT-CONTINGENTES      COBRO-JURIDICO"
                 NomInforme = "NA".
      END.

      WHEN "Detallado-Dias Mora" THEN
          ASSIGN   W_Reporte   = "REPORTE   : Detallado-Dias Mora: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") + "  - Creditos: " + FPN
                   W_EncColumna = "AGE NIT            NUM-CRED  NOMBRE       TASA PLAZO FEC.DESE        MONTO      SDO.CAP  INTERESES  D.AT A".
      WHEN "Desembolsos" THEN DO:
          ASSIGN W_Reporte    = "REPORTE   : Acta Desembolso Créditos: " + " - FECHA: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + "      Desde " + STRING(FecIni) + " Hasta " + STRING(FecFin)
                 W_EncColumna = "AGE CED/NIT      NOMBRE-ASOCIADO         PCTO  #SOLIC. #CREDITO        MONTO     PLAZO PP  TASA-NA       CUOTA   FEC.DESEMB USUARIO    PAGADURIA". /*ADMI. RADIC CPTOF DESEM". */
          CREATE IEx.
          ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                 NomInforme = "Cre_Desembolsos"
                 IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "COD.CREDITO" + Cma + "NUM.SOLICITUD" + Cma +
                              "PAGARE" + Cma + "MONTO" + Cma + "PLAZO" + Cma + "PER.PAGO" + Cma + "TASA" + Cma + "CUOTA" + Cma + "FEC.DESEMBOLSO".
      END.
      WHEN "Por Empresa - Canc. y Desembolsos" THEN DO:
          ASSIGN W_Reporte    = "REPORTE   : Cancelaciones y Desembolso Créditos: " + " - FECHA: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + "      Desde " + STRING(FecIni) + " Hasta " + STRING(FecFin)
                  W_EncColumna = "AGE CED/NIT      NOMBRE-ASOCIADO               PCTO  #CREDITO   MONTO-CREDITO PLAZO PP       CUOTA F-DESEMB  F-CANCELAC". 
          CREATE IEx.
          ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                 NomInforme = "Cre_CancyDesXEmpresa"
                 IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "COD.CREDITO" + Cma + "PAGARE" + Cma + 
                              "MONTO" + Cma + "PLAZO" + Cma + "PERIODO.PAGO" + Cma + "CUOTA" + Cma + "FEC.DESEMBOLSO" + Cma + "FEC.ULTPAGO".
      END.

      WHEN "Vctos para Débitar" THEN DO:
          IF fecini NE fecfin THEN
             ASSIGN   W_Reporte    = "REPORTE   : Vctos.para Débitar: " + " - FECHA: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + " DEL " + STRING(FECINI) + " AL " + STRING(FECFIN)
                      W_EncColumna = "AGE CED/NIT     PCTO  #CREDITO  PROX_PAGO  CUOTA/SDO_CANCELAR  S_DISP.AHORROS D.Atraso FEC.ULT.MVTO COMENTARIO".
          ELSE
             ASSIGN   W_Reporte    = "REPORTE   : Vctos.para Débitar: " + " - FECHA: " + STRING(W_Fecha) + " - " + STRING(TIME,"hh:mm am") + " AL " + STRING(FECFIN) 
                      W_EncColumna = "AGE CED/NIT     PCTO  #CREDITO  PROX_PAGO  CUOTA/SDO_CANCELAR  S_DISP.AHORROS D.Atraso FEC.ULT.MVTO COMENTARIO".
      END.
      WHEN "Créditos en Abogado" THEN DO:
          ASSIGN   W_Reporte   = "REPORTE   : Créditos en Abogado: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                                /*          1         2         3         4         5         6         7         8         9         1         11        12        13        14        15        16        17        18        19        20        21        22
                                   12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                   W_EncColumna = "".
      END.
      WHEN "Reestructurados" THEN DO:
          ASSIGN   W_Reporte   = "REPORTE   : Créditos REESTRUCTURADOS: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                              /*          1         2         3         4         5         6         7         8         9         1         11        12        13        14        15        16        17        18        19        20        21        22
                                 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                 W_EncColumna = "".
    END.
    WHEN "Acuerdos Pago No Cumplidos" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Acuerdos No Cumplidos: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "AGE NIT         NUM.CREDIT  FEC.ACUER VALOR ACUE USUARIO".
    END.
    WHEN "Acuerdos Pago Cumplidos" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Acuerdos Cumplidos: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "AGE NIT         NUM.CREDIT  FEC.COMPROMI FEC.ACUER VALOR ACUE USUARIO".
    END.
    WHEN "Cancelados" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Créditos Cancelados: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                              /*          1         2         3         4         5         6         7         8         9         1         11        12        13        14        15        16        17        18        19        20        21        22
                                 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                 W_EncColumna = "AGE NIT            PDT TIP  PAGARE    TASA  PLAZO  FEC.CANCELA       MONTO            CUOTA    CuotasPag".
    END.
    WHEN "Cancelados en Abogado" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Créd.Cancelados(En Abogados): " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                              /*          1         2         3         4         5         6         7         8         9         1         11        12        13        14        15        16        17        18        19        20        21        22
                                 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                 W_EncColumna = "AGE NIT            PDT TIP  PAGARE    TASA  PLAZO  FEC.CANCELA       MONTO            CUOTA    CuotasPag".
    END.
    WHEN "Libranzas(Empresa,Dias-Mora)" THEN 
       ASSIGN   W_Reporte   = "REPORTE   : Créditos por Libranzas: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").    

    WHEN "Fondo Mutual" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Créditos Mutuales: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")    
        W_EncColumna = "AGE NIT            NOMBRES COMPLETOS               FEC-NAC           SALDO     INTERESES    DIR-RESIDENCIA              CIUDAD                     TEL".
    END.
    WHEN "Fondo Mutual General" THEN DO:
        ASSIGN   W_Reporte   = "REPORTE   : Fondo Mutual General: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")    
        W_EncColumna = "FONDO              AGENCIA CR ASO NIT            NOMBRES COMPLETOS                        PAGO_POR             FEC_NAC    FEC_INGR   FEC_ULTP    SALDO CAPIT DIR _RESID                               CIUDAD                    TEL_RESI".
        CREATE IEx.
        ASSIGN Ct = Ct + 1 IEx.NLinea = Ct 
               NomInforme = "Cre_MutualGral"
               IEx.Linea  = "Fondo;Agen_Credi;Agen_Aso;nit;Nombres_Asociado;Pago_Por;Fec_Nacto;Fec_Ingre.;Fec_Ult.Pago;Saldo K;Dir_Resi;Ciudad;Tel_Resi" .
    END.

  END CASE.
  VIEW FRAME F-Encabezado.
IF Cmb_BCreditos:SCREEN-VALUE EQ "Desembolsos" THEN 
   RUN Inf_Desembolsos.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Empresa - Canc. y Desembolsos" THEN 
   RUN Inf_EmpCancDesemb.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Resumen Creditos" THEN 
   RUN Resumen_Creditos.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Detallado-Dias Mora" THEN DO:
   RUN Informes_Creditos_DiasMora.
END.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Datos Demograficos" THEN DO:
   RUN Informes_Creditos_Demograficos.
END.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Estados" THEN
   RUN Informes_Creditos_Estados.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Instancias" THEN 
   RUN Informes_Creditos_Instancias.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Cartera" THEN DO:
/*   TCupoRota:SENSITIVE IN FRAME F_Filtros = NO.
   TCupoRota:HIDDEN IN FRAME F_Filtros = YES.*/
   RUN Informes_Creditos_Cartera.
/*   TCupoRota:HIDDEN IN FRAME F_Filtros = NO.*/
  END.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Cartera Complemento" THEN 
   RUN Informes_Creditos_Complemento.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Vencimiento" THEN 
   RUN Informes_Creditos_Vencimiento.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Vctos para Débitar" THEN 
   RUN Informes_VctosADebitar.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Acuerdos Pago no cumplidos" THEN 
   RUN Informes_Creditos_AcuerdosNC.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Acuerdos Pago cumplidos" THEN 
   RUN Informes_Creditos_AcuerdosCM.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Créditos en Abogado" THEN 
   RUN Informes_Creditos_Abogado.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Reestructurados" THEN 
   RUN Informes_Creditos_Reestructurados.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Cancelados" THEN 
   RUN Informes_Creditos_Cancelados.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Cancelados en Abogado" THEN 
   RUN Inf_Credi_CancEnAbog.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Libranzas(Empresa,Dias-Mora)" THEN 
   RUN Inf_CredLibranzas.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Fondo Mutual" THEN 
   RUN Inf_CredMutual.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Fondo Mutual General" THEN 
   RUN Inf_CredMutualGral.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Desplazamiento de la Cartera" THEN 
   RUN Informes_Creditos_Desplazamiento.
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Abogado wWin 
PROCEDURE Informes_Creditos_Abogado :
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt    AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR CTotCos    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPol    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPro    AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotMon     AS DECIMAL FORMAT "->>>,>>>,>>9".  
DEFINE VAR TotInt     AS DECIMAL FORMAT "->>>,>>>,>>9".  

DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCos     AS DECIMAL FORMAT ">>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPol     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPro     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  

DEFINE VAR TotAvi     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotApo     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAte     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCtr     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  

DEFINE VAR TotSdoAvi  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoApo  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoAte  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoCtr  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  

DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0.
DISPLAY "AGE NIT            PAGARE       TASA     PLAZO     FEC.DESEMBO        MONTO         SDO.CAPITAL         INTERESES            CUOTA           D.AT          ABOGADO " 
    WITH FRAME F_Encabe WIDTH 250.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin AND
           Creditos.Abogado      EQ YES    AND
           creditos.sdo_capital  GT 0   
           NO-LOCK BREAK BY Creditos.Agencia BY creditos.nit BY Creditos.Cod_Credito:
      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      FIND FIRST Cobros WHERE
                 Cobros.Num_Credito EQ Creditos.Num_Credito AND
                 Cobros.Nit         EQ Creditos.Nit AND
                 Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
      W_Compromiso = "".
      IF AVAILABLE Cobros THEN W_Compromiso = "x".
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      IF NOT TTotales THEN 
        ASSIGN  TotSdoAvi = 0 TotSdoCtr = 0 TotSdoAte = 0 TotSdoApo = 0.
        FOR EACH ahorros WHERE 
                 Ahorros.nit      EQ creditos.nit
             AND Ahorros.estado   EQ 1
             AND (Ahorros.Sdo_disponible + Ahorros.Sdo_Canje) GT 0: 
            CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN TotSdoAvi = TotSdoAvi + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 2 THEN TotSdoCtr = TotSdoCtr + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 3 THEN TotSdoAte = TotSdoAte + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 4 THEN TotSdoApo = TotSdoApo + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
            END CASE.
             
        END.
        DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
                Creditos.Nit            AT 5  FORMAT "X(14)"
                TRIM(Creditos.Pagare)   AT 25 FORMAT "X(9)"
                Creditos.Tasa           AT 35 FORMAT ">9.99"
                Creditos.Plazo          AT 41 FORMAT "9999"
                Creditos.Fec_Desembolso AT 52 FORMAT "99/99/9999"
                Creditos.Monto          AT 64 FORMAT ">>>,>>>,>>9"
                Creditos.Sdo_Capital    AT 79 FORMAT ">>>,>>>,>>9"
                W_Interes               AT 94 FORMAT "->>>,>>>,>>9"
                Creditos.cuota          AT 128 FORMAT ">>>,>>>,>>9"      
                Creditos.Dias_Atraso    AT 142 FORMAT ">>,>>9"
                nit_juzgado             AT 152  FORMAT "X(14)"
        WITH FRAME FCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotPol    = TotPol + Creditos.Polizas
             TotPro    = TotPro + Creditos.Provision
             TotCos    = TotCos + Creditos.Costas
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             TotAvi     = TotAvi + TotSdoAvi
             TotCtr     = TotCtr + TotSdoCtr
             TotAte     = TotAte + TotSdoAte
             TotApo     = TotApo + TotSdoApo.
      /*IF LAST-OF(Creditos.categoria) THEN
         DISPLAY "Total: " AT 1
                 CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"
                 CTotMon   AT 61
                 CTotSdo   AT 76
                 CTotInt   AT 92 
         WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS.*/
  END.
 

  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  
     TotInt                          AT 94  
  WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_AcuerdosCM wWin 
PROCEDURE Informes_Creditos_AcuerdosCM :
/*Cobros.Agencia
Cobros.Age_Recaudo
Cobros.Estado
Cobros.Fec_Acuerdo
Cobros.Fec_Compromiso
Cobros.Fec_Cumplimiento
Cobros.Nit
Cobros.Nro_Cobro
Cobros.Nro_Transaccion
Cobros.Num_Credito
Cobros.Observacion
Cobros.Usuario
Cobros.Usu_Recaudo
Cobros.Val_Compromiso
Cobros.Val_Cumplido*/
  
DEFINE VAR NomUsu AS CHARACTER FORMAT "X(50)".
FOR EACH Cobros WHERE Cobros.Fec_Cumplimiento GE FecIni AND
                      Cobros.Fec_Cumplimiento LE FecFin AND
                      Cobros.Estado           EQ 2
                      NO-LOCK BREAK BY Cobros.Usuario:
    IF FIRST-OF(Cobros.Usuario) THEN DO:
       FIND Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR.
       IF AVAILABLE Usuarios THEN
          DISPLAY Usuarios.Usuario Usuarios.Nombre SKIP WITH FRAME F_UEn NO-LABELS. 
       ELSE
          DISPLAY "Usuario:" Cobros.Usuario " no existe en usuarios" SKIP WITH FRAME F_UNoEn NO-LABELS. 
    END.
    DISPLAY Cobros.Agencia
            Cobros.Nit
            Cobros.Num_Credito
            Cobros.Fec_Cumplimiento
            Cobros.Fec_Compromiso
            Cobros.Val_Compromiso
            Cobros.Usuario
        WITH FRAME F_Acu WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_AcuerdosNC wWin 
PROCEDURE Informes_Creditos_AcuerdosNC :
/*Cobros.Agencia
Cobros.Age_Recaudo
Cobros.Estado
Cobros.Fec_Acuerdo
Cobros.Fec_Compromiso
Cobros.Fec_Cumplimiento
Cobros.Nit
Cobros.Nro_Cobro
Cobros.Nro_Transaccion
Cobros.Num_Credito
Cobros.Observacion
Cobros.Usuario
Cobros.Usu_Recaudo
Cobros.Val_Compromiso
Cobros.Val_Cumplido*/
  
DEFINE VAR NomUsu AS CHARACTER FORMAT "X(50)".
FOR EACH Cobros WHERE Cobros.Fec_Compromiso   GE FecIni AND
                      Cobros.Fec_Compromiso   LE FecFin AND
                      Cobros.Estado           EQ 1 AND
                      Cobros.Fec_Cumplimiento EQ ?
         NO-LOCK BREAK BY Cobros.Usuario BY Cobros.Fec_Compromiso:
    IF FIRST-OF(Cobros.Usuario) THEN DO:
       FIND Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR.
       IF AVAILABLE Usuarios THEN
          DISPLAY Usuarios.Usuario Usuarios.Nombre SKIP WITH FRAME F_UEn NO-LABELS. 
       ELSE
          DISPLAY "Usuario: " Cobros.Usuario " no existe en usuarios" SKIP WITH FRAME F_UNoEn NO-LABELS. 
    END.
    DISPLAY Cobros.Agencia Cobros.Nit Cobros.Num_Credito Cobros.Fec_Compromiso Cobros.Val_Compromiso Cobros.Usuario
        WITH FRAME F_Acu WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Cancelados wWin 
PROCEDURE Informes_Creditos_Cancelados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  

DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0
       TotSdoAho = 0.

  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Tip_Credito    GE TPdIni AND
           Creditos.Tip_Credito    LE TpdFin AND
           Creditos.Cod_Credito    GE ProIni AND
           Creditos.Cod_Credito    LE ProFin AND
           Creditos.Fec_CanceTotal NE ?      AND
           Creditos.Fec_CanceTotal GE FecIni AND    
           Creditos.Fec_CanceTotal LE FecFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.pagare BY Creditos.Cod_Credito:
      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.

      DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
              Creditos.Nit            AT 5  FORMAT "X(14)"
              Creditos.Cod_Credito    AT 20 FORMAT "999"
              W_TipPdt                AT 24 FORMAT "X(4)"
              Creditos.Num_Credito    AT 29 FORMAT "999999999"
              Creditos.Tasa           AT 40 FORMAT ">9.99"
              Creditos.Plazo          AT 47 FORMAT "9999"
              Creditos.Fec_CanceTotal AT 52 FORMAT "99/99/9999"
              Creditos.Monto          AT 64 FORMAT ">>>,>>>,>>9"
              Creditos.cuota          AT 79 FORMAT ">,>>>,>>>,>>9"  
              Creditos.Cuo_Pagadas    AT 95 FORMAT ">9"
        WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  

  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Cartera wWin 
PROCEDURE Informes_Creditos_Cartera :
DEFINE VAR W_DiaA     AS CHARACTER.
DEFINE VAR WkNom      AS CHARACTER FORMAT "X(60)".
DEFINE VAR wfec       AS DATE.
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt    AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR CTotCos    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPol    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPro    AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotInt     AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCos     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPol     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPro     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR FecDesAno AS DATE.
DEFINE VAR vcFPagAnti AS CHARACTER FORMAT "X(10)".
DEFINE VAR vcFPago AS CHARACTER FORMAT "X(10)".
DEFINE VAR W_negocio AS CHARACTER FORMAT "X(6)".
DEFINE VAR W_pago AS CHARACTER FORMAT "X(6)".


ASSIGN TotSdo = 0
       TotSdoAho = 0.

ASSIGN TpdFin = 4. 

/* AND   * se inactiva todo esto para impresion completa
           MONTH(Creditos.Fec_Desembolso) GE MONTH(FecIni)  AND
           MONTH(Creditos.Fec_Desembolso) LE MONTH(FecFin)  AND
           DAY(Creditos.Fec_Desembolso)   GE DAY(FecIni)      AND
           DAY(Creditos.Fec_Desembolso)   LE DAY(FecFin)      AND
           Creditos.Dias_Atraso  LE ValFin 
           AND Creditos.Dias_Atraso  GE ValIni                       */

FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Estado       EQ 2      AND
          /*Creditos.Dias_Atraso  GE ValIni AND*/
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin  /*AND
           Creditos.Fec_Desembolso GE FecIni  AND
           Creditos.Fec_Desembolso LE FecFin */  NO-LOCK 
           BREAK BY Creditos.Agencia BY creditos.nit BY Creditos.Cod_Credito:
      IF (creditos.cod_credito = 570 OR creditos.cod_credito = 870)  AND sdo_capital = 0 THEN
         IF NOT TCupoRota THEN NEXT.

      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      FIND FIRST Cobros WHERE
                 Cobros.Num_Credito EQ Creditos.Num_Credito AND
                 Cobros.Nit         EQ Creditos.Nit AND
                 Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
      W_Compromiso = "".
      IF AVAILABLE Cobros THEN W_Compromiso = "x".
      W_Interes = Creditos.INT_Corriente - Creditos.INT_Anticipado. /* + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.*/
      W_IntMora = Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar.
      W_CobJuri = Creditos.Honorarios.
      IF NOT TTotales THEN 
        ASSIGN  TotSdoAho = 0.
       /* FOR EACH ahorros WHERE ahorros.agencia = creditos.agencia AND ahorros.nit = creditos.nit
             AND ahorros.estado = 1 AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 AND ahorros.tip_ahorro EQ 1: 
             ASSIGN  TotSdoAho = TotSdoAho + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
        END.*/
        WkNom = "".
        DEFINE VARIABLE xpag LIKE clientes.cod_empresa INITIAL 0.
        FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit  NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
           WkNom = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
           WkNom = SUBSTRING(Wknom,1,30).
           IF clientes.cod_empresa = ? THEN xpag = 0.
              ELSE xpag = clientes.cod_empresa.
           wfec = fec_nacimiento.
        END.
        DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
                Creditos.Nit            AT 5  FORMAT "X(12)"
                Creditos.Cod_Credito    AT 18 FORMAT "999"
                WkNom AT 22 FORMAT "X(24)" WHEN AVAIL(Clientes)
                TRIM(STRING(Creditos.Num_Credito))   AT 47 FORMAT "X(9)"
                Creditos.Tasa           AT 57 FORMAT ">9.99"
                Creditos.Plazo          AT 63 FORMAT "9999"
                Creditos.Fec_Desembolso AT 68 FORMAT "99/99/99"
                Creditos.Monto          AT 76 FORMAT ">>>,>>>,>>9"
                Creditos.Sdo_Capital    AT 88 FORMAT "->>>,>>>,>>9"
                W_Interes               AT 102 FORMAT "->>>,>>>,>>9"
                Creditos.cuota          AT 116 FORMAT ">>>,>>>,>>9"
                wfec                    AT 129 FORMAT "99/99/9999"
        WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.

        IF creditos.sdo_capital GT 0  THEN DO:
            IF Creditos.Dias_Atraso EQ ? THEN
                W_DiaA = "0".
            ELSE 
                W_DiaA = TRIM(STRING(Creditos.Dias_Atraso,"-zzzz9")).

            IF Creditos.Fec_PagAnti EQ ? THEN ASSIGN vcFPagAnti = "No Reporta".
                ELSE ASSIGN vcFPagAnti = STRING(Creditos.Fec_PagAnti,"99/99/9999").
            IF Creditos.Fec_Pago EQ ? THEN ASSIGN vcFPago = "No Reporta".
                ELSE ASSIGN vcFPago = STRING(Creditos.Fec_PagAnti,"99/99/9999").
            FIND FIRST anexos_clientes WHERE anexos_clientes.nit = creditos.nit NO-LOCK NO-ERROR.
            /*IF AVAILABLE(anexos_clientes) THEN DO:*/
               IF creditos.cod_credito LT 700 THEN DO:
                  IF (creditos.cod_credito EQ 12 OR  creditos.cod_credito EQ 13 OR  creditos.cod_credito EQ 14 OR 
                      creditos.cod_credito EQ 46 OR  creditos.cod_credito EQ 55 OR  creditos.cod_credito EQ 60 OR
                      creditos.cod_credito EQ 65 OR  creditos.cod_credito EQ 528 OR creditos.cod_credito EQ 543 OR 
                      creditos.cod_credito EQ 544 OR creditos.cod_credito EQ 545 OR creditos.cod_credito EQ 546 OR
                      creditos.cod_credito EQ 547 OR creditos.cod_credito EQ 548 OR creditos.cod_credito EQ 549 OR
                      creditos.cod_credito EQ 550 OR creditos.cod_credito EQ 551 OR creditos.cod_credito EQ 552 OR
                      creditos.cod_credito EQ 553 OR creditos.cod_credito EQ 554 OR creditos.cod_credito EQ 555  or
                      creditos.cod_credito EQ 562 OR creditos.cod_credito EQ 563 OR creditos.cod_credito EQ 565 or
                      creditos.cod_credito EQ 565 OR creditos.cod_credito EQ 566 OR creditos.cod_credito EQ 567 OR 
                      creditos.cod_credito EQ 568 OR creditos.cod_credito EQ 569) THEN W_Negocio = "Mult".
                  ELSE W_Negocio = "Finan".    
               END.
               ELSE W_Negocio = "Mult".
            /*END.*/
            IF creditos.FOR_pago = 2 THEN W_pago = "Nomina".
            ELSE W_pago = "Caja".
          CREATE IEx.
          ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = TRIM(STRING(Creditos.Agencia)) + Cma + TRIM(Creditos.Nit) + Cma + TRIM(STRING(Creditos.tip_Credito)) + Cma +
                            TRIM(STRING(Creditos.Cod_Credito)) + Cma + TRIM(WkNom) + Cma  + TRIM(STRING(Creditos.Num_Credito)) + Cma + 
                            TRIM(STRING(Creditos.Tasa,"->>>>>.99999")) + Cma + TRIM(STRING(Creditos.Plazo,"9999")) + Cma + 
                            TRIM(STRING(Creditos.Fec_Desembolso)) + Cma + TRIM(STRING(Creditos.Monto,"->>>>>>>>>>>9.99")) + Cma + 
                            TRIM(STRING(Creditos.Sdo_Capital,"->>>>>>>>>>>9.99")) + Cma + TRIM(STRING(W_Interes)) + Cma + 
                            TRIM(STRING(W_IntMora)) + Cma + TRIM(STRING(W_CobJuri)) + Cma + TRIM(STRING(Creditos.Cuota,"->>>>>>>>>>>9.99")) + Cma + 
                            TRIM(STRING(Creditos.Provision)) + Cma + TRIM(STRING(Creditos.Provision_Interes)) + Cma + TRIM(Creditos.Pagare) + Cma +
                            /* TRIM(STRING(Creditos.Per_Pago)) + Cma + */ W_DiaA + Cma + TRIM(Creditos.Categoria) + Cma + 
                            TRIM(Creditos.Categoriames) + Cma + TRIM(STRING(Creditos.Val_Atraso)) + Cma + TRIM(STRING(xpag)) + Cma + TRIM(W_pago) + Cma +
                            TRIM(vcFPagAnti) + Cma + TRIM(vcFPago) + Cma + TRIM(vcFPagAnti) + Cma + 
                            TRIM(STRING(Creditos.Int_DifCobro)) + Cma + TRIM(STRING(Creditos.Int_MorCobrar)) + Cma + TRIM(W_Negocio).
        END.

/*IEx.Linea  = TRIM(STRING(Creditos.Agencia)) + Cma + TRIM(Creditos.Nit) + Cma + TRIM(STRING(Creditos.tip_Credito)) + Cma +
                            TRIM(STRING(Creditos.Cod_Credito)) + Cma + TRIM(WkNom) + Cma  + 
                            TRIM(STRING(Creditos.Num_Credito)) + Cma + TRIM(STRING(Creditos.Tasa,"->>>>>.99999")) + Cma +
                            TRIM(STRING(Creditos.Plazo,"9999")) + Cma + TRIM(STRING(Creditos.Fec_Desembolso)) + Cma +
                            TRIM(STRING(Creditos.Monto,"->>>>>>>>>>>9.99")) + Cma + 
                            TRIM(STRING(Creditos.Sdo_Capital,"->>>>>>>>>>>9.99")) + Cma +
                            TRIM(STRING(W_Interes)) + Cma + TRIM(STRING(Creditos.Cuota,"->>>>>>>>>>>9.99")) + Cma +
                            TRIM(Creditos.Pagare) + Cma + TRIM(STRING(Creditos.Per_Pago)) + Cma +
                            TRIM(STRING(Creditos.Dias_Atraso,"zzzz9")) + Cma + TRIM(Creditos.Categoria) + Cma +
                            TRIM(STRING(Creditos.Val_Atraso)) + Cma + TRIM(STRING(xpag)) + Cma + TRIM(STRING(FOR_pago)).*/


      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotPol    = TotPol + Creditos.Polizas
             TotPro    = TotPro + Creditos.Provision
             TotCos    = TotCos + Creditos.Costas
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             TotAho    = TotAho + TotSdoAho.
      /*IF LAST-OF(Creditos.categoria) THEN
         DISPLAY "Total: " AT 1
                 CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"
                 CTotMon   AT 61
                 CTotSdo   AT 76
                 CTotInt   AT 92 
         WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS.*/
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64
     TotSdo                          AT 79
     TotInt                          AT 94
     totcos                          AT 108
     totpol                          AT 121
     totpro                          AT 139
     totAho                          AT 158

  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Complemento wWin 
PROCEDURE Informes_Creditos_Complemento :
DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotCos    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPol    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPro    AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCos     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPol     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPro     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit BY Creditos.Cod_Credito:
     /*    NO-LOCK BREAK BY Creditos.Categoria BY Creditos.Agencia BY Creditos.Cod_Credito: 
      IF FIRST-OF(Creditos.Categoria) THEN DO:
         ASSIGN CTotPol = 0 CTotSdo = 0 CTotCos = 0  .
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Creditos.Cod_Califica NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY CAPS(Varios.Descripcion) AT 1  FORMAT "X(30)"
            WITH FRAME TitCart WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
         ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCart2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      END. */
      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      FIND FIRST Cobros WHERE
                 Cobros.Num_Credito EQ Creditos.Num_Credito AND
                 Cobros.Nit         EQ Creditos.Nit AND
                 Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
      W_Compromiso = "".
      IF AVAILABLE Cobros THEN W_Compromiso = "x".
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      IF NOT TTotales THEN 
        ASSIGN  TotSdoAho = 0.
        FOR EACH ahorros WHERE ahorros.agencia = creditos.agencia AND ahorros.nit = creditos.nit
             AND ahorros.estado = 1 AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 AND ahorros.tip_ahorro EQ 1: 
             ASSIGN  TotSdoAho = TotSdoAho + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
        END.
        DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
                Creditos.Nit            AT 5  FORMAT "X(14)"
                Creditos.Cod_Credito    AT 20 FORMAT "999"
                W_TipPdt                AT 24 FORMAT "X(4)"
                Creditos.Num_Credito    AT 29 FORMAT "999999999"
                Creditos.Costas         AT 40 FORMAT ">>>,>>>,>>9"
                Creditos.Polizas        AT 53 FORMAT ">>>,>>>,>>9"
                Creditos.Dias_Atraso    AT 66 FORMAT ">>9"
                Creditos.Provision      AT 71 FORMAT ">>,>>>,>>9"
                Creditos.Cuo_Pagadas    AT 83 FORMAT ">9"
                W_Compromiso            AT 87 FORMAT "X"
                TotSdoAho               AT 90 FORMAT "->,>>>,>>>,>>9"  
        WITH FRAME FCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotRegAge = TotReg + 1
             TotPol    = TotPol + Creditos.Polizas
             TotPro    = TotPro + Creditos.Provision
             TotCos    = TotCos + Creditos.Costas
             TotSdo    = TotSdo + TotSdoAho
             TotReg    = TotReg + 1.
             
     /* IF LAST-OF(Creditos.categoria) THEN
         DISPLAY "Total: " AT 1
                 CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"
                 CTotCos   AT 40 FORMAT ">>>,>>>,>>9"
                 CTotPol   AT 53 FORMAT ">>>,>>>,>>9"
                 CTotPro   AT 71 FORMAT ">>,>>>,>>9" 
                 CTotSdo   AT 90 FORMAT "->,>>>,>>>,>>9"  
         WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS. */
  END.
  DISPLAY SKIP(1)
     "Total de Registros  :"          AT 1
      TotReg                          AT 25
     "Totales :   Vlr.Polizas    Provision      Costas    Sdo Ahorros"  AT 30 SKIP(1)
      TotCos   AT 40 FORMAT ">>>,>>>,>>9"
      TotPol   AT 53 FORMAT ">>>,>>>,>>9"
      TotPro   AT 71 FORMAT ">>,>>>,>>9" 
      TotSdo   AT 90 FORMAT "->,>>>,>>>,>>9"  

  WITH FRAME ttCartera2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Demograficos wWin 
PROCEDURE Informes_Creditos_Demograficos :
    DEFINE VAR W_DiaA     AS CHARACTER.
    DEFINE VAR WkNom      AS CHARACTER FORMAT "X(60)".
    DEFINE VAR wfec       AS DATE.
    DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
    DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
    DEFINE VAR CTotInt    AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
    DEFINE VAR CTotCos    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
    DEFINE VAR CTotPol    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
    DEFINE VAR CTotPro    AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
    DEFINE VAR TotInt     AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
    DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
    DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR TotCos     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR TotPol     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR TotPro     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
    DEFINE VAR FecDesAno AS DATE.
    DEFINE VAR vcFPagAnti AS CHARACTER FORMAT "X(10)".
    DEFINE VAR vcFPago AS CHARACTER FORMAT "X(10)".
    DEFINE VAR W_negocio AS CHARACTER FORMAT "X(6)".
    DEFINE VAR W_pago AS CHARACTER FORMAT "X(6)".
    
    DEFINE VARIABLE vcCliente AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcUbcResCli AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcUbcComCli AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE viCnt AS INTEGER     NO-UNDO. /*Contador- Cantidad de codeudores por obligcaión*/
    
    
    ASSIGN TotSdo = 0
           TotSdoAho = 0.
    
    ASSIGN TpdFin = 4. 
    
    /* AND   * se inactiva todo esto para impresion completa
               MONTH(Creditos.Fec_Desembolso) GE MONTH(FecIni)  AND
               MONTH(Creditos.Fec_Desembolso) LE MONTH(FecFin)  AND
               DAY(Creditos.Fec_Desembolso)   GE DAY(FecIni)      AND
               DAY(Creditos.Fec_Desembolso)   LE DAY(FecFin)      AND
               Creditos.Dias_Atraso  LE ValFin 
               AND Creditos.Dias_Atraso  GE ValIni                       */
    
    ASSIGN Listado = W_Pathspl + "\CreditosDemografico.txt".
    OUTPUT TO VALUE(Listado).
    PUT
        "Age|Agencia|Nit|CLiente|TP.Cred|Cod.Cred|Num.Cred|Tasa|Plazo|Pagaré|Fec. Desm|Monto|Sdo. Cap.|Int. Corr.|Int.Mora.Cobrar|" 
        "Cuota|Frma Pago|Dias Atr.|Dir. Resid.|Tel. Resid.|Lug. Resid.|Dir. Comer.|Tel. Comer.|Lug. Comer.|" 
        "Nit Cod.A|Codeudor A|Dir.Res. Cod. A|Tel.Res.Cod.A|Lug.Res.Cos.A|Dir.Com. Cod. A|Tel.Com.Cod.A|Lug.Comer.Cod.A|" 
        "Nit Cod.B|Codeudor B|Dir.Res. Cod. B|Tel.Res.Cod.B|Lug.Res.Cos.B|Dir.Com. Cod. B|Tel.Com.Cod.B|Lug.Comer.Cod.B|"  
        "Nit Cod.C|Codeudor C|Dir.Res. Cod. C|Tel.Res.Cod.C|Lug.Res.Cos.C|Dir.Com. Cod. C|Tel.Com.Cod.C|Lug.Comer.Cod.C"
        SKIP.
    
    FOR EACH Creditos WHERE 
               Creditos.Agencia      GE AgeIni AND
               Creditos.Agencia      LE AgeFin AND
               Creditos.Estado       EQ 2      AND
              /*Creditos.Dias_Atraso  GE ValIni AND*/
               Creditos.Tip_Credito  GE TPdIni AND
               Creditos.Tip_Credito  LE TpdFin AND
               Creditos.Cod_Credito  GE ProIni AND
               Creditos.Cod_Credito  LE ProFin  /*AND
               Creditos.Fec_Desembolso GE FecIni  AND
               Creditos.Fec_Desembolso LE FecFin */  NO-LOCK 
               BREAK BY Creditos.Agencia BY creditos.nit BY Creditos.Cod_Credito:
          IF (creditos.cod_credito = 570 OR creditos.cod_credito = 870)  AND sdo_capital = 0 THEN
             IF NOT TCupoRota THEN NEXT.
    
          RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
          j = j + 1.
          RUN Progreso.
    
    
       /*Creamos TTCreditosDemog*/
        CREATE TTCreditosDemog.
        BUFFER-COPY creditos TO TTCreditosDemog.
    
        FIND FIRST clientes WHERE clientes.nit EQ creditos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            ASSIGN vcCliente = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
            UPDATE
                TTCreditosDemog.Dir_Residencia    = Clientes.Dir_Residencia 
                TTCreditosDemog.Tel_Residencia    = Clientes.Tel_Residencia 
                TTCreditosDemog.Dir_comercial     = Clientes.Dir_comercial
                TTCreditosDemog.Tel_comercial     = Clientes.Tel_comercial.
        END.
        ELSE DO:
            ASSIGN vcCliente = "".
            UPDATE
                TTCreditosDemog.Dir_Residencia    = ""
                TTCreditosDemog.Tel_Residencia    = ""
                TTCreditosDemog.Dir_comercial     = ""
                TTCreditosDemog.Tel_comercial     = "".
        END.
    
        /* Lugar Residencial */
        FIND FIRST ubicacion WHERE ubicacion.tipo EQ "C" AND
            SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(Clientes.Lugar_residencia,1,5)
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN 
            ASSIGN vcUbcResCli = Ubicacion.Nombre.
        ELSE
            ASSIGN vcUbcResCli = "".
    
        /* Lugar Comercial */
        FIND FIRST ubicacion WHERE ubicacion.tipo EQ "C" AND
            SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(Clientes.Lugar_Comercial,1,5)
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN 
            ASSIGN vcUbcComCli = Ubicacion.Nombre.
        ELSE
            ASSIGN vcUbcComCli = "".
    
        UPDATE 
            TTCreditosDemog.Cliente = vcCliente
            TTCreditosDemog.UbcResCli = vcUbcResCli
            TTCreditosDemog.UbcComCli = vcUbcComCli.
    
        FIND FIRST agencias WHERE agencias.agencia EQ TTCreditosDemog.agencia NO-LOCK NO-ERROR.    
        UPDATE TTCreditosDemog.nomAge = agencias.Nombre.
    
       /*buscamos codeudores*/
    
        ASSIGN viCnt = 0.
        FOR EACH relaciones WHERE 
            relaciones.nit EQ creditos.nit AND 
            relaciones.Cod_relacion EQ 11 AND
            relaciones.estado EQ 1 AND 
            Relaciones.Clase_Producto EQ 2 AND
            Relaciones.Cod_Producto EQ Creditos.Cod_Credito AND
            TRUE NO-LOCK:
            IF (Relaciones.Cuenta EQ TRIM(STRING(Creditos.Num_Credito)) OR Relaciones.Cuenta EQ Creditos.Pagare) THEN DO:
                ASSIGN viCnt = viCnt + 1.
                CASE viCnt:
                    WHEN 1 THEN DO:
                        FIND FIRST clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK NO-ERROR.
                        IF AVAILABLE clientes THEN DO:
                            UPDATE
                                TTCreditosDemog.nitCodA        = relaciones.nit
                                TTCreditosDemog.nomCodA        = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
                                TTCreditosDemog.dirResCodA     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelResCodA     = Clientes.Tel_Residencia
                                TTCreditosDemog.dirComCodA     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelComCodA     = Clientes.Tel_Residencia.
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Residencia,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcResCodA = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcResCodA = "No ENCONTRADA UBICACION".
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Comercial,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcComCodA = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcComCodA = "No ENCONTRADA UBICACION".
                        END.
                        ELSE
                            UPDATE
                                TTCreditosDemog.nitCodA        = ""
                                TTCreditosDemog.nomCodA        = ""
                                TTCreditosDemog.dirResCodA     = ""
                                TTCreditosDemog.TelResCodA     = ""
                                TTCreditosDemog.dirComCodA     = ""
                                TTCreditosDemog.TelComCodA     = "".
                    END. /*WHEN 1 */
                    WHEN 2 THEN DO:
                        FIND FIRST clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK NO-ERROR.
                        IF AVAILABLE clientes THEN DO:
                            UPDATE
                                TTCreditosDemog.nitCodB        = relaciones.nit
                                TTCreditosDemog.nomCodB        = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
                                TTCreditosDemog.dirResCodB     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelResCodB     = Clientes.Tel_Residencia
                                TTCreditosDemog.dirComCodB     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelComCodB     = Clientes.Tel_Residencia.
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Residencia,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcResCodB = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcResCodB = "No ENCONTRADA UBICACION".
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Comercial,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcComCodB = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcComCodB = "No ENCONTRADA UBICACION".
                        END.
                        ELSE
                            UPDATE
                                TTCreditosDemog.nitCodB        = ""
                                TTCreditosDemog.nomCodB        = ""
                                TTCreditosDemog.dirResCodB     = ""
                                TTCreditosDemog.TelResCodB     = ""
                                TTCreditosDemog.dirComCodB     = ""
                                TTCreditosDemog.TelComCodB     = "".
                    END. /*WHEN 2 */
                    WHEN 3 THEN DO:
                        FIND FIRST clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK NO-ERROR.
                        IF AVAILABLE clientes THEN DO:
                            UPDATE
                                TTCreditosDemog.nitCodC        = relaciones.nit
                                TTCreditosDemog.nomCodC        = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
                                TTCreditosDemog.dirResCodC     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelResCodC     = Clientes.Tel_Residencia
                                TTCreditosDemog.dirComCodC     = Clientes.Dir_Residencia
                                TTCreditosDemog.TelComCodC     = Clientes.Tel_Residencia.
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Residencia,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcResCodC = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcResCodB = "No ENCONTRADA UBICACION".
                            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) EQ SUBSTRING(clientes.lugar_Comercial,1,5) NO-LOCK NO-ERROR.
                            IF AVAILABLE ubicacion THEN 
                                    UPDATE TTCreditosDemog.UbcComCodC = Ubicacion.Nombre.
                            ELSE 
                                UPDATE TTCreditosDemog.UbcComCodC = "No ENCONTRADA UBICACION".
                        END.
                        ELSE
                            UPDATE
                                TTCreditosDemog.nitCodC        = ""
                                TTCreditosDemog.nomCodC        = ""
                                TTCreditosDemog.dirResCodC     = ""
                                TTCreditosDemog.TelResCodC     = ""
                                TTCreditosDemog.dirComCodC     = ""
                                TTCreditosDemog.TelComCodC     = "".
                    END. /*WHEN 3 */
                END CASE.
            END. /*IF (Relaciones.Cuenta EQ TRIM(STRING(.....*/
        END. /*FOR EACH relaciones */
    
    
        EXPORT DELIMITER "|"
            TTCreditosDemog.Agencia        
            TTCreditosDemog.nomAge
            TTCreditosDemog.Nit            
            TTCreditosDemog.Cliente        
            TTCreditosDemog.Tip_Credito    
            TTCreditosDemog.Cod_Credito    
            TTCreditosDemog.Num_Credito    
            TTCreditosDemog.Tasa           
            TTCreditosDemog.Plazo          
            TTCreditosDemog.Pagare         
            TTCreditosDemog.Fec_Desembolso 
            TTCreditosDemog.Monto          
            TTCreditosDemog.Sdo_Capital      FORMAT ">>,>>>,>>>,>>9.99"
            TTCreditosDemog.Int_Corrientes 
            TTCreditosDemog.Int_MorCobrar  
            TTCreditosDemog.Cuota          
            TTCreditosDemog.For_Pago       
            TTCreditosDemog.Dias_Atraso    
            TTCreditosDemog.Dir_Residencia 
            TTCreditosDemog.Tel_Residencia 
            TTCreditosDemog.UbcResCli      
            TTCreditosDemog.Dir_comercial  
            TTCreditosDemog.Tel_comercial  
            TTCreditosDemog.UbcComCli      
            TTCreditosDemog.nitCodA        
            TTCreditosDemog.nomCodA        
            TTCreditosDemog.dirResCodA     
            TTCreditosDemog.TelResCodA     
            TTCreditosDemog.UbcResCodA     
            TTCreditosDemog.dirComCodA     
            TTCreditosDemog.TelComCodA     
            TTCreditosDemog.UbcComCodA     
            TTCreditosDemog.nitCodB        
            TTCreditosDemog.nomCodB        
            TTCreditosDemog.dirResCodB     
            TTCreditosDemog.TelResCodB     
            TTCreditosDemog.UbcResCodB     
            TTCreditosDemog.dirComCodB     
            TTCreditosDemog.TelComCodB     
            TTCreditosDemog.UbcComCodB     
            TTCreditosDemog.nitCodC        
            TTCreditosDemog.nomCodC        
            TTCreditosDemog.dirResCodC     
            TTCreditosDemog.TelResCodC     
            TTCreditosDemog.UbcResCodC     
            TTCreditosDemog.dirComCodC     
            TTCreditosDemog.TelComCodC     
            TTCreditosDemog.UbcComCodC.       
    END.

    /*       FIND FIRST Cobros WHERE                                                                                                                             */
    /*                  Cobros.Num_Credito EQ Creditos.Num_Credito AND                                                                                           */
    /*                  Cobros.Nit         EQ Creditos.Nit AND                                                                                                   */
    /*                  Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.                                                                                           */
    /*       W_Compromiso = "".                                                                                                                                  */
    /*       IF AVAILABLE Cobros THEN W_Compromiso = "x".                                                                                                        */
    /*       W_Interes = Creditos.INT_Corriente - Creditos.INT_Anticipado. /* + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.*/      */
    /*       W_IntMora = Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar.                                                               */
    /*       W_CobJuri = Creditos.Honorarios.                                                                                                                    */
    /*       IF NOT TTotales THEN                                                                                                                                */
    /*         ASSIGN  TotSdoAho = 0.                                                                                                                            */
    /*        /* FOR EACH ahorros WHERE ahorros.agencia = creditos.agencia AND ahorros.nit = creditos.nit                                                        */
    /*              AND ahorros.estado = 1 AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 AND ahorros.tip_ahorro EQ 1:                                      */
    /*              ASSIGN  TotSdoAho = TotSdoAho + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.                                                                  */
    /*         END.*/                                                                                                                                            */
    /*         WkNom = "".                                                                                                                                       */
    /*         DEFINE VARIABLE xpag LIKE clientes.cod_empresa INITIAL 0.                                                                                         */
    /*         FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit  NO-LOCK NO-ERROR.                                                                         */
    /*         IF AVAILABLE Clientes THEN DO:                                                                                                                    */
    /*            WkNom = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).                                               */
    /*            WkNom = SUBSTRING(Wknom,1,30).                                                                                                                 */
    /*            IF clientes.cod_empresa = ? THEN xpag = 0.                                                                                                     */
    /*               ELSE xpag = clientes.cod_empresa.                                                                                                           */
    /*            wfec = fec_nacimiento.                                                                                                                         */
    /*         END.                                                                                                                                              */
    /*         DISPLAY Creditos.Agencia        AT 1  FORMAT "999"                                                                                                */
    /*                 Creditos.Nit            AT 5  FORMAT "X(12)"                                                                                              */
    /*                 Creditos.Cod_Credito    AT 18 FORMAT "999"                                                                                                */
    /*                 WkNom AT 22 FORMAT "X(24)" WHEN AVAIL(Clientes)                                                                                           */
    /*                 TRIM(STRING(Creditos.Num_Credito))   AT 47 FORMAT "X(9)"                                                                                  */
    /*                 Creditos.Tasa           AT 57 FORMAT ">9.99"                                                                                              */
    /*                 Creditos.Plazo          AT 63 FORMAT "9999"                                                                                               */
    /*                 Creditos.Fec_Desembolso AT 68 FORMAT "99/99/99"                                                                                           */
    /*                 Creditos.Monto          AT 76 FORMAT ">>>,>>>,>>9"                                                                                        */
    /*                 Creditos.Sdo_Capital    AT 88 FORMAT "->>>,>>>,>>9"                                                                                       */
    /*                 W_Interes               AT 102 FORMAT "->>>,>>>,>>9"                                                                                      */
    /*                 Creditos.cuota          AT 116 FORMAT ">>>,>>>,>>9"                                                                                       */
    /*                 wfec                    AT 129 FORMAT "99/99/9999"                                                                                        */
    /*         WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.                                                                   */
    /*                                                                                                                                                           */
    /*         IF creditos.sdo_capital GT 0  THEN DO:                                                                                                            */
    /*             IF Creditos.Dias_Atraso EQ ? THEN                                                                                                             */
    /*                 W_DiaA = "0".                                                                                                                             */
    /*             ELSE                                                                                                                                          */
    /*                 W_DiaA = TRIM(STRING(Creditos.Dias_Atraso,"-zzzz9")).                                                                                     */
    /*                                                                                                                                                           */
    /*             IF Creditos.Fec_PagAnti EQ ? THEN ASSIGN vcFPagAnti = "No Reporta".                                                                           */
    /*                 ELSE ASSIGN vcFPagAnti = STRING(Creditos.Fec_PagAnti,"99/99/9999").                                                                       */
    /*             IF Creditos.Fec_Pago EQ ? THEN ASSIGN vcFPago = "No Reporta".                                                                                 */
    /*                 ELSE ASSIGN vcFPago = STRING(Creditos.Fec_PagAnti,"99/99/9999").                                                                          */
    /*             FIND FIRST anexos_clientes WHERE anexos_clientes.nit = creditos.nit NO-LOCK NO-ERROR.                                                         */
    /*             /*IF AVAILABLE(anexos_clientes) THEN DO:*/                                                                                                    */
    /*                IF creditos.cod_credito LT 700 THEN DO:                                                                                                    */
    /*                   IF (creditos.cod_credito EQ 12 OR  creditos.cod_credito EQ 13 OR  creditos.cod_credito EQ 14 OR                                         */
    /*                       creditos.cod_credito EQ 46 OR  creditos.cod_credito EQ 55 OR  creditos.cod_credito EQ 60 OR                                         */
    /*                       creditos.cod_credito EQ 65 OR  creditos.cod_credito EQ 528 OR creditos.cod_credito EQ 543 OR                                        */
    /*                       creditos.cod_credito EQ 544 OR creditos.cod_credito EQ 545 OR creditos.cod_credito EQ 546 OR                                        */
    /*                       creditos.cod_credito EQ 547 OR creditos.cod_credito EQ 548 OR creditos.cod_credito EQ 549 OR                                        */
    /*                       creditos.cod_credito EQ 550 OR creditos.cod_credito EQ 551 OR creditos.cod_credito EQ 552 OR                                        */
    /*                       creditos.cod_credito EQ 553 OR creditos.cod_credito EQ 554 OR creditos.cod_credito EQ 555  or                                       */
    /*                       creditos.cod_credito EQ 562 OR creditos.cod_credito EQ 563 OR creditos.cod_credito EQ 565 or                                        */
    /*                       creditos.cod_credito EQ 565 OR creditos.cod_credito EQ 566 OR creditos.cod_credito EQ 567 OR                                        */
    /*                       creditos.cod_credito EQ 568 OR creditos.cod_credito EQ 569) THEN W_Negocio = "Mult".                                                */
    /*                   ELSE W_Negocio = "Finan".                                                                                                               */
    /*                END.                                                                                                                                       */
    /*                ELSE W_Negocio = "Mult".                                                                                                                   */
    /*             /*END.*/                                                                                                                                      */
    /*             IF creditos.FOR_pago = 2 THEN W_pago = "Nomina".                                                                                              */
    /*             ELSE W_pago = "Caja".                                                                                                                         */
    /*           CREATE IEx.                                                                                                                                     */
    /*           ASSIGN Ct = Ct + 1                                                                                                                              */
    /*                IEx.NLinea = Ct                                                                                                                            */
    /*                IEx.Linea  = TRIM(STRING(Creditos.Agencia)) + Cma + TRIM(Creditos.Nit) + Cma + TRIM(STRING(Creditos.tip_Credito)) + Cma +                  */
    /*                             TRIM(STRING(Creditos.Cod_Credito)) + Cma + TRIM(WkNom) + Cma  + TRIM(STRING(Creditos.Num_Credito)) + Cma +                    */
    /*                             TRIM(STRING(Creditos.Tasa,"->>>>>.99999")) + Cma + TRIM(STRING(Creditos.Plazo,"9999")) + Cma +                                */
    /*                             TRIM(STRING(Creditos.Fec_Desembolso)) + Cma + TRIM(STRING(Creditos.Monto,"->>>>>>>>>>>9.99")) + Cma +                         */
    /*                             TRIM(STRING(Creditos.Sdo_Capital,"->>>>>>>>>>>9.99")) + Cma + TRIM(STRING(W_Interes)) + Cma +                                 */
    /*                             TRIM(STRING(W_IntMora)) + Cma + TRIM(STRING(W_CobJuri)) + Cma + TRIM(STRING(Creditos.Cuota,"->>>>>>>>>>>9.99")) + Cma +       */
    /*                             TRIM(STRING(Creditos.Provision)) + Cma + TRIM(STRING(Creditos.Provision_Interes)) + Cma + TRIM(Creditos.Pagare) + Cma +       */
    /*                             /* TRIM(STRING(Creditos.Per_Pago)) + Cma + */ W_DiaA + Cma + TRIM(Creditos.Categoria) + Cma +                                 */
    /*                             TRIM(Creditos.Categoriames) + Cma + TRIM(STRING(Creditos.Val_Atraso)) + Cma + TRIM(STRING(xpag)) + Cma + TRIM(W_pago) + Cma + */
    /*                             TRIM(vcFPagAnti) + Cma + TRIM(vcFPago) + Cma + TRIM(vcFPagAnti) + Cma +                                                       */
    /*                             TRIM(STRING(Creditos.Int_DifCobro)) + Cma + TRIM(STRING(Creditos.Int_MorCobrar)) + Cma + TRIM(W_Negocio).                     */
    /*         END.                                                                                                                                              */
    /*                                                                                                                                                           */
    /* /*IEx.Linea  = TRIM(STRING(Creditos.Agencia)) + Cma + TRIM(Creditos.Nit) + Cma + TRIM(STRING(Creditos.tip_Credito)) + Cma +                               */
    /*                             TRIM(STRING(Creditos.Cod_Credito)) + Cma + TRIM(WkNom) + Cma  +                                                               */
    /*                             TRIM(STRING(Creditos.Num_Credito)) + Cma + TRIM(STRING(Creditos.Tasa,"->>>>>.99999")) + Cma +                                 */
    /*                             TRIM(STRING(Creditos.Plazo,"9999")) + Cma + TRIM(STRING(Creditos.Fec_Desembolso)) + Cma +                                     */
    /*                             TRIM(STRING(Creditos.Monto,"->>>>>>>>>>>9.99")) + Cma +                                                                       */
    /*                             TRIM(STRING(Creditos.Sdo_Capital,"->>>>>>>>>>>9.99")) + Cma +                                                                 */
    /*                             TRIM(STRING(W_Interes)) + Cma + TRIM(STRING(Creditos.Cuota,"->>>>>>>>>>>9.99")) + Cma +                                       */
    /*                             TRIM(Creditos.Pagare) + Cma + TRIM(STRING(Creditos.Per_Pago)) + Cma +                                                         */
    /*                             TRIM(STRING(Creditos.Dias_Atraso,"zzzz9")) + Cma + TRIM(Creditos.Categoria) + Cma +                                           */
    /*                             TRIM(STRING(Creditos.Val_Atraso)) + Cma + TRIM(STRING(xpag)) + Cma + TRIM(STRING(FOR_pago)).*/                                */
    /*                                                                                                                                                           */
    /*                                                                                                                                                           */
    /*       ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto                                                                                                       */
    /*              TotRegAge = TotReg + 1                                                                                                                       */
    /*              TotMon    = TotMon + Creditos.Monto                                                                                                          */
    /*              TotSdo    = TotSdo + Creditos.Sdo_Capital                                                                                                    */
    /*              TotInt    = TotInt + W_Interes                                                                                                               */
    /*              TotPol    = TotPol + Creditos.Polizas                                                                                                        */
    /*              TotPro    = TotPro + Creditos.Provision                                                                                                      */
    /*              TotCos    = TotCos + Creditos.Costas                                                                                                         */
    /*              TotReg    = TotReg + 1                                                                                                                       */
    /*              CTotMon    = CTotMon + Creditos.Monto                                                                                                        */
    /*              CTotSdo    = CTotSdo + Creditos.Sdo_Capital                                                                                                  */
    /*              CTotInt    = CTotInt + W_Interes                                                                                                             */
    /*              TotAho    = TotAho + TotSdoAho.                                                                                                              */
    /*       /*IF LAST-OF(Creditos.categoria) THEN                                                                                                               */
    /*          DISPLAY "Total: " AT 1                                                                                                                           */
    /*                  CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"                                                                                            */
    /*                  CTotMon   AT 61                                                                                                                          */
    /*                  CTotSdo   AT 76                                                                                                                          */
    /*                  CTotInt   AT 92                                                                                                                          */
    /*          WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS.*/                                                                                       */
    /*   END.                                                                                                                                                    */
    /*   DISPLAY SKIP(1)                                                                                                                                         */
    /*     "Total de Registros  :"          AT 1                                                                                                                 */
    /*      TotReg                          AT 25                                                                                                                */
    /*     "Total x Entidad     : "         AT 42                                                                                                                */
    /*      TotMon                          AT 64                                                                                                                */
    /*      TotSdo                          AT 79                                                                                                                */
    /*      TotInt                          AT 94                                                                                                                */
    /*      totcos                          AT 108                                                                                                               */
    /*      totpol                          AT 121                                                                                                               */
    /*      totpro                          AT 139                                                                                                               */
    /*      totAho                          AT 158                                                                                                               */
    /*                                                                                                                                                           */
    /*   WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.                                                                                  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_DiasMora wWin 
PROCEDURE Informes_Creditos_DiasMora :
/*------------------------------------------------------------------------------
  Purpose:   Cartera por Dias-Mora
  Notes:     Agosto 17/06 - GAER  
------------------------------------------------------------------------------*/
DEFINE VAR WkNom   AS CHARACTER FORMAT "X(60)".
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".  
DEFINE VAR TotSdo  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFINE VAR TotInt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR PorMor  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR TotCarNM AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TaCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TeCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
 
DEFINE VAR Tot1     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot2     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot3     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot4     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot5     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot6     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot7     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot8     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot9     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR Tot10    AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFI   VAR IA       AS CHAR FORM "X(1)".
DEFINE VAR Dias_Vdos LIKE creditos.dias_atraso.
    
DEFINE VAR TApo LIKE Ahorros.Sdo_Disponible.

FOR EACH TTCreditos: DELETE TTCreditos. END.

ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0
       tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0 TotCarNM = 0
       TCreCla = 0 TaCreCla = 0 TeCreCla = 0.
ASSIGN TotSdo = 0
       TotMon = 0
       TotInt = 0.

/*    Creditos.For_Pago     GE FP1 AND
    Creditos.FOR_Pago     LE FP2 AND */

FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND  
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin AND 
           Creditos.Sdo_Capital  GT 0 NO-LOCK:

    CREATE TTCreditos.
    BUFFER-COPY Creditos TO TTCreditos.
    ASSIGN Dias_Vdos = Creditos.Dias_Atraso.
    FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    IF Creditos.FOR_Pago EQ 2 AND AVAIL (Clientes) AND Creditos.Val_Atraso GT 0 THEN DO:
       FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
       IF AVAIL(Empresas) THEN 
          ASSIGN Dias_Vdos = Dias_Vdos - Empresas.Dias_Gracia.
    END.
    IF dias_Vdos LT 0 THEN dias_Vdos = 0.
    ttcreditos.dias_atraso = dias_Vdos.

    IF      Dias_Vdos GT 1080 THEN
       ASSIGN DM = 1080
              NM = "Superior a 3 Años".
    ELSE IF Dias_Vdos GT 720 THEN
       ASSIGN DM = 720
              NM = "Superior a 2 Años Hasta 1080 Dias".
    ELSE IF Dias_Vdos GT 540 THEN
       ASSIGN DM = 540
              NM = "Superior a 18 Meses Hasta 720 Dias".
    ELSE IF Dias_Vdos GT 360 THEN
       ASSIGN DM = 360
              NM = "Superior a Un Año Hasta 540 Dias".
    ELSE IF Dias_Vdos GT 180 THEN
       ASSIGN DM = 180
              NM = "Superior a 6 Meses Hasta 360 Dias".
    ELSE IF Dias_Vdos GT 120 THEN
       ASSIGN DM = 120
              NM = "Desde 121 Dias Hasta 180 Dias".
    ELSE IF Dias_Vdos GT 90 THEN
       ASSIGN DM = 90
              NM = "Desde 91 Dias Hasta 120 Dias".
    ELSE IF Dias_Vdos GT 60 THEN
       ASSIGN DM = 60
              NM = "Desde 61 Dias Hasta 90 Dias".
    ELSE IF Dias_Vdos GT 30 THEN
       ASSIGN DM = 30
              NM = "Desde 31 Dias Hasta 60 Dias".
    ELSE 
       ASSIGN DM = 0
              NM = "Desde 0 Dias Hasta 30 Dias".
END.

FOR EACH TTCreditos NO-LOCK BREAK BY TTCreditos.Agencia BY TTCreditos.DM BY TTCreditos.Nit: 
      IF FIRST-OF(TTCreditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ TTCreditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY CAPS(Agencias.Nombre) AT 1  FORMAT "X(30)"
            WITH FRAME TitCartAge WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
         ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCartAge2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      END.
      
      IF FIRST-OF(TTCreditos.DM) THEN DO:
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.
         DISPLAY "*****"
                 TTCreditos.NM
                 "*****"
             WITH FRAME FT111 WIDTH 120 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
      END.

      CASE TTCreditos.Tip_Credito:
        WHEN 1 THEN W_TipPdt = "Consumo".
        WHEN 2 THEN W_TipPdt = "Comercial".
        WHEN 3 THEN W_TipPdt = "Hipotecario".
        WHEN 4 THEN W_TipPdt = "Convenios".
      END CASE.

      j = j + 1.
      RUN Progreso.

      W_Interes = TTCreditos.INT_Corriente + TTCreditos.INT_DifCobro + TTCreditos.INT_MorCobrar - TTCreditos.INT_Anticipado.
      ASSIGN TotSdoAge = TotSdoAge + TTCreditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + TTCreditos.Monto
             TotSdo    = TotSdo + TTCreditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + TTCreditos.Monto
             CTotSdo    = CTotSdo + TTCreditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             Tot1       = Tot1 + TTCreditos.Monto
             Tot2       = Tot2 + TTCreditos.Sdo_Capital
             Tot3       = Tot3 + W_Interes
             Tot4       = Tot4 + TTCreditos.INT_MorCobrar
             TCreCla    = TCreCla + 1
             IA         = "".

      IF TTCreditos.Abogado THEN
         IA = "A".
      WkNom = "".
      FIND FIRST Clientes WHERE Clientes.Nit EQ TTCreditos.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN WKNom = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +  " " + TRIM(Clientes.Nombre).
      
      DISPLAY TTCreditos.Agencia         FORMAT "999"
              TTCreditos.Nit             FORMAT "X(14)"
              TTCreditos.Num_Credito     
              WkNom                     FORM "X(12)" WHEN AVAIL(Clientes)
              TTCreditos.Tasa           FORMAT ">9.99"
              TTCreditos.Plazo          FORMAT "9999"
              TTCreditos.Fec_Desembolso FORMAT "99/99/99"
              TTCreditos.Val_Desembolso FORMAT "->>>,>>>,>>9"
              TTCreditos.Sdo_Capital     FORMAT "->>>,>>>,>>9"
              W_Interes                  FORMAT "->,>>>,>>9"
              TTCreditos.Dias_Atraso     FORMAT ">>,>>9" 
              IA                         SKIP(0.5)
          WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
          CREATE IEx.
          ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                 IEx.Linea  = STRING(TTCreditos.Agencia) + cma + TTCreditos.Nit + cma + STRING(TTCreditos.Num_Credito) + cma +
                              STRING(TTCreditos.Num_Credito) + cma + WkNom + cma + STRING(TTCreditos.Tasa,"->>>9.999") + cma +
                              STRING(TTCreditos.Plazo,"9999") + cma + STRING(TTCreditos.Fec_Desembolso) + cma +
                              STRING(TTCreditos.Val_Desembolso,"->>>>>>>>>>>9.99") + cma + STRING(TTCreditos.Sdo_Capital,"->>>>>>>>>>>9.99") + cma +
                              STRING(W_Interes,"->>>>>>>>>>>9.99") + STRING(TTCreditos.Dias_Atraso,"9999") + cma + STRING(ia).

/*anterior
      DISPLAY TTCreditos.Agencia        AT 1  FORMAT "999"
              TTCreditos.Nit            AT 5  FORMAT "X(14)"
              TTCreditos.Num_Credito   AT 20 
              TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
              " " + TRIM(Clientes.Nombre)        AT 30 FORM "X(35)" WHEN AVAIL(Clientes)
              TTCreditos.Tasa           AT 66 FORMAT ">9.99"
              TTCreditos.Plazo          AT 72 FORMAT "9999"
              TTCreditos.Fec_Desembolso AT 77 FORMAT "99/99/9999"
              TTCreditos.Val_Desembolso AT 87 FORMAT "->>>,>>>,>>9"
              TTCreditos.Sdo_Capital    AT 103 FORMAT "->>>,>>>,>>9"
              W_Interes                 AT 115 FORMAT "->>>,>>>,>>9"
              TTCreditos.Dias_Atraso    AT 127 FORMAT ">>,>>9" 
              IA                        AT 136 SKIP(0)
          WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
*/
      IF LAST-OF(TTCreditos.DM)  THEN DO:
         DISPLAY "No.de Crèditos :" AT 1
                 TCreCla   AT 18 FORMAT ">>>>,>>9"
                 TTCreditos.NM   AT 30 FORMAT "X(40)"
                 CTotMon   AT 61 FORMAT ">>,>>>,>>>,>>9"
                 CTotSdo   AT 75 FORMAT ">>,>>>,>>>,>>9"
                 CTotInt   AT 91 FORMAT ">>,>>>,>>>,>>9"
                  SKIP(1)
/*           003 42133724       000009925 RESTREPO ESC 24.41 0019 16/09/05      590,519      329,293    55,863    106
                      1         2         3         4         5         6         7         8         9         1
             12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
           WITH FRAME F_TotCar WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.

         ASSIGN TaCreCla = TaCreCla + TCreCla.

         ASSIGN CTotMon = 0 
                CTotSdo = 0 
                CTotInt = 0 
                TCreCla = 0.         
      END.

      IF LAST-OF(TTCreditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ TTCreditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY SKIP(1) "TOTALES:" AT 1 CAPS(Agencias.Nombre) AT 15  FORMAT "X(30)" SKIP
                 "No.de Crèditos :" AT 1
                 TaCreCla   AT 18 FORMAT ">>>>,>>9"
                 Tot1       AT 59 FORMAT ">>,>>>,>>>,>>9"
                 Tot2       AT 75 FORMAT ">>,>>>,>>>,>>9"
                 Tot3       AT 91 FORMAT ">>,>>>,>>>,>>9"                  
                 SKIP(1)
         WITH FRAME F_TotCar2 WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TeCreCla = TeCreCla + TaCreCla.
         /*ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.*/
         ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 TaCreCla = 0.
      END.
  END.

  DISPLAY SKIP(1)    
    "Total x Entidad  : "     AT 18 SKIP
    "No.de Crèditos :" AT 1
     TeCreCla                 AT 18
     TotMon                   AT 59
     TotSdo                   AT 75
     TotInt                   AT 91  FORMAT "->,>>>,>>>,>>9"           
  WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Estados wWin 
PROCEDURE Informes_Creditos_Estados :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
ASSIGN TotSdo = 0.

DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".  
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Estado       EQ EstadoC
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit BY Creditos.Cod_Credito: 

      IF EstadoC              EQ 2 AND 
      Creditos.Fec_Aprobacion GE FecIni AND 
      Creditos.Fec_Aprobacion LE FecFin THEN.
      ELSE IF EstadoC EQ 2 THEN NEXT.

      IF EstadoC              EQ 3 AND 
      Creditos.Fec_UltPago    GE FecIni AND 
      Creditos.Fec_UltPago    LE FecFin THEN.
      ELSE IF EstadoC EQ 3 THEN NEXT.

      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      W_Interes = Creditos.INT_Corrientes + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      FIND Usuarios WHERE Usuarios.Usuario EQ Creditos.Usuario NO-LOCK NO-ERROR.
      IF AVAILABLE Usuarios THEN W_Nus = Usuarios.Nombre.
      ELSE W_Nus = "Usuario no existe".
      DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
              Creditos.Nit            AT 5  FORMAT "X(14)"
              Creditos.Cod_Credito    AT 20 FORMAT "999"
              W_TipPdt                AT 24 FORMAT "X(5)"
              Creditos.Num_Credito    AT 30 FORMAT "999999999"
              Creditos.Tasa           AT 41 FORMAT ">9.99"
              Creditos.Plazo          AT 47 FORMAT "9999"
              Creditos.Fec_Aprobacion AT 52 FORMAT "99/99/9999"
              Creditos.Monto          AT 64 FORMAT ">,>>>,>>>,>>9"
              Creditos.Sdo_Capital    AT 79 FORMAT ">,>>>,>>>,>>9"
              W_Interes               AT 94 FORMAT "->,>>>,>>>,>>9"
              W_Nus                   AT 111 FORMAT "X(12)"
      WITH FRAME FEstados WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_interes
             TotReg    = TotReg + 1.
      
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 61
     TotSdo                          AT 77
     TotInt                          AT 92
  WITH FRAME ttEstados WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Instancias wWin 
PROCEDURE Informes_Creditos_Instancias :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
ASSIGN TotSdo = 0.
   FOR EACH Instancias WHERE 
            Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros,1,1)) AND
            Instancias.Estado EQ 1 NO-LOCK BREAK BY Instancias.Instancia:
       IF FIRST-OF(Instancias.Instancia) THEN DO:
          W_NIn = "Instancia: " + STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia.
          DISPLAY SKIP(1) W_Nin     AT 1 WITH FRAME F_TitIns WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
       END.
       FOR EACH Mov_Instancias WHERE 
                Mov_Instancias.Instancia EQ Instancias.Instancia AND
                Mov_Instancias.Estado    EQ NO NO-LOCK BREAK BY Mov_Instancias.Instancia BY Mov_Instancias.Fec_Ingreso:
           W_Nus = "Usuario Inconsistente".
           FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancia.Usuario NO-LOCK NO-ERROR.
           IF AVAILABLE Usuarios THEN W_Nus = Usuarios.Nombre.
           W_Vig = TODAY - Mov_Instancias.Fec_Ingreso.
           FIND Creditos WHERE 
                Creditos.Num_Credito EQ DECIMAL(Mov_Instancias.Cuenta) AND
                Creditos.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
           IF AVAILABLE Creditos THEN DO:
              RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
              W_Interes = Creditos.INT_Corrientes + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
              j = j + 1.
              RUN Progreso.
              DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
                      Creditos.Nit            AT 5  FORMAT "X(14)"
                      Creditos.Cod_Credito    AT 20 FORMAT "999"
                      W_TipPdt                AT 24 FORMAT "X(5)"
                      TRIM(Creditos.Pagare)   AT 30 FORMAT "X(9)"
                      Creditos.Tasa           AT 41 FORMAT ">9.99"
                      Creditos.Plazo          AT 47 FORMAT "9999"
                      Creditos.Fec_Aprobacion AT 52 FORMAT "99/99/9999"
                      Creditos.Monto          AT 64 FORMAT ">,>>>,>>>,>>9"
                      Creditos.Sdo_Capital    AT 79 FORMAT ">,>>>,>>>,>>9"
                      W_Interes               AT 94 FORMAT "->,>>>,>>>,>>9"
                      W_Nus                   AT 110 FORMAT "X(15)"
                      W_Vig                   AT 127 FORMAT "999"
              WITH FRAME FInfIns WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
                     TotRegAge = TotReg + 1
                     TotMon    = TotMon + Creditos.Monto
                     TotSdo    = TotSdo + Creditos.Sdo_Capital
                     TotInt    = TotInt + W_Interes
                     TotReg    = TotReg + 1.
           END.
       END.
   END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 61
     TotSdo                          AT 77
     TotInt                          AT 92
  WITH FRAME ttInstancias WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Reestructurados wWin 
PROCEDURE Informes_Creditos_Reestructurados :
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt    AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR CTotCos    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPol    AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.
DEFINE VAR CTotPro    AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotMon     AS DECIMAL FORMAT "->>>,>>>,>>9".  
DEFINE VAR TotInt     AS DECIMAL FORMAT "->>>,>>>,>>9".  

DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCos     AS DECIMAL FORMAT ">>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPol     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotPro     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  

DEFINE VAR TotAvi     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotApo     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAte     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotCtr     AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0.  

DEFINE VAR TotSdoAvi  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoApo  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoAte  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoCtr  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  

DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0.
DISPLAY "AGE NIT            PDT TIP  PAGARE    TASA  PLAZO  FEC.DESEMBO     MONTO      SDO.CAPITAL      INTERESES        COSTAS      POLIZAS    D.AT   PROVISION CPA COMPR  AHO.AVISTA    AHO.CONTRACTUAL  AHO.ATERM      APORTES        CUOTA "
    WITH FRAME F_Encabe WIDTH 250.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Reestructurado EQ 1 
           NO-LOCK BREAK BY Creditos.Agencia BY creditos.nit BY Creditos.Cod_Credito:
      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      FIND FIRST Cobros WHERE
                 Cobros.Num_Credito EQ Creditos.Num_Credito AND
                 Cobros.Nit         EQ Creditos.Nit AND
                 Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
      W_Compromiso = "".
      IF AVAILABLE Cobros THEN W_Compromiso = "x".
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      IF NOT TTotales THEN 
        ASSIGN  TotSdoAvi = 0 TotSdoCtr = 0 TotSdoAte = 0 TotSdoApo = 0.
        FOR EACH ahorros WHERE 
                 Ahorros.nit      EQ creditos.nit
             AND Ahorros.estado   EQ 1
             AND (Ahorros.Sdo_disponible + Ahorros.Sdo_Canje) GT 0: 
            CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN TotSdoAvi = TotSdoAvi + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 2 THEN TotSdoCtr = TotSdoCtr + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 3 THEN TotSdoAte = TotSdoAte + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
                WHEN 4 THEN TotSdoApo = TotSdoApo + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
            END CASE.
             
        END.
        DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
                Creditos.Nit            AT 5  FORMAT "X(14)"
                Creditos.Cod_Credito    AT 20 FORMAT "999"
                W_TipPdt                AT 24 FORMAT "X(4)"
                TRIM(Creditos.Pagare)   AT 29 FORMAT "X(9)"
                Creditos.Tasa           AT 40 FORMAT ">9.99"
                Creditos.Plazo          AT 47 FORMAT "9999"
                Creditos.Fec_Desembolso AT 52 FORMAT "99/99/9999"
                Creditos.Monto          AT 64 FORMAT ">>>,>>>,>>9"
                Creditos.Sdo_Capital    AT 79 FORMAT ">>>,>>>,>>9"
                W_Interes               AT 94 FORMAT "->>>,>>>,>>9"
                Creditos.Costas         AT 108 FORMAT ">>>,>>>,>>9"
                Creditos.Polizas        AT 121 FORMAT ">>>,>>>,>>9"
                Creditos.Dias_Atraso    AT 134 FORMAT ">>,>>9"
                Creditos.Provision      AT 142 FORMAT ">>,>>>,>>9"
                Creditos.Cuo_Pagadas    AT 154 FORMAT ">9"
                W_Compromiso            AT 158 FORMAT "X"
                TotSdoAvi               AT 161 FORMAT "->,>>>,>>>,>>9"  
                TotSdoCtr               AT 176 FORMAT "->,>>>,>>>,>>9"  
                TotSdoAte               AT 192 FORMAT "->,>>>,>>>,>>9"  
                TotSdoApo               AT 207 FORMAT "->,>>>,>>>,>>9"  
                Creditos.cuota          AT 223 FORMAT ">>>,>>>,>>9"  
                Creditos.Reestructurado AT 236 FORMAT ">>9"
        WITH FRAME FCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotPol    = TotPol + Creditos.Polizas
             TotPro    = TotPro + Creditos.Provision
             TotCos    = TotCos + Creditos.Costas
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             TotAvi     = TotAvi + TotSdoAvi
             TotCtr     = TotCtr + TotSdoCtr
             TotAte     = TotAte + TotSdoAte
             TotApo     = TotApo + TotSdoApo.
      /*IF LAST-OF(Creditos.categoria) THEN
         DISPLAY "Total: " AT 1
                 CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"
                 CTotMon   AT 61
                 CTotSdo   AT 76
                 CTotInt   AT 92 
         WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS.*/
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  
     TotInt                          AT 94  
     totcos                          AT 108 
     totpol                          AT 121 
     totpro                          AT 142 
     totAvi                          AT 161 FORMAT "->,>>>,>>>,>>9"  
     TotCtr                          AT 176 FORMAT "->,>>>,>>>,>>9"  
     TotAte                          AT 192 FORMAT "->,>>>,>>>,>>9"  
     TotApo                          AT 207 FORMAT "->,>>>,>>>,>>9"  
  WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Vencimiento wWin 
PROCEDURE Informes_Creditos_Vencimiento :
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt    AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotInt     AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
ASSIGN TotSdo = 0
       TotSdoAho = 0.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Estado       EQ 2      AND
           Creditos.Fec_Pago     GE FecIni AND
           Creditos.Fec_Pago     LE FecFin NO-LOCK
         BREAK BY Creditos.Categoria BY Creditos.Agencia BY Creditos.Cod_Credito BY Creditos.Fec_Pago:
     /*,EACH PlanPagos WHERE   PlanPagos.Agencia     EQ Creditos.Agencia        AND
                             PlanPagos.Tip_Credito EQ Creditos.Tip_Credito    AND 
                             PlanPagos.Cod_Credito EQ Creditos.Cod_Credito    AND
                             PlanPagos.Num_Credito EQ Creditos.Num_Credito    AND
                             PlanPagos.Nit         EQ Creditos.Nit            AND 
                             PlanPagos.Fec_Vcto    GE FecIni                  AND
                             PlanPagos.Fec_Vcto    LE FecFin                  AND
                             PlanPagos.Nro_Cuota   EQ PlanPagos.PLAZO             
          NO-LOCK BREAK BY Creditos.Categoria BY Creditos.Agencia BY Creditos.Cod_Credito BY PlanPagos.Fec_Vcto: */
      
      IF FIRST-OF(Creditos.Categoria) THEN DO:
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Creditos.Cod_Califica NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY CAPS(Varios.Descripcion) AT 1  FORMAT "X(30)"
            WITH FRAME TitCart WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
         ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCart2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      END.
      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.
      FIND FIRST Cobros WHERE
                 Cobros.Num_Credito EQ Creditos.Num_Credito AND
                 Cobros.Nit         EQ Creditos.Nit AND
                 Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
      W_Compromiso = "".
      IF AVAILABLE Cobros THEN W_Compromiso = "x".
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.

      IF NOT TTotales THEN 
        ASSIGN  TotSdoAho = 0.
        FOR EACH ahorros WHERE ahorros.agencia = creditos.agencia AND ahorros.nit = creditos.nit
             AND ahorros.estado = 1 AND Ahorros.Sdo_disponible + Ahorros.Sdo_Canje GT 0 AND ahorros.tip_ahorro EQ 1: 
             ASSIGN  TotSdoAho = TotSdoAho + Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.
        END.
        
        
        DISPLAY Creditos.Agencia        AT 1   FORMAT "999"
                Creditos.Nit            AT 5   FORMAT "X(14)"
                W_TipPdt                AT 21  FORMAT "X(4)"
                Creditos.Num_Credito    AT 26  FORMAT "999999999"
                Creditos.Tasa           AT 36  FORMAT ">9.99"
                Creditos.Plazo          AT 42  FORMAT ">>9"
                Creditos.Fec_Pago       AT 47  FORMAT "99/99/9999"
                Creditos.Sdo_Capital    AT 58  FORMAT ">>>,>>>,>>9"
                Creditos.Dias_Atraso    AT 76  FORMAT ">>>9"
                TotSdoAho               AT 81  FORMAT "->,>>>,>>>,>>9"
                Creditos.cuota          AT 98  FORMAT ">>,>>>,>>9"
                WITH FRAME FCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes.
      IF LAST-OF(Creditos.categoria) THEN
         DISPLAY "Total: " AT 1
                 CAPS(Varios.Descripcion) AT 10 FORMAT "X(30)"
                 CTotSdo   AT 56
        WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total Entidad: "                AT 42
     TotSdo                          AT 56
  WITH FRAME ttCartera WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Empresas wWin 
PROCEDURE Informes_Empresas :
Listado = W_PathSpl + "Empresas-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR NomEmp AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.

  IF Cmb_BEmpresas:SCREEN-VALUE IN FRAME F_Basicos EQ "Asociados de Empresa" THEN DO:
     RUN Empresa_Pdctos. 
     RETURN.
  END.

{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Empresas: " + Cmb_BEmpresas
              + " - Fec.Inicial: " + STRING(FecIni) + " - Fec.Final: " + STRING(FecFin) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BUsuarios:
    WHEN "Pagos" THEN  
        W_EncColumna = "Cod.Empresa  Nit Empresa   Nom.Empresa                               Fec.Ult.Pago".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Empresas WHERE 
           Empresas.Agencia GE AgeIni AND
           Empresas.Agencia LE AgeFin
           NO-LOCK BREAK BY Empresas.Cod_Empresa:  
      j = j + 1.
      RUN Progreso.
      NomEmp = "No se encuentra en clientes".
      FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN
         NomEmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      CASE Cmb_BEmpresas:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Pagos" THEN DO:
          DO i = 1 TO 8 BY 1:
             IF Empresas.Fec_UltPago[i] GE FecIni AND
                Empresas.Fec_UltPago[i] LE FecFin THEN DO:
                DISPLAY 
                  Empresas.Cod_Empresa  AT 1 FORMAT "9999"
                  Empresas.Nit          AT 7 FORMAT "X(14)"
                  NomEmp                AT 30 FORMAT "X(50)"
                  Empresas.Fec_UltPago[i] AT 85 FORMAT "99/99/9999"
                WITH FRAME F_E1 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
                RETURN.
             END.
          END.
        END.
        WHEN "No Pagos" THEN DO:
          DO i = 1 TO 8 BY 1:
             IF Empresas.Fec_UltPago[i] LT FecIni OR
                Empresas.Fec_UltPago[i] GT FecFin THEN DO:
                DISPLAY 
                  Empresas.Cod_Empresa  AT 1 FORMAT "9999"
                  Empresas.Nit          AT 7 FORMAT "X(14)"
                  NomEmp                AT 30 FORMAT "X(50)"
                  Empresas.Fec_UltPago[i] AT 85 FORMAT "99/99/9999"
                WITH FRAME F_E2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
                RETURN.
             END.
          END.
        END.
      END CASE.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Garantias wWin 
PROCEDURE Informes_Garantias :
Listado = W_PathSpl + "Garantias-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(61)"
/*                  1         2         3         4         5         6         7         8         9         1
           12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
INITIAL "AGE NIT                 ".
DEFINE VAR W_Cupo  LIKE Creditos.Monto.
DEFINE VAR W_Monto LIKE Creditos.Monto.
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipGar AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".

ASSIGN J = 0 k = 0.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Solicitudes: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  
  CASE Cmb_BGarantias:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Garantias Admisibles" THEN
        W_EncColumna = "Ag.    Nit             PAGARE      NUM.CREDIT  VLR ASEGUR. IDENT.BIEN   NOMBRE BIEN   VALOR DEL BIEN  VALOR.GARANTIA     TIPO".
    WHEN "Vencimiento Impuestos" THEN
       W_EncColumna = WEnc1 + "NUM.SOLIC   NUM.CREDIT   ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA    FEC.VEN.IMPUESTO".
    WHEN "Vencimientos Poliza" THEN
       W_EncColumna = WEnc1 + "NUM.SOLIC   NUM.CREDIT   ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA    FEC.VENCIMIENTO".
    WHEN "Cupos" THEN          
                          /*       3         4         5         6         7         8         9         1         2
                               6789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789*/
       W_EncColumna = WEnc1 + "ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA      MONTO.CREDITO   CUPO DISPONIBLE.".
  END CASE.
  VIEW FRAME F-Encabezado.
  FOR EACH Garantias WHERE 
           Garantias.Agencia      GE AgeIni AND
           Garantias.Agencia      LE AgeFin
           NO-LOCK BREAK BY Garantias.Agencia BY Garantias.Nit BY Garantias.Cod_Credito: 
      j = j + 1.
      CASE Garantias.Tipo_Garantia:
        WHEN 1 THEN W_TipGar = "Propiedad".
        WHEN 2 THEN W_TipGar = "Vehículo".
        WHEN 3 THEN W_TipGar = "Inversión".
      END CASE.
      RUN Progreso.
      CASE Cmb_BGarantias:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Garantias Admisibles" THEN DO:
           IF Garantias.Estado EQ 1 THEN DO:
             FIND creditos WHERE Creditos.Agencia EQ Garantias.Agencia  AND
                      Creditos.Num_Credito EQ Garantias.Num_Credito NO-LOCK NO-ERROR.
             IF AVAILABLE(creditos) THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Creditos.Nit                   AT 5  FORMAT "X(14)"
                      Garantias.Pagare               AT 25 FORMAT "X(9)"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Val_asegurado        AT 48 FORMAT ">>>,>>>,>>9"
                      Garantias.Identificacion_Bien  AT 61 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 75 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 102 FORMAT ">>>,>>>,>>>,>>9"
                      W_TipGar                       AT 119 FORMAT "X(10)"
              WITH FRAME F_G1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Asegurado
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Asegurado
                     TotReg    = TotReg + 1.
             END.
           END.
        END.
        WHEN "Vencimientos Poliza" THEN DO:
           IF Garantias.Fec_FinSeguro NE ? AND
              Garantias.Fec_FinSeguro GE FecIni AND
              Garantias.Fec_FinSeguro LE FecFin AND
              Garantias.Estado EQ 1 THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Garantias.Nit                  AT 5  FORMAT "X(14)"
                      Garantias.Num_Solicitud        AT 25 FORMAT "999999999"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Identificacion_Bien  AT 50 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 65 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 94 FORMAT ">>>,>>>,>>>,>>9"
                      GarantIas.Fec_FinSeguro        AT 112 FORMAT "99/99/9999"
              WITH FRAME F_G2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Bien
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Cupos" THEN DO:
           IF Garantias.Estado EQ 1 THEN DO:
              IF Garantias.Num_Credito GT 0 THEN DO:
                 FIND Creditos WHERE Creditos.Num_Credito EQ Garantias.Num_Credito AND
                                     Creditos.Cod_Credito EQ Garantias.Cod_Credito AND
                                     Creditos.Tip_Credito EQ Garantias.Tip_Credito AND
                                     Creditos.Nit         EQ Garantias.Nit NO-LOCK NO-ERROR.
                 IF AVAILABLE Creditos  THEN DO:
                    ASSIGN W_Monto = Creditos.Monto
                           W_Cupo  = Garantias.Val_Bien - Creditos.Monto.
                    DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                            Garantias.Nit                  AT 5  FORMAT "X(14)"
                            Garantias.Identificacion_Bien  AT 25 FORMAT "X(12)"
                            Garantias.Nom_Bien             AT 40 FORMAT "X(25)"
                            Garantias.Val_Bien             AT 70 FORMAT ">>>,>>>,>>>,>>9"
                            W_Monto                        AT 87 FORMAT ">>>,>>>,>>>,>>9"
                            W_Cupo                         AT 105 FORMAT ">>>,>>>,>>>,>>9"
                    WITH FRAME F_G3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
                    ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                           TotRegAge = TotReg + 1
                           TotSdo    = TotSdo + Garantias.Val_Bien
                           TotReg    = TotReg + 1.
                 END.
                 ELSE 
                   DISPLAY Garantias.Nit Garantias.Cod_Credito Garantias.Tip_Credito
                   WITH FRAME F_Gi WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              END.
           END.
        END.
        WHEN "Vencimiento Impuestos" THEN DO:
           IF Garantias.Fec_FinSeguro NE ? AND
              Garantias.Fec_VctoImpuesto GE FecIni AND
              Garantias.Fec_VctoImpuesto LE FecFin AND
              Garantias.Estado EQ 1 THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Garantias.Nit                  AT 5  FORMAT "X(14)"
                      Garantias.Num_Solicitud        AT 25 FORMAT "999999999"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Identificacion_Bien  AT 50 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 65 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 94 FORMAT ">>>,>>>,>>>,>>9"
                      Garantias.Fec_VctoImpuesto     AT 112 FORMAT "99/99/9999"
              WITH FRAME F_G4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Bien
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Garantias.Agencia) THEN
         DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Garantias.Agencia                 AT 30 FORMAT "999"
                ": "                             AT 35
                 TotRegAge                       AT 40
                "Tot.Valor de las Garantias:"    AT 60
                 TotSdoAge                       AT 89
        WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Tot.Valor de las Garantias:"    AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Solicitudes wWin 
PROCEDURE Informes_Solicitudes :
/*
-------------------------------------------------------------------*/
DEFI VAR Obs AS CHAR FORM "X(140)".

Listado = W_PathSpl + "InfSol-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin.  

DEFI   VAR W_AprTodas  AS INTEG FORM "9".
DEFI   VAR W_AprEstado AS INTEG FORM "9".
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(61)" 
/*                  1         2         3         4         5         6
           1234567890123456789012345678901234567890123456789012345678901234567890*/
INITIAL "AGE NIT            PDT TIP.PDT        NUM.SOLIC  TASA  PLAZO ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".

ASSIGN J = 0 k = 0.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Solicitudes: " + Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Aprobados" THEN
        W_EncColumna = WEnc1 + " FEC.RADICACION FEC.APROBACION  FEC.DESEMBOL.  MONTO  COD.USU".  
/*         W_EncColumna = WEnc1 + " FEC.APROBACION     MONTO VIGENCIA COD.USU".  */
    WHEN "Condicionadas" THEN
        W_EncColumna = "CED./NIT     PTO SOLICITUD F-RADICA.      MONTO SOLIC.  C.USU   OBSERVACION".
/*         W_EncColumna = "CED./NIT     PTO SOLICITUD   FEC.RADICA.   F-SOLIC.   MONTO SOLIC. C.USU  OBSERVACION".  */
    OTHERWISE W_EncColumna = WEnc1 + " FEC.RADICA.    MONTO          VIG. C.USU".
/*     OTHERWISE W_EncColumna = WEnc1 + " FEC.RADICA. FEC.INGR.      MONTO        VIG.   C.USU".  */
  END CASE.
  VIEW FRAME F-Encabezado.

  IF Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos EQ "Negadas" THEN DO:
     RUN InfSoli_Negadas.
     RETURN.
  END.

  IF Cmb_BSolicitudes:SCREEN-VALUE EQ "Aprobadas" THEN DO:
     MESSAGE "Todas la APROBADAS, Teclee SI" SKIP
             "NO-DESEMBOLSADAS,   Teclee NO" SKIP
             "YA-DESEMBOLSADAS,   Teclee CANCELAR"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "FILTRO SOLICITADO"
                       UPDATE W_SolA AS LOGICAL.
     IF W_SolA THEN
        ASSIGN W_EncColumna = "Todas la APROBADAS Desde : " + STRING(FecIni) + " Hasta : " + STRING(W_FecFin)
               W_AprTodas  = 1
               W_AprEstado = 2.
     ELSE IF NOT W_SolA THEN
        ASSIGN W_EncColumna = "NO-DESEMBOLSADAS Desde : " + STRING(FecIni) + " Hasta : " + STRING(W_FecFin)
               W_AprTodas  = 1
               W_AprEstado = 1.
     ELSE DO:
        ASSIGN W_EncColumna = "YA-DESEMBOLSADAS Desde : " + STRING(FecIni) + " Hasta : " + STRING(W_FecFin)
              W_AprTodas  = 2
              W_AprEstado = 2.
        RUN Aprob_YaDesemb.  /*Marzo 17/05 GAER*/
        RETURN.
     END.

     IF W_AprTodas EQ 1 THEN /*Menos las ya Desembolsadas*/
        FOR EACH Creditos WHERE Creditos.Agencia      GE AgeIni AND
                             Creditos.Agencia      LE AgeFin AND
                             Creditos.Cod_Credito  GE ProIni AND
                             Creditos.Cod_Credito  LE ProFin AND
                             Creditos.Fec_Aprob    GE FecIni AND
                             Creditos.Fec_Aprob    LE FecFin AND
                             Creditos.Estado       GE W_AprTodas AND
                             Creditos.Estado       LE W_AprEstado NO-LOCK
                BREAK BY Creditos.Agencia BY Creditos.Fec_Aprob:
        
         /******* Fecha de Radicacion de Solicitudes ****/
         ASSIGN W_FecRetRad = DATE ("  /  /  ").
         FIND FIRST Solicitud WHERE Solicitud.Nit           EQ Creditos.Nit AND
                                    Solicitud.Num_Solicitud EQ Creditos.Num_Solicitud AND
                                    Solicitud.Estado        EQ 2
                                    NO-ERROR.
         IF AVAILABLE Solicitud THEN
            ASSIGN W_FecRetRad =  Solicitud.Fec_Solicitud. 
         /*****************************************************/
         DISPLAY Creditos.Agencia        LABEL "Ag."           FORMAT "999"
                 Creditos.Nit            LABEL "Ced./Nit"      FORMAT "X(12)"
                 Creditos.Cod_Credito    LABEL "Pdto"          FORMAT "999"
                 Creditos.Num_Solicitud  LABEL "Num-Solicit"   FORMAT "999999999"
                 Creditos.Tasa           LABEL "Tasa"          FORMAT ">9.9999"
                 Creditos.Plazo          LABEL "Plazo"         FORMAT "9999"
                 W_FecRetRad             LABEL "Fec-Radica."   FORMAT "99/99/9999"
                 Creditos.Fec_Aprobacion LABEL "Fec-Aprobac"   FORMAT "99/99/9999"
                 Creditos.Fec_Desemb     LABEL "Fec-Desemb."   FORMAT "99/99/9999" WHEN Creditos.Estado EQ 2
                 Creditos.Monto          LABEL "Monto Crédito" FORMAT ">,>>>,>>>,>>9"
                 Creditos.Usuario        LABEL "Usuario"
              WITH DOWN FRAME F_AprCred WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

         ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
                TotRegAge = TotRegAge + 1
                TotSdo    = TotSdo    + Creditos.Monto
                TotReg    = TotReg    + 1.

         IF last-of(Creditos.Agencia) THEN DO:
            DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Creditos.Agencia                AT 30 FORMAT "999"
                 ": "                            AT 35
                 TotRegAge                       AT 40
                "Tot.Sdo.a Prestar Agencia:"     AT 60
                 TotSdoAge                       AT 89 SKIP(1)
                WITH FRAME tApro1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

            ASSIGN TotRegAge = 0
                   TotSdoAge = 0.
         END.
     END.
  END.
  
  ELSE FOR EACH Solicitud WHERE 
           Solicitud.Agencia      GE AgeIni AND
           Solicitud.Agencia      LE AgeFin AND
           Solicitud.Cod_Credito  GE ProIni AND
           Solicitud.Cod_Credito  LE ProFin 
           /*(Solicitud.Estado       EQ 1 OR Solicitud.Estado EQ 4)*/
           NO-LOCK BREAK BY Solicitud.Agencia BY Solicitud.Nit BY Solicitud.Cod_Credito: 
      CASE Solicitud.Tip_Credito:
        WHEN 1 THEN W_TipPdt = "Consumo".
        WHEN 2 THEN W_TipPdt = "Comercial".
        WHEN 3 THEN W_TipPdt = "Hipotecario".
        WHEN 4 THEN W_TipPdt = "MicroCredito".
      END CASE.
      CASE Solicitud.FOR_Pago:
        WHEN 1 THEN W_ForPag = "Nomina".
        WHEN 2 THEN W_ForPag = "Caja".
        WHEN 3 THEN W_ForPag = "DB.Automatic".
      END CASE.
      ASSIGN W_Vigen = TODAY - Solicitud.Fec_Solicitud.
/*              W_FecRetRad = DATE ("  /  /  ").  */
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Básico" THEN DO:
           IF Solicitud.Monto GE ValIni AND
              Solicitud.Monto LE ValFin AND 
              (Solicitud.Estado       EQ 1 OR Solicitud.Estado EQ 4) AND
              Solicitud.Fec_Aprobacion EQ ? THEN DO:
              DISPLAY Solicitud.Agencia        AT 1  FORMAT "999"
                      Solicitud.Nit            AT 5  FORMAT "X(14)"
                      Solicitud.Cod_Credito    AT 20 FORMAT "999"
                      W_TipPdt                 AT 24 FORMAT "X(12)"
                      Solicitud.Num_Solicitud  AT 39 FORMAT "999999999"
                      Solicitud.Tasa           AT 50 FORMAT ">9.99"
                      Solicitud.Plazo          AT 56 FORMAT "9999"
                      Solicitud.Fec_Solicitud  AT 63 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 75 FORMAT ">,>>>,>>>,>>9"
                      W_Vigen                  AT 90 FORMAT ">>>9"
                      Solicitud.Usuario        AT 99
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotRegAge + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Ingreso" THEN DO:
           IF Solicitud.Monto         GE ValIni AND
              Solicitud.Monto         LE ValFin AND 
              Solicitud.Fec_Solicitud GE FecIni AND 
              Solicitud.Fec_Solicitud LE FecFin AND
              Solicitud.Fec_Aprobacion EQ ? AND 
             (Solicitud.Estado       EQ 1 OR Solicitud.Estado EQ 4) THEN DO:
              DISPLAY Solicitud.Agencia        AT 1  FORMAT "999"
                      Solicitud.Nit            AT 5  FORMAT "X(14)"
                      Solicitud.Cod_Credito    AT 20 FORMAT "999"
                      W_TipPdt                 AT 24 FORMAT "X(12)"
                      Solicitud.Num_Solicitud  AT 39 FORMAT "999999999"
                      Solicitud.Tasa           AT 50 FORMAT ">9.99"
                      Solicitud.Plazo          AT 56 FORMAT "9999"
                      Solicitud.Fec_Aprobacion AT 63 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 75 FORMAT ">,>>>,>>>,>>9"
                      W_Vigen                  AT 90 FORMAT ">>>9"
                      Solicitud.Usuario        AT 99
              WITH FRAME F2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotRegAge + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
        
        WHEN "Condicionadas" THEN DO:
           IF Solicitud.Monto          GE ValIni AND
              Solicitud.Monto          LE ValFin AND 
              Solicitud.Fec_Solicitud  GE FecIni AND 
              Solicitud.Fec_Solicitud  LE FecFin AND
              Solicitud.Estado         EQ 4 THEN DO:
              Obs = "".
              /* Buscar Fecha de Radicacion  Instancia = 20 */
/*               FIND LAST Mov_Instancias WHERE Mov_Instancias.Nit           EQ Solicitud.Nit AND            */
/*                                              Mov_Instancias.Instancia     EQ 20            AND            */
/*                                              Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud AND  */
/*                                              Mov_Instancias.Estado    NO-ERROR.                           */
/*               IF AVAILABLE Mov_Instancias THEN                                                            */
/*                  ASSIGN W_FecRetRad =  Mov_Instancias.fec_retiro. /*  AT 28 FORMAT "99/99/9999" */        */
              /********************************************************/
              
              FIND LAST Mov_Instancias WHERE Mov_Instancias.Nit            EQ Solicitud.Nit AND
                                              Mov_Instancias.Instancia     EQ 50            AND
                                              Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
                                              Mov_Instancias.Estado    NO-ERROR.
              IF AVAILABLE Mov_Instancias THEN 
                 ASSIGN Obs = Mov_Instancias.Descripcion. 
              DISPLAY Solicitud.Nit            AT 1  FORMAT "X(12)"
                      Solicitud.Cod_Credito    AT 14 FORMAT "999"
                      Solicitud.Num_Solicitud  AT 18 FORMAT "999999999"
                      Solicitud.Fec_Solicitud  AT 28 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 40 FORMAT ">>>>,>>>,>>9"
                      Solicitud.Usuario        AT 58
                      Obs                      AT 61 FORM "X(78)" SKIP (0)
/*                       W_FecRetRad              AT 29 FORMAT "99/99/9999" */
/*                       Solicitud.Fec_Solicitud  AT 41 FORMAT "99/99/9999"    */
/*                       Solicitud.Monto          AT 52 FORMAT ">>>>,>>>,>>9"  */
/*                       Solicitud.Usuario        AT 65                        */
/*                       Obs                      AT 70 FORM "X(78)" SKIP (0)  */
                  WITH DOWN FRAME F_Condi WIDTH 150 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotRegAge + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Por Tipo de Producto" THEN DO:
           IF Solicitud.Monto          GE ValIni AND
              Solicitud.Monto          LE ValFin AND 
              Solicitud.Tip_Credito    EQ INTEGER(SUBSTRING(Cmb_TProducto:SCREEN-VALUE IN FRAME F_Filtros,1,1)) AND
              Solicitud.Cod_Credito    GE ProIni AND
              Solicitud.Cod_Credito    LE ProFin AND
              Solicitud.Fec_Solicitud  GE FecIni AND 
              Solicitud.Fec_Solicitud  LE FecFin AND
              Solicitud.Estado         NE 2 THEN DO:
              DISPLAY Solicitud.Agencia        AT 1  FORMAT "999"
                      Solicitud.Nit            AT 5  FORMAT "X(14)"
                      Solicitud.Cod_Credito    AT 20 FORMAT "999"
                      W_TipPdt                 AT 24 FORMAT "X(12)"
                      Solicitud.Num_Solicitud  AT 39 FORMAT "999999999"
                      Solicitud.Tasa           AT 50 FORMAT ">9.99"
                      Solicitud.Plazo          AT 56 FORMAT "9999"
                      Solicitud.Fec_Solicitud  AT 63 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 75 FORMAT ">,>>>,>>>,>>9"
                      W_Vigen                  AT 90 FORMAT ">>>9"
                      Solicitud.Usuario        AT 99
              WITH FRAME F_3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotRegAge + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Forma de Pago" THEN DO:
           IF Solicitud.Monto          GE ValIni AND
              Solicitud.Monto          LE ValFin AND 
              Solicitud.Tip_Credito    EQ INTEGER(SUBSTRING(Cmb_TProducto:SCREEN-VALUE IN FRAME F_Filtros,1,1)) AND
              Solicitud.Cod_Credito    GE ProIni AND
              Solicitud.Cod_Credito    LE ProFin AND
              Solicitud.Fec_Solicitud  GE FecIni AND 
              Solicitud.Fec_Solicitud  LE FecFin AND
              Solicitud.Estado         NE 2 THEN DO:
              DISPLAY Solicitud.Agencia        AT 1  FORMAT "999"
                      Solicitud.Nit            AT 5  FORMAT "X(14)"
                      Solicitud.Cod_Credito    AT 20 FORMAT "999"
                      W_TipPdt                 AT 24 FORMAT "X(12)"
                      Solicitud.Num_Solicitud  AT 39 FORMAT "999999999"
                      Solicitud.Tasa           AT 50 FORMAT ">9.99"
                      Solicitud.Plazo          AT 56 FORMAT "9999"
                      Solicitud.Fec_Solicitud  AT 63 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 75 FORMAT ">,>>>,>>>,>>9"
                      W_Vigen                  AT 90 FORMAT ">>>9"
                      W_ForPag                 AT 100 FORMAT "X(10)"
              WITH FRAME F_3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotRegAge + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Solicitud.Agencia) THEN DO:
         DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Solicitud.Agencia                 AT 30 FORMAT "999"
                ": "                             AT 35
                 TotRegAge                       AT 40
                "Tot.Sdo.a Prestar Agencia:"    AT 60
                 TotSdoAge                       AT 89
        WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
        ASSIGN TotRegAge = 0
               TotSdoAge = 0.
      END.
  END.

  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Total a Prestar Entidad: "      AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

  ASSIGN TotReg = 0
         TotSdo = 0.

  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* 14:24:55 Looking for C:\Cartera\W-Informes_Basicos.w                               */
/* Compiling C:\Cartera\W-Informes_Basicos.w                                          */
/* ** No se puede comprender lo que sigue a -- "FORMAT 99/99/9999". (247)             */
/* ** El elemento debe ser una expresion, una frase SPACE o una frase SKIP. (406)     */
/* ** C:\Cartera\W-Informes_Basicos.w No se ha podido comprender la linea 6462. (198) */
/* ** C:\Cartera\W-Informes_Basicos.w Falta una sentencia END o CLOSE CURSOR. (1629)  */
/* ** C:\Cartera\W-Informes_Basicos.w No se ha podido comprender la linea 6463. (196) */
/*                                                                                    */
/* Compilation Finished.                                                              */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Usuarios wWin 
PROCEDURE Informes_Usuarios :
Listado = W_PathSpl + "Usuarios-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Usuarios: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BUsuarios:
    WHEN "Pendientes de Cobro" THEN  
        W_EncColumna = "Num.Credito Nit               Detalle                                                        Fec.Ingreso  Fec.Limite Cumplido".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Usuarios WHERE 
           DECIMAL(Usuarios.Usuario) GE DECIMAL(UsuIni) AND
           DECIMAL(Usuarios.Usuario) LE DECIMAL(UsuFin)
           NO-LOCK BREAK BY Usuarios.Usuario:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BUsuarios:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Pendientes de Cobro" THEN DO:
          FOR EACH Hoja_Vida WHERE
                   Hoja_Vida.Tipo   EQ 9 AND
                   Hoja_Vida.Codigo EQ 2 AND
                   Hoja_Vida.Fec_Grabacion GE FecIni AND
                   Hoja_Vida.Fec_Grabacion LE FecFin AND
                   Hoja_Vida.Usuario EQ Usuarios.Usuario NO-LOCK BREAK BY 
                   Hoja_Vida.Usuario BY Hoja_Vida.DoctoRefer:
              IF FIRST-OF(Hoja_Vida.Usuario) THEN DO:
                 WEnc1 = "Notas pendientes de Cobro del Usuario: " + Usuarios.Usuario + " - " + CAPS(Usuarios.Nombre).
                 DISPLAY SKIP(1)
                         "------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                         WEnc1 AT 1 SKIP(1)
                         "Num.Credito Nit               Detalle                                                        Fec.Ingreso  Fec.Limite Cumplido" AT 1 SKIP(1)
                         "------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                 WITH FRAME F_U1 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
              END.
              IF Hoja_Vida.Asunto_Cumplido THEN WEnc2 = "Si".
              ELSE WEnc2 = "No".
              DISPLAY 
                 Hoja_Vida.DoctoRefer  AT 1 FORMAT "999999999"
                 Hoja_Vida.Nit         AT 12 FORMAT "X(14)"
                 SUBSTRING(Hoja_Vida.Observacion,1,100) AT 30 FORMAT "x(60)"
                 Hoja_Vida.Fec_Grabacion AT 95
                 Hoja_Vida.Fec_Limite    AT 107
                 WEnc2 AT 120 FORMAT "X(2)"
                 WITH FRAME F_U3 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
          END.
        END.
      END CASE.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_VctosADebitar wWin 
PROCEDURE Informes_VctosADebitar :
/*---------------------------------------------------------------------------------
  Purpose:  Créditos Vencidos o al Día, que se vencen entre las fechas seleccionadas
            con los sdos-disponibles de los Ahorros a la vista.
------------------------------------------------------------------------------*/
  DEFI VAR T_Aho     LIKE Ahorros.Sdo_Dispon   INIT 0.
  DEFI VAR T_SdoCre  LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR T_TSdoCre LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR T_Tot     LIKE Creditos.Sdo_Capital INIT 0 EXTENT 4.
  DEFI VAR W_Coment  AS CHARACTER FORMAT "X(11)".
  SESSION:SET-WAIT-STATE("GENERAL").
  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Sdo_Capital    GT 0      AND
           Creditos.Int_Anticipado LE 0      AND
           DAY(Creditos.Fec_Pago)  GE DAY(FECINI) AND
           DAY(Creditos.Fec_Pago)  LE DAY(FECFIN) NO-LOCK 
           BREAK BY Creditos.Agencia BY Creditos.Nit BY Creditos.Num_Credito: 

      ASSIGN T_Aho    = 0
             T_SdoCre = Creditos.Cuota
             W_Coment = ''.

      IF (Creditos.Sdo_Capital   + Creditos.INT_Corriente + Creditos.INT_DifCobro + 
          Creditos.INT_MorCobrar + Int_MoraDifCob + Honorarios + Costas + Polizas)
          LT Creditos.Cuota THEN
          T_SdoCre = (Creditos.Sdo_Capital   + Creditos.INT_Corriente + Creditos.INT_DifCobro + 
                      Creditos.INT_MorCobrar + Int_MoraDifCob + Honorarios + Costas + Polizas).

      ASSIGN T_Tot[3]  = T_Tot[3] + T_SdoCre
             T_Tot[4]  = T_Tot[4] + T_SdoCre
             T_TSdoCre = T_TSdoCre + T_SdoCre.

      IF LAST-OF(Creditos.Nit) THEN DO:
         ASSIGN T_Aho = 0.
         FOR EACH Ahorros WHERE Ahorros.Nit        EQ Creditos.Nit
                            AND Ahorros.Tip_Ahorro EQ 1
                            AND Ahorros.Sdo_Dispon GT 0 NO-LOCK:
             ASSIGN T_Aho = T_Aho + Ahorros.Sdo_Dispon.
         END.

         IF T_TSdoCre LE T_Aho THEN
            ASSIGN T_Aho = T_TSdoCre.

         ASSIGN T_Tot[1]  = T_Tot[1] + T_Aho
                T_Tot[2]  = T_Tot[2] + T_Aho
                T_TSdoCre = 0.
      END.


      IF T_SdoCre > T_aho THEN 
        NEXT.
      ELSE
         IF  Creditos.Fec_UltPago   GE FECINI AND
             Creditos.Fec_UltPago   LE FECFIN THEN
             W_coment = "Tiene Mvto".

      DISPLAY Creditos.Agencia 
              Creditos.Nit
              Creditos.Cod_Credito
              Creditos.Num_Credito
              Creditos.Fec_Pago
              T_SdoCre   FORM ">>>>>,>>>,>>>,>>9"
              T_Aho      FORM ">>>>>,>>>,>>>,>>9"
              Creditos.Dias_Atraso FORMAT "zz,zz9"
              Creditos.Fec_UltPago FORMAT "99/99/9999"
              W_coment   FORMAT "X(11)"
          WITH DOWN WIDTH 180 FRAME F_Vctos NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      IF LAST-OF(Creditos.Agencia) THEN DO:      
         DISPLAY "                                          -----------------  ----------------" SKIP
                 "                TOTALES DE LA AGENCIA :  "
                 T_Tot[3] FORM ">>>>>,>>>,>>>,>>9"
                 T_Tot[1] FORM ">>>>>,>>>,>>>,>>9"
                 SKIP(2)
            WITH DOWN WIDTH 180 FRAME F_VctosT1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

         ASSIGN T_Tot[1] = 0
                T_Tot[3] = 0.
      END.
  END.

  DISPLAY "                    TOTALES GENERALES :  "
          T_Tot[4] FORM ">>>>>,>>>,>>>,>>9"          
          T_Tot[2] FORM ">>>>>,>>>,>>>,>>9"
       WITH DOWN WIDTH 180 FRAME F_VctosT2 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN T_Tot[2] = 0
         T_Tot[4] = 0.

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Crecimiento wWin 
PROCEDURE Informe_Crecimiento :
DEFINE VAR Sini AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR Sfin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR TFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR NMES AS INTEGER.
NMES = MONTH(W_Fecha) - 1.
IF NMES < 1 THEN NMES = 12.
Listado = W_PathSpl + "Crecimiento-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

OS-DELETE VALUE(Listado).
{Incluido\RepEncabezado.i}
    FOR EACH tc: DELETE tc. END.
  W_Reporte   = "REPORTE   : Informe de Crecimiento "
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE PRODUCTO                                                                                                                                                             TOTAL".


ASSIGN J = 0 k = 0 TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 .
ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
       TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 
       TotFdoEnt = 0 TotAhoEnt = 0.
FOR EACH tt: DELETE tt. END.
FOR EACH tc: DELETE tc. END.
ASSIGN J = 0 k = 0.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 73.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
/*ahorros*/
FOR EACH Ahorros WHERE Ahorros.Agencia NE 0 NO-LOCK
         BREAK BY Ahorros.Tip_Ahorro BY Ahorros.Cod_Ahorro:
    j = j + 1.
    RUN Progreso.
    /*calcula el total por tipo de cliente*/
    FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       CASE Clientes.Tipo_Cliente:
           WHEN 1 THEN TotDMay = TotDMay + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 2 THEN TotDMen = TotDMen + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 3 THEN TotDJsl = TotDJsl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 4 THEN TotDJCl = TotDJCl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
       END CASE.
    END.
    FIND TC WHERE Tc.T_Tpd EQ Ahorros.Tip_Ahorro AND
                  Tc.T_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Ahorros.Tip_Ahorro
              Tc.T_Cod = Ahorros.Cod_Ahorro.
       FIND Pro_Ahorros WHERE 
            Pro_Ahorros.Tip_Ahorro EQ Ahorros.Tip_Ahorro AND
            Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN Tc.T_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    IF MONTH(W_Fecha) EQ 1 THEN 
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_AnualPerAnt[12].
    ELSE
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_Anuales[NMES].
    ASSIGN Tc.T_SActual[Ahorros.Agencia] = Tc.T_SActual[Ahorros.Agencia] + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).

    /*calcula el total por tipo de ahorro*/
    FIND TT WHERE TT.TO_Tpd EQ Ahorros.Tip_Ahorro AND 
                  TT.TO_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE TT THEN DO:
       CREATE Tt.
       ASSIGN Tt.TO_Tpd = Ahorros.Tip_Ahorro
              Tt.TO_Cod = Ahorros.Cod_Ahorro
              Tt.To_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    ASSIGN Tt.TO_Total = Tt.TO_Total + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
           Tt.TO_Fondo = Tt.TO_Fondo + ((Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) * 0.10).
END.

DEFI VAR W_Dif AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    
ASSIGN J = 0 k = 0.
FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    DISPLAY Tc.T_Cod
            Tc.T_NomPdt
            Tc.T_SActual[1]  - Tc.T_SInicial[1] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[2]  - Tc.T_SInicial[2] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[3]  - Tc.T_SInicial[3] FORMAT "->>,>>>,>>>,>>9" 
            Tc.T_SActual[4]  - Tc.T_SInicial[4] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[5]  - Tc.T_SInicial[5] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[6]  - Tc.T_SInicial[6] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[7]  - Tc.T_SInicial[7] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[8]  - Tc.T_SInicial[8] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[9]  - Tc.T_SInicial[9] FORMAT "->>,>>>,>>>,>>9"
            (Tc.T_SActual[1]  - Tc.T_SInicial[1]) + (Tc.T_SActual[2]  - Tc.T_SInicial[2]) +
            (Tc.T_SActual[3]  - Tc.T_SInicial[3]) + (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
            (Tc.T_SActual[5]  - Tc.T_SInicial[5]) + (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
            (Tc.T_SActual[7]  - Tc.T_SInicial[7]) + (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
            (Tc.T_SActual[9]  - Tc.T_SInicial[9]) FORMAT "->>,>>>,>>>,>>9"
    WITH FRAME I1 WIDTH 240 STREAM-IO NO-BOX USE-TEXT NO-LABELS.
    ASSIGN TotTpd1 =  TotTpd1 + (Tc.T_SActual[1]  - Tc.T_SInicial[1])
           TotTpd2 =  TotTpd2 + (Tc.T_SActual[2]  - Tc.T_SInicial[2])
           TotTpd3 =  TotTpd3 + (Tc.T_SActual[3]  - Tc.T_SInicial[3])
           TotTpd4 =  TotTpd4 + (Tc.T_SActual[4]  - Tc.T_SInicial[4])
           TotTpd5 =  TotTpd5 + (Tc.T_SActual[5]  - Tc.T_SInicial[5])
           TotTpd6 =  TotTpd6 + (Tc.T_SActual[6]  - Tc.T_SInicial[6])
           TotTpd7 =  TotTpd7 + (Tc.T_SActual[7]  - Tc.T_SInicial[7])
           TotTpd8 =  TotTpd8 + (Tc.T_SActual[8]  - Tc.T_SInicial[8])
           TotTpd9 =  TotTpd9 + (Tc.T_SActual[9]  - Tc.T_SInicial[9])
           TotTpd10 = TotTpd10 + (Tc.T_SActual[1]  - Tc.T_SInicial[1]) +
                                 (Tc.T_SActual[2]  - Tc.T_SInicial[2]) +
                                 (Tc.T_SActual[3]  - Tc.T_SInicial[3]) +
                                 (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
                                 (Tc.T_SActual[5]  - Tc.T_SInicial[5]) +
                                 (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
                                 (Tc.T_SActual[7]  - Tc.T_SInicial[7]) +
                                 (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
                                 (Tc.T_SActual[9]  - Tc.T_SInicial[9]).
    IF LAST-OF(Tc.T_Tpd) THEN DO:
        IF Tc.T_Tpd EQ 1 THEN W_NomTpd = "Tot.A la Vista ".
        IF Tc.T_Tpd EQ 2 THEN W_NomTpd = "Tot.Contractual".
        IF Tc.T_Tpd EQ 3 THEN W_NomTpd = "Tot.A Termino  ".
        IF Tc.T_Tpd EQ 4 THEN W_NomTpd = "Tot.Aportes    ".

        DISPLAY Skip(1)
                W_NomTpd TotTpd1 TotTpd2 TotTpd3 TotTpd4 TotTpd5 TotTpd6 TotTpd7 TotTpd8 TotTpd9 TotTpd10
                SKIP(1)
        WITH FRAME Fttpo1 WIDTH 240 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
        ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
               TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 .
    END.
    ASSIGN TotAge1 = TotAge1 + (Tc.T_SActual[1]  - Tc.T_SInicial[1])
           TotAge2 = TotAge2 + (Tc.T_SActual[2]  - Tc.T_SInicial[2])
           TotAge3 = TotAge3 + (Tc.T_SActual[3]  - Tc.T_SInicial[3])
           TotAge4 = TotAge4 + (Tc.T_SActual[4]  - Tc.T_SInicial[4])
           TotAge5 = TotAge5 + (Tc.T_SActual[5]  - Tc.T_SInicial[5])
           TotAge6 = TotAge6 + (Tc.T_SActual[6]  - Tc.T_SInicial[6])
           TotAge7 = TotAge7 + (Tc.T_SActual[7]  - Tc.T_SInicial[7])
           TotAge8 = TotAge8 + (Tc.T_SActual[8]  - Tc.T_SInicial[8])
           TotAge9 = TotAge9 + (Tc.T_SActual[9]  - Tc.T_SInicial[9])
           TotAge10 = TotAge10 + (Tc.T_SActual[1]  - Tc.T_SInicial[1]) +
                                 (Tc.T_SActual[2]  - Tc.T_SInicial[2]) +
                                 (Tc.T_SActual[3]  - Tc.T_SInicial[3]) +
                                 (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
                                 (Tc.T_SActual[5]  - Tc.T_SInicial[5]) +
                                 (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
                                 (Tc.T_SActual[7]  - Tc.T_SInicial[7]) +
                                 (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
                                 (Tc.T_SActual[9]  - Tc.T_SInicial[9]).
END.

DISPLAY "                    --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- -------------" SKIP
        "Total Ahorros      "
        TotAge1 TotAge2 TotAge3 TotAge4 TotAge5 TotAge6 TotAge7 TotAge8 TotAge9 TotAge10
        SKIP(1)
WITH FRAME Ft1 WIDTH 240 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
ASSIGN J = 0 k = 0 TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 .
ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
       TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 .

FOR EACH tc: DELETE tc. END.

/*creditos*/
FOR EACH Creditos WHERE Creditos.Agencia NE 0 NO-LOCK
         BREAK BY Creditos.Tip_Credito BY Creditos.Cod_Credito:
    j = j + 1.
    RUN Progreso.

    /*calcula el total por tipo de cliente*/
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       CASE Clientes.Tipo_Cliente:
           WHEN 1 THEN TotCMay = TotCMay + Creditos.Sdo_Capital.
           WHEN 2 THEN TotCMen = TotCMen + Creditos.Sdo_Capital.
           WHEN 3 THEN TotCJsl = TotCJsl + Creditos.Sdo_Capital.
           WHEN 4 THEN TotCJCl = TotCJCl + Creditos.Sdo_Capital.
       END CASE.
    END.

    /*calcula el total por tipo de ahorro*/
    CASE Creditos.Tip_Credito:
        WHEN 1 THEN TotCons = TotCons + Creditos.Sdo_Capital.
        WHEN 2 THEN TotCome = TotCome + Creditos.Sdo_Capital.
        WHEN 3 THEN TotHipo = TotHipo + Creditos.Sdo_Capital.
        WHEN 4 THEN TotMicr = TotMicr + Creditos.Sdo_Capital.
    END CASE.

    FIND TC WHERE Tc.T_Tpd EQ Creditos.Tip_Credito AND
                  Tc.T_Cod EQ Creditos.Cod_Credito NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Creditos.Tip_Credito
              Tc.T_Cod = Creditos.Cod_Credito.
       FIND Pro_Creditos WHERE 
            Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
            Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Creditos THEN Tc.T_NomPdt = Pro_Creditos.Nom_Producto.
    END.
    ASSIGN Tc.T_SInicial[Creditos.Agencia] = Tc.T_SInicial[Creditos.Agencia] + Creditos.Sdo_Anuales[NMES].
    ASSIGN Tc.T_SActual[Creditos.Agencia] = Tc.T_SActual[Creditos.Agencia] + Creditos.Sdo_Capital.
END.

ASSIGN J = 0 k = 0 TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 .
FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    DISPLAY Tc.T_Cod
            Tc.T_NomPdt
            Tc.T_SActual[1]  - Tc.T_SInicial[1] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[2]  - Tc.T_SInicial[2] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[3]  - Tc.T_SInicial[3] FORMAT "->>,>>>,>>>,>>9" 
            Tc.T_SActual[4]  - Tc.T_SInicial[4] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[5]  - Tc.T_SInicial[5] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[6]  - Tc.T_SInicial[6] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[7]  - Tc.T_SInicial[7] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[8]  - Tc.T_SInicial[8] FORMAT "->>,>>>,>>>,>>9"
            Tc.T_SActual[9]  - Tc.T_SInicial[9] FORMAT "->>,>>>,>>>,>>9"
            (Tc.T_SActual[1]  - Tc.T_SInicial[1]) + (Tc.T_SActual[2]  - Tc.T_SInicial[2]) + 
            (Tc.T_SActual[3]  - Tc.T_SInicial[3]) + (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
            (Tc.T_SActual[5]  - Tc.T_SInicial[5]) + (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
            (Tc.T_SActual[7]  - Tc.T_SInicial[7]) + (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
            Tc.T_SActual[9]  - Tc.T_SInicial[9] FORMAT "->>,>>>,>>>,>>9"
    WITH FRAME I2 WIDTH 240 STREAM-IO NO-BOX USE-TEXT NO-LABELS.
    ASSIGN TotTpd1 =  TotTpd1 + (Tc.T_SActual[1]  - Tc.T_SInicial[1])
           TotTpd2 =  TotTpd2 + (Tc.T_SActual[2]  - Tc.T_SInicial[2])
           TotTpd3 =  TotTpd3 + (Tc.T_SActual[3]  - Tc.T_SInicial[3])
           TotTpd4 =  TotTpd4 + (Tc.T_SActual[4]  - Tc.T_SInicial[4])
           TotTpd5 =  TotTpd5 + (Tc.T_SActual[5]  - Tc.T_SInicial[5])
           TotTpd6 =  TotTpd6 + (Tc.T_SActual[6]  - Tc.T_SInicial[6])
           TotTpd7 =  TotTpd7 + (Tc.T_SActual[7]  - Tc.T_SInicial[7])
           TotTpd8 =  TotTpd8 + (Tc.T_SActual[8]  - Tc.T_SInicial[8])
           TotTpd9 =  TotTpd9 + (Tc.T_SActual[9]  - Tc.T_SInicial[9])
           TotTpd10 = TotTpd10 + (Tc.T_SActual[1]  - Tc.T_SInicial[1]) +
                                 (Tc.T_SActual[2]  - Tc.T_SInicial[2]) +
                                 (Tc.T_SActual[3]  - Tc.T_SInicial[3]) +
                                 (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
                                 (Tc.T_SActual[5]  - Tc.T_SInicial[5]) +
                                 (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
                                 (Tc.T_SActual[7]  - Tc.T_SInicial[7]) +
                                 (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
                                 (Tc.T_SActual[9]  - Tc.T_SInicial[9]).
    IF LAST-OF(Tc.T_Tpd) THEN DO:
        IF Tc.T_Tpd EQ 1 THEN W_NomTpd = "Tot.Consumo    ".
        IF Tc.T_Tpd EQ 2 THEN W_NomTpd = "Tot.Comercial  ".
        IF Tc.T_Tpd EQ 3 THEN W_NomTpd = "Tot.Hipotecario".
        IF Tc.T_Tpd EQ 4 THEN W_NomTpd = "Tot.Microcredit".

        DISPLAY Skip(1)
                W_NomTpd
                TotTpd1 TotTpd2 TotTpd3 TotTpd4 TotTpd5 TotTpd6 TotTpd7 TotTpd8 TotTpd9 TotTpd10
                SKIP(1)
        WITH FRAME FttpoC1 WIDTH 240 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
        ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
               TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 .
    END.
    ASSIGN TotAge1 = TotAge1 + (Tc.T_SActual[1]  - Tc.T_SInicial[1])
           TotAge2 = TotAge2 + (Tc.T_SActual[2]  - Tc.T_SInicial[2])
           TotAge3 = TotAge3 + (Tc.T_SActual[3]  - Tc.T_SInicial[3])
           TotAge4 = TotAge4 + (Tc.T_SActual[4]  - Tc.T_SInicial[4])
           TotAge5 = TotAge5 + (Tc.T_SActual[5]  - Tc.T_SInicial[5])
           TotAge6 = TotAge6 + (Tc.T_SActual[6]  - Tc.T_SInicial[6])
           TotAge7 = TotAge7 + (Tc.T_SActual[7]  - Tc.T_SInicial[7])
           TotAge8 = TotAge8 + (Tc.T_SActual[8]  - Tc.T_SInicial[8])
           TotAge9 = TotAge9 + (Tc.T_SActual[9]  - Tc.T_SInicial[9])
           TotAge10 = TotAge10 + (Tc.T_SActual[1]  - Tc.T_SInicial[1]) +
                                 (Tc.T_SActual[2]  - Tc.T_SInicial[2]) +
                                 (Tc.T_SActual[3]  - Tc.T_SInicial[3]) +
                                 (Tc.T_SActual[4]  - Tc.T_SInicial[4]) +
                                 (Tc.T_SActual[5]  - Tc.T_SInicial[5]) +
                                 (Tc.T_SActual[6]  - Tc.T_SInicial[6]) +
                                 (Tc.T_SActual[7]  - Tc.T_SInicial[7]) +
                                 (Tc.T_SActual[8]  - Tc.T_SInicial[8]) +
                                 (Tc.T_SActual[9]  - Tc.T_SInicial[9]).
END.

DISPLAY "                    --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- -------------" SKIP
        "Totales Cartera    "
        TotAge1 TotAge2 TotAge3 TotAge4 TotAge5 TotAge6 TotAge7 TotAge8 TotAge9 TotAge10 SKIP(1)
WITH FRAME Ft2 WIDTH 240 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

FOR EACH Tc: DELETE Tc. END.
ASSIGN J = 0 k = 0.

RUN Informe_Crecimiento_Asoc.

DEFINE VAR TotFdo AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TOTAL_Tipo AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
i = 0.
FOR EACH Tt BREAK BY Tt.TO_Tpd BY Tt.To_Cod:
    i = i + 1.
    CREATE TxLinea.
    ASSIGN TxLinea.TxLId = i
           TxLinea.TxLin = TxLinea.TxLin + STRING(Tt.TO_Tpd,"999") + " " +
                           STRING(Tt.TO_cod,"999") + " " + STRING(Tt.To_nomPdt,"X(15)") + " " + 
                           STRING(Tt.TO_Total,"->>,>>>,>>>,>>9") + "  " + 
                           STRING(Tt.TO_Fondo,"->>,>>>,>>>,>>9").
    ASSIGN TOTAL_Tipo = TOTAL_Tipo + tt.TO_total
           TotFdo     = TotFdo + Tt.TO_Fondo.
    IF LAST-OF(tt.TO_tpd) THEN DO:
        I = i + 1.
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = TxLinea.TxLin + "                   ____________________  ________________".
        I = i + 1.
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = TxLinea.TxLin + "                        " + STRING(TOTAL_tipo,"->>,>>>,>>>,>>9") + 
               "  " + STRING(TOTFdo,"->>,>>>,>>>,>>9").
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = "                                                       .".
       ASSIGN TotAhoEnt = TotAhoEnt + TOTAL_Tipo
              TotFdoEnt = TotFdoEnt + TotFdo
              TOTAL_tipo = 0 TotFdo = 0.
       IF Tt.TO_Tpd EQ 3 THEN DO:
          FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Cuenta BEGINS "1203" NO-LOCK:
              RUN HallaSdo1203(INPUT MONTH(W_Fecha), OUTPUT SIni, OUTPUT SFin).
              TFin = TFin + SFin.
          END.
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "Minimos Fdo Liquidez    " + STRING(TotAhoEnt,"->>,>>>,>>>,>>9") +
                                 "  " + STRING(TotFdoEnt,"->>,>>>,>>>,>>9").
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "Valor Actual del Fondo                   " + 
                                 STRING(TFin,"->>,>>>,>>>,>>9").
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
          IF TFin GT TotFdoEnt THEN DO:
              I = i + 1.
              CREATE TxLinea.
              ASSIGN TxLinea.TxLId = i
                     TxLinea.TxLin = "Sobrante del Fondo                       " + 
                                     STRING(TFin - TotFdoEnt,"->>,>>>,>>>,>>9").
          END.
          ELSE DO:
              I = i + 1.
              CREATE TxLinea.
              ASSIGN TxLinea.TxLId = i
                     TxLinea.TxLin = "Faltante del Fondo                       " + 
                                     STRING(TotFdoEnt - TFin,"->>,>>>,>>>,>>9").
          END.
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
       END.
    END.
END.

RUN Informe_Crecimiento_Totales.
PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Crecimiento_Asoc wWin 
PROCEDURE Informe_Crecimiento_Asoc :
DEFINE VAR wage   AS INTEGER INITIAL 0.
DEFINE VAR wcon   AS INTEGER INITIAL 0.    
DEFINE VAR TotTpd AS DECIMAL INITIAL 0.00 EXTENT 100. 
DEFINE VAR TotAge AS DECIMAL INITIAL 0.00 EXTENT 100.         
DEFINE VAR wsuma  AS DECIMAL INITIAL 0.00 EXTENT 5.    
    
FIND LAST agencias NO-LOCK NO-ERROR.
wage = agencias.agencia.
    
ASSIGN J = 0 k = 0.
/*clientes*/
/* PAGE.*/
FOR EACH Clientes WHERE 
         Clientes.Agencia      NE 0 AND 
         Clientes.Tipo_Vinculo EQ 1 AND
         Clientes.Fec_Retiro   EQ ?
         NO-LOCK BREAK BY Clientes.Tipo_Cliente:
    j = j + 1.
    RUN Progreso.
    FIND TC WHERE Tc.T_Tpd EQ Clientes.Tipo_Cliente NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Clientes.Tipo_Cliente.
       IF Tc.T_Tpd EQ 1 THEN W_NomPdt = "Asoc.Nat.Mayor ".
       IF Tc.T_Tpd EQ 2 THEN W_NomPdt = "Asoc.Nat.Menor ".
       IF Tc.T_Tpd EQ 3 THEN W_NomPdt = "Asoc.Juridic.SL".
       IF Tc.T_Tpd EQ 4 THEN W_NomPdt = "Asoc.Juridic.CL".
       Tc.T_NomPdt = W_NomPdt.
    END.
    IF MONTH(Clientes.Fec_Ingreso) EQ MONTH(W_Fecha) AND
       YEAR(Clientes.Fec_Ingreso) EQ YEAR(W_Fecha) THEN DO:
        ASSIGN Tc.T_SInicial[Clientes.Agencia] = Tc.T_SInicial[Clientes.Agencia] + 1.
    END.
    IF Tc.T_Tpd EQ 2 THEN DO:
       FIND FIRST Ahorros WHERE Ahorros.Nit EQ Clientes.Nit AND 
                                Ahorros.Cod_Ahorro EQ 10 AND
                                Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
       IF NOT AVAILABLE Ahorros THEN NEXT.
    END.
    ELSE DO:
        FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Clientes.Nit AND
                                 Ahorros.Cod_Ahorro EQ 5 AND
                                 Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ahorros THEN NEXT.
    END.
    Tc.T_SActual[Clientes.Agencia] = Tc.T_SActual[Clientes.Agencia] + 1.
END.

ASSIGN J = 0 k = 0.
REPEAT wcon = 1 TO wage:
  ASSIGN TotAge[wcon] = 0   wsuma[1] = 0.
END.

FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    PUT Tc.T_Tpd " - " Tc.T_NomPdt ";".
    REPEAT wcon = 1 TO wage:
            
       PUT Tc.T_SInicial[wcon] FORMAT "->>,>>>,>>>,>>9" ";".
       wsuma[1] = wsuma[1] + Tc.T_SInicial[wcon].
       TotAge[wcon] = TotAge[wcon] +  Tc.T_SInicial[wcon].
       wsuma[2] = wsuma[2] + Tc.T_SInicial[wcon].
    END.
    PUT Wsuma[1]  FORMAT "->>,>>>,>>>,>>9" SKIP(0).
    wsuma[1] = 0.
END.

PUT "Total Asoc. Nuevos ;".
REPEAT wcon = 1 TO wage:
   PUT TotAge[wcon] FORMAT "->>,>>>,>>>,>>9" ";".
   ASSIGN TotAge[wcon] = 0.
END.
PUT wsuma[2] SKIP(0).    
ASSIGN wsuma[1] = 0  wsuma[2] = 0.    
ASSIGN J = 0 k = 0. 

FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    PUT Tc.T_Tpd " - " Tc.T_NomPdt ";".
    REPEAT wcon = 1 TO wage:
      PUT Tc.T_SActual[wcon] FORMAT "->>,>>>,>>>,>>9" ";".      
      wsuma[1]   =  wsuma[1] + Tc.T_SActual[wcon].
      TotAge[wcon] = TotAge[wcon] +  Tc.T_SActual[wcon].  
    END.
    PUT wsuma[1] FORMAT "->>,>>>,>>>,>>9" SKIP(0).   
    wsuma[1] = 0.    
END.
PUT "Total Asociados    ;".
REPEAT wcon = 1 TO wage:
   PUT TotAge[wcon] FORMAT "->>,>>>,>>>,>>9"  ";".     
END.
PUT wsuma[1]  FORMAT "->>,>>>,>>>,>>9" SKIP(0).
PUT "Total de Ahorros y Creditos; Fondo de Liquidez;".
    
REPEAT wcon = 1 TO wage:
   ASSIGN TotAge[wcon] = 0.
END.
ASSIGN J = 0 k = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Crecimiento_Totales wWin 
PROCEDURE Informe_Crecimiento_Totales :
FIND FIRST TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "Total Consumo        " + STRING(TotCons,"->>,>>>,>>>,>>9").
FIND NEXT TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "      Comercial      " + STRING(TotCome,"->>,>>>,>>>,>>9").
FIND NEXT TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "      Hipotecario    " + STRING(TotHipo,"->>,>>>,>>>,>>9").
FIND NEXT TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "      Microcredito   " + STRING(TotMicr,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + " Ahorros Aso.Mayores " + STRING(TotDMay,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Menores " + STRING(TotDMen,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Jur. S.L" + STRING(TotDJsl,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Jur. C.L" + STRING(TotDjcl,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "Credito Aso.Mayores " + STRING(TotCMay,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Menores " + STRING(TotCMen,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Jur. S.L" + STRING(TotCJsl,"->>,>>>,>>>,>>9").
FIND NEXT  TxLinea.
TxLinea.TxLin = TxLinea.TxLin + "       "  + "         Aso.Jur. C.L" + STRING(TotCjcl,"->>,>>>,>>>,>>9").

FOR EACH txLinea:
    DISPLAY txlinea.txlin WITH FRAME ftotpdt WIDTH 240 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Info_Creditos wWin 
PROCEDURE Info_Creditos :
DEFINE OUTPUT PARAMETER W_Tip AS CHARACTER FORMAT "X(15)".      
DEFINE OUTPUT PARAMETER W_For AS CHARACTER FORMAT "X(15)".      
      CASE Creditos.Tip_Credito:
        WHEN 1 THEN W_Tip = "Consumo".
        WHEN 2 THEN W_Tip = "Comercial".
        WHEN 3 THEN W_Tip = "Hipotecario".
        WHEN 4 THEN W_Tip = "MicroCredito".
      END CASE.
      CASE Creditos.FOR_Pago:
        WHEN 1 THEN W_For = "Nomina".
        WHEN 2 THEN W_For = "Caja".
        WHEN 3 THEN W_For = "DB.Automatic".
      END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfSoli_Negadas wWin 
PROCEDURE InfSoli_Negadas :
FOR EACH Solicitud WHERE Solicitud.Agencia      GE AgeIni AND
                           Solicitud.Agencia      LE AgeFin AND
                           Solicitud.Cod_Credito  GE ProIni AND
                           Solicitud.Cod_Credito  LE ProFin AND
                           Solicitud.Estado       EQ 3
           NO-LOCK BREAK BY Solicitud.Cod_Negacion:
           ASSIGN W_FecRetRad = DATE ("  /  /  ").
           IF Solicitud.Monto          GE ValIni AND
              Solicitud.Monto          LE ValFin AND 
              Solicitud.Fec_Solicitud  GE FecIni AND 
              Solicitud.Fec_Solicitud  LE FecFin THEN DO:
              /* Buscar Fecha de Radicacion  Instancia Negada= 70 */
/*               FIND LAST Mov_Instancias WHERE Mov_Instancias.Nit           EQ Solicitud.Nit AND            */
/*                                              Mov_Instancias.Instancia     EQ 70            AND            */
/*                                              Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud AND  */
/*                                              NOT Mov_Instancias.Estado    NO-ERROR.                       */
/*               IF AVAILABLE Mov_Instancias THEN                                                            */
/*                   ASSIGN W_FecRetRad =  Mov_Instancias.fec_ingreso. /*  AT 28 FORMAT "99/99/9999" */      */
              /******************************************************************/ 
               
              IF FIRST-OF(Solicitud.Cod_Negacion) THEN DO:
                 FIND FIRST Varios WHERE Varios.Tipo EQ 26 
                                    AND Varios.Codigo EQ Solicitud.Cod_Negacion NO-LOCK NO-ERROR.    
                 DISPLAY SKIP(1)
                     "Causal de Negaciòn : " + 
                         Varios.Descripcion    FORMAT "X(50)" WHEN AVAIL(Varios)
                     WITH FRAME F_Negt WIDTH 140 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              END.
              DISPLAY Solicitud.Agencia        AT   1 FORMAT "999"
                      Solicitud.Nit            AT   5 FORMAT "X(14)"
                      Solicitud.Cod_Credito    AT  20 FORMAT "999"
                      W_TipPdt                 AT  24 FORMAT "X(12)"
                      Solicitud.Num_Solicitud  AT  39 FORMAT "999999999"
                      Solicitud.Tasa           AT  50 FORMAT ">9.99"
                      Solicitud.Plazo          AT  56 FORMAT "9999"
                      Solicitud.Fec_Solicitud  AT 63 FORMAT "99/99/9999"
                      Solicitud.Monto          AT 75 FORMAT ">,>>>,>>>,>>9"
                      W_Vigen                  AT 90 FORMAT ">>>9"
                      Solicitud.Usuario        AT 99 SKIP(0)
/*                       W_FecRetRad              AT  63 FORMAT "99/99/9999"     */
/*                       Solicitud.Fec_Solicitud  AT  75 FORMAT "99/99/9999"     */
/*                       Solicitud.Monto          AT  88 FORMAT ">>>>,>>>,>>9"   */
/*                       W_Vigen                  AT 102 FORMAT ">>>9"           */
/*                       Solicitud.Usuario        AT 111 SKIP(0) */
              WITH FRAME F_Neg WIDTH 140 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_AhoCod2425 wWin 
PROCEDURE Inf_AhoCod2425 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Listado = W_PathSpl + "InfAho-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin W-DiaVdo.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT           CODPDT CUENTA            TASA   PLAZO ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCje    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCjeAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotIntAge AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCauAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotInt    AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotCau    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR w_pl  AS CHARACTER FORMAT "X(14)" INITIAL "".
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(150)".
DEFINE VAR Reporte    AS CHARACTER FORMAT "X(150)".
ASSIGN J = 0 k = 0 Ct = 0 Reporte = "" Encabezado = "".
/*RUN Aho_Encabezados(OUTPUT Encabezado, OUTPUT Reporte).*/

{Incluido\RepEncabezado.i}
  W_EncColumna = "".
  W_Reporte   = "REPORTE   : Ahorros: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") + " - Filtro: " + NomInf.
  IF Reporte NE "" THEN
     W_Reporte = Reporte.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.    

  FOR EACH Ahorros WHERE Ahorros.Agencia  GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Tip_Ahorro   GE TpdIni AND Ahorros.Tip_Ahorro   LE TpdFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND Ahorros.Cod_Ahorro   LE ProFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit BY Ahorros.Cod_Ahorro:  
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN NomAho = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
      END.
      IF FIRST-OF(Ahorros.Agencia) THEN 
         TotRegAge = 0.
      j = j + 1.
      RUN Progreso.

      IF Ahorros.Sdo_Disponible GE ValIni AND
         Ahorros.Sdo_Disponible LE ValFin THEN DO:
         DISPLAY Ahorros.Agencia              FORMAT "999"   LABEL "Ag."
                 Ahorros.Nit                  FORMAT "X(14)" LABEL "Ced/Nit"    
                 NomAho                       FORMAT "X(35)" LABEL "Nombre del Asociado"
                 Ahorros.Cuota                FORMAT ">>>,>>>,>>9" LABEL "Vr.de Cuota"
                 Ahorros.Fec_Apertura                              LABEL "F-Apertura"                                                
                 Ahorros.Sdo_disponible       FORMAT ">>>,>>>,>>9"  LABEL "Sdo-Disponible"
                 Ahorros.Fec_Ulttransaccion   LABEL "F-UltTrans."                                              
            WITH DOWN FRAME F8 WIDTH 140 NO-BOX USE-TEXT NO-LABELS STREAM-IO.                                                                               
         CREATE IEx.                                                                                                                                   
         ASSIGN Ct = Ct + 1                                                                                                                            
                IEx.NLinea = Ct                                                                                                                        
                IEx.Linea  = STRING(Ahorros.Agencia) + Cma + Ahorros.Nit + Cma + STRING(Ahorros.Cue_Ahorros) + Cma +                                   
                             STRING((Ahorros.Cuota * ROUND(Ahorros.Plazo / 30,0)),"->>>>>>>>>>>9.99") + Cma +                                          
                             STRING(Ahorros.Cuota,"->>>>>>>>>>>9.99") + Cma + STRING(Ahorros.Fec_Apertura) + Cma +                                     
                             STRING(Ahorros.Fec_Vencimiento) + Cma + STRING(Ahorros.Sdo_Disponible,"->>>>>>>>>>>9.99") + Cma +                         
                             STRING(Ahorros.Plazo) + Cma + STRING(Ahorros.Tasa) + Cma + STRING(Ahorros.INT_Causado,"->>>>>>>>>>>9.99") + Cma +         
                             STRING(Ahorros.Fec_UltTransaccion).                                                                                       
         ASSIGN TotCauAge = TotCauAge + Ahorros.Int_causado                                                                                            
                TotSdo    = TotSdo + Ahorros.Sdo_Disponible  + Ahorros.Sdo_canje                                                                                          
                TotCau    = TotCau + Ahorros.Int_Causado                                                                                              
                TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje                                                                     
                TotReg    = TotReg  + 1
                TotRegAge = TotRegAge + 1.                                                                                                             
      END.
            
      IF LAST-OF(Ahorros.Agencia) THEN DO:
         DISPLAY SKIP(1)
                "Tot Reg Ag: "            AT 1
                 Ahorros.Agencia          AT 13 FORMAT "999"
                ": "                      AT 16
                 TotRegAge                AT 18 FORMAT ">>>,>>9"
                 "  Saldo Disponible Inteses Pagados  Intereses Causados" AT 27 SKIP(1)
                 TotSdoAge                AT 27 FORMAT ">>>,>>>,>>>,>>9"
                 TotInt                   AT 44 FORMAT "->>>,>>>,>>>,>>9"
                 TotCau                   AT 62 FORMAT ">>>,>>>,>>>,>>9"
             WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
         ASSIGN TotSdoAge = 0 
                TotRegAge = 0.
      END.
  END.

  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1 TotReg                          AT 40
    "Total Disponible   : "          AT 60 TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_ClientesActualizados wWin 
PROCEDURE Inf_ClientesActualizados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR W_nombres AS CHARACTER.

CREATE IEx.
ASSIGN Ct = Ct + 1
       IEx.NLinea = Ct
       IEx.Linea  = "AGENCIA;NIT;NOMBRES Y APELLIDOS;FEC.ULT.ACT;TEL.RESIDENCIA;TEL.COMERCIAL;E-MAIL;".

FOR EACH Clientes WHERE Clientes.Agencia GE AgeIni AND
                        Clientes.Agencia LE AgeFin AND
                        Clientes.Tipo_Vinculo LT 3 AND
                        Clientes.Fec_UltActualiza GE FecIni AND
                        Clientes.Fec_UltActualiza LE FecFin NO-LOCK 
                        BREAK BY Clientes.Agencia BY Clientes.Fec_UltActualiza:

        ASSIGN W_nombres = STRING(Clientes.Nombre) + " " + 
                           STRING(Clientes.Apellido1) + " " + 
                           STRING(Clientes.Apellido2).
        PUT Clientes.Agencia FORMAT "999" " "
            Clientes.Nit FORMAT "X(14)" " "
            W_nombres FORMAT "X(40)" " "
            Clientes.Fec_UltActualiza FORMAT "99/99/9999" " "
            Clientes.Tel_Residencia FORMAT "X(10)" " "
            Clientes.Tel_Comercial FORMAT "X(10)" " "
            Clientes.Email FORMAT "X(50)" SKIP(0).
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = STRING(Clientes.Agencia) + Cma +
                            STRING(Clientes.Nit) + Cma + W_nombres + Cma +
                            STRING(Clientes.Fec_UltActualiza) + Cma + 
                            STRING(Clientes.Tel_Residencia) + Cma + 
                            STRING(Clientes.Tel_Comercial) + Cma + 
                            STRING(Clientes.Email) + Cma.
        TotReg = TotReg + 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Clientes_AsoSinPdto wWin 
PROCEDURE Inf_Clientes_AsoSinPdto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN wcodagei = clientes.agencia
       wnomagecli=""
       wnomemp=""
       wnomcli=trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)
       wsalcli=clientes.Salario
       wtotahoapo= 0.00.
CASE clientes.tip_contrato:
     WHEN 0 THEN ASSIGN wtipocont = "0- Ninguno".
     WHEN 1 THEN ASSIGN wtipocont = "1- Indefinido".
     WHEN 2 THEN ASSIGN wtipocont = "2- Fijo".
     WHEN 3 THEN ASSIGN wtipocont = "3- Labor Contratada".
     WHEN 4 THEN ASSIGN wtipocont = "4- Prestación de Servicios".
     OTHERWISE 
         ASSIGN wtipocont = "No Asignado".
END CASE.
     
CASE clientes.estado:
     WHEN 1 THEN ASSIGN wnomestado = "1- Activo".
     WHEN 2 THEN ASSIGN wnomestado = "2- Inactivo".
     OTHERWISE 
         ASSIGN wnomestado = "No Asignado".
END CASE.

FIND FIRST agencia WHERE Agencias.Agencia=clientes.agencia NO-LOCK NO-ERROR.
IF AVAILABLE(agencia)THEN ASSIGN wnomagecli = agencias.nombre.
FIND FIRST empresas WHERE empresas.cod_empresa=clientes.cod_empresa NO-LOCK NO-ERROR.
IF AVAILABLE(empresas)THEN ASSIGN wnomemp=empresas.alias_empresa.

/* Tabla de Ahorros **
 Cod. 005 - Aportes Obligatorios , 010 - Aportes Voluntarios , 015 - Aportes Extraordinarios , 216 - Remanentes de Pagaduría */ 
ASSIGN Wtieahor = 0.
FOR EACH ahorros WHERE ahorros.nit=clientes.nit AND (cod_ahorro NE 5 AND cod_ahorro NE 10 AND cod_ahorro NE 15)
    AND sdo_disponible NE 0 NO-LOCK:
    /*IF ((cod_ahorro NE 005) AND (cod_ahorro NE 010) AND (cod_ahorro NE 015) AND (cod_ahorro NE 216)) THEN*/
       ASSIGN Wtieahor = 1.
END.

/* Solo Aportes */
FOR EACH ahorros WHERE ahorros.nit=clientes.nit AND (cod_ahorro = 5 OR cod_ahorro = 10 OR cod_ahorro = 15) and
                       sdo_disponible NE 0 NO-LOCK:
    ASSIGN wtotahoapo = wtotahoapo + sdo_disponible.
END.

/* Tabla de Créditos **
 Cod. 569 - Cuota de Afiliacion , 545 - Fdo. Mutual Funarario , 546 - Fdo. Mutual Educativo , 570 - Credito Rotativo*/ 
ASSIGN Wtiecre = 0.
FOR EACH creditos WHERE creditos.nit=clientes.nit AND sdo_capital NE 0 NO-LOCK:
    ASSIGN Wtiecre = 1.
END.

IF Wtieahor = 0 AND Wtiecre = 0 THEN
   PUT clientes.agencia       ";" 
       wnomagecli             ";" 
       clientes.nit           ";"
       wnomcli                ";" 
       wnomemp                ";"
       wtipocont              ";"          
       wnomestado             ";"
       clientes.fec_ingreso   ";"
       wsalcli                ";"
       wtotahoapo             ";"
       Clientes.Dir_comercial ";"
       Clientes.Tel_comercial SKIP(0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Clientes_SoloAso wWin 
PROCEDURE Inf_Clientes_SoloAso :
/*------------------------------------------------------------------------------
  Descripción: Lista la informacion del cliente asociado y activo.
  Author : Félix Vargas:     
  Fecha: 26/Ago/2007.
  Modificado: Alan Gordon - 30/11/2007
              Se adiciona Exportacion de Campos especificos a excel
              Se Corrige error de Tamaño en Sheet de Ventana P/pal  
------------------------------------------------------------------------------*/
DEFINE VAR WCuota AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0.
DEFINE VAR WFecAf AS CHARACTER.

ASSIGN wcodagei = clientes.agencia
       wnomcli = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
ASSIGN wsalcli = clientes.Salario.
ASSIGN wtoting = clientes.Ing_arriendos + clientes.Ing_Honorarios + clientes.Ing_financieros + clientes.Ing_otros
       wtotegr = clientes.gto_familiar + clientes.gto_arriendo + clientes.gto_obligacion + clientes.sdo_obligaciones
       Wlugresid = ""  Wlugcomer = "" wnomcargo = "" wnomemp = "" wnomprofe = "".

IF clientes.Fec_UltActualiza EQ ? THEN
    ASSIGN WFecAf = "00/00/0000".
ELSE
    ASSIGN WFecAf = STRING(clientes.Fec_UltActualiza).

    FIND FIRST Ahorros WHERE Clientes.Nit EQ Ahorros.Nit AND
                         Ahorros.Cod_ahorro EQ 5 AND 
                         Ahorros.Tip_ahorro EQ 4 AND
                         (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) GT 0
                         NO-LOCK NO-ERROR.
IF AVAILABLE (Ahorros) THEN DO:
    ASSIGN WCuota = Ahorros.Cuota.
IF NOT AVAILABLE (Ahorros) THEN
    ASSIGN WCuota = 0.
FIND FIRST agencias WHERE agencias.agencia = wcodagei NO-LOCK NO-ERROR.
IF AVAILABLE(agencias)THEN ASSIGN wnomagecli = agencias.nombre. 
FIND FIRST empresas WHERE empresas.cod_empresa=clientes.cod_empresa NO-LOCK NO-ERROR.
IF AVAILABLE(empresas)THEN ASSIGN wnomemp=empresas.alias_empresa.
/* Tipo = 2 - Cargo del Asociado * - Tipo = 1 -- Profesion*/
FIND FIRST varios WHERE varios.tipo = 2 AND varios.codigo=clientes.cod_cargo NO-LOCK NO-ERROR.
IF AVAILABLE(varios)THEN ASSIGN wnomcargo=varios.descripcion.
FIND FIRST varios WHERE varios.tipo = 1 AND varios.codigo=clientes.Cod_Profesion NO-LOCK NO-ERROR.
IF AVAILABLE(varios)THEN ASSIGN wnomprofe=varios.descripcion.
/* Tipo de Vinculo */
CASE clientes.tipo_vinculo:
     WHEN 1 THEN ASSIGN wtipovinc = "1- Asociado".
     WHEN 2 THEN ASSIGN wtipovinc = "2- Cliente no Asociado".
     WHEN 3 THEN ASSIGN wtipovinc = "3- Tercero".
     WHEN 4 THEN ASSIGN wtipovinc = "4- Proveedor".
     OTHERWISE 
         ASSIGN wtipovinc = "No Asignado".
END CASE.
/* Estado y Sexo del Asociado */
CASE clientes.estado:
     WHEN 1 THEN ASSIGN wnomestado = "1- Activo".
     WHEN 2 THEN ASSIGN wnomestado = "2- Inactivo".
     OTHERWISE 
         ASSIGN wnomestado = "No Asignado".
END CASE.
CASE clientes.sexo:
     WHEN 1 THEN ASSIGN wsexo = "1- Hombre".
     WHEN 2 THEN ASSIGN wsexo = "2- Mujer".
     OTHERWISE 
         ASSIGN wsexo = "No Asignado".
END CASE.
CASE clientes.tipo_cliente:
     WHEN 1 THEN ASSIGN wtipoclie = "1- Natural Mayor de Edad".
     WHEN 2 THEN ASSIGN wtipoclie = "2- Natural Menor de Edad".
     WHEN 3 THEN ASSIGN wtipoclie = "3- Juridica S.A".
     WHEN 4 THEN ASSIGN wtipoclie = "4- Juridica C.A".
     OTHERWISE 
         ASSIGN wtipoclie = "No Asignado".
END CASE.
CASE clientes.tipo_identificacion:
     WHEN "C.C" THEN ASSIGN wtipoiden = "C.C- Cedula de Ciudadania".
     WHEN "C.E" THEN ASSIGN wtipoiden = "C.E- Cedula de Extranjeria".
     WHEN "T.I" THEN ASSIGN wtipoiden = "T.I- Tarjeta de Identidad".
     WHEN "Nit" THEN ASSIGN wtipoiden = "Nit- Nit".
     WHEN "R.C" THEN ASSIGN wtipoiden = "R.C- Registro civil".
     OTHERWISE 
         ASSIGN wtipoiden = "No Asignado".
END CASE.
/*Lugar de Residencia*/

FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(clientes.lugar_residencia,1,5) NO-LOCK NO-ERROR.
IF AVAILABLE Ubicacion THEN ASSIGN Wlugresid  = trim(Ubicacion.Nombre).
Wlugresid = LC(Wlugresid).
CASE clientes.tipo_vivienda:
     WHEN 1 THEN ASSIGN wtipovivi = "1- Propia".
     WHEN 2 THEN ASSIGN wtipovivi = "2- Arrendada".
     OTHERWISE 
         ASSIGN wtipovivi = "No Asignado".
END CASE.
CASE clientes.tip_contrato:
     WHEN 0 THEN ASSIGN wtipocont = "0- Ninguno".
     WHEN 1 THEN ASSIGN wtipocont = "1- Indefinido".
     WHEN 2 THEN ASSIGN wtipocont = "2- Fijo".
     WHEN 3 THEN ASSIGN wtipocont = "3- Labor Contratada".
     WHEN 4 THEN ASSIGN wtipocont = "4- Prestación de Servicios".
     OTHERWISE 
         ASSIGN wtipocont = "No Asignado".
END CASE.
/*Lugar Comercial - Empresa en que labora*/
FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(clientes.Lugar_comercial,1,5) NO-LOCK NO-ERROR.
IF AVAILABLE Ubicacion THEN ASSIGN Wlugcomer  = Wlugcomer + " " + trim(Ubicacion.Nombre).
Wlugcomer = LC(Wlugcomer).
PUT  clientes.agencia               ";" 
     wnomagecli                     ";" 
     clientes.Fec_UltActualiza      ";" 
     wtipoclie                      ";" 
     wtipovinc                      ";" 
     clientes.nit                   ";" 
     wnomcli                        ";" 
     wtipoiden                      ";" 
     clientes.fec_ingreso           ";" 
     wnomestado                     ";" 
     clientes.fec_nacimiento        ";" 
     wsexo                          ";" 
     clientes.Est_Civil             ";" 
     clientes.Per_Acargo            ";" 
     clientes.Num_Hijos             ";" 
     clientes.Niv_Educativo         ";" 
     wnomprofe                      ";" 
     clientes.dir_residencia        ";" 
     clientes.estrato               ";" 
     Wlugresid                      ";" 
     clientes.tel_residencia        ";" 
     clientes.celular               ";" 
     clientes.email                 ";" 
     wtipovivi                      ";" 
     clientes.nom_arrendatario      ";" 
     clientes.tel_arrendatario      ";" 
     clientes.tipo_actividad        ";" 
     wnomemp                        ";" 
     clientes.tiempo_empresa        ";" 
     wnomcargo                      ";" 
     clientes.fec_ingempresa        ";" 
     wtipocont                      ";" 
     clientes.dir_comercial         ";" 
     Wlugcomer                      ";" 
     clientes.tel_comercial         ";" 
     wsalcli                        ";" 
     wtoting                        ";" 
     clientes.sdo_obligaciones      ";" 
     clientes.gto_familiar          ";" 
     clientes.gto_obligacion        ";" 
     clientes.gto_arriendo          ";" 
     wtotegr                        skip(0).

CREATE IEx.
ASSIGN Ct = Ct + 1
       IEx.NLinea = Ct
       IEx.Linea =  STRING(clientes.agencia) + Cma + WFecAf + Cma +  
                    STRING(wtipovinc) + Cma + STRING(clientes.nit) + Cma + STRING(wnomcli) + Cma + STRING(wnomestado) + Cma + 
                    STRING(WCuota) + Cma + STRING(wnomemp) + Cma + STRING(wnomcargo, "X(30)") + Cma +
                    STRING(wsalcli) + Cma.
                    
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_CrecAgencias wWin 
PROCEDURE Inf_CrecAgencias :
DEFINE VAR Sini AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR Sfin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR TFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR NMES AS INTEGER.
DEFINE VAR wage AS INTEGER.
DEFINE VAR wcre AS CHARACTER FORMAT "X(20)".
DEFINE VAR wcon AS INTEGER INITIAL 0.    
DEFINE VAR TotTpd AS DECIMAL INITIAL 0.00 EXTENT 100. 
DEFINE VAR TotAge AS DECIMAL INITIAL 0.00 EXTENT 100.         
DEFINE VAR wsuma  AS DECIMAL INITIAL 0.00 EXTENT 5.    
    
    
FIND LAST agencias NO-LOCK NO-ERROR.
wage = agencias.agencia.
wcre = "Inf_Crec_Agencias_" +  trim(STRING(DAY(TODAY))) + TRIM(STRING(MONTH(TODAY))) + TRIM(STRING(YEAR(TODAY))) + ".Csv".
OUTPUT TO VALUE(wcre).
NMES = MONTH(W_Fecha) - 1.
IF NMES < 1 THEN NMES = 12.
  FOR EACH tc: DELETE tc. END.

REPEAT wcon = 1 TO wage: 
   ASSIGN  TotAge[wcon] = 0  
           TotTpd[wcon] = 0. 
END.
    
ASSIGN J = 0 k = 0  wsuma[1] = 0  wsuma[2] = 0 TotFdoEnt = 0 TotAhoEnt = 0.
     
FOR EACH tt: DELETE tt. END.
FOR EACH tc: DELETE tc. END.
ASSIGN J = 0 k = 0.
/*ahorros*/
FOR EACH Ahorros WHERE Ahorros.Agencia NE 0 NO-LOCK
         BREAK BY Ahorros.Tip_Ahorro BY Ahorros.Cod_Ahorro:
    j = j + 1.
    RUN Progreso.
    /*calcula el total por tipo de cliente*/
    FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       CASE Clientes.Tipo_Cliente:
           WHEN 1 THEN TotDMay = TotDMay + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 2 THEN TotDMen = TotDMen + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 3 THEN TotDJsl = TotDJsl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 4 THEN TotDJCl = TotDJCl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
       END CASE.
    END.
    FIND TC WHERE Tc.T_Tpd EQ Ahorros.Tip_Ahorro AND
                  Tc.T_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Ahorros.Tip_Ahorro
              Tc.T_Cod = Ahorros.Cod_Ahorro.
       FIND Pro_Ahorros WHERE 
            Pro_Ahorros.Tip_Ahorro EQ Ahorros.Tip_Ahorro AND
            Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN Tc.T_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    IF MONTH(W_Fecha) EQ 1 THEN 
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_AnualPerAnt[12].
    ELSE
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_Anuales[NMES].
    ASSIGN Tc.T_SActual[Ahorros.Agencia] = Tc.T_SActual[Ahorros.Agencia] + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).

    /*calcula el total por tipo de ahorro*/
    FIND TT WHERE TT.TO_Tpd EQ Ahorros.Tip_Ahorro AND 
                  TT.TO_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE TT THEN DO:
       CREATE Tt.
       ASSIGN Tt.TO_Tpd = Ahorros.Tip_Ahorro
              Tt.TO_Cod = Ahorros.Cod_Ahorro
              Tt.To_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    ASSIGN Tt.TO_Total = Tt.TO_Total + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
           Tt.TO_Fondo = Tt.TO_Fondo + ((Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) * 0.10).
END.

DEFI VAR W_Dif AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    
ASSIGN J = 0 k = 0.

FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    PUT Tc.T_Cod ";" Tc.T_NomPdt ";".
    REPEAT wcon = 1 TO wage:
        PUT Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon] FORMAT "->>,>>>,>>>,>>9" ";" .  
        totTpd[wcon] = totTpd[wcon] + Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon].      /* Asignacion */
        wsuma[1] = wsuma[1] + Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon].              /* Sumatorias */
    END.
    PUT wsuma[1] FORMAT "->>,>>>,>>>,>>9" SKIP(0). 
        
    IF LAST-OF(Tc.T_Tpd) THEN DO:
        IF Tc.T_Tpd EQ 1 THEN W_NomTpd = "Tot.A la Vista ".
        IF Tc.T_Tpd EQ 2 THEN W_NomTpd = "Tot.Contractual".
        IF Tc.T_Tpd EQ 3 THEN W_NomTpd = "Tot.A Termino  ".
        IF Tc.T_Tpd EQ 4 THEN W_NomTpd = "Tot.Aportes    ".

        PUT W_NomTpd ";".
        REPEAT wcon = 1 TO wage:
            PUT TotTpd[wcon]  FORMAT "->>,>>>,>>>,>>9" ";" .
            ASSIGN TotTpd[wcon] = 0.
        END.
        PUT wsuma[1] FORMAT "->>,>>>,>>>,>>9" SKIP(0).
        wsuma[1] = 0.   
            
    END.
    REPEAT wcon = 1 TO wage:
        PUT TotTpd[wcon]  FORMAT "->>,>>>,>>>,>>9" ";" .
        ASSIGN TotAge[wcon]    = TotAge[wcon] + (Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon]).
               wsuma[2] = wsuma[2] +  (Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon]).       
    END.
        
END.
PUT  "Total Ahorros;" .
REPEAT wcon = 1 TO wage:
  PUT TotAge[wcon] FORMAT "->>,>>>,>>>,>>9" ";".
  ASSIGN TotAge[wcon] = 0 TotTpd[wcon] = 0.  
END.
PUT wsuma[2] FORMAT "->>,>>>,>>>,>>9" SKIP(0).
ASSIGN wsuma[1] = 0   wsuma[2] = 0.

ASSIGN J = 0 k = 0. /* TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 .
ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
       TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 . */

FOR EACH tc: DELETE tc. END.

/*creditos*/
FOR EACH Creditos WHERE Creditos.Agencia NE 0 NO-LOCK
         BREAK BY Creditos.Tip_Credito BY Creditos.Cod_Credito:
    j = j + 1.
    RUN Progreso.

    /*calcula el total por tipo de cliente*/
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       CASE Clientes.Tipo_Cliente:
           WHEN 1 THEN TotCMay = TotCMay + Creditos.Sdo_Capital.
           WHEN 2 THEN TotCMen = TotCMen + Creditos.Sdo_Capital.
           WHEN 3 THEN TotCJsl = TotCJsl + Creditos.Sdo_Capital.
           WHEN 4 THEN TotCJCl = TotCJCl + Creditos.Sdo_Capital.
       END CASE.
    END.

    /*calcula el total por tipo de ahorro*/
    CASE Creditos.Tip_Credito:
        WHEN 1 THEN TotCons = TotCons + Creditos.Sdo_Capital.
        WHEN 2 THEN TotCome = TotCome + Creditos.Sdo_Capital.
        WHEN 3 THEN TotHipo = TotHipo + Creditos.Sdo_Capital.
        WHEN 4 THEN TotMicr = TotMicr + Creditos.Sdo_Capital.
    END CASE.

    FIND TC WHERE Tc.T_Tpd EQ Creditos.Tip_Credito AND
                  Tc.T_Cod EQ Creditos.Cod_Credito NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Creditos.Tip_Credito
              Tc.T_Cod = Creditos.Cod_Credito.
       FIND Pro_Creditos WHERE 
            Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
            Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Creditos THEN Tc.T_NomPdt = Pro_Creditos.Nom_Producto.
    END.
    ASSIGN Tc.T_SInicial[Creditos.Agencia] = Tc.T_SInicial[Creditos.Agencia] + Creditos.Sdo_Anuales[NMES].
    ASSIGN Tc.T_SActual[Creditos.Agencia] = Tc.T_SActual[Creditos.Agencia] + Creditos.Sdo_Capital.
END.

REPEAT wcon = 1 TO wage:
  ASSIGN TotAge[wcon] = 0.  
END.
ASSIGN wsuma[2] = 0.    
ASSIGN J = 0 k = 0. /* TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 . */
FOR EACH Tc BREAK BY Tc.T_Tpd:
    j = j + 1.
    RUN Progreso.
    PUT Tc.T_Cod ";"  Tc.T_NomPdt  ";".
    REPEAT wcon = 1 TO wage:
      PUT Tc.T_SActual[wcon] - Tc.T_SInicial[wcon] FORMAT "->>,>>>,>>>,>>9" ";".  
      wsuma[1] = wsuma[1] + Tc.T_SActual[wcon] - Tc.T_SInicial[wcon]. 
      ASSIGN TotTpd[wcon] =  TotTpd[wcon] + (Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon]).  
    END.
    PUT wsuma[1] FORMAT "->>,>>>,>>>,>>9" SKIP(0).
    IF LAST-OF(Tc.T_Tpd) THEN DO:
        IF Tc.T_Tpd EQ 1 THEN W_NomTpd = "Tot.Consumo    ".
        IF Tc.T_Tpd EQ 2 THEN W_NomTpd = "Tot.Comercial  ".
        IF Tc.T_Tpd EQ 3 THEN W_NomTpd = "Tot.Hipotecario".
        IF Tc.T_Tpd EQ 4 THEN W_NomTpd = "Tot.Microcredit".

        PUT W_NomTpd ";".
        REPEAT wcon = 1 TO wage:
           PUT TotTpd[wcon] FORMAT "->>,>>>,>>>,>>9" ";".
           ASSIGN TotTpd[wcon] = 0.     
        END.
        PUT wsuma[1]  FORMAT "->>,>>>,>>>,>>9" SKIP(0).  
        ASSIGN wsuma[1] = 0.
    END.
    REPEAT wcon = 1 TO wage:
       TotAge[wcon] = TotAge[wcon] + (Tc.T_SActual[wcon]  - Tc.T_SInicial[wcon]).
    END.
    ASSIGN wsuma[1] = 0.
END.

PUT "Totales Cartera;" .
REPEAT wcon = 1 TO wage:
   PUT TotAge[wcon] ";".     
END.
PUT wsuma[1] SKIP(0). 
    
FOR EACH Tc: DELETE Tc. END.
ASSIGN J = 0 k = 0.

RUN Informe_Crecimiento_Asoc.
    
OUTPUT CLOSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_CrecTotales wWin 
PROCEDURE Inf_CrecTotales :
DEFINE VAR Sini AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR Sfin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR TFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR NMES AS INTEGER.
NMES = MONTH(W_Fecha) - 1.
IF NMES < 1 THEN NMES = 12.
Listado = W_PathSpl + "Crecimiento-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".

OS-DELETE VALUE(Listado).
{Incluido\RepEncabezado.i}
    FOR EACH tc: DELETE tc. END.
  W_Reporte   = "REPORTE   : Informe de Crecimiento "
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE PRODUCTO                                                                                                                                                             TOTAL".


ASSIGN J = 0 k = 0 TotAge1 = 0 TotAge2 = 0 TotAge3 = 0 TotAge4 = 0
       TotAge5 = 0 TotAge6 = 0 TotAge7 = 0 TotAge8 = 0 TotAge9 = 0 TotAge10 = 0 .
ASSIGN TotTpd1 = 0 TotTpd2 = 0 TotTpd3 = 0 TotTpd4 = 0
       TotTpd5 = 0 TotTpd6 = 0 TotTpd7 = 0 TotTpd8 = 0 TotTpd9 = 0 TotTpd10 = 0 
       TotFdoEnt = 0 TotAhoEnt = 0.
FOR EACH tt: DELETE tt. END.
FOR EACH tc: DELETE tc. END.
ASSIGN J = 0 k = 0.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 73.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
/*ahorros*/
FOR EACH Ahorros WHERE Ahorros.Agencia NE 0 NO-LOCK
         BREAK BY Ahorros.Tip_Ahorro BY Ahorros.Cod_Ahorro:
    j = j + 1.
    RUN Progreso.
    /*calcula el total por tipo de cliente*/
    FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       CASE Clientes.Tipo_Cliente:
           WHEN 1 THEN TotDMay = TotDMay + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 2 THEN TotDMen = TotDMen + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 3 THEN TotDJsl = TotDJsl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
           WHEN 4 THEN TotDJCl = TotDJCl + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
       END CASE.
    END.
    FIND TC WHERE Tc.T_Tpd EQ Ahorros.Tip_Ahorro AND
                  Tc.T_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE Tc THEN DO:
       CREATE Tc.
       ASSIGN Tc.T_Tpd = Ahorros.Tip_Ahorro
              Tc.T_Cod = Ahorros.Cod_Ahorro.
       FIND Pro_Ahorros WHERE 
            Pro_Ahorros.Tip_Ahorro EQ Ahorros.Tip_Ahorro AND
            Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN Tc.T_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    IF MONTH(W_Fecha) EQ 1 THEN 
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_AnualPerAnt[12].
    ELSE
       Tc.T_SInicial[Ahorros.Agencia] = Tc.T_SInicial[Ahorros.Agencia] + Ahorros.Sdo_Anuales[NMES].
    ASSIGN Tc.T_SActual[Ahorros.Agencia] = Tc.T_SActual[Ahorros.Agencia] + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje).

    /*calcula el total por tipo de ahorro*/
    FIND TT WHERE TT.TO_Tpd EQ Ahorros.Tip_Ahorro AND 
                  TT.TO_Cod EQ Ahorros.Cod_Ahorro NO-ERROR.
    IF NOT AVAILABLE TT THEN DO:
       CREATE Tt.
       ASSIGN Tt.TO_Tpd = Ahorros.Tip_Ahorro
              Tt.TO_Cod = Ahorros.Cod_Ahorro
              Tt.To_NomPdt = Pro_Ahorros.Nom_Producto.
    END.
    ASSIGN Tt.TO_Total = Tt.TO_Total + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
           Tt.TO_Fondo = Tt.TO_Fondo + ((Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) * 0.10).
END.

DEFINE VAR TotFdo AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TOTAL_Tipo AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
i = 0.
FOR EACH Tt BREAK BY Tt.TO_Tpd BY Tt.To_Cod:
    i = i + 1.
    CREATE TxLinea.
    ASSIGN TxLinea.TxLId = i
           TxLinea.TxLin = TxLinea.TxLin + STRING(Tt.TO_Tpd,"999") + " " +
                           STRING(Tt.TO_cod,"999") + " " + STRING(Tt.To_nomPdt,"X(15)") + " " + 
                           STRING(Tt.TO_Total,"->>,>>>,>>>,>>9") + "  " + 
                           STRING(Tt.TO_Fondo,"->>,>>>,>>>,>>9").
    ASSIGN TOTAL_Tipo = TOTAL_Tipo + tt.TO_total
           TotFdo     = TotFdo + Tt.TO_Fondo.
    IF LAST-OF(tt.TO_tpd) THEN DO:
        I = i + 1.
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = TxLinea.TxLin + "                   ____________________  ________________".
        I = i + 1.
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = TxLinea.TxLin + "                        " + STRING(TOTAL_tipo,"->>,>>>,>>>,>>9") + 
               "  " + STRING(TOTFdo,"->>,>>>,>>>,>>9").
        CREATE TxLinea.
        ASSIGN TxLinea.TxLId = i
               TxLinea.TxLin = "                                                       .".
       ASSIGN TotAhoEnt = TotAhoEnt + TOTAL_Tipo
              TotFdoEnt = TotFdoEnt + TotFdo
              TOTAL_tipo = 0 TotFdo = 0.
       IF Tt.TO_Tpd EQ 3 THEN DO:
          FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Cuenta BEGINS "1203" NO-LOCK:
              RUN HallaSdo1203(INPUT MONTH(W_Fecha), OUTPUT SIni, OUTPUT SFin).
              TFin = TFin + SFin.
          END.
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "Minimos Fdo Liquidez    " + STRING(TotAhoEnt,"->>,>>>,>>>,>>9") +
                                 "  " + STRING(TotFdoEnt,"->>,>>>,>>>,>>9").
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "Valor Actual del Fondo                   " + 
                                 STRING(TFin,"->>,>>>,>>>,>>9").
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
          IF TFin GT TotFdoEnt THEN DO:
              I = i + 1.
              CREATE TxLinea.
              ASSIGN TxLinea.TxLId = i
                     TxLinea.TxLin = "Sobrante del Fondo                       " + 
                                     STRING(TFin - TotFdoEnt,"->>,>>>,>>>,>>9").
          END.
          ELSE DO:
              I = i + 1.
              CREATE TxLinea.
              ASSIGN TxLinea.TxLId = i
                     TxLinea.TxLin = "Faltante del Fondo                       " + 
                                     STRING(TotFdoEnt - TFin,"->>,>>>,>>>,>>9").
          END.
          I = i + 1.
          CREATE TxLinea.
          ASSIGN TxLinea.TxLId = i
                 TxLinea.TxLin = "                                                       .".
       END.
    END.
END.

RUN Informe_Crecimiento_Totales.
PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Credi_CancEnAbog wWin 
PROCEDURE Inf_Credi_CancEnAbog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  

DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0
       TotMon = 0
       TotSdoAho = 0.

  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Tip_Credito    GE TPdIni AND
           Creditos.Tip_Credito    LE TpdFin AND
           Creditos.Fec_CanceTotal GE FecIni AND    
           Creditos.Fec_CanceTotal LE FecFin AND
           Creditos.Cod_Credito    GE ProIni AND
           Creditos.Cod_Credito    LE ProFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.pagare BY Creditos.Cod_Credito:

      IF NOT Creditos.Abogado THEN
         NEXT.

      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.

      DISPLAY Creditos.Agencia        AT 1  FORMAT "999"
              Creditos.Nit            AT 5  FORMAT "X(14)"
              Creditos.Cod_Credito    AT 20 FORMAT "999"
              W_TipPdt                AT 24 FORMAT "X(4)"
              TRIM(Creditos.Pagare)   AT 29 FORMAT "X(9)"
              Creditos.Tasa           AT 40 FORMAT ">9.99"
              Creditos.Plazo          AT 47 FORMAT "9999"
              Creditos.Fec_CanceTotal AT 52 FORMAT "99/99/9999"
              Creditos.Monto          AT 64 FORMAT ">>>,>>>,>>9"
              Creditos.cuota          AT 79 FORMAT ">,>>>,>>>,>>9"  
              Creditos.Cuo_Pagadas    AT 95 FORMAT ">9"
        WITH FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  

  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_CredLibranzas wWin 
PROCEDURE Inf_CredLibranzas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  

DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0
       TotMon = 0
       TotSdoAho = 0.

  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Tip_Credito    GE TPdIni AND
           Creditos.Tip_Credito    LE TpdFin AND           
           Creditos.Cod_Credito    GE ProIni AND
           Creditos.Cod_Credito    LE ProFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:

      IF NOT Creditos.FOR_pago EQ 2 OR Sdo_Capital LE 0 THEN
         NEXT.

      FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.      

      RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
      j = j + 1.
      RUN Progreso.

      DISPLAY Creditos.Agencia         FORMAT "999"                LABEL "Ag."
              Creditos.Nit             FORMAT "X(14)"              LABEL "Ced./Nit"
              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2)
                                       FORMAT "X(30)"              LABEL "Nombre del Asociado"        
              TRIM(Creditos.Pagare)    FORMAT "X(9)"               LABEL "No.Pagaré"
              Clientes.Cod_Empresa     FORMAT "9999"               LABEL "Empresa"
              Creditos.Plazo           FORMAT "9999"               LABEL "Plazo"
              Creditos.Cuo_Pagadas     FORMAT "9999"               LABEL "CPagas"
              Creditos.Fec_Desemb      FORMAT "99/99/9999"         LABEL "F-Desemb."
              Creditos.Fec_UltPago     FORMAT "99/99/9999"         LABEL "FUlt-Pago"
              Creditos.Fec_Pago        FORMAT "99/99/9999"         LABEL "FProx-Pago"
              Creditos.Monto           FORMAT ">>>>>,>>>,>>9"      LABEL "Monto-Crèdito"
              Creditos.Sdo_capital     FORMAT ">>>>>,>>>,>>9"      LABEL "Sdo-Capital"
              Creditos.Cuota           FORMAT ">>>>>,>>>,>>9"      LABEL "Cuota"
              Creditos.Dias_Atraso     FORMAT "9999"               LABEL "DMora"
              Creditos.Val_Atraso      FORMAT ">>>>>,>>>,>>9"      LABEL "Valor-Vencido"        
        WITH DOWN FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

      ASSIGN TotSdoAge = TotSdoAge + Creditos.Sdo_capital 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Sdo_capital
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Sdo_capital.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  
  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_CredMutual wWin 
PROCEDURE Inf_CredMutual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR W_NUbicacion    AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VAR W_UbiResidencia AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0
       TotMon = 0
       TotSdoAho = 0.

  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Tip_Credito    GE TPdIni AND
           Creditos.Tip_Credito    LE TpdFin AND           
           Creditos.Cod_Credito    GE ProIni AND
           Creditos.Cod_Credito    LE ProFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:

      IF NOT Creditos.FOR_pago EQ 2 OR Sdo_Capital LE 0 THEN
         NEXT.

      FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.      
      
      /*Ciudad residencia*/
    /*  FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre. */

      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
      W_UbiResidencia = TRIM(LC(W_NUbicacion)).

      j = j + 1.
      RUN Progreso.

      DISPLAY Creditos.Agencia         FORMAT "999"                
              Creditos.Nit             FORMAT "X(14)"              
              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2)
                                       FORMAT "X(30)"                     
              clientes.fec_nacimiento  FORMAT "99/99/9999"
              Creditos.Sdo_capital     FORMAT ">>>>>,>>>,>>9"      
              Creditos.Int_Corrientes  FORMAT ">>>>>,>>>,>>9"     
              Clientes.DIR_residencia  FORMAT "X(30)"
              W_UbiResidencia          FORMAT "X(26)"
              clientes.tel_residencia  FORMAT "X(15)"
        WITH DOWN FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
        
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Sdo_capital 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Sdo_capital
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Sdo_capital.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  
  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_CredMutualGral wWin 
PROCEDURE Inf_CredMutualGral :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR W_NUbicacion    LIKE Ubicacion.Nombre        INITIAL "".
DEFINE VAR W_UbiResidencia LIKE Ubicacion.Nombre        INITIAL "".
DEFINE VAR W_ageaso        LIKE Clientes.agencia.
DEFINE VAR W_agecre        LIKE Creditos.agencia.
DEFINE VAR W_fecnac        LIKE Clientes.fec_nacimiento.
DEFINE VAR W_fecing        LIKE Clientes.fec_ingreso.
DEFINE VAR W_dirrec        LIKE Clientes.dir_residencia INITIAL "".
DEFINE VAR W_telres        LIKE Clientes.tel_residencia INITIAL "".
DEFINE VAR W_fecpag        LIKE mov_creditos.fecha.      
DEFINE VAR W_nomaso        AS CHARACTER FORMAT "X(60)"  INITIAL "".
DEFINE VAR W_nomfdo        AS CHARACTER FORMAT "X(25)"  INITIAL "".
DEFINE VAR W_forpag        AS CHARACTER FORMAT "X(20)"  INITIAL "".

/* Asociados Fondo Mutual Educativo - 546 */
/* OUTPUT TO "c:\fdomutualedu.txt". */
/* PUT "Fondo;Agen_Credi;Agen_Aso;nit;Nombres_Asociado;Pago_Por;Fec_Nacto;Fec_Ingre.;Fec_Ult.Pago;Saldo K;Dir_Resi;Ciudad;Tel_Resi" SKIP(0). */
PUT "FONDO              AGENCIA CR ASO NIT            NOMBRES COMPLETOS                        PAGO_POR             FEC_NAC    FEC_INGR   FEC_ULTP     SALDO CAPIT DIR _RESID                               CIUDAD                    TEL_RESI" SKIP(0).
FOR EACH Creditos WHERE 
         Creditos.Agencia        GE AgeIni AND
         Creditos.Agencia        LE AgeFin AND
         Creditos.Tip_Credito    GE TPdIni AND
         Creditos.Tip_Credito    LE TpdFin AND           
         Creditos.Cod_Credito    GE ProIni AND
         Creditos.Cod_Credito    LE ProFin /* AND 
         Creditos.fec_desembolso GE FecIni AND
         Creditos.Fec_desembolso LE FecFin    */
         NO-LOCK BREAK BY Creditos.Cod_Credito BY Creditos.Agencia BY Creditos.Nit:

    ASSIGN W_fecnac = DATE(" /  / ")
           W_fecing = DATE(" /  / ")
           W_dirrec = ""
           W_telres = ""
           W_nomaso = " No Existe"
           W_fecpag = DATE(" /  / ")
           W_NUbicacion = ""
           W_UbiResidencia = ""
           W_forpag = ""
           W_ageaso = 0
           W_nomfdo = ""
           W_agecre = creditos.agencia.      
   
   FIND FIRST PRO_CREDITOS WHERE pro_creditos.Tip_Credito = Creditos.Tip_Credito AND pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
   IF AVAILABLE(pro_creditos) THEN
      W_nomfdo = trim(pro_creditos.nom_producto).
   
   IF Creditos.for_pago = 1 THEN w_forpag = "Caja".
     ELSE IF Creditos.for_pago = 2 THEN W_forpag = "Nomina".
          ELSE W_forpag = "DB Automático".

    FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       DO:
          ASSIGN W_ageaso = clientes.agencia
                 W_fecnac = clientes.fec_nacimiento
                 W_fecing = clientes.fec_ingreso
                 W_dirrec = Clientes.dir_residencia
                 W_telres = Clientes.tel_residencia
                 W_nomaso = TRIM(Clientes.Nombre) + " " + 
                            TRIM(Clientes.Apellido1) + " " + 
                            TRIM(Clientes.Apellido2).

          FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
          IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
          W_UbiResidencia = TRIM(LC(W_NUbicacion)).
        END.

    IF FIRST-OF(creditos.agencia) OR FIRST-OF(creditos.nit) THEN DO:
      FIND LAST mov_creditos WHERE mov_creditos.Agencia = Creditos.Agencia AND
           mov_creditos.Cod_Credito = Creditos.Cod_Credito AND
           mov_creditos.Num_Credito = Creditos.Num_Credito AND
           mov_creditos.Nit = Creditos.Nit AND
           mov_creditos.cod_operacion NE 030303001 /* Traslado de Agencia */        
           NO-LOCK NO-ERROR.
      IF AVAILABLE mov_creditos THEN ASSIGN W_fecpag = Mov_Creditos.Fecha.
    END.
  
    PUT TRIM(W_nomfdo)          FORMAT "X(25)" " "        
        W_agecre                FORMAT "999" " "          
        W_ageaso                FORMAT "999" " "          
        Creditos.nit            FORMAT "X(14)" " "        
        TRIM(W_nomaso)          FORMAT "X(40)" " "        
        TRIM(W_forpag)          FORMAT "X(20)" " "        
        W_fecnac                FORMAT "99/99/9999" " "   
        W_fecing                FORMAT "99/99/9999" " "   
        W_fecpag                FORMAT "99/99/9999" " "   
        Creditos.fec_desembolso FORMAT "99/99/9999" " "
        Creditos.sdo_capital    FORMAT ">>>>>,>>>,>>9"  " "     
        W_dirrec                FORMAT "X(40)" " "          
        W_UbiResidencia         FORMAT "X(26)" " "          
        W_telres                FORMAT "X(15)" SKIP(0).     
    CREATE IEx.
    ASSIGN Ct = Ct + 1 
       IEx.NLinea = Ct 
        IEx.Linea  = trim(W_nomfdo) + cma + TRIM(STRING(W_agecre,"999"))  + cma + 
        TRIM(STRING(W_ageaso,"999"))  + cma +
        TRIM(Creditos.nit) + cma +
        TRIM(W_nomaso)  + cma +
        TRIM(W_forpag)  + cma.
        IF W_fecnac = ?  THEN IEx.Linea = IEx.Linea + ";".
        ELSE IEx.Linea = IEx.Linea + STRING(W_fecnac) + cma.
        IF W_fecing = ?  THEN IEx.Linea = IEx.Linea + ";".
        ELSE IEx.Linea = IEx.Linea + STRING(W_fecing) + cma.
        IF W_fecpag = ?  THEN IEx.Linea = IEx.Linea + ";".
        ELSE IEx.Linea = IEx.Linea + STRING(W_fecpag) + cma.
        IEx.Linea = IEx.Linea + STRING(Creditos.sdo_capital,">>>>>>>>>>9") + cma +
        TRIM(W_dirrec) + cma +
        TRIM(W_UbiResidencia) + cma +
        TRIM(W_telres).

 END.

/* 
DEFINE VAR CTotMon    AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR TotMon     AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotSdoAho  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotSdo     AS DECIMAL FORMAT ">>,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR TotAho     AS DECIMAL FORMAT "->,>>>,>>>,>>9" INITIAL 0.  
DEFINE VAR W_NUbicacion    AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VAR W_UbiResidencia AS CHARACTER FORMAT "X(30)" INITIAL "".
DEFINE VAR FecDesAno AS DATE.

ASSIGN TotSdo = 0
       TotMon = 0
       TotSdoAho = 0.

  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND
           Creditos.Agencia        LE AgeFin AND
           Creditos.Tip_Credito    GE TPdIni AND
           Creditos.Tip_Credito    LE TpdFin AND           
           Creditos.Cod_Credito    GE ProIni AND
           Creditos.Cod_Credito    LE ProFin 
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:

      IF NOT Creditos.FOR_pago EQ 2 OR Sdo_Capital LE 0 THEN
         NEXT.

      FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.      
      

      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
      W_UbiResidencia = TRIM(LC(W_NUbicacion)).

      j = j + 1.
      RUN Progreso.

      DISPLAY Creditos.Agencia         FORMAT "999"                
              Creditos.Nit             FORMAT "X(14)"              
              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2)
                                       FORMAT "X(30)"                     
              clientes.fec_nacimiento  FORMAT "99/99/9999"
              Creditos.Sdo_capital     FORMAT ">>>>>,>>>,>>9"      
              Creditos.Int_Corrientes  FORMAT ">>>>>,>>>,>>9"     
              Clientes.DIR_residencia  FORMAT "X(30)"
              W_UbiResidencia          FORMAT "X(26)"
              clientes.tel_residencia  FORMAT "X(15)"
        WITH DOWN FRAME FCartera WIDTH 200 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
        
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Sdo_capital 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Sdo_capital
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Sdo_capital.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotMon                          AT 64  
     TotSdo                          AT 79  
  WITH FRAME ttCartera WIDTH 180 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Desembolsos wWin 
PROCEDURE Inf_Desembolsos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR UsuDes LIKE Usuarios.Usuario.
  DEFI VAR Nro AS INTEG FORM "999999" EXTENT 2 INIT 0.
  DEFI VAR WKNom AS CHARACTER FORMAT "X(60)".
  DEFI VAR TVr LIKE Creditos.Monto    EXTENT 2 INIT 0.
  DEFI VAR W_Rel AS CHAR FORM "X(2)".
  DEFI VAR W_Adm AS CHAR FORM "X(2)".
  DEFI VAR Usu1  LIKE Mov_instancias.Usuario EXTENT 3.
  DEFI VAR NomUsu AS CHAR FORM "X(19)".
  DEFI VAR CodEmp LIKE Empresas.Cod_Empresa   INITIAL 0.
  DEFI VAR NomEmp LIKE Empresas.Alias_Empresa INITIAL "".

  FOR EACH Creditos WHERE Creditos.Agencia GE AgeIni AND Creditos.Agencia LE AgeFin
                      AND Creditos.Estado     EQ 2 
                      AND Creditos.Fec_Desemb GE FecIni
                      AND Creditos.Fec_Desemb LE FecFin NO-LOCK 
                      BREAK BY Creditos.Agencia BY Creditos.Nit:
/*       ASSIGN Nro[1] = Nro[1] + 1               */
/*              Nro[2] = Nro[2] + 1               */
/*              TVr[1] = TVr[1] + Creditos.Monto  */
/*              TVr[2] = TVr[2] + Creditos.Monto  */
/*              W_Rel = "No"                      */
/*              W_Adm = "No"                      */
/*              Usu1  = "".                       */
      FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
      IF NOT AVAILABLE (Clientes) THEN 
         NEXT.
      ELSE DO:
      /**** Se Incluyo ************/
       IF W_CodEmp NE 0 AND W_CodEmp NE ? THEN
          IF Clientes.Cod_Empresa NE W_CodEmp THEN
             NEXT.
          ELSE DO:
            FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
            IF AVAILABLE (Empresas) THEN
               ASSIGN CodEmp = Empresas.Cod_Empresa NomEmp = Empresas.Alias_Empresa.
            ELSE 
               ASSIGN CodEmp = 0 NomEmp = "".
            ASSIGN Nro[1] = Nro[1] + 1
                   Nro[2] = Nro[2] + 1
                   TVr[1] = TVr[1] + Creditos.Monto
                   TVr[2] = TVr[2] + Creditos.Monto
                   W_Rel = "No"
                   W_Adm = "No"
                   Usu1  = "".
          END.
       ELSE DO: /*** Todas Las Empresas ***/
          FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
          IF AVAILABLE (Empresas) THEN
             ASSIGN CodEmp = Empresas.Cod_Empresa NomEmp = Empresas.Alias_Empresa.
          ELSE 
             ASSIGN CodEmp = 0 NomEmp = "".
          ASSIGN Nro[1] = Nro[1] + 1
                 Nro[2] = Nro[2] + 1
                 TVr[1] = TVr[1] + Creditos.Monto
                 TVr[2] = TVr[2] + Creditos.Monto
                 W_Rel = "No"
                 W_Adm = "No"
                 Usu1  = "".
        END.
      /**************************/
      FIND FIRST Relaciones WHERE Relaciones.Nit EQ Creditos.Nit  AND
                             Relaciones.Clase_Producto EQ 2                AND
                             Relaciones.Cod_Relacion   EQ 11               AND
                             Relaciones.Estado         EQ 1                AND
                             INTEG (Relaciones.Cuenta) EQ Creditos.Num_Credito NO-LOCK NO-ERROR.
      IF AVAIL(Relaciones) THEN
         W_Rel = "Si".

      FIND FIRST  Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                              AND Garantias.Nit     EQ Creditos.Nit  
                              AND Garantias.Num_Credito EQ Creditos.Num_Credito 
                              AND Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
      IF AVAIL(Garantias) THEN
         W_Adm = "Si". 

      FOR EACH Mov_Instancias WHERE
             Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud AND
             Mov_instancias.Nit           EQ Creditos.Nit NO-LOCK:
          IF Mov_Instancias.Instancia EQ 20 THEN 
             Usu1[1] = Mov_instancias.Usuario.
          ELSE IF Mov_instancias.Instancia EQ 50 THEN
             Usu1[2] = Mov_instancias.Usuario.
          ELSE IF Mov_instancias.Instancia EQ 200 THEN
             Usu1[3] = Mov_instancias.Usuario.
      END.
      
      FIND FIRST Mov_Creditos WHERE
           Mov_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
           Mov_Creditos.Num_Credito EQ Creditos.Num_Credito AND
           Mov_Creditos.Nit         EQ Creditos.Nit AND
           (Mov_Creditos.Cod_Operacion EQ 20102001 OR Mov_Creditos.Cod_Operacion EQ 20102002) NO-LOCK NO-ERROR.
      IF AVAILABLE Mov_Creditos THEN UsuDes = Mov_Creditos.Usuario.

      IF Usu1[1] = "" AND Usu1[2] = "" AND Usu1[3] = "" THEN DO:
         FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
         ASSIGN NomUsu = SUBSTRING(Pro_Creditos.Nom_Produc,4,25) WHEN AVAIL(Pro_Creditos).
      END.
      ELSE 
         NomUsu = "  " + STRING(Usu1[1],"X(7)") + " " + STRING(Usu1[2],"X(5)") + " " + STRING(Usu1[3],"X(5)").
      WkNom = "".
      IF AVAILABLE Clientes THEN
         WkNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      DISPLAY Creditos.Agencia
              Creditos.Nit
              WkNom FORM "X(25)" WHEN AVAIL(Clientes)
              Creditos.Cod_Credito
              Creditos.Num_Solicitud
              Creditos.Pagare       FORM "X(8)"
              Creditos.Monto        FORM ">>>>,>>>,>>9.99"
              Creditos.Plazo   
              Creditos.Per_Pago     FORM ">>9"
              Creditos.Tasa         FORM ">99.9999"  
              Creditos.Cuota        FORM ">>>>>>>,>>9.99"
              Creditos.Fec_Desembolso 
              UsuDes
              STRING(CodEmp) + "-" + NomEmp FORMAT "X(35)"
          WITH DOWN WIDTH 220 FRAME Fc1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
      CREATE IEx.
      ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
             IEx.Linea  = STRING(Creditos.Agencia) + Cma + Creditos.Nit + Cma + WkNom + Cma +
                          STRING(Creditos.Cod_Credito) + Cma + STRING(Creditos.Num_Solicitud) + Cma +
                          STRING(Creditos.Pagare) + Cma + STRING(Creditos.Monto,"->>>>>>>>>>>9.99") + Cma + 
                          STRING(Creditos.Plazo) + Cma + STRING(Creditos.Per_Pago) + Cma + STRING(Creditos.Tasa,"->>>>9.999") + Cma +
                          STRING(Creditos.Cuota,"->>>>>>>>>>>9.99") + Cma + STRING(Creditos.Fec_Desembolso).
      END.
      IF LAST-OF(Creditos.Agencia) THEN DO:
         DISPLAY "--------------------------------------------------------------------------------------------" SKIP
                 "Total Agencia Número Desembolsos :" Nro[1]
                 "              Valor Montos :" TVr[1]
              WITH DOWN WIDTH 150 FRAME Fc12 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN Nro[1] = 0
                TVr[1] = 0.
      END.
  END.
  DISPLAY SKIP(1)
          "--------------------------------------------------------------------------------------------" SKIP
          "Total General Número Desembolsos :" Nro[2]                                                            
          "              Valor Montos :" TVr[2]  
          SKIP(4)
          "                              ------------------------------    ----------------------------" SKIP
          "                              AUTORIZADO COMITE                 AUTORIZADO COMITE"
       WITH DOWN WIDTH 150 FRAME Fc13 NO-BOX NO-LABELS STREAM-IO USE-TEXT.     

  ASSIGN Nro[2] = 0                                                                                  
         TVr[2] = 0.                                                                                 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_EmpCancDesemb wWin 
PROCEDURE Inf_EmpCancDesemb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR WKNom AS CHARACTER FORMAT "X(60)".
  DEFI VAR Nro AS INTEG FORM "999999" EXTENT 2 INIT 0.
  DEFI VAR TVr LIKE Creditos.Monto    EXTENT 2 INIT 0.
  DEFI VAR W_Rel AS CHAR FORM "X(2)".
  DEFI VAR W_Adm AS CHAR FORM "X(2)".
  DEFI VAR Usu1  LIKE Mov_instancias.Usuario EXTENT 3.
  DEFI VAR NomUsu AS CHAR FORM "X(19)".

  FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ W_CodEmp NO-LOCK NO-ERROR.

  DISPLAY "                  Empresa : "
          W_CodEmp FORM "9999"
          Empresas.Alias_Empresa FORM "X(40)" WITH WIDTH 150 NO-BOX NO-LABELS.

  FOR EACH Creditos WHERE (Creditos.Estado EQ 2 AND Creditos.Fec_Desemb NE ? AND 
                           Creditos.Fec_Desemb GE FecIni AND Creditos.Fec_Desemb LE FecFin)
                       OR (Creditos.Estado NE 2 AND Sdo_Capital LE 0 AND Creditos.Fec_UltPago NE ?
                                                                     AND Creditos.Fec_UltPago GE FecIni
                                                                     AND Creditos.Fec_UltPago LE FecFin)     NO-LOCK 
                       BY Creditos.Nit:
      IF Creditos.Estado EQ 1 OR Creditos.Estado GT 3 THEN
         NEXT.
      FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
      IF AVAIL(Clientes) AND Clientes.Cod_Empresa NE W_CodEmp THEN
         NEXT.

      IF NOT AVAIL(Clientes) THEN
         NEXT.

      IF Creditos.FOR_pago NE 2 THEN
         NEXT.

      ASSIGN Nro[2] = Nro[2] + 1
             TVr[2] = TVr[2] + Creditos.Monto.
      WkNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      DISPLAY Creditos.Agencia
              Creditos.Nit
              WKNOm FORM "X(30)" WHEN AVAIL(Clientes)
              Creditos.Cod_Credito
              Creditos.Pagare       FORM "X(8)"
              Creditos.Monto        FORM "->>>>,>>>,>>9.99"
              Creditos.Plazo   
              Creditos.Per_Pago     FORM ">>9"
              Creditos.Cuota        FORM "->>>>,>>>,>>9.99"
              Creditos.Fec_Desemb   
              Creditos.Fec_UltPago WHEN Creditos.Estado NE 2                            
          WITH DOWN WIDTH 220 FRAME Fc1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
          CREATE IEx.
          ASSIGN Ct = Ct + 1 IEx.NLinea = Ct
                 IEx.Linea  = STRING(Creditos.Agencia) + Cma + Creditos.Nit + Cma + WkNom + Cma +
                              STRING(Creditos.Cod_Credito) + Cma + STRING(Creditos.Pagare) + Cma + 
                              STRING(Creditos.Monto,"->>>>>>>>>>>9.99") + Cma + STRING(Creditos.Plazo,"99999") + Cma + 
                              STRING(Creditos.Per_Pago) + Cma + STRING(Creditos.Cuota,"->>>>>>>>>>>9.99") + Cma +
                              STRING(Creditos.Fec_Desembolso) + Cma + STRING(Creditos.Fec_UltPago).
  END.

  DISPLAY SKIP(1)
          "--------------------------------------------------------------------------------------------" SKIP
          "Total General Número :" Nro[2]                                                            
          "              Valor Montos :" TVr[2]            
       WITH DOWN WIDTH 150 FRAME Fc13 NO-BOX NO-LABELS STREAM-IO USE-TEXT.     

  ASSIGN Nro[2] = 0                                                                                  
         TVr[2] = 0.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_UbicInconsistente wWin 
PROCEDURE Inf_UbicInconsistente :
/*------------------------------------------------------------------------------
  Purpose:   Listar los Clientes que tienenuna ubicación no creadA en tabla ubicacion  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR W_nombres AS CHARACTER.

CREATE IEx.
ASSIGN Ct = Ct + 1
       IEx.NLinea = Ct
       IEx.Linea  = "AGENCIA;NIT;NOMBRES Y APELLIDOS;UBICACION;TEL.RESIDENCIA;TEL.COMERCIAL;E-MAIL;".

FOR EACH Clientes WHERE Clientes.Agencia GE AgeIni AND
                        Clientes.Agencia LE AgeFin AND
                        Clientes.estado EQ 1 NO-LOCK 
                        BREAK BY Clientes.Agencia BY Clientes.nit:

    FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_resi NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ubicacion THEN DO:
        ASSIGN W_nombres = STRING(Clientes.Nombre) + " " + 
                           STRING(Clientes.Apellido1) + " " + 
                           STRING(Clientes.Apellido2).
        PUT Clientes.Agencia FORMAT "999" " "
            Clientes.Nit FORMAT "X(14)" " "
            W_nombres FORMAT "X(40)" " "
            Clientes.Lugar_residencia FORMAT "X(10)" " "
            Clientes.Tel_Residencia FORMAT "X(10)" " "
            Clientes.Tel_Comercial FORMAT "X(10)" " "
            Clientes.Email FORMAT "X(50)" SKIP(0).
        CREATE IEx.
        ASSIGN Ct = Ct + 1
               IEx.NLinea = Ct
               IEx.Linea  = STRING(Clientes.Agencia) + Cma +
                            STRING(Clientes.Nit) + Cma + W_nombres + Cma +
                            STRING(Clientes.Lugar_residencia) + Cma + 
                            STRING(Clientes.Tel_Residencia) + Cma + 
                            STRING(Clientes.Tel_Comercial) + Cma + 
                            STRING(Clientes.Email) + Cma.
        TotReg = TotReg + 1.
    END.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Filtros:
     FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        IF Agencias.Agencia EQ W_Agencia THEN
           Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     END.
  END.
  W_DiaFin = DAY(TODAY).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
           AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
           MesIni = MONTH(TODAY)
           MesFin = MONTH(TODAY)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = TODAY
           FecFin = TODAY.
   END.
   DO WITH FRAME F_Basicos:
     HIDE {&List-1}.
     Cmb_BClientes:SCREEN-VALUE = Cmb_BClientes:ENTRY(1).
     ENABLE Cmb_BClientes.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 250 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN R1:BGCOL = 18.
          WHEN 2 THEN R2:BGCOL = 18.
          WHEN 3 THEN R3:BGCOL = 18.
          WHEN 4 THEN R4:BGCOL = 18.
          WHEN 5 THEN R5:BGCOL = 18.
          WHEN 6 THEN R6:BGCOL = 18.
          WHEN 7 THEN R7:BGCOL = 18.
          WHEN 8 THEN R8:BGCOL = 18.
          WHEN 9 THEN R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Creditos wWin 
PROCEDURE Resumen_Creditos :
/*------------------------------------------------------------------------------
  Purpose: Invocado desde Informes_creditos    
  Notes:   Sept.8/06 GAER
------------------------------------------------------------------------------*/
  DEFI VAR T_Tot1 LIKE Creditos.Sdo_Capital INIT 0 EXTENT 6 FORM "->>>,>>>,>>>,>>9.99".
  DEFI VAR T_Tot2 LIKE Creditos.Sdo_Capital INIT 0 EXTENT 6 FORM "->>>,>>>,>>>,>>9.99".
  DEFI VAR T_Tot3 LIKE Creditos.Sdo_Capital INIT 0 EXTENT 6 FORM "->>>,>>>,>>>,>>9.99".
  DEFI VAR T_Nro1 AS INTEG FORM "999999" EXTENT 3.
  DEFI VAR W_Cta  LIKE Cuentas.Cuenta.

  SESSION:SET-WAIT-STATE("GENERAL").

  FOR EACH TCCtas: DELETE TCCtas. END.

  FOR EACH Creditos WHERE Creditos.Sdo_Capital GT 0 NO-LOCK 
           BREAK BY Creditos.Agencia BY Creditos.Cod_Credito: 

      ASSIGN T_Nro1[1] = T_Nro1[1] + 1
             T_Nro1[2] = T_Nro1[2] + 1
             T_Nro1[3] = T_Nro1[3] + 1
             T_Tot1[1] = T_Tot1[1] + Creditos.Sdo_Capital
             T_Tot2[1] = T_Tot2[1] + Creditos.Sdo_Capital
             T_Tot3[1] = T_Tot3[1] + Creditos.Sdo_Capital
             T_Tot1[2] = T_Tot1[2] + Creditos.INT_Corrientes
             T_Tot2[2] = T_Tot2[2] + Creditos.INT_Corrientes
             T_Tot3[2] = T_Tot3[2] + Creditos.INT_Corrientes
             T_Tot1[3] = T_Tot1[3] + Creditos.INT_Anticip
             T_Tot2[3] = T_Tot2[3] + Creditos.INT_Anticip
             T_Tot3[3] = T_Tot3[3] + Creditos.INT_Anticip
             T_Tot1[4] = T_Tot1[4] + Creditos.INT_MorCob
             T_Tot2[4] = T_Tot2[4] + Creditos.INT_MorCob
             T_Tot3[4] = T_Tot3[4] + Creditos.INT_MorCob
             T_Tot1[5] = T_Tot1[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot2[5] = T_Tot2[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot3[5] = T_Tot3[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot1[6] = T_Tot1[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot2[6] = T_Tot2[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot3[6] = T_Tot3[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas.

      IF LAST-OF(Creditos.Cod_Credito) THEN DO:
         DISPLAY "          "
                 Creditos.Agencia      FORM "999"
                 Creditos.Cod_Credito  FORM "999"
                 "   "
                 T_Nro1[1]             
                 T_Tot1[1]             
                 T_Tot1[2]
                 T_Tot1[3]
                 T_Tot1[4]
                 T_Tot1[5]
                 T_Tot1[6]
             WITH DOWN WIDTH 150 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

         
         ASSIGN T_Nro1[1] = 0
                T_Tot1    = 0.
      END.       
      IF LAST-OF(Creditos.Agencia) THEN DO:
         DISPLAY "          --- --- ---" SKIP
          "          "
          Creditos.Agencia
          "TOTAL  "
          T_Nro1[2]             
          T_Tot2[1]             
          T_Tot2[2]
          T_Tot2[3]
          T_Tot2[4]
          T_Tot2[5]
          T_Tot2[6]
          WITH DOWN WIDTH 150 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         
         ASSIGN T_Nro1[2] = 0
                T_Tot2    = 0.
      END.
  END.           

  DISPLAY "          --- --- ---" SKIP(2)
          "          "
          "TOTAL GENER"
          T_Nro1[3]             
          T_Tot3[1]             
          T_Tot3[2]
          T_Tot3[3]
          T_Tot3[4]
          T_Tot3[5]
          T_Tot3[6]
        WITH DOWN WIDTH 150 FRAME F3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        
  ASSIGN T_Nro1[3] = 0
         T_Tot3    = 0.

  DISPLAY SKIP(2)
          "          Resumen por Producto y Código de Calificación" SKIP
          "          ---------------------------------------------" SKIP(0)
     WITH DOWN WIDTH 150 FRAME F3tit NO-LABELS NO-BOX USE-TEXT STREAM-IO.

  FOR EACH Creditos WHERE Creditos.Estado EQ 2 NO-LOCK 
           BREAK BY Creditos.Agencia BY Creditos.Cod_Credito BY Creditos.Cod_Califica:

      ASSIGN T_Nro1[1] = T_Nro1[1] + 1
             T_Nro1[2] = T_Nro1[2] + 1
             T_Nro1[3] = T_Nro1[3] + 1
             T_Tot1[1] = T_Tot1[1] + Creditos.Sdo_Capital
             T_Tot2[1] = T_Tot2[1] + Creditos.Sdo_Capital
             T_Tot3[1] = T_Tot3[1] + Creditos.Sdo_Capital
             T_Tot1[2] = T_Tot1[2] + Creditos.INT_Corrientes
             T_Tot2[2] = T_Tot2[2] + Creditos.INT_Corrientes
             T_Tot3[2] = T_Tot3[2] + Creditos.INT_Corrientes
             T_Tot1[3] = T_Tot1[3] + Creditos.INT_Anticip
             T_Tot2[3] = T_Tot2[3] + Creditos.INT_Anticip
             T_Tot3[3] = T_Tot3[3] + Creditos.INT_Anticip
             T_Tot1[4] = T_Tot1[4] + Creditos.INT_MorCob
             T_Tot2[4] = T_Tot2[4] + Creditos.INT_MorCob
             T_Tot3[4] = T_Tot3[4] + Creditos.INT_MorCob
             T_Tot1[5] = T_Tot1[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot2[5] = T_Tot2[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot3[5] = T_Tot3[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot1[6] = T_Tot1[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot2[6] = T_Tot2[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot3[6] = T_Tot3[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas.

      FIND CarteraVencida WHERE 
              CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND              
              CarteraVencida.Cod_Califica EQ Creditos.Cod_Califica  NO-LOCK NO-ERROR.
      IF NOT AVAIL(CarteraVencida) THEN
         FIND FIRST CarteraVencida WHERE 
                    CarteraVencida.Cod_Producto EQ Creditos.Cod_Credito AND              
                    CarteraVencida.Cod_Califica GE 0 NO-LOCK NO-ERROR.
      FIND FIRST Garantias WHERE   
                    Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
                    Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
                    Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
                    Garantias.Num_Credito   EQ Creditos.Num_Credito   AND
                    Garantias.Estado        EQ 1                      AND
                    Garantias.Val_Bien      GT 0 NO-LOCK NO-ERROR.
      W_Cta = CarteraVencida.Cta_NoaNaDb.
      IF          AVAIL(Garantias) AND Creditos.FOR_pago EQ 2 THEN
         W_Cta = CarteraVencida.Cta_AsoAdDb.
      ELSE     IF AVAIL(Garantias) AND Creditos.FOR_pago NE 2 THEN
         W_Cta = CarteraVencida.Cta_NoaAdDb.
      ELSE IF NOT AVAIL(Garantias) AND Creditos.FOR_pago EQ 2 THEN
         W_Cta = CarteraVencida.Cta_AsoNaDb.

      FIND FIRST TCCtas WHERE TCCtas.Age EQ Creditos.Agencia
                          AND TCCtas.Cta EQ W_Cta NO-ERROR.
      IF NOT AVAIL(TCCtas) THEN
         CREATE TCCtas.
      ASSIGN TCCtas.Age = Creditos.Agencia
             TCCtas.Cta = W_Cta
             TCCtas.Vlr = TCCtas.Vlr + Creditos.Sdo_Capital.

      IF LAST-OF(Creditos.Cod_Califica) THEN DO:
         DISPLAY "          "
                 Creditos.Agencia      FORM "999"
                 Creditos.Cod_Credito
                 Creditos.Cod_Califica FORM "999"
                 T_Nro1[1]             
                 T_Tot1[1]             
                 T_Tot1[2]
                 T_Tot1[3]
                 T_Tot1[4]
                 T_Tot1[5]
                 T_Tot1[6]
             WITH DOWN WIDTH 150 FRAME F11 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

         
         ASSIGN T_Nro1[1] = 0
                T_Tot1    = 0.
      END.  

      IF LAST-OF(Creditos.Agencia) THEN DO:
         DISPLAY "          --- --- ---" SKIP
          "          "
          Creditos.Agencia
          "TOTAL  "
          T_Nro1[2]             
          T_Tot2[1]             
          T_Tot2[2]
          T_Tot2[3]
          T_Tot2[4]
          T_Tot2[5]
          T_Tot2[6]
          WITH DOWN WIDTH 150 FRAME F22 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         
         ASSIGN T_Nro1[2] = 0
                T_Tot2    = 0.
      END.
  END.           

  DISPLAY "          --- --- ---" SKIP(2)
          "          "
          "TOTAL.GENER"
          T_Nro1[3]             
          T_Tot3[1]             
          T_Tot3[2]
          T_Tot3[3]
          T_Tot3[4]
          T_Tot3[5]
          T_Tot3[6]
        WITH DOWN WIDTH 150 FRAME F33 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        
  ASSIGN T_Nro1[3] = 0
         T_Tot3    = 0.


  DISPLAY SKIP(2)
          "          Resumen por Código de Calificación" SKIP
          "          ----------------------------------"
     WITH DOWN WIDTH 150 FRAME F3tit2a NO-LABELS NO-BOX USE-TEXT STREAM-IO.

  FOR EACH Creditos WHERE Creditos.Estado EQ 2 NO-LOCK 
           BREAK BY Creditos.Agencia BY Creditos.Cod_Califica:

      ASSIGN T_Nro1[1] = T_Nro1[1] + 1
             T_Nro1[2] = T_Nro1[2] + 1
             T_Nro1[3] = T_Nro1[3] + 1
             T_Tot1[1] = T_Tot1[1] + Creditos.Sdo_Capital
             T_Tot2[1] = T_Tot2[1] + Creditos.Sdo_Capital
             T_Tot3[1] = T_Tot3[1] + Creditos.Sdo_Capital
             T_Tot1[2] = T_Tot1[2] + Creditos.INT_Corrientes
             T_Tot2[2] = T_Tot2[2] + Creditos.INT_Corrientes
             T_Tot3[2] = T_Tot3[2] + Creditos.INT_Corrientes
             T_Tot1[3] = T_Tot1[3] + Creditos.INT_Anticip
             T_Tot2[3] = T_Tot2[3] + Creditos.INT_Anticip
             T_Tot3[3] = T_Tot3[3] + Creditos.INT_Anticip
             T_Tot1[4] = T_Tot1[4] + Creditos.INT_MorCob
             T_Tot2[4] = T_Tot2[4] + Creditos.INT_MorCob
             T_Tot3[4] = T_Tot3[4] + Creditos.INT_MorCob
             T_Tot1[5] = T_Tot1[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot2[5] = T_Tot2[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot3[5] = T_Tot3[5] + Creditos.INT_DifCob + Creditos.INT_MoraDifC
             T_Tot1[6] = T_Tot1[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot2[6] = T_Tot2[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas
             T_Tot3[6] = T_Tot3[6] + Creditos.Polizas + Creditos.Honorarios + Creditos.Costas.

      IF LAST-OF(Creditos.Cod_Califica) THEN DO:
         DISPLAY "          "
                 Creditos.Agencia      FORM "999"
                 "   "
                 Creditos.Cod_Califica FORM "999"
                 T_Nro1[1]             
                 T_Tot1[1]             
                 T_Tot1[2]
                 T_Tot1[3]
                 T_Tot1[4]
                 T_Tot1[5]
                 T_Tot1[6]
             WITH DOWN WIDTH 150 FRAME F111 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

         
         ASSIGN T_Nro1[1] = 0
                T_Tot1    = 0.
      END.  

      IF LAST-OF(Creditos.Agencia) THEN DO:
         DISPLAY "          --- --- ---" SKIP
          "          "
          Creditos.Agencia
          "TOTAL  "
          T_Nro1[2]             
          T_Tot2[1]             
          T_Tot2[2]
          T_Tot2[3]
          T_Tot2[4]
          T_Tot2[5]
          T_Tot2[6]
          WITH DOWN WIDTH 150 FRAME F222 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         
         ASSIGN T_Nro1[2] = 0
                T_Tot2    = 0.
      END.
  END.           

  DISPLAY "          --- --- ---" SKIP(2)
          "          "
          "TOT.GENER."
          T_Nro1[3]             
          T_Tot3[1]             
          T_Tot3[2]
          T_Tot3[3]
          T_Tot3[4]
          T_Tot3[5]
          T_Tot3[6]
        WITH DOWN WIDTH 150 FRAME F333 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        
  ASSIGN T_Nro1[3] = 0
         T_Tot3    = 0.

  DISPLAY SKIP(2)
        "Resumen por Cuenta Contable" SKIP
        "---------------------------"
   WITH DOWN WIDTH 150 FRAME F3tit2acta NO-LABELS NO-BOX USE-TEXT STREAM-IO.

FOR EACH TCCtas BREAK BY TCCtas.Age BY TCCtas.Cta:
    ASSIGN T_Nro1[1] = T_Nro1[1] + 1
           T_Nro1[2] = T_Nro1[2] + 1
           T_Tot1[1] = T_Tot1[1] + TCCtas.Vlr
           T_Tot2[1] = T_Tot2[1] + TCCtas.Vlr.
           
          /* T_Tot1[2] = T_Tot1[2] 
           T_Tot2[2] = T_Tot2[2] + 
           T_Tot3[2] = T_Tot3[2] + 
           T_Tot1[3] = T_Tot1[3] + 
           T_Tot2[3] = T_Tot2[3] + 
           T_Tot3[3] = T_Tot3[3] + 
           T_Tot1[4] = T_Tot1[4] + 
           T_Tot2[4] = T_Tot2[4] + 
           T_Tot3[4] = T_Tot3[4] + 
           T_Tot1[5] = T_Tot1[5] + 
           T_Tot2[5] = T_Tot2[5] + 
           T_Tot3[5] = T_Tot3[5] + 
           T_Tot1[6] = T_Tot1[6] + 
           T_Tot2[6] = T_Tot2[6] + 
           T_Tot3[6] = T_Tot3[6] + */

    DISPLAY "          "
            TCCtas.Age      FORM "999" LABEL "Age."
            TCCtas.Cta                 LABEL "Cta-Contable"
            TCCtas.Vlr                 LABEL "Valor Sdos.Capital"
       WITH DOWN WIDTH 150 FRAME FCta1112c NO-LABELS NO-BOX USE-TEXT STREAM-IO.

    IF LAST-OF(TCCtas.Age) THEN DO:
       DISPLAY "          --- --------------- ------------------------" SKIP
               "          "
               TCCtas.Age      
               "              "
               T_Tot1[1]             
              /* T_Tot1[2]
               T_Tot1[3]
               T_Tot1[4]
               T_Tot1[5]
               T_Tot1[6]*/
           WITH DOWN WIDTH 150 FRAME FCta1112 NO-LABELS NO-BOX USE-TEXT STREAM-IO.


       ASSIGN T_Nro1[1] = 0
              T_Tot1    = 0.
    END.      
END.           

DISPLAY "          --- --------------- ------------------------" SKIP(2)
        "          TOTAL GENERAL      "
         T_Tot2[1]             

    /*    T_Tot3[2]
        T_Tot3[3]
        T_Tot3[4]
        T_Tot3[5]
        T_Tot3[6]*/
     WITH DOWN WIDTH 150 FRAME FCta3332 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

ASSIGN T_Nro1[3] = 0
       T_Tot2    = 0.

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tarjetas_Debito wWin 
PROCEDURE Tarjetas_Debito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VAR zdisp   like ahorros.sdo_disponible INITIAL 0.
     DEFINE VAR ztdisp  like ahorros.sdo_disponible INITIAL 0.
     DEFINE VAR zcanje  like ahorros.sdo_disponible INITIAL 0.
     DEFINE VAR zTcanje like ahorros.sdo_disponible INITIAL 0.
     DEFINE VAR znro    AS INTEGER INITIAL 0.
     DEFINE VAR zTnro   AS INTEGER INITIAL 0.
     DEFINE VAR wNcta   LIKE creditos.num_credito.
     DEFINE VAR wsdok   LIKE creditos.sdo_capital.
     DEFINE VAR wmonto  LIKE creditos.monto.
     FOR EACH ahorros WHERE ahorros.agencia GE AgeIni  AND      
                            ahorros.agencia LE AgeFin  AND
                            ahorros.cod_ahorro = 3     AND
                            ahorros.estado     = 1     AND
                            ahorros.tarjetadb  NE ''   AND 
                        /*  ahorros.fec_creaTdb GE fecini AND 
                            ahorros.fec_creaTdb LE fecfin AND */
                            ahorros.sdo_disponible GE ValIni AND 
                            ahorros.sdo_disponible LE ValFin NO-LOCK BREAK BY ahorros.agencia:
         IF FIRST-OF(ahorros.agencia) THEN DO:
            FIND FIRST agencias WHERE agencias.agencia EQ ahorros.agencia NO-LOCK NO-ERROR.
            PUT "Agencia : " agencias.nombre SKIP(0).
            PUT "CEDULA       NOMBRES                        CTA-AHO   SDO_DISPON    SDO_CANJE  NRO.TARJETA DB   #CUPOROTA  CUPO TOTAL   DISPONIBLE  FEC_TARJDB"  SKIP(1).             
            ASSIGN zdisp = 0  zcanje = 0 znro = 0.
         END.
         FIND FIRST creditos WHERE creditos.nit = ahorros.nit AND creditos.cod_credito = 570 AND creditos.estado = 2 NO-LOCK NO-ERROR.
         IF AVAILABLE(creditos) THEN
            ASSIGN wNcta  = creditos.num_credito
                   wsdok  = Creditos.monto - creditos.sdo_capital
                   wmonto = creditos.monto.
        ELSE ASSIGN wNcta   = 0 wsdok   = 0 wmonto  = 0.

         znro = znro + 1.
         FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
         PUT ahorros.nit                FORMAT "X(12)" " "
             TRIM(Clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.nombre) FORMAT "x(30)" " "
             ahorros.cue_ahorros        FORMAT "X(8)"  " "
             ahorros.sdo_disponible     FORMAT "-zz,zzz,zz9" "  "
             ahorros.sdo_canje          FORMAT "-zz,zzz,zz9" "  "
             ahorros.TarjetaDb          FORMAT "X(16)" " "
             wNcta                      FORMAT "zzzzzzzz9"    " "
             wmonto                     FORMAT "zzz,zzz,zz9"  " "
             wsdok                      FORMAT "-zzz,zzz,zz9" " "
             ahorros.fec_creaTdb        FORMAT "99/99/9999" SKIP(0).
         ASSIGN zdisp  = zdisp  + ahorros.sdo_disponible
                zcanje = zcanje + ahorros.sdo_canje.

         IF LAST-OF(ahorros.agencia) THEN DO:
            PUT "Total " substring(agencias.nombre,1,15) ": #Tarj:" znro FORMAT "zzz,zz9"  
                "                       " zdisp  FORMAT "-zzz,zzz,zz9" " " 
                                          zcanje FORMAT "-zzz,zzz,zz9" SKIP(2).
            ASSIGN zTdisp = ztdisp + zdisp    zTcanje = zTcanje + zcanje   ztnro = ztnro + znro.
         END.
     END.
     PUT "Total General  : #Tarj: " zTnro FORMAT "zzz,zz9"  
               "                    " zTdisp  FORMAT "-zzzzz,zzz,zz9" " " 
                                  zTcanje FORMAT "-zzzz,zzz,zz9" SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

