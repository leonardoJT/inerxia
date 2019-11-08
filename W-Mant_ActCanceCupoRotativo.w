&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
{Incluido\variable.i "shared"}
/*     DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.                                                    */
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario.                                                       */
/*     DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".                                          */
/*     DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".                                          */
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.                                             */
/*     DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.                                               */
/*     DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".                                               */
/*     DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".                                                       */
/*     /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/                                            */
/*     DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".                                               */
/*     DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.                                                        */
/*     DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".                                                */
/*     DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.                                            */
/*                                                                                                               */
/*                                                                                                               */
/*     DEFINE {1} VAR W_Manija        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_ManFin        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_ManTaq        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.                                                          */
/*     DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.                                                         */
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.                                 */
/*     DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.  */
/*     DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.                                                  */
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.                                                        */
/*     DEFINE {1} VAR W_Eleccion      AS LOGICAL.                                                                */
/*     DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.                                                           */
/*     DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.                                                  */
/*     DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".                            */
/*     /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/                                               */
/*     DEFINE {1} VAR P-Valida        AS LOGICAL.                                                                */
/*     DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.                                                     */
/*     DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.                                                    */
/*                                                                                                               */
/*     DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.                                                */
/*     DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.                                                        */
/*                                                                                                               */
/*     DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.                                                   */
/*     DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.                                                       */
/* /*****************************/                                                                               */

    DEFI VARIABLE TotReg       AS INTEG FORM "9999999"   NO-UNDO.
    DEFI VARIABLE CuoAdel      LIKE Creditos.Cuo_Pagadas NO-UNDO.
    DEFI VARIABLE W_Proyectado LIKE Creditos.Sdo_Proyectado NO-UNDO.
    DEFI VARIABLE W_Tasa       LIKE Creditos.Tasa NO-UNDO.
    DEFI VARIABLE W_PdoLiq     AS INTEG FORM "99" NO-UNDO.
    DEFI VARIABLE W_DiaPdo     AS INTEG FORM "99" NO-UNDO.
    DEFI VARIABLE KSdo         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KInt         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KCap         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KIAcu        LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KCAcu        LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.

    DEFI VARIABLE KCuo         LIKE Creditos.Cuota   NO-UNDO.              
    DEFI VARIABLE NN           AS INTEG FORM "99999" NO-UNDO.
    DEFI VARIABLE CuoFalt      AS INTEG FORM "99999" NO-UNDO.  
    DEFI VARIABLE PdoTrans     AS INTEG FORM "99999" NO-UNDO.
    DEFI VARIABLE W_FecIni     AS DATE NO-UNDO.                      
    DEFI VARIABLE xFecIni      AS DATE NO-UNDO.                      
    DEFI VARIABLE W_FecTra     AS DATE NO-UNDO.
    DEFI VARIABLE W_FecIniCont AS DATE NO-UNDO.
    DEFI VARIABLE W_SiPdoTr    AS LOG INIT FALSE NO-UNDO.
    DEFINE VARIABLE iax AS INTEGER NO-UNDO.
    DEFINE VARIABLE xtas LIKE creditos.tasa NO-UNDO.


    DEFINE VARIABLE Puntero AS ROWID.
    DEFINE VAR W_Age  LIKE Ahorros.Agencia.
    DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
    DEFINE VAR W_Nit  LIKE Ahorros.Nit.
    DEFINE VAR vinumcre LIKE creditos.num_credito.
    DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.
    DEFINE VAR W_vlr  LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
    DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
    DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
    DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
    DEFINE VARIABLE W_Error       AS LOGICAL.
    DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
    DEFINE VARIABLE W_NUEVO       AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Clientes.Nit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Clientes.Nit 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Clientes
&Scoped-define QUERY-STRING-fMain FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Nit 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS ECT-309 RECT-274 RECT-276 RECT-277 RECT-278 ~
RECT-279 RECT-280 BUTTON-1 w_Credito Btn_Consulta Btn_Cancelar btn_procesar ~
BtnDone-2 BUTTON-95 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS W_NomTitular W_EstadoCli w_Credito ~
W_EstadoCre W_Cuota W_Int_Mora W_Sdo_Capital W_Honorarios W_Int_Corrientes ~
W_SegCartera W_Int_DifCobro W_TSdoVdo W_Int_Anticipado W_SdoDeuda W_AgeCre ~
W_FecDesem W_SdoMora W_Pago_Total W_FecCance W_Pago_Minimo W_Val_Recaudo ~
W_Monto W_PagoTotal W_Plazo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 BUTTON-1 Btn_Consulta Btn_Cancelar BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dfacturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.62 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar Cupo" 
     SIZE 6 BY 1.62 TOOLTIP "Cancelar Cupo Rotativo"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 6 BY 1.62 TOOLTIP "Buscar".

DEFINE BUTTON btn_procesar 
     IMAGE-UP FILE "imagenes/proceso.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/proceso_g.bmp":U
     LABEL "&Activar Cupo" 
     SIZE 6 BY 1.62 TOOLTIP "Activar Cupo Rotativo".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 6 BY 1.62 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE W_AgeCre AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Agencia:" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Credito AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Nro.Credito" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vlr.Cuota" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecCance AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec.Cancel:" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecDesem AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Fec.Desem:" 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Honorarios AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Honorarios" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Int_Anticipado AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int.Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Int_Corrientes AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int.Corrientes" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Int_DifCobro AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int.Dif.Cobro" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Int_Mora AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int.Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Monto:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PagoTotal AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "Pago?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Pago_Minimo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pago Minimo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Pago_Total AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pago Total" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Plazo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Plazo:" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoDeuda AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Tot. Deuda" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoMora AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sdo.Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Sdo_Capital AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Capital" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SegCartera AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Seg.Cartera" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TSdoVdo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vr. Para Estar Al Día" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Val_Recaudo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vlr. Recaudado" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_EstadoCli AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activo", 1,
"Inactivo", 2
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE W_EstadoCre AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Aprobado No Desembolsado", 1,
"Normal Desembolsado", 2,
"Cancelado", 3,
"Retirado sin Aprobar", 4,
"Castigado", 5
     SIZE 25 BY 2.96 NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 11.04.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8.29 BY 9.96.

DEFINE RECTANGLE RECT-276
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 4.58.

DEFINE RECTANGLE RECT-277
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 3.5.

DEFINE RECTANGLE RECT-278
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 3.5.

DEFINE RECTANGLE RECT-279
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 2.15.

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 4.58.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1.54 COL 102.14 WIDGET-ID 58
     Clientes.Nit AT ROW 1.81 COL 11 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     W_NomTitular AT ROW 1.81 COL 37.72 COLON-ALIGNED WIDGET-ID 70
     W_EstadoCli AT ROW 2.62 COL 39.72 NO-LABEL WIDGET-ID 118
     w_Credito AT ROW 2.81 COL 11 COLON-ALIGNED WIDGET-ID 68
     Btn_Consulta AT ROW 3.15 COL 102.14 WIDGET-ID 54
     W_EstadoCre AT ROW 4 COL 5 NO-LABEL WIDGET-ID 110
     W_Cuota AT ROW 4.15 COL 43 COLON-ALIGNED WIDGET-ID 92
     W_Int_Mora AT ROW 4.15 COL 76 COLON-ALIGNED WIDGET-ID 84
     Btn_Cancelar AT ROW 4.77 COL 102.14 WIDGET-ID 52
     W_Sdo_Capital AT ROW 4.96 COL 43 COLON-ALIGNED WIDGET-ID 72
     W_Honorarios AT ROW 4.96 COL 76 COLON-ALIGNED WIDGET-ID 86
     W_Int_Corrientes AT ROW 5.77 COL 43 COLON-ALIGNED WIDGET-ID 78
     W_SegCartera AT ROW 5.77 COL 76 COLON-ALIGNED WIDGET-ID 88
     btn_procesar AT ROW 6.38 COL 102.14 WIDGET-ID 96
     W_Int_DifCobro AT ROW 6.58 COL 43 COLON-ALIGNED WIDGET-ID 80
     W_TSdoVdo AT ROW 6.58 COL 76 COLON-ALIGNED WIDGET-ID 74
     W_Int_Anticipado AT ROW 7.38 COL 43 COLON-ALIGNED WIDGET-ID 82
     W_SdoDeuda AT ROW 7.38 COL 76 COLON-ALIGNED WIDGET-ID 76
     W_AgeCre AT ROW 7.65 COL 12.43 COLON-ALIGNED WIDGET-ID 126
     BtnDone-2 AT ROW 8 COL 102.14 WIDGET-ID 50
     W_FecDesem AT ROW 8.5 COL 12.43 COLON-ALIGNED WIDGET-ID 128
     W_SdoMora AT ROW 8.81 COL 43 COLON-ALIGNED WIDGET-ID 90
     W_Pago_Total AT ROW 8.81 COL 76 COLON-ALIGNED WIDGET-ID 106
     W_FecCance AT ROW 9.35 COL 12.43 COLON-ALIGNED WIDGET-ID 134
     W_Pago_Minimo AT ROW 9.62 COL 43 COLON-ALIGNED WIDGET-ID 102
     W_Val_Recaudo AT ROW 9.62 COL 76 COLON-ALIGNED WIDGET-ID 104
     BUTTON-95 AT ROW 9.88 COL 103 WIDGET-ID 60
     W_Monto AT ROW 10.19 COL 12.43 COLON-ALIGNED WIDGET-ID 124
     W_PagoTotal AT ROW 10.42 COL 76 COLON-ALIGNED WIDGET-ID 94
     W_Plazo AT ROW 11 COL 12.57 COLON-ALIGNED WIDGET-ID 130
     ECT-309 AT ROW 1.27 COL 3 WIDGET-ID 62
     RECT-274 AT ROW 1.27 COL 101 WIDGET-ID 66
     RECT-276 AT ROW 3.88 COL 33 WIDGET-ID 100
     RECT-277 AT ROW 8.54 COL 33 WIDGET-ID 108
     RECT-278 AT ROW 3.73 COL 4 WIDGET-ID 116
     RECT-279 AT ROW 1.54 COL 33 WIDGET-ID 122
     RECT-280 AT ROW 7.46 COL 4 WIDGET-ID 132
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.29 BY 15.54
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Plan Pagos - W-Mant_ActCanceCupoRotativo.w"
         HEIGHT             = 15.54
         WIDTH              = 109.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR FILL-IN W_AgeCre IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_AgeCre:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Cuota IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Cuota:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR RADIO-SET W_EstadoCli IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET W_EstadoCre IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecCance IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_FecCance:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_FecDesem IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_FecDesem:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Honorarios IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Honorarios:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Int_Anticipado IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Int_Anticipado:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Int_Corrientes IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Int_Corrientes:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Int_DifCobro IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Int_DifCobro:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Int_Mora IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Int_Mora:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Monto IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Monto:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_PagoTotal IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_PagoTotal:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Pago_Minimo IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Pago_Minimo:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Pago_Total IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Pago_Total:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Plazo IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Plazo:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_SdoDeuda IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_SdoDeuda:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_SdoMora IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_SdoMora:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Sdo_Capital IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Sdo_Capital:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_SegCartera IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_SegCartera:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_TSdoVdo IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_TSdoVdo:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN W_Val_Recaudo IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_Val_Recaudo:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Plan Pagos - W-Mant_ActCanceCupoRotativo.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Plan Pagos - W-Mant_ActCanceCupoRotativo.w */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME fMain
DO:
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


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME fMain /* Cancelar Cupo */
DO:
IF INTEGER(W_Credito:SCREEN-VALUE) NE 0 THEN DO:
    MESSAGE "Desea Cancelar ó Anular el Cupo Rotativo....?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Cancelar ó Anular el Cupo Rotativo" UPDATE choice AS LOGICAL.
    IF NOT choice THEN 
        RETURN.

    FIND FIRST Facturacion WHERE 
         Facturacion.nit    EQ w_nit AND Facturacion.num_credito EQ W_Credito AND
         Facturacion.estado EQ 1 
         NO-LOCK NO-ERROR.
    IF AVAILABLE(Facturacion) THEN DO:
       IF Facturacion.Val_Recaudo GE Facturacion.Pago_Total AND Facturacion.PagoTotal EQ YES THEN DO:
          IF W_TSdoVdo EQ 0 AND W_SdoDeuda EQ 0 THEN DO:
             FIND FIRST Creditos WHERE 
                  creditos.nit EQ w_nit AND creditos.num_credito EQ W_Credito AND
                  (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND
                  creditos.estado EQ 2 AND creditos.sdo_capital  LE 0
                  NO-ERROR.
             IF AVAILABLE (Creditos) THEN DO:
                IF Creditos.sdo_capital LT 0 THEN
                   UPDATE Creditos.sdo_capital = 0.
                UPDATE Creditos.Estado = 3
                       Creditos.Fec_cancetotal = W_Fecha.
                MESSAGE "Cupo Rotativo Cancelado ó Anulado...."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                W_Credito:SCREEN-VALUE = "".
             END.
             ELSE DO:
                  MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP
                          "Porque Ya Esta Cancelado ó el Crédito Tiene Saldo en Capital." 
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  RETURN.
            END.
          END.
          ELSE DO:
              MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP
                      "Porque el Crédito Tiene Saldos Pendientes Para Ponerse al Día."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.

       END.
       ELSE DO:
         IF Facturacion.Pago_Total EQ 0 AND Facturacion.Pago_Minimo  EQ 0 THEN DO:
            FIND FIRST Creditos WHERE 
                 creditos.nit EQ w_nit AND creditos.num_credito EQ W_Credito AND
                 (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND
                 creditos.estado EQ 2 AND creditos.sdo_capital  LE 0
                 NO-ERROR.
            IF AVAILABLE (Creditos) THEN DO:
               IF Creditos.sdo_capital LT 0 THEN
                  UPDATE Creditos.sdo_capital = 0.
               UPDATE Creditos.Estado = 3
                      Creditos.Fec_cancetotal = W_Fecha.
               MESSAGE "Cupo Rotativo Cancelado ó Anulado...."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
               W_Credito:SCREEN-VALUE = "".
            END.
            ELSE DO:
                 MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP
                         "Porque Ya Esta Cancelado ó el Crédito Tiene Saldo en Capital." 
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 RETURN.
            END.
         END.
         ELSE DO:
              MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP
                      "Porque Tiene Saldo Pendiente en el Crédito ó Facturacion Actual." 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
         END.
       END.
/*        ELSE DO:                                                               */
/*            MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP  */
/*                    "Porque el Vlr. Recaudo NO es Superior o Igual al "        */
/*                    "Pago Total de la Factura.."                               */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                            */
/*            RETURN.                                                            */
    END.
    /*** No le fue facturado*/
    ELSE DO:
       FIND FIRST Creditos WHERE 
            creditos.nit EQ w_nit AND creditos.num_credito EQ W_Credito AND
            (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND
            creditos.sdo_capital  LE 0
            NO-ERROR.
       IF AVAILABLE (Creditos) THEN DO:
          IF Creditos.sdo_capital LT 0 THEN
             UPDATE Creditos.sdo_capital = 0.
          UPDATE Creditos.Estado = 3
                 Creditos.Fec_CanceTotal = W_Fecha.
          MESSAGE "Cupo Rotativo Cancelado ó Anulado...."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          W_Credito:SCREEN-VALUE = "".
       END.
       ELSE DO:
            MESSAGE "El Cupo Rotativo NO Puede Ser Cancelado ó Anulado." SKIP
                    "Porque el Crédito Tiene Saldo en Capital."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
       END.
    END.
    DYNAMIC-FUNCTION('closeQuery':U IN h_dfacturacion).

END.
ELSE DO:
    MESSAGE "El Número del Crédito Debe Ser Digitado." SKIP
            "Debe Ser Diferente a Cero...."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME fMain /* Button 3 */
DO:
/*     RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar wWin
ON CHOOSE OF btn_procesar IN FRAME fMain /* Activar Cupo */
DO:
IF INTEGER(W_Credito:SCREEN-VALUE) NE 0 THEN DO:
  MESSAGE "Desea Activar el Cupo Rotativo....?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Activar el Cupo Rotativo" UPDATE choice AS LOGICAL.
  IF NOT choice THEN 
      RETURN.

  IF INTEGER(W_EstadoCli:SCREEN-VALUE)  EQ 1 THEN DO:
     FIND FIRST Facturacion WHERE 
          Facturacion.nit    EQ w_nit AND Facturacion.num_credito EQ W_Credito AND
          Facturacion.estado EQ 1 
          NO-LOCK NO-ERROR.
     IF AVAILABLE(Facturacion) THEN DO:
        FIND FIRST Creditos WHERE 
             creditos.nit EQ w_nit AND creditos.num_credito EQ W_Credito AND
             (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND
             creditos.estado EQ 3
             NO-ERROR.
        IF AVAILABLE (Creditos) THEN DO:
           FIND FIRST planpagos WHERE
                planpagos.agencia     = creditos.agencia     AND   
                planpagos.nit         = creditos.nit         AND   
                planpagos.cod_credito = creditos.Cod_credito AND   
                planpagos.num_credito = creditos.num_credito
                NO-ERROR.
           IF NOT AVAILABLE(planpagos) THEN DO:
              RUN CrearPlanPagos.
           END.
           ELSE DO:
              UPDATE Creditos.Estado = 2  /* Normal*/
                     Creditos.Fec_cancetotal = ?.
              MESSAGE "Cupo Rotativo Activado...."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
           END.
           W_Credito:SCREEN-VALUE = "".
        END.
        ELSE DO:
             MESSAGE "El Cupo Rotativo Ya esta Activo ó NO ha sido Creado."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN.
        END.
     END.
     /*** No le fue facturado*/
     ELSE DO:
        FIND FIRST Creditos WHERE 
             creditos.nit EQ w_nit AND creditos.num_credito EQ W_Credito AND
             (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND
             creditos.estado EQ 3
             NO-ERROR.
        IF AVAILABLE (Creditos) THEN DO:
           FIND FIRST planpagos WHERE
                planpagos.agencia     = creditos.agencia     AND   
                planpagos.nit         = creditos.nit         AND   
                planpagos.cod_credito = creditos.Cod_credito AND   
                planpagos.num_credito = creditos.num_credito
                NO-ERROR.
           IF NOT AVAILABLE(planpagos) THEN DO:
              RUN CrearPlanPagos.
           END.
           ELSE DO:
              UPDATE Creditos.Estado = 2.  /* Normal*/
                     Creditos.Fec_cancetotal = ?.
              MESSAGE "Cupo Rotativo Activado...."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
           END.
           W_Credito:SCREEN-VALUE = "".
        END.
        ELSE DO:
             MESSAGE "El Cupo Rotativo Ya esta Activo ó NO ha sido Creado."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN.
        END.
     END.
  END.
  ELSE DO:
      MESSAGE "El Cupo Rotativo NO Puede Ser Activado." SKIP
              "Porque El Asociado Esta Inactivo..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  DYNAMIC-FUNCTION('closeQuery':U IN h_dfacturacion).

END.
ELSE DO:
    MESSAGE "El Número del Crédito Debe Ser Digitado." SKIP
            "Debe Ser Diferente a Cero...."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

END.
/*    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dfacturacion,                                           */
/*      INPUT "nit EQ '" + w_nit + "' AND num_credito EQ " + STRING(vinumcre) + " AND ESTADO EQ 1").  */
/* /*      INPUT "nit EQ '" + w_nit + "' AND num_credito EQ " + STRING(vinumcre) + ")" ). */          */
/*    DYNAMIC-FUNCTION('setQuerySort':U IN h_dfacturacion,                                            */
/*      INPUT "by Num_Credito").                                                                      */
/*    DYNAMIC-FUNCTION('openQuery':U IN h_dfacturacion).                                              */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit wWin
ON LEAVE OF Clientes.Nit IN FRAME fMain /* Nit */
DO:
  DO WITH FRAME F_Clientes:
     FIND Clientes WHERE 
          (Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE) /*AND
          Clientes.estado EQ 1*/
          NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN DO:
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ASSIGN W_Nit = Clientes.Nit.
        ASSIGN W_EstadoCli:SCREEN-VALUE = TRIM(STRING(Clientes.Estado)).
     END.
     ELSE DO:
        MESSAGE "Cliente No Existe..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END.
     W_Credito:SCREEN-VALUE = "".
     btn_procesar:SENSITIVE = FALSE.
     btn_cancelar:SENSITIVE = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Credito wWin
ON LEAVE OF w_Credito IN FRAME fMain /* Nro.Credito */
DO:
  ASSIGN w_credito.
  ASSIGN vinumcre = integer(w_credito:SCREEN-VALUE).
  DO WITH FRAME F_Clientes:
      DYNAMIC-FUNCTION('setQueryWhere':U IN h_dfacturacion,
        INPUT "nit EQ '" + w_nit + "' AND num_credito EQ " + STRING(vinumcre) + " AND ESTADO EQ 1").
      DYNAMIC-FUNCTION('setQuerySort':U IN h_dfacturacion,
        INPUT "by Num_Credito").
      DYNAMIC-FUNCTION('openQuery':U IN h_dfacturacion).
     FIND Creditos WHERE 
         Creditos.Nit         EQ Clientes.nit:SCREEN-VALUE AND 
         Creditos.num_credito EQ vinumcre AND
        (Creditos.cod_credito EQ 570 OR Creditos.cod_credito EQ 870) /*AND*/
         /*Creditos.estado      EQ 3*/ NO-ERROR.
     IF AVAILABLE(Creditos) THEN DO:
        ASSIGN W_EstadoCre:SCREEN-VALUE = TRIM(STRING(Creditos.Estado)).
        ASSIGN W_Cuota:SCREEN-VALUE = TRIM(STRING(Creditos.Cuota,"zzz,zzz,zz9.99")).
        ASSIGN W_Monto:SCREEN-VALUE = TRIM(STRING(Creditos.Monto,"zzz,zzz,zz9.99")).
        ASSIGN W_AgeCre:SCREEN-VALUE = TRIM(STRING(Creditos.Agencia,"999")).
        ASSIGN W_FecDesem:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso).
        ASSIGN W_FecCance:SCREEN-VALUE = STRING(Creditos.Fec_CanceTotal).
        ASSIGN W_Plazo:SCREEN-VALUE = TRIM(STRING(Creditos.Plazo,"999")).
        ASSIGN W_Sdo_Capital:SCREEN-VALUE = TRIM(STRING(Creditos.Sdo_Capital,"->>>,>>>,>>>,>>9")).
        ASSIGN W_Int_Corrientes:SCREEN-VALUE = STRING(Creditos.Int_Corrientes).
        ASSIGN W_Int_DifCobro:SCREEN-VALUE = STRING(Creditos.Int_DifCobro).
        ASSIGN W_Int_Anticipado:SCREEN-VALUE = STRING(Creditos.Int_Anticipad).
        ASSIGN W_Int_Mora:SCREEN-VALUE = STRING(Creditos.Int_Mora).
        ASSIGN W_Honorarios:SCREEN-VALUE = STRING(Creditos.Honorarios).
        ASSIGN W_SegCartera:SCREEN-VALUE = STRING(Creditos.Poliza).
        ASSIGN W_TSdoVdo:SCREEN-VALUE = 
            TRIM(STRING(Creditos.Honorarios + Creditos.Costas + Creditos.Polizas +
                  Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                  Creditos.Int_DifCobro - Creditos.Int_Anticipado,"->>>,>>>,>>>,>>9")).
        ASSIGN  W_SdoDeuda:SCREEN-VALUE = 
            TRIM(STRING(Creditos.Honorarios + Creditos.Costas + Creditos.Polizas +
                  Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                  Creditos.Int_DifCobro  + Creditos.Sdo_Capital - Creditos.Int_Anticipado,"->>>,>>>,>>>,>>9")).
        FIND FIRST Facturacion WHERE facturacion.nit EQ creditos.nit AND
            facturacion.num_credito EQ creditos.num_credito AND
            facturacion.estado EQ 1
            NO-LOCK NO-ERROR.
        IF AVAILABLE (Facturacion) THEN DO:
           ASSIGN W_SdoMora:SCREEN-VALUE = 
                  TRIM(STRING(Facturacion.Val_atrasokpen,"->>>,>>>,>>>,>>9")).
           ASSIGN W_Pago_Minimo:SCREEN-VALUE = 
                  TRIM(STRING(Facturacion.Pago_Minimo,"->>>,>>>,>>>,>>9")).
           ASSIGN W_Pago_Total:SCREEN-VALUE = 
                  TRIM(STRING(Facturacion.Pago_Total,"->>>,>>>,>>>,>>9")).

           ASSIGN W_Val_Recaudo:FORMAT = "->>>,>>>,>>>,>>9.99".
           ASSIGN W_Val_Recaudo:SCREEN-VALUE = 
                  TRIM(STRING(Facturacion.Val_Recaudo,"->>>,>>>,>>>,>>9")).
           ASSIGN W_PagoTotal:SCREEN-VALUE = 
                  STRING(Facturacion.PagoTotal).
        END.
        ELSE
           ASSIGN W_SdoMOra:SCREEN-VALUE = STRING(0).
        btn_procesar:SENSITIVE = TRUE.
        btn_cancelar:SENSITIVE = TRUE.
     END.
     ELSE DO:
        MESSAGE "El Credito No Existe...."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        W_Credito:SCREEN-VALUE = "".
        btn_procesar:SENSITIVE = FALSE.
        btn_cancelar:SENSITIVE = FALSE.
     END.
  END.
END.
 
/* /*Cupo*/        W_TSdoVdo = Creditos.Honorarios       + Creditos.Costas         + Creditos.Polizas        +             */
/*                  Creditos.Int_MorCobrar    + Creditos.Int_MoraDifCob +  Creditos.Int_Corrientes + Creditos.Int_DifCobro */
/*                  - Creditos.Int_Anticipado + Creditos.cuota.                                                            */


/*       W_SdoDeuda = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        + */
/*                    Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes + */
/*                    Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado   */
/*           W_SdoDeuda:SCREEN-VALUE              = STRING(W_SdoDeuda) */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dfacturacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedfacturacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfacturacion ).
       RUN repositionObject IN h_dfacturacion ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 6.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsPer_Factura,Agencia,Nit,Num_factura,fec_corte,Pago_Minimo,Pago_Total,Val_Recaudo,PagoTotal,Rec_Capital,Rec_Honorarios,Rec_IntMora,Rec_SegcarteraEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdfacturacionUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 12.58 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 3.50 , 102.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dfacturacion , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             W_Plazo:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearPlan wWin 
PROCEDURE CrearPlan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE PlanPagos.
ASSIGN PlanPagos.Agencia      = Creditos.Agencia          
       PlanPAgos.Nit          = Creditos.Nit              
       PlanPagos.Num_Credito  = Creditos.Num_credito          
       PlanPagos.Pagare       = Creditos.Pagare        
       PlanPagos.Cod_Credito  = Creditos.Cod_Credito           
       PlanPagos.Tip_Credito  = Creditos.Tip_Credito
       PlanPagos.Cuota        = Creditos.Cuota            
       PlanPagos.Tasa         = Creditos.Tasa             
       PlanPagos.Plazo        = Creditos.Plazo            
       PlanPagos.Monto_Actual = Creditos.Monto            
       PlanPagos.Id_PdoMes    = 0.                 /*Inicia en futuro*/
ASSIGN TotReg = TotReg + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearPlanPagos wWin 
PROCEDURE CrearPlanPagos :
/*------------------------------------------------------------------------------
  Author: Félix Vargas     
  Fecha : 4 de Abril de 2008
------------------------------------------------------------------------------*/
DEFINE VARIABLE wfec_proxpago AS DATE.                
DEFINE VARIABLE wfec_inic     AS DATE.                
DEFINE VARIABLE wfec_vcto     AS DATE.                
DEFINE VARIABLE wplazo        LIKE creditos.plazo.
DEFINE VARIABLE wtasa         LIKE creditos.tasa.
DEFINE VARIABLE wperiodo      AS INTEGER.
DEFINE VARIABLE vdsdoproyec   AS DECIMAL.

FIND FIRST pro_creditos WHERE
     pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
IF AVAILABLE(pro_creditos) THEN
    ASSIGN  wplazo   = pro_creditos.Pla_Maximo.
    FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
    IF AVAILABLE(indicadores) THEN DO:
       ASSIGN wtasa = (((EXP( (indicadores.tasa / 100) + 1,1 / 12)) - 1 )  * 100) * 12.
    END.
ELSE DO:
    MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
ASSIGN creditos.tasa  = wtasa
       creditos.plazo = wplazo
       creditos.Per_pago = 4.

IF Creditos.Sdo_Capital       LT 0
   OR Creditos.Nit            LE "0"
   OR Creditos.Fec_Desembolso EQ ? OR STRING(Creditos.Fec_Desembolso) LE " "
   OR Creditos.Monto          LE 0
   OR Creditos.Plazo          LE 0 
   THEN DO:
       MESSAGE "No Es Posible Crear Plan de Pagos - Ni Activar Cupo Rotativo" SKIP
               "Agencia      : " creditos.agencia        SKIP
               "Nit               : " creditos.Nit            SKIP
               "Credito        : " creditos.num_credito    SKIP
               "Sdo.Capital  : " creditos.Sdo_Capital    SKIP
               "Fec.Desemb: " creditos.Fec_Desembolso SKIP
               "Monto          : " creditos.Monto          SKIP
               "Plazo            : " creditos.Plazo  
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN.
   END.

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
    MESSAGE "No se Encontro Periodo Vigente Para La Facturacion Actual" SKIP
            "Consulte con el Administrador del Sistema."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Actualiza Credito y Crea el Plan de Pagos*/
ASSIGN Per_pago                = 4
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
       Creditos.Fec_cancetotal = ?
       Creditos.For_Interes    = 1.

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
       PlanPagos.Fec_Inic                = W_Fecha
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

MESSAGE "Cupo Rotativo Activado...."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*****************/
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY W_NomTitular W_EstadoCli w_Credito W_EstadoCre W_Cuota W_Int_Mora 
          W_Sdo_Capital W_Honorarios W_Int_Corrientes W_SegCartera 
          W_Int_DifCobro W_TSdoVdo W_Int_Anticipado W_SdoDeuda W_AgeCre 
          W_FecDesem W_SdoMora W_Pago_Total W_FecCance W_Pago_Minimo 
          W_Val_Recaudo W_Monto W_PagoTotal W_Plazo 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE ECT-309 RECT-274 RECT-276 RECT-277 RECT-278 RECT-279 RECT-280 BUTTON-1 
         Clientes.Nit w_Credito Btn_Consulta Btn_Cancelar btn_procesar 
         BtnDone-2 BUTTON-95 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
RUN SUPER.
DYNAMIC-FUNCTION('closeQuery':U IN h_dfacturacion).
/* DEBUGGER:INITIATE().   */
/* DEBUGGER:SET-BREAK().  */
RUN Valida_Usuario NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  APPLY "Choose" TO BtnDone-2 IN FRAME fMain.
  RETURN NO-APPLY.
END.
ELSE DO:
   btn_procesar:SENSITIVE = FALSE. 
   btn_cancelar:SENSITIVE = FALSE. 
   DYNAMIC-FUNCTION('closeQuery':U IN h_dfacturacion).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Usuario wWin 
PROCEDURE Valida_Usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE vlempre AS LOGICAL INITIAL FALSE NO-UNDO.

FOR EACH Empresas WHERE Empresas.Cod_empresa EQ 6  AND
                        Empresas.Estado      EQ 1         AND
                        Empresas.Agencia     EQ 24 NO-LOCK:
    FIND Usuarios WHERE
         Usuarios.Estado  EQ 1                    AND  
         Usuarios.Usuario EQ W_Usuario            AND 
         Usuarios.grupo   EQ 6 
        NO-LOCK NO-ERROR.     
    IF AVAILABLE(usuarios) THEN DO:
       FIND Clientes WHERE
            Clientes.Cod_Empresa EQ Empresas.Cod_Empresa AND
            Clientes.Estado      EQ 1                    AND
            Clientes.Nit         EQ Usuarios.Nit
            NO-LOCK NO-ERROR.
       IF NOT AVAILABLE(Clientes) THEN DO:
          MESSAGE "El Usuario: " W_Usuario "- NO Existe en Clientes con la Empresa de Juriscoop(6)." SKIP
                  "Este Proceso Será Cancelado...."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN ERROR.
       END.
    END.
    ELSE DO:
      MESSAGE "El Usuario: " W_Usuario "- NO Pertenece al Grupo de Operaciones(6)." SKIP
              "Este Proceso Será Cancelado...."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN ERROR.
    END.
    ASSIGN vlempre = YES.
END.
IF vlempre EQ FALSE THEN DO:
    MESSAGE "La Empresa Juriscoop, NO Existe o Esta Incativa..." SKIP
            "Este Proceso Será Cancelado...."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*     APPLY "Choose" TO BtnDone-2 IN FRAME fMain. */
    RETURN ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

