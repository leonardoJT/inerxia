&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
{incluido\Variable.i "SHARED"}
/* /** Temporales*/                                                                                             */
/*   DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "308". /* 308 - Contabiliza*/               */
/*   DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.                                                        */
/*   DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".                       */
/*   DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "035". /*"035".*/                           */
/*                                                                                                               */
/*     DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.                                                    */
/* /*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */                                                 */
/*     DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".                                          */
/*     DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".                                          */
/* /*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */                                      */
/*     DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.                                               */
/*     DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".                                               */
/*     DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".                                                       */
/*     /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/                                            */
/*     DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".                                               */
/*     DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.                                                        */
/*     DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".                                                */
/*     DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.                                            */
/*     DEFINE {1} VAR W_Manija        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_ManFin        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_ManTaq        AS HANDLE.                                                                 */
/*     DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.                                                          */
/*     DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.                                                         */
/* /*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */                          */
/*     DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.  */
/*     DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.                                                  */
/* /*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */                                                 */
/*     DEFINE {1} VAR W_Eleccion      AS LOGICAL.                                                                */
/*     DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.                                                           */
/*     DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.                                                  */
/*     DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".                            */
/*     /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/                                               */
/*     DEFINE {1} VAR P-Valida        AS LOGICAL.                                                                */
/*     DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.                                                     */
/*     DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.                                                    */
/*     DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.                                                */
/*     DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.                                                        */
/*     DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.                                                   */
/*     DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.                                                       */
/* /***********************************/                                                                        */
DEFINE VARIABLE AgeIni       AS INTEGER FORMAT "999" INITIAL 0.
DEFINE VARIABLE AgeFin       AS INTEGER FORMAT "999" INITIAL 999.
DEFINE VARIABLE Listado      AS CHARACTER INITIAL "".
DEFINE VARIABLE w_preliminar AS LOGICAL INITIAL FALSE.
DEFINE VARIABLE w_oficial    AS LOGICAL INITIAL FALSE.

DEFINE VAR vlgenofi       AS LOGICAL INITIAL FALSE.
DEFINE VAR vlgenpagos     AS LOGICAL INITIAL FALSE.
DEFINE VAR vttiempo       AS INTEGER.
DEFINE VAR vtsegundos     AS INTEGER.
DEFINE VAR vtminutos      AS INTEGER.
DEFINE VAR vthora         AS INTEGER.

DEFINE VAR wfecinicorte   AS DATE.
DEFINE VAR wvlrAboi       AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboim      AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrabok       AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrRet        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrHon        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotabok       AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtothon        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotpol        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotret        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotaboi       AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotaboim      AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboSC      AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wfec_Aproxpago AS DATE.
DEFINE VAR wfec_Ainic     AS DATE.
DEFINE VAR wfec_Avcto     AS DATE.
DEFINE VAR wfec_Pproxpago AS DATE.
DEFINE VAR wfec_Pinic     AS DATE.
DEFINE VAR wfec_Pvcto     AS DATE.
DEFINE VAR wperiodo       AS INTEGER INITIAL 0.
DEFINE VAR wvlrrecaudo    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.

DEFINE TEMP-TABLE Temcreditos
    FIELD TAgencia   LIKE mov_creditos.agencia
    FIELD TNit       LIKE mov_creditos.nit
    FIELD TNumcred   LIKE mov_creditos.num_credito
    FIELD TCodoper   LIKE mov_creditos.cod_operacion
    FIELD TValcheque LIKE mov_creditos.Val_Cheque   INITIAL 0
    FIELD TValefecti LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TFecha     LIKE mov_creditos.Fecha INITIAL ?
    FIELD Tcpte      LIKE mov_creditos.Cpte  INITIAL 0
    INDEX idxagenit TAgencia TNit.
                                  
DEFINE TEMP-TABLE tmpcre
    FIELD tAge     LIKE creditos.agencia
    FIELD tNit     LIKE creditos.nit
    FIELD tCre     LIKE creditos.num_credito
    FIELD tOpe     LIKE mov_creditos.cod_operacion
    FIELD tVal_Che LIKE creditos.sdo_capital
    FIELD tVal_Efe LIKE creditos.sdo_capital
    FIELD TFecha   LIKE creditos.Fec_UltPago
    FIELD TCpte    LIKE mov_creditos.cpte.

DEFINE TEMP-TABLE tmp_enc_extracto /* Encabezado de Extracto */
    FIELD agencia   LIKE creditos.agencia
    FIELD nom_agen  AS CHARACTER FORMAT "X(30)"
    FIELD nit       LIKE mov_creditos.nit
    FIELD fecor     LIKE mov_creditos.Fecha
    FIELD nombre    AS CHAR FORMAT "X(60)"
    FIELD direccion LIKE clientes.Dir_comercial
    FIELD telefono  LIKE clientes.Tel_comercial
    FIELD ciudad    AS CHARACTER FORMAT "X(60)"
    FIELD numcre    LIKE mov_creditos.Num_Credito
    FIELD cuptot    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD cupDis    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Sobrecupo AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD sdo_ant   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Cargos    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD intCtes   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD IntMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD OtrosCar  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Pagos     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD NvoSdo    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD SdoMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD PagoTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD PagoMin   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Tasa_IC   AS DECIMAL FORMAT "->9.99"                 INITIAL 0
    FIELD Tasa_EA   AS DECIMAL FORMAT "->9.99"                 INITIAL 0
    FIELD FecMora   AS DATE
    FIELD FecPlazo  AS DATE
    FIELD FacDesde  AS DATE
    FIELD FecHasta  AS DATE
    FIELD cuota     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    FIELD valatrak  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    FIELD seg_cartera AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    INDEX idxAgenit agencia nit.

DEFINE TEMP-TABLE tmp_det_Extracto /* Detalle de Extracto */
    FIELD agencia      LIKE creditos.agencia
    FIELD Nit          LIKE clientes.nit
    FIELD num_credito  LIKE creditos.num_credito
    FIELD fec_trans    LIKE creditos.fec_pago
    FIELD descripcion  LIKE mov_creditos.descrip
    FIELD NumDocto     LIKE mov_contable.nro_auditoria
    FIELD CR           AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD DB           AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    INDEX idxagnit agencia nit.

DEFINE TEMP-TABLE tmp_Tot_extracto /* Estadisticas */
    FIELD agencia    LIKE creditos.agencia
    FIELD Nombre     LIKE agencias.nombre
    FIELD TotAso     AS INTEGER
    FIELD TotAsoExt  AS INTEGER
    FIELD Tcuptot    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TcupDis    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TSobrecupo AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD Tsdo_ant   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TCargos    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TintCtes   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TIntMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TOtrosCar  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagos     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TNvoSdo    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TSdoMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoMin   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoSegC  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD Tasa_IC    AS DECIMAL FORMAT "->9.99"
    FIELD Tasa_EA    AS DECIMAL FORMAT "->9.99"
    FIELD FecMora    AS DATE
    FIELD FecPlazo   AS DATE
    FIELD FacDesde   AS DATE
    FIELD FecHasta   AS DATE
    INDEX idxAge agencia.

/* TEMPORAL para control d seguro de cartera */
DEFINE TEMP-TABLE Temseguro
    FIELD TAgencia   LIKE mov_contable.agencia
    FIELD TNit       LIKE clientes.nit  
    FIELD TNumcre    LIKE creditos.num_credito
    FIELD TCuenta    LIKE mov_contable.cuenta     
    FIELD TSaldo     LIKE creditos.sdo_capital INITIAL 0
    FIELD Taplicar   AS LOGICAL INITIAL FALSE
    INDEX idxagenit TAgencia TNit TNumcre.

/* Definicion de Variables de Trabajo */
DEFINE VAR wVlrOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wzswDet    AS LOGICAL INITIAL FALSE.
DEFINE VAR wcupdis    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wsobcup    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wnomciu    AS CHARACTER FORMAT "X(60)"   INITIAL "".
DEFINE VAR wdircorres LIKE clientes.DIR_comercial   INITIAL "".
DEFINE VAR wcodciu    LIKE clientes.lugar_comercial INITIAL "".
DEFINE VAR wtel       LIKE clientes.Tel_comercial   INITIAL "".
DEFINE VAR wintmor    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagtot    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wcuota     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagomin   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagoSegC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wdetaFact  AS CHARACTER FORMAT "X(45)".
DEFINE VAR wsdoant    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.    
DEFINE VAR wcant      AS INTEGER INITIAL 0.
DEFINE VAR wcantot    AS INTEGER INITIAL 0.
DEFINE VAR wcantage   AS INTEGER INITIAL 0.
DEFINE VAR wcantotage AS INTEGER INITIAL 0.
/* Fechas de Corte */
DEFINE VAR wfecIni    AS DATE.
DEFINE VAR wfecfin    AS DATE.
DEFINE VAR wfecplazo  AS DATE.
DEFINE VAR wTotAso     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotAsoExt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTcuptot    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTcupDis    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTSobrecupo AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTsdo_ant   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTCargos    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTintCtes   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTIntMora   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTOtrosCar  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagos     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTNvoSdo    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTSdoMora   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagoTotal AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagoMin   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wTPagoSegC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wvlrCobSC   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wTCobroSC   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wnro_auditoria LIKE mov_contable.nro_auditoria.
DEFINE VAR wperNuevo        AS INTEGER INITIAL 0.   
DEFINE VAR wfec_proxpago    AS DATE.                
DEFINE VAR wfec_inic        AS DATE.                
DEFINE VAR wfec_vcto        AS DATE.                
DEFINE VAR wfec_ANTproxpago AS DATE.
DEFINE VAR wfec_ANTinic     AS DATE.
DEFINE VAR wfec_ANTvcto     AS DATE.
DEFINE VAR wcontador        AS INTEGER INITIAL 0.
DEFINE VAR wsalsegcar       LIKE mov_contable.db        INITIAL 0  NO-UNDO.
DEFINE VAR wnatura          LIKE cuentas.naturaleza     INITIAL "" NO-UNDO.
DEFINE VAR Pagoinme         AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.

DEFINE VARIABLE viEstado AS INTEGER NO-UNDO.

ASSIGN viEstado = 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 BUTTON-143 BtnDone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vper_facturacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 6 BY 1.5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.14 BY 1.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-143 AT ROW 1.23 COL 62 WIDGET-ID 68
     BtnDone AT ROW 1.23 COL 69 WIDGET-ID 70
     "            ACTIVOS FIJOS - ADMINISTRATIVO" VIEW-AS TEXT
          SIZE 62 BY 1.08 AT ROW 3.04 COL 23 WIDGET-ID 6
          BGCOLOR 18 FGCOLOR 15 FONT 0
     RECT-7 AT ROW 1 COL 1 WIDGET-ID 74
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 21
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 3
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Activos Fijos - Administrativo"
         HEIGHT             = 21
         WIDTH              = 126
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 17
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Activos Fijos - Administrativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Activos Fijos - Administrativo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F-Main
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


/*   &IF DEFINED (adm-panel) <> 0 &THEN            */
/*       RUN dispatch IN THIS-PROCEDURE ('exit').  */
/*   &ELSE                                         */
/*       APPLY "CLOSE":U TO THIS-PROCEDURE.        */
/*   &ENDIF                                        */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F-Main /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActuaFactPeriodo wWin 
PROCEDURE ActuaFactPeriodo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addLink wWin 
PROCEDURE addLink :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER phSource AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER pcLink   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phTarget AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT phSource, INPUT pcLink, INPUT phTarget).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNamedyntoolbarDisabledActionsHiddenActionsUndoChange,CopyHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.46 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.00 , 47.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FolderLabels':U + 'Activo Fijo|Responsables|Procesos' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 4.31 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 17.15 , 120.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             BtnDone:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vper_facturacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vper_facturacion ).
       RUN repositionObject IN h_vper_facturacion ( 8.00 , 28.57 ) NO-ERROR.
       /* Size in AB:  ( 3.23 , 71.00 ) */

       /* Links to SmartDataViewer h_vper_facturacion. */
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_vper_facturacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vper_facturacion ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArchivoPagos wWin 
PROCEDURE ArchivoPagos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArchivosPlanos wWin 
PROCEDURE ArchivosPlanos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.
  DEFINE VARIABLE viPage AS INTEGER  INITIAL 0 NO-UNDO.

  ASSIGN viPage = DYNAMIC-FUNCTION('getcurrentpage').

/*   ASSIGN FRAME F-Main:VISIBLE    = TRUE.                       */
/* /*   Btn_Preliminar:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  */  */
/* /*   DISABLE Btn_Preliminar.                               */  */
/* /*   Btn_Facturacion:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */  */
/* /*   DISABLE Btn_Facturacion.                              */  */
/*   IF viPage NE 2 THEN                                          */
/*      DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,  */
/*         INPUT "add,delete,update,cancel" /* CHARACTER */).     */
/*                                                                */
/*   CASE viPage:                                                 */
/*       WHEN 1 THEN DO:                                                                      */
/*           ASSIGN FRAME FR-Proceso:VISIBLE = FALSE.                                         */
/*           RUN viewObject IN h_dynbrowser-Periodos.                                         */
/* /*           RUN repositionObject IN h_dynbrowser-Periodos ( 6.38 , 7.00 ) NO-ERROR. */    */
/* /*           RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 51.00 ) NO-ERROR.    */    */
/*                                                                                            */
/*                                                                                            */
/*           RUN repositionObject IN h_dynbrowser-Periodos ( 6.38 , 4.57 ) NO-ERROR.          */
/*           RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 53.00 ) NO-ERROR.             */
/*                                                                                            */
/*                                                                                            */
/*       END.                                                                                 */
/*       WHEN 2 THEN DO:                                                                      */
/*           ASSIGN FRAME FR-Proceso:VISIBLE = FALSE.                                         */
/*           DYNAMIC-FUNCTION('modifyDisabledActions':U IN h_dyntoolbar,                      */
/*              INPUT "REMOVE" /* CHARACTER */,                                               */
/*              INPUT "add,delete,update,cancel" /* CHARACTER */).                            */
/*           RUN resetTableio IN h_dyntoolbar.                                                */
/*           RUN viewObject IN h_dynbrowser-Periodos.                                         */
/* /*           RUN repositionObject IN h_dynbrowser-Periodos ( 12.31 , 20.00 ) NO-ERROR. */  */
/* /*           RUN resizeObject IN h_dynbrowser-Periodos (  4.85 , 51.00 ) NO-ERROR.     */  */
/*                                                                                            */
/*                                                                                            */
/*           RUN repositionObject IN h_dynbrowser-Periodos ( 11.50 , 29.00 ) NO-ERROR.        */
/*           RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 53.00 ) NO-ERROR.             */
/*                                                                                            */
/*       END.                                                                                 */
/*       WHEN 3 THEN DO:                                                                      */
/*           RUN hideObject IN h_dynbrowser-Periodos.                                         */
/*           ASSIGN FRAME FR-Proceso:VISIBLE = TRUE.                                          */
/*       END.                                                                                 */
/* /*       WHEN 4 THEN DO:                                */                                 */
/* /*           RUN hideObject IN h_dynbrowser-Periodos.   */                                 */
/* /*            ENABLE Btn_Facturacion WITH FRAME f-main. */                                 */
/* /*       END.                                           */                                 */
/*   END CASE.                                                                                */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DiasValAtraso wWin 
PROCEDURE DiasValAtraso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableObject wWin 
PROCEDURE disableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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
  ENABLE RECT-7 BUTTON-143 BtnDone 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_NuevaFactura wWin 
PROCEDURE Grabar_NuevaFactura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
RUN SUPER.
/* ASSIGN  W_CmbOfi:SCREEN-VALUE IN FRAME FR-Proceso = ("").                                       */
/* W_CmbOfi:ADD-LAST("000 CONSOLIDADO").                                                           */
/* FOR EACH Agencias WHERE Agencias.Estado  EQ 1                                                   */
/*                     AND Agencias.Agencia GT 0 NO-LOCK BY agencia:                               */
/*     W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(80)")).  */
/*     IF Agencias.Agencia EQ W_Agencia THEN                                                       */
/*        ASSIGN W_CmbOfi:SCREEN-VALUE = ("000 CONSOLIDADO").                                      */
/* END.                                                                                            */
DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar,
   INPUT 17).
DYNAMIC-FUNCTION('setBGColor':U IN h_folder,
   INPUT 19).
DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
   INPUT "add,delete,update,cancel" /* CHARACTER */).    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaFacturacion wWin 
PROCEDURE ValidaFacturacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaPagos wWin 
PROCEDURE ValidaPagos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

