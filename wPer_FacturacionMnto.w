&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
{incluido\Variable.i "SHARED"}
/* DEBUGGER:INITIATE().  */
/* DEBUGGER:SET-BREAK(). */

/** Temporales*/
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
DEFINE VAR vcnomage       AS CHARACTER FORMAT "X(40)".
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
    FIELD TCodoper   LIKE mov_creditos.cod_credito
    FIELD TFecha     LIKE mov_creditos.Fecha        INITIAL ?
    FIELD Tcpte      LIKE mov_creditos.Cpte         INITIAL 0
    FIELD TRecAboK   LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TRecAboIC  LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TRecAboIM  LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TRecHonora LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TRecSegCar LIKE mov_creditos.Val_Efectivo INITIAL 0
    FIELD TActualiza AS LOGICAL INITIAL FALSE
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

/* Nuevo 8-Febrero-2008**/
DEFINE TEMP-TABLE TemCodigo
    FIELD cod_credito LIKE mov_creditos.cod_credito.

DEFINE TEMP-TABLE TemSdoAntCre
    FIELD num_credito LIKE mov_creditos.num_credito
    FIELD fecha       LIKE mov_creditos.fecha
    FIELD sdo_capital LIKE mov_creditos.sdo_capital.

DEFINE TEMP-TABLE TemMovCre
    FIELD agencia       LIKE mov_creditos.agencia
    FIELD nit           LIKE mov_creditos.nit
    FIELD num_credito   LIKE mov_creditos.num_credito
    FIELD cod_credito   LIKE mov_creditos.cod_credito
    FIELD cod_operacion LIKE mov_creditos.cod_operacion
    FIELD cpte          LIKE mov_creditos.cpte
    FIELD num_documento LIKE mov_creditos.num_documento
    FIELD fecha         LIKE mov_creditos.fecha
    FIELD descrip       LIKE mov_creditos.descrip
    FIELD detalle       AS CHARACTER FORMAT "X(2)"
    FIELD nro_auditoria LIKE mov_contable.nro_auditoria
    FIELD comentario    LIKE mov_contable.comentario
    FIELD val_efectivo  LIKE mov_creditos.val_efectivo
    FIELD val_cheque    LIKE mov_creditos.val_cheque
    INDEX idfedes fecha descrip.
/***********************************/

/* Definicion de Variables de Trabajo */
DEFINE VAR wVlrOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
/* 04-Mar-2008*/
DEFINE VAR wVlrOtrosCMan AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR vdfecmora     AS DATE INITIAL ? NO-UNDO.
/*************/
DEFINE VAR wzswDet    AS LOGICAL INITIAL FALSE.
DEFINE VAR wcupdis    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wsobcup    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wnomciu    AS CHARACTER FORMAT "X(60)"   INITIAL "".
DEFINE VARIABLE vicorres AS INTEGER NO-UNDO.
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
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dclientes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dfacturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dPer_Facturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Facturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Periodos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientes_Ubic AS HANDLE NO-UNDO.
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

DEFINE BUTTON Btn_ActPagos 
     LABEL "Actualiza Pagos" 
     SIZE 18 BY 1.27.

DEFINE BUTTON Btn_FactOficial 
     LABEL "&Generación Oficial" 
     SIZE 18 BY 1.27.

DEFINE BUTTON Btn_Preliminar 
     LABEL "&Preliminar" 
     SIZE 18 BY 1.27.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(80)":U 
     LABEL "Oficina" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 50 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1.08
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 4.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-143 AT ROW 1.23 COL 62 WIDGET-ID 68
     BtnDone AT ROW 1.23 COL 69 WIDGET-ID 70
     "            CRÉDITO ROTATIVO - FACTURACIÓN" VIEW-AS TEXT
          SIZE 62 BY 1.08 AT ROW 3.04 COL 23 WIDGET-ID 6
          BGCOLOR 18 FGCOLOR 15 FONT 0
     RECT-7 AT ROW 1 COL 1 WIDGET-ID 74
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 21
         BGCOLOR 17  WIDGET-ID 100.

DEFINE FRAME FR-Proceso
     W_CmbOfi AT ROW 2.08 COL 7 COLON-ALIGNED WIDGET-ID 4
     Btn_ActPagos AT ROW 2.27 COL 61 WIDGET-ID 42
     Btn_Preliminar AT ROW 3.58 COL 61 WIDGET-ID 6
     Btn_FactOficial AT ROW 4.88 COL 61 WIDGET-ID 8
     Msaje AT ROW 5.31 COL 3 NO-LABEL WIDGET-ID 38
     RECT-3 AT ROW 2.08 COL 60 WIDGET-ID 14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23.57 ROW 9.54
         SIZE 81 BY 6.73
         BGCOLOR 17 
         TITLE BGCOLOR 17 "Proceso de Facturacion Cupo Rotativo" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Facturación Cupo Rotativo"
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
/* REPARENT FRAME */
ASSIGN FRAME FR-Proceso:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FR-Proceso
                                                                        */
/* SETTINGS FOR FILL-IN Msaje IN FRAME FR-Proceso
   NO-ENABLE ALIGN-L 2                                                  */
ASSIGN 
       Msaje:HIDDEN IN FRAME FR-Proceso           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Facturación Cupo Rotativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Facturación Cupo Rotativo */
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


&Scoped-define FRAME-NAME FR-Proceso
&Scoped-define SELF-NAME Btn_ActPagos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ActPagos wWin
ON CHOOSE OF Btn_ActPagos IN FRAME FR-Proceso /* Actualiza Pagos */
DO:
  ASSIGN W_CmbOfi = "000 CONSOLIDADO".
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
  DISABLE W_CmbOfi   WITH FRAME {&frame-name}.   
  EMPTY TEMP-TABLE Temcreditos.
  /* No se Usa*/
  EMPTY TEMP-TABLE tmpcre.

  RUN DiasValAtraso NO-ERROR.
  RUN SdoNegativos  NO-ERROR.
/*   RUN ActivaCupoCancelado NO-ERROR.  */
/*   RUN ValidaPagos NO-ERROR.  */
  RUN ValidaPagosTotal  NO-ERROR.
/*   RUN ArchivoPagos      NO-ERROR. */
/*   MESSAGE "Desea Continuar con la Actualización....?" SKIP                                        */
/*         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Cupo Rotativo" UPDATE choice AS LOGICAL. */
/*   IF NOT choice THEN                                                                              */
/*       RETURN.                                                                                     */

  RUN ArchivoPagosTotal NO-ERROR.
  RUN ImpriPagosFacturacion NO-ERROR.

  EMPTY TEMP-TABLE Temcreditos.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "La Actualización de Pagos en Facturación Presento Errores...Proceso cancelado."
            VIEW-AS ALERT-BOX ERROR.
     ASSIGN Msaje:SCREEN-VALUE IN FRAME {&frame-name} = "Actualización de Pagos en Facturación Cancelada...Revise Por Favor".
    RETURN.
  END.
  EMPTY TEMP-TABLE Temcreditos.
  /* No se Usa*/
  EMPTY TEMP-TABLE tmpcre.
  ENABLE W_CmbOfi WITH FRAME {&frame-name}.
  ENABLE Btn_Preliminar WITH FRAME FR-Proceso.
  ENABLE Btn_FactOficial WITH FRAME FR-Proceso.
  ASSIGN vlgenofi = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_FactOficial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FactOficial wWin
ON CHOOSE OF Btn_FactOficial IN FRAME FR-Proceso /* Generación Oficial */
DO:
   IF vlgenofi THEN DO:
       /* Nuevo*/
       ASSIGN W_CmbOfi = "000 CONSOLIDADO".
       ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
       ASSIGN AgeIni = 0 AgeFin = 999.
       DISABLE W_CmbOfi        WITH FRAME {&frame-name}.
       DISABLE Btn_Preliminar  WITH FRAME FR-Proceso.
       DISABLE Btn_FactOficial WITH FRAME FR-Proceso.
       DISABLE Btn_ActPagos    WITH FRAME FR-Proceso.
    
       ASSIGN w_oficial    = TRUE.
       ASSIGN w_preliminar = FALSE.
       EMPTY TEMP-TABLE TemCodigo.
       EMPTY TEMP-TABLE TemSdoAntCre.
       EMPTY TEMP-TABLE TemMovCre.
       EMPTY TEMP-TABLE Tmp_Tot_Extracto.
       EMPTY TEMP-TABLE Tmp_Enc_Extracto.
       EMPTY TEMP-TABLE Tmp_Det_Extracto.

       RUN DiasValAtraso NO-ERROR.
       RUN SdoNegativos  NO-ERROR.
       RUN CreaCodigos   NO-ERROR.
       RUN ValidaFacturacion NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "La Generación de la Facturación Presento Errores...Proceso cancelado."
                 VIEW-AS ALERT-BOX ERROR.
          ASSIGN Msaje:SCREEN-VALUE IN FRAME {&frame-name} = "Generación de la Facturación Cancelada...Revise Por Favor".
         RETURN.
       END.
       /* Antes acá RUN ArchivosPlanos.*/
/*        MESSAGE "Desea GRABAR LA NUEVA Facturacion con las siguientes Fechas"  SKIP(1)  */
/*                "Periodo de Facturacion: " wperNUEVO SKIP                               */
/*                "Fecha Inicial Corte     : " STRING(wfec_inic,"99/99/9999")    SKIP     */
/*                "Fecha Final de Corte   : " STRING(wfec_vcto,"99/99/9999")     SKIP     */
/*                "Fecha Limite Plazo      : " STRING(wfec_proxpago,"99/99/9999")         */
/*                 VIEW-AS ALERT-BOX                                                      */
/*        QUESTION BUTTONS YES-NO UPDATE Ask2 AS LOGICAL.                                 */
/*        IF NOT Ask2 THEN                                                                */
/*           RETURN.                                                                      */
       RUN Grabar_NuevaFactura NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "La Generación de la Facturación Presento Errores...Proceso cancelado."
                 VIEW-AS ALERT-BOX ERROR.
          ASSIGN Msaje:SCREEN-VALUE IN FRAME {&frame-name} = "Generación de la Facturación Cancelada...Revise Por Favor".
         RETURN.
       END.
       RUN ArchivosPlanos   NO-ERROR.
       RUN ActuaFactPeriodo NO-ERROR.
       MESSAGE "Periodos Actualizados...."
            VIEW-AS ALERT-BOX.
       ASSIGN Msaje:SCREEN-VALUE IN FRAME {&frame-name} = " ".
       ASSIGN msaje.
       DYNAMIC-FUNCTION('refreshquery':U IN h_dper_facturacion).

/*        ENABLE W_CmbOfi WITH FRAME {&frame-name}. */
   END.
   ELSE DO:
     MESSAGE "Antes Debe Ejecutar la Opción Actualiza_Pagos." SKIP
             "Luego Repita de Nuevo Su Ejecución."
            VIEW-AS ALERT-BOX ERROR.
     ENABLE Btn_ActPagos WITH FRAME FR-Proceso.
     RETURN. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Preliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Preliminar wWin
ON CHOOSE OF Btn_Preliminar IN FRAME FR-Proceso /* Preliminar */
DO:
  IF vlgenofi THEN DO:
      ENABLE W_CmbOfi WITH FRAME {&frame-name}.
      ASSIGN w_preliminar = TRUE.
      ASSIGN w_oficial    = FALSE.
    
      DISABLE Btn_ActPagos    WITH FRAME FR-Proceso.
      DISABLE Btn_Preliminar  WITH FRAME FR-Proceso.
      DISABLE Btn_FactOficial WITH FRAME FR-Proceso.
    
      EMPTY TEMP-TABLE TemCodigo.
      EMPTY TEMP-TABLE TemSdoAntCre.
      EMPTY TEMP-TABLE TemMovCre.
      EMPTY TEMP-TABLE Tmp_Tot_Extracto.
      EMPTY TEMP-TABLE Tmp_Enc_Extracto.
      EMPTY TEMP-TABLE Tmp_Det_Extracto.
    
      RUN DiasValAtraso NO-ERROR.
      RUN SdoNegativos  NO-ERROR.
      RUN CreaCodigos   NO-ERROR.
      RUN ValidaFacturacion NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "La Generación de la Facturación Presento Errores...Proceso cancelado."
                VIEW-AS ALERT-BOX ERROR.
         ASSIGN Msaje:SCREEN-VALUE IN FRAME {&frame-name} = "Generación de la Facturación Cancelada...Revise Por Favor".
         RETURN.
      END.
      RUN ArchivosPlanos NO-ERROR.
      EMPTY TEMP-TABLE Tmp_Tot_Extracto.
      EMPTY TEMP-TABLE Tmp_Enc_Extracto.
      EMPTY TEMP-TABLE Tmp_Det_Extracto.
    
      ENABLE Btn_Preliminar WITH FRAME FR-Proceso.
      ENABLE Btn_FactOficial WITH FRAME FR-Proceso.
  END.
  ELSE DO:
    MESSAGE "Antes Debe Ejecutar la Opción Actualiza_Pagos." SKIP
            "Luego Repita de Nuevo Su Ejecución."
           VIEW-AS ALERT-BOX ERROR.
    ENABLE Btn_ActPagos WITH FRAME FR-Proceso.
    RETURN. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F-Main /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FR-Proceso
&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi wWin
ON VALUE-CHANGED OF W_CmbOfi IN FRAME FR-Proceso /* Oficina */
DO:
  ASSIGN FRAME {&frame-name} W_CmbOfi.
  IF SUBSTRING(W_CmbOfi,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(W_CmbOfi,1,3)) 
            AgeFin = INTEGER(SUBSTRING(W_CmbOfi,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivaCupoCancelado wWin 
PROCEDURE ActivaCupoCancelado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR totact1      AS INTEGER INITIAL 0 NO-UNDO.
ASSIGN totact1 = 0.
ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " Preparándose Para La Activación Cupos Rotativos.... ".
ASSIGN Msaje.

ActCupo:
REPEAT TRANSACTION ON ERROR UNDO ActCupo, LEAVE ActCupo:
    FOR EACH facturacion WHERE
        estado EQ 1 NO-LOCK:
        FIND FIRST clientes WHERE
            clientes.nit    EQ facturacion.nit AND
            clientes.estado EQ 1
            NO-LOCK NO-ERROR.
        IF AVAILABLE (clientes)  THEN DO:
            FIND FIRST creditos WHERE
                creditos.nit         EQ facturacion.nit         AND
                creditos.num_credito EQ facturacion.num_credito AND
                creditos.estado      EQ 3
                NO-ERROR.
            IF AVAILABLE creditos THEN DO:
               ASSIGN totact1 = totact1 + 1.
               UPDATE creditos.fec_cancetotal = ?
                      creditos.estado = 2.
            END.
        END.
    END.
    LEAVE.
END. /*TRANS*/
MESSAGE "Creditos Activados : " totact1
   VIEW-AS ALERT-BOX INFO BUTTONS OK.

ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".
ASSIGN msaje.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActuaFactPeriodo wWin 
PROCEDURE ActuaFactPeriodo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH facturacion   WHERE /*facturacion.nit         = creditos.nit         AND 
                              facturacion.num_credito = creditos.num_credito AND*/
                              facturacion.per_factura = wperiodo:
     UPDATE facturacion.estado = 2.
END.
 
FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 AND 
           per_facturacion.per_factura EQ wperiodo NO-ERROR.
IF AVAILABLE(per_facturacion) THEN   /* Facturacion Vigente */
   ASSIGN per_facturacion.estado = 2.  /* Inactivo */
 
FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperNuevo NO-ERROR.
IF AVAILABLE(per_facturacion) THEN  /* Nueva Facturacion */ 
   ASSIGN per_facturacion.estado = 1.  /* Activo   */

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
             INPUT  'dPer_Facturacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_dPer_Facturacion ).
       /* Position in AB:  ( 1.27 , 86.00 ) */
       /* Size in AB:  ( 1.62 , 8.00 ) */

       RUN constructObject (
             INPUT  'dfacturacion.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsFacturacion.Per_Factura,Per_FacturaObjectNamedFacturacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfacturacion ).
       RUN repositionObject IN h_dfacturacion ( 1.27 , 95.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 6.00 ) */

       RUN constructObject (
             INPUT  'dclientes.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsClientes.Nit,NitObjectNamed-clientes_facturacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dclientes ).
       RUN repositionObject IN h_dclientes ( 1.27 , 109.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 6.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'DisplayedFieldsPer_Factura,Fec_Inicial,Fec_Final,Fec_LimPago,EstadoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndnoUseSortIndicatornoSearchFieldPer_FacturaDataSourceNamesdPeriodosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Periodos ).
       RUN repositionObject IN h_dynbrowser-Periodos ( 11.50 , 29.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 53.00 ) NO-ERROR.

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
             INPUT  'FolderLabels':U + 'Consulta Facturación|Crear/Mod. Períodos|Procesos' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 4.31 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 17.15 , 120.00 ) NO-ERROR.

       /* Links to  h_dPer_Facturacion. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dPer_Facturacion ).

       /* Links to SmartDataObject h_dfacturacion. */
       RUN addLink ( h_dPer_Facturacion , 'Data':U , h_dfacturacion ).

       /* Links to SmartDataObject h_dclientes. */
       RUN addLink ( h_dfacturacion , 'Data':U , h_dclientes ).

       /* Links to SmartDataBrowser h_dynbrowser-Periodos. */
       RUN addLink ( h_dPer_Facturacion , 'Data':U , h_dynbrowser-Periodos ).
       RUN addLink ( h_dynbrowser-Periodos , 'Update':U , h_dPer_Facturacion ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             BtnDone:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-Periodos ,
             FRAME FR-Proceso:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vclientes_Ubicacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientes_Ubic ).
       RUN repositionObject IN h_vclientes_Ubic ( 11.50 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 9.42 , 118.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'DisplayedFieldsNit,Per_Factura,Agencia,Num_factura,Estado,Num_Credito,Tasa_Ic,Tasa_Ea,fec_corte,Fec_LimPago,Sdo_Anterior,Sdo_Mora,Val_atrasokpen,Fec_Mora,Cuota,CupoDisponible,Sobrecupo,CupoTotal,Pago_Minimo,Pago_Total,Nuevo_Saldo,Int_Corrientes,Int_MorCobrar,Seg_cartera,Cargos,OtrosCargos,Pagos,PagoTotal,Val_Recaudo,Rec_Capital,Rec_Honorarios,Rec_IntCorrientes,Rec_IntMora,Rec_SegcarteraEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldNitDataSourceNamesdFacturacionUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Facturacion ).
       RUN repositionObject IN h_dynbrowser-Facturacion ( 6.42 , 57.86 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Facturacion ( 4.81 , 65.29 ) NO-ERROR.

       /* Links to SmartDataViewer h_vclientes_Ubic. */
       RUN addLink ( h_dclientes , 'Data':U , h_vclientes_Ubic ).

       /* Links to SmartDataBrowser h_dynbrowser-Facturacion. */
       RUN addLink ( h_dfacturacion , 'Data':U , h_dynbrowser-Facturacion ).
       RUN addLink ( h_dynbrowser-Facturacion , 'Update':U , h_dfacturacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Facturacion ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientes_Ubic ,
             FRAME FR-Proceso:HANDLE , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vper_facturacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vper_facturacion ).
       RUN repositionObject IN h_vper_facturacion ( 8.00 , 28.57 ) NO-ERROR.
       /* Size in AB:  ( 3.23 , 71.00 ) */

       /* Links to SmartDataViewer h_vper_facturacion. */
       RUN addLink ( h_dPer_Facturacion , 'Data':U , h_vper_facturacion ).
       RUN addLink ( h_vper_facturacion , 'Update':U , h_dPer_Facturacion ).
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
/* /*------------------------------------------------------------------------------  */
/*   Purpose:                                                                        */
/*   Parameters:  <none>                                                             */
/*   Notes:                                                                          */
/* ------------------------------------------------------------------------------*/  */
/* DISABLE TRIGGERS FOR LOAD OF Creditos.                                            */
/* DEFINE VAR totact  AS INTEGER INITIAL 0 NO-UNDO.                                  */
/* DEFINE VAR totarc  AS INTEGER INITIAL 0 NO-UNDO.                                  */

/* ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " Preparándose Para La Actualización.... ".  */
/* ASSIGN Msaje.                                                                                */
/*                                                                                              */
/* FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.                */
/* IF AVAILABLE(per_facturacion) THEN DO:                                                       */
/*    wperiodo = per_facturacion.per_factura +  1.                                              */
/*    ASSIGN wfec_Aproxpago  = per_facturacion.fec_limpago                                      */
/*           wfec_Ainic      = per_facturacion.fec_inicial                                      */
/*           wfec_Avcto      = per_facturacion.fec_final.                                       */

/*    FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.  */
/*    IF AVAILABLE(per_facturacion) THEN                                                         */
/*       ASSIGN wfec_Pproxpago  = per_facturacion.fec_limpago                                    */
/*              wfec_Pinic      = per_facturacion.fec_inicial                                    */
/*              wfec_Pvcto      = per_facturacion.fec_final.                                     */
/* END.                                                                                          */
/* ELSE DO:                                                                                      */
/*    MESSAGE "El administrador  debe  configurar los  periodos  de"  SKIP                       */
/*            "facturacion de Cupo Rotativo, Avise al administrador"                             */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                     */
/*    RETURN ERROR.                                                                              */
/* END.                                                                                          */

/* ActFactura:                                                              */
/* REPEAT TRANSACTION ON ERROR UNDO ActFactura, LEAVE ActFactura:           */
/*     FOR EACH Temcreditos:                                                */
/*         FIND FIRST creditos WHERE                                        */
/*              creditos.nit          = temcreditos.tnit     AND            */
/*              creditos.num_credito  = temcreditos.tnumcred AND            */
/*              (creditos.cod_credito = 570 OR creditos.cod_credito = 870)  */
/*              NO-ERROR.                                                   */
/*         FIND FIRST planpagos WHERE                                       */
/*              planpagos.agencia     = creditos.agencia     AND            */
/*              planpagos.tip_credito = creditos.tip_credito AND            */
/*              planpagos.cod_credito = creditos.cod_credito AND            */
/*              planpagos.num_credito = creditos.num_credito AND            */
/*              planpagos.nit         = creditos.nit         AND            */
/*              planpagos.id_pdomes   = 1                                   */
/*              NO-ERROR.                                                   */
    
/*         ASSIGN totarc = totarc + 1.                                                                                                                    */
/*         ASSIGN wtotabok  = 0 wtotret = 0 wtothon = 0  wtotAboi = 0  wtotAboim = 0   wtotpol   = 0.                                                     */
/*         /* Ojo*/                                                                                                                                       */
/*         FIND FIRST facturacion WHERE facturacion.nit               EQ creditos.nit         AND                                                         */
/*                                      facturacion.num_credito       EQ creditos.num_credito AND                                                         */
/*                                      facturacion.estado            EQ 1 /*AND                                                                          */
/*                                      facturacion.Val_Recaudo       EQ 0 AND                                                                            */
/*                                      facturacion.rec_capital       EQ 0 AND                                                                            */
/*                                      facturacion.Rec_IntCorrientes EQ 0 AND                                                                            */
/*                                      facturacion.Rec_IntMora       EQ 0 AND                                                                            */
/*                                      facturacion.Rec_Honorarios    EQ 0*/ NO-ERROR.                                                                    */
/*         IF AVAILABLE(facturacion) THEN DO:                                                                                                             */
/*             ASSIGN totAct = totAct + 1.                                                                                                                */
/*             ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Actua. Cédula : " + facturacion.Nit + " - Credito : " + STRING(facturacion.num_credito).  */
/*             ASSIGN Msaje.                                                                                                                              */
/*             FOR EACH mov_creditos WHERE                                                                                                                */
/*                      mov_creditos.nit          EQ creditos.nit         AND                                                                             */
/*                      mov_creditos.num_credito  EQ creditos.num_credito AND                                                                             */
/*                      (mov_creditos.cod_credito EQ 570 OR mov_creditos.cod_credito EQ 870) AND                                                          */
/*                      mov_creditos.fecha        GT wfec_Avcto                                                                                           */
/*                      NO-LOCK:                                                                                                                          */
/*                  ASSIGN wvlrHon   = 0  wvlrRet = 0  wvlrAbok = 0  wvlrAboi  = 0  wvlrAboim = 0                                                         */
/*                         wvlrAboSC = 0.                                                                                                                 */
/*                                                                                                                                                        */
/*                  CASE mov_creditos.cod_operacion :                                                                                                     */
/*                      WHEN 20101007 THEN    /* Abono a Honorarios */                                                                                    */
/*                           wvlrHon = (mov_creditos.val_efectivo + mov_creditos.val_cheque).                                                             */
/*                      WHEN 20102001 THEN    /* Retiro */                                                                                                */
/*                           wvlrRet = (mov_creditos.val_efectivo + mov_creditos.val_cheque).                                                             */
/*                      WHEN 20101001 THEN   /* Abono A Capital */                                                                                        */
/*                           wvlrAbok = mov_creditos.val_efectivo + mov_creditos.val_cheque.                                                              */
/*                      WHEN 20101003 OR WHEN 20102006 THEN  /* Abono a Int. Corrientes y Cargo de Int. Corrientes  */                                    */
/*                           wvlrAboi  = mov_creditos.val_efectivo + mov_creditos.val_cheque.                                                             */
/*                      WHEN 20101002 OR WHEN 20101004 THEN  /* Abono a Int. Mora   para la fact 2 tenerlo  en cuenta                                     */
/*                                                              al comparar con el pago minimo */                                                         */
/*                           wvlrAboiM = mov_creditos.val_efectivo + mov_creditos.val_cheque.                                                             */
/*                      WHEN 20101006 THEN /* Abono de polizas/ Seguro de cartera */                                                                      */
/*                           wvlrAboSC = mov_creditos.val_efectivo + mov_creditos.val_cheque.                                                             */
    
/*                  END CASE.                                                                                             */
/*                  ASSIGN wtotabok  = wtotabok  + wvlrabok   wtotret  = wtotret + wvlrret                                */
/*                         wtotaboi  = wtotaboi  + wvlrAboi   wtothon  = wtothon + wvlrhon                                */
/*                         wtotaboim = wtotaboim + wvlrAboim  wtotpol  = wtotpol + wvlrAboSC.                             */
/*             END.                                                                                                       */
/*             UPDATE Facturacion.Rec_capital        = wtotabok                                                           */
/*                    Facturacion.Rec_IntCorrientes  = wtotaboi                                                           */
/*                    Facturacion.Rec_Intmora        = wtotaboim                                                          */
/*                    Facturacion.Rec_Honorarios     = wtothon                                                            */
/*                    Facturacion.Rec_Segcartera     = wtotpol                                                            */
/*                    Wvlrrecaudo                    = wtotabok + wtotAboi + wtothon + wtotaboim + wtotpol                */
/*                    Facturacion.Val_Recaudo        = wtotabok + wtotAboi + wtothon + wtotaboim + wtotpol.               */
/*                                                                                                                        */
/*             /* 1er. caso:  Trae valores pendientes por pagas - sdo_mora */                                             */
/*             IF W_Fecha LT wfec_Aproxpago THEN DO:                                                                      */
/*               IF Facturacion.fec_mora NE ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN DO:  */
/*                  Creditos.Val_atraso = val_atrasokpen.                                                                 */
/*                  IF Creditos.Val_atraso GT 0 THEN                                                                      */
/*                     ASSIGN creditos.fec_pago    = Facturacion.fec_mora                                                 */
/*                            creditos.dias_atraso = (W_Fecha + 1) - Facturacion.Fec_mora.                                */
/*                  ELSE                                                                                                  */
/*                     ASSIGN Creditos.Val_atraso  = 0                                                                    */
/*                            Creditos.Dias_atraso = 0                                                                    */
/*                            creditos.fec_pago    = wfec_Aproxpago.                                                      */
/*               END.                                                                                                     */
/*               ELSE                                                                                                     */
/*                  ASSIGN Creditos.Val_atraso  = 0                                                                       */
/*                         Creditos.Dias_atraso = 0                                                                       */
/*                         creditos.fec_pago    = wfec_Aproxpago.                                                         */
/*             END.                                                                                                       */
/*             ELSE DO:                                                                                                 */
/*             /* 2do. caso: Es mayor o igual a la fecha limite de pago    */                                           */
/*               ASSIGN creditos.val_atraso = Facturacion.Cuota - Facturacion.INT_corrientes +                          */
/*                                            Facturacion.sobrecupo + Facturacion.sdo_mora -                            */
/*                                            wtotabok.                                                                 */
/*               IF Creditos.val_atraso GT 0 THEN DO:                                                                   */
/*                 IF Facturacion.fec_mora NE ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN  */
/*                   ASSIGN creditos.fec_pago      = Facturacion.fec_mora                                               */
/*                          creditos.dias_atraso   = (W_Fecha + 1) - Facturacion.Fec_mora                               */
/*                          Facturacion.val_atraso = creditos.val_atraso.                                               */
/*                 ELSE                                                                                                 */
/*                   ASSIGN creditos.fec_pago      = wfec_Aproxpago                                                     */
/*                          creditos.dias_atraso   = (W_Fecha + 1) - wfec_Aproxpago                                     */
/*                          Facturacion.val_atraso = creditos.val_atraso.                                               */
/*               END.                                                                                                   */
/*               ELSE                                                                                                   */
/*                   ASSIGN Creditos.Val_atraso    = 0                                                                  */
/*                          Creditos.Dias_atraso   = 0                                                                  */
/*                          creditos.fec_pago      = wfec_Pproxpago                                                     */
/*                          Facturacion.val_atraso = 0.                                                                 */
/*             END.                                                                                                     */
/*                                                                                                                      */
/*             IF wvlrrecaudo < Facturacion.pago_minimo THEN                                                            */
/*                 ASSIGN Facturacion.PagoTotal   = FALSE.                                                              */
/*             ELSE                                                                                                     */
/*                 ASSIGN Facturacion.Pagototal          = TRUE                                                         */
/*                        Facturacion.Val_atraso         = 0                                                            */
/*                        Facturacion.val_atrasokpen     = 0                                                            */
/*                        planpagos.fec_proxpago         = wfec_Pproxpago                                               */
/*                        planpagos.fec_inic             = wfec_Pinic                                                   */
/*                        planpagos.fec_vcto             = wfec_Pvcto                                                   */
/*                        creditos.fec_pago              = wfec_Pproxpago                                               */
/*                        creditos.val_atraso            = 0                                                            */
/*                        creditos.cuo_atraso            = 0                                                            */
/*                        creditos.dias_atraso           = 0.                                                           */
    
/*             IF wtotAboi > 0  THEN                                        */
/*                ASSIGN PlanPagos.INT_LiqPdo = wtotAboi.                   */
/*                                                                          */
/*             IF creditos.val_atraso LT 0 THEN                             */
/*                ASSIGN creditos.val_atraso  = 0                           */
/*                       creditos.cuo_atraso  = 0                           */
/*                       creditos.dias_atraso = 0.                          */
/*             ELSE                                                         */
/*                ASSIGN Creditos.Cuo_Atraso   = 1.                         */
/*                                                                          */
/*             IF Creditos.Dias_atraso LT 0 THEN creditos.dias_atraso = 0.  */
/*                ASSIGN facturacion.val_atraso = creditos.val_atraso.      */
/*                                                                          */
/*             IF Creditos.Val_Atraso GT Creditos.Sdo_capital THEN          */
/*                ASSIGN Creditos.Val_Atraso = Creditos.Sdo_capital.        */
/*         END.                                                             */
/*     END.                                                                 */
/*     ASSIGN vlgenofi = TRUE.                                 */
/*     MESSAGE "Facturas Actualizadas : " totact "De " totarc  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
/*     LEAVE.                                                  */
/* END. /*TRANS*/  */
/* ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArchivoPagosTotal wWin 
PROCEDURE ArchivoPagosTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR totact       AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VAR totarc       AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VAR vdtotrecaudo AS DECIMAL INITIAL 0 NO-UNDO.

ASSIGN totact = 0
       totarc = 0
       vdtotrecaudo = 0.

ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " Preparándose Para La Actualización.... ".
ASSIGN Msaje.

FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE(per_facturacion) THEN DO:
   wperiodo = per_facturacion.per_factura +  1.  
   ASSIGN wfec_Aproxpago  = per_facturacion.fec_limpago
          wfec_Ainic      = per_facturacion.fec_inicial
          wfec_Avcto      = per_facturacion.fec_final.

   FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.
   IF AVAILABLE(per_facturacion) THEN
      ASSIGN wfec_Pproxpago  = per_facturacion.fec_limpago
             wfec_Pinic      = per_facturacion.fec_inicial
             wfec_Pvcto      = per_facturacion.fec_final.
END.
ELSE DO:
   MESSAGE "El administrador  debe  configurar los  periodos  de"  SKIP 
           "facturacion de Cupo Rotativo, Avise al administrador"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.


ActFactura:
REPEAT TRANSACTION ON ERROR UNDO ActFactura, LEAVE ActFactura:
    FOR EACH Temcreditos
        NO-LOCK BREAK BY Temcreditos.TAgencia BY Temcreditos.TNumcred:

        IF FIRST-OF (Temcreditos.TAgencia) OR FIRST-OF (Temcreditos.TNumcred) THEN
           ASSIGN vdtotrecaudo = 0.

        ASSIGN vdtotrecaudo = Temcreditos.TRecAboK   + Temcreditos.TRecAboIC  +
                              Temcreditos.TRecAboIM  + Temcreditos.TRecHonora +
                              Temcreditos.TRecSegCar.

        FIND FIRST creditos WHERE 
             creditos.nit          = temcreditos.tnit     AND 
             creditos.num_credito  = temcreditos.tnumcred AND 
             (creditos.cod_credito = 570 OR creditos.cod_credito = 870)
             NO-ERROR.
        FIND FIRST planpagos WHERE
             planpagos.agencia     = creditos.agencia     AND 
             planpagos.tip_credito = creditos.tip_credito AND 
             planpagos.cod_credito = creditos.cod_credito AND 
             planpagos.num_credito = creditos.num_credito AND 
             planpagos.nit         = creditos.nit         AND 
             planpagos.id_pdomes   = 1
             NO-ERROR.
    
        ASSIGN totarc = totarc + 1.
        FIND FIRST facturacion WHERE 
             facturacion.nit = Temcreditos.TNit             AND
             facturacion.num_credito = Temcreditos.TNumcred AND 
             facturacion.estado = 1 NO-ERROR.

        IF AVAILABLE(facturacion) AND facturacion.val_recaudo NE vdtotrecaudo AND vdtotrecaudo GT 0 THEN DO:
           ASSIGN totAct = totAct + 1.
           ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Actua. Cédula : " + facturacion.Nit + " - Credito : " + STRING(facturacion.num_credito).
           ASSIGN Msaje.
           UPDATE Facturacion.Rec_capital        = Temcreditos.TRecAboK 
                  Facturacion.Rec_IntCorrientes  = Temcreditos.TRecAboIC
                  Facturacion.Rec_Intmora        = Temcreditos.TRecAboIM
                  Facturacion.Rec_Honorarios     = Temcreditos.TRecHonora
                  Facturacion.Rec_Segcartera     = Temcreditos.TRecSegCar
                  Wvlrrecaudo                    = vdtotrecaudo
                  Facturacion.Val_Recaudo        = vdtotrecaudo.
    
           /* 1er. caso:  Trae valores pendientes por pagas - sdo_mora */
           IF Temcreditos.TFecha LT wfec_Aproxpago THEN DO:    /*W_Fecha LT wfec_Aproxpago*/
               IF Facturacion.fec_mora NE ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN DO:
                  ASSIGN Creditos.Val_atraso = Facturacion.val_atrasokpen.
                  IF Creditos.Val_atraso GT 0 THEN
                     ASSIGN creditos.fec_pago    = Facturacion.fec_mora
                            creditos.dias_atraso = (Temcreditos.TFecha + 1) - Facturacion.Fec_mora.  /*w_fecha + 1 */
                  ELSE                           
                     ASSIGN Creditos.Val_atraso  = 0
                            Creditos.Dias_atraso = 0
                            creditos.fec_pago    = wfec_Aproxpago.
               END.
               ELSE
                  ASSIGN Creditos.Val_atraso  = 0              
                         Creditos.Dias_atraso = 0              
                         creditos.fec_pago    = wfec_Aproxpago.
           END.
           ELSE DO:
             /* 2do. caso: Es mayor o igual a la fecha limite de pago    */
               ASSIGN creditos.val_atraso = Facturacion.Cuota - Facturacion.INT_corrientes +
                                            Facturacion.sobrecupo + Facturacion.sdo_mora -
                                            wtotabok. 
               IF Creditos.val_atraso GT 0 THEN DO:
                  IF Facturacion.fec_mora NE ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN 
                    ASSIGN creditos.fec_pago      = Facturacion.fec_mora
                           creditos.dias_atraso   = (Temcreditos.TFecha + 1) - Facturacion.Fec_mora  /*w_fecha + 1 */
                           Facturacion.val_atraso = creditos.val_atraso.
                  ELSE
                    ASSIGN creditos.fec_pago      = wfec_Aproxpago                   
                           creditos.dias_atraso   = (Temcreditos.TFecha + 1) - wfec_Aproxpago  /*w_fecha + 1 */
                           Facturacion.val_atraso = creditos.val_atraso.  
               END.
               ELSE
                   ASSIGN Creditos.Val_atraso    = 0              
                          Creditos.Dias_atraso   = 0              
                          creditos.fec_pago      = wfec_Pproxpago
                          Facturacion.val_atraso = 0.
           END.

           IF Facturacion.Val_Recaudo LT Facturacion.pago_minimo THEN  /* wvlrrecaudo */
               ASSIGN Facturacion.PagoTotal   = FALSE.
           ELSE 
               ASSIGN Facturacion.Pagototal          = TRUE
                      Facturacion.Val_atraso         = 0
                      Facturacion.val_atrasokpen     = 0
                      planpagos.fec_proxpago         = wfec_Pproxpago
                      planpagos.fec_inic             = wfec_Pinic    
                      planpagos.fec_vcto             = wfec_Pvcto    
                      creditos.fec_pago              = wfec_Pproxpago
                      creditos.val_atraso            = 0 
                      creditos.cuo_atraso            = 0 
                      creditos.dias_atraso           = 0.

           IF wtotAboi > 0  THEN 
              ASSIGN PlanPagos.INT_LiqPdo = wtotAboi.

           IF creditos.val_atraso LT 0 THEN
              ASSIGN creditos.val_atraso  = 0   
                     creditos.cuo_atraso  = 0  
                     creditos.dias_atraso = 0.
           ELSE
              ASSIGN Creditos.Cuo_Atraso   = 1.

           IF Creditos.Dias_atraso LT 0 THEN creditos.dias_atraso = 0.
              ASSIGN facturacion.val_atraso = creditos.val_atraso.                

           IF Creditos.Val_Atraso GT Creditos.Sdo_capital THEN
              ASSIGN Creditos.Val_Atraso = Creditos.Sdo_capital.
           /* Registra que actualizo*/
           UPDATE Temcreditos.TActualiza = YES.
        END.
    END.
    LEAVE.
END. /*TRANS*/
MESSAGE "Facturas Actualizadas : " totact "De " totarc
   VIEW-AS ALERT-BOX INFO BUTTONS OK.

ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".
ASSIGN msaje.
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
  DEFINE VAR LisEncabezado AS CHARACTER FORMAT "X(40)".  
  DEFINE VAR LisDetalle    AS CHARACTER FORMAT "X(40)".
  DEFINE VAR LisEstadis    AS CHARACTER FORMAT "X(40)".

  ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Seleccionando Registros... Espere Por Favor.".
  ASSIGN Msaje.

  IF w_preliminar THEN 
     LisEncabezado = W_PathSpl + "PEncabezado-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".
  IF w_oficial THEN
     LisEncabezado = W_PathSpl + "Encabezado-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".

  OS-DELETE VALUE(LisEncabezado).
  OUTPUT TO value(LisEncabezado). /* NO-ECHO PAGED PAGE-SIZE 65 */

/*   DEFINE VAR zarcEncabezado AS CHARACTER FORMAT "X(40)".                                                                           */
/*   zarcEncabezado = "c:\Encabezado_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT". */
/*   OUTPUT TO VALUE(zarcEncabezado).                                                                                                 */
  
  PUT "nit;fecor;nombre;direccion;telefono;ciudad;numcre;cuptot;cupDis;Sobrecupo;sdo_ant;Cargos;intCtes;IntMora;OtrosCar;Pagos;NvoSdo;SdoMora;PagoTotal;PagoMin;Tasa_IC;Tasa_EA;FecMora;FecPagAnt;FacDesde;FecHasta;NomAgenc;S_Cartera" SKIP(0).
  FOR EACH tmp_Enc_Extracto BREAK BY tmp_Enc_Extracto.agencia BY tmp_Enc_Extracto.nit:

    ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Encab. Cédula : " + tmp_Enc_Extracto.Nit + " - Credito : " + STRING(tmp_Enc_Extracto.numcre).
    ASSIGN Msaje.
   /* Nuevo */
    IF tmp_Enc_Extracto.FecMora NE ? THEN
     ASSIGN Pagoinme = "Inmediato". /*STRING(FecPlazo)*/
    ELSE
     /*ASSIGN Pagoinme = STRING(FecPlazo).*/
     ASSIGN Pagoinme = STRING(YEAR(tmp_Enc_Extracto.FecPlazo),"9999") + "/" +
                       STRING(MONTH(tmp_Enc_Extracto.FecPlazo),"99" ) + "/" +
                       STRING(DAY(tmp_Enc_Extracto.FecPlazo),"99").
   /*********/
  
   IF FIRST-OF(tmp_Enc_Extracto.agencia)  THEN DO:
     /* Limpioar variables */
     ASSIGN wTotAso   = 0  wTotAsoExt = 0  wTcuptot   = 0 wTcupDis    = 0 wTSobrecupo = 0
            wTsdo_ant = 0  wTCargos   = 0  wTintCtes  = 0 wTIntMora   = 0 wTOtrosCar  = 0
            wTPagos   = 0  wTNvoSdo   = 0  wTSdoMora  = 0 wTPagoTotal = 0 wTPagoMin   = 0
            wcantage  = 0  wcantotage = 0  wTPagoSegC = 0.
  
     /*CREATE Tmp_Tot_Extracto.*/
   END.
   ASSIGN wcantage      =  wcantage      +  1
          wTcuptot      =  wTcuptot      +  tmp_Enc_Extracto.cuptot     
          wTcupDis      =  wTcupDis      +  tmp_Enc_Extracto.cupDis     
          wTSobrecupo   =  wTSobrecupo   +  tmp_Enc_Extracto.Sobrecupo  
          wTsdo_ant     =  wTsdo_ant     +  tmp_Enc_Extracto.sdo_ant    
          wTCargos      =  wTCargos      +  tmp_Enc_Extracto.Cargos     
          wTintCtes     =  wTintCtes     +  tmp_Enc_Extracto.intCtes 
          wTintMora     =  WTIntMora     +  tmp_Enc_Extracto.IntMora  
          wTOtrosCar    =  wTOtrosCar    +  tmp_Enc_Extracto.OtrosCar   
          wTPagos       =  wTPagos       +  tmp_Enc_Extracto.Pagos      
          wTNvoSdo      =  wTNvoSdo      +  tmp_Enc_Extracto.NvoSdo     
          wTSdoMora     =  wTSdoMora     +  tmp_Enc_Extracto.SdoMora    
          wTPagoTotal   =  wTPagoTotal   +  tmp_Enc_Extracto.PagoTotal  
          wTPagoMin     =  wTPagoMin     +  tmp_Enc_Extracto.PagoMin.  
          wTPagoSegC    =  wTPagoSegC    +  tmp_Enc_Extracto.Seg_Cartera.  
  
    PUT tmp_Enc_Extracto.nit        ";"      
        tmp_Enc_Extracto.fecor      ";"    
        tmp_Enc_Extracto.nombre     ";"
        tmp_Enc_Extracto.direccion  ";"
        tmp_Enc_Extracto.telefono   ";"
        tmp_Enc_Extracto.ciudad     ";"
        tmp_Enc_Extracto.numcre     ";"
        tmp_Enc_Extracto.cuptot     ";"
        tmp_Enc_Extracto.cupDis     ";"
        tmp_Enc_Extracto.Sobrecupo  ";"
        tmp_Enc_Extracto.sdo_ant    ";"
        tmp_Enc_Extracto.Cargos     ";"
        tmp_Enc_Extracto.intCtes    ";"
        tmp_Enc_Extracto.IntMora    ";"
        tmp_Enc_Extracto.OtrosCar   ";"
        tmp_Enc_Extracto.Pagos      ";"
        tmp_Enc_Extracto.NvoSdo     ";"
        tmp_Enc_Extracto.SdoMora    ";"
        tmp_Enc_Extracto.PagoTotal  ";"
        tmp_Enc_Extracto.PagoMin    ";"
        tmp_Enc_Extracto.Tasa_IC  FORMAT "->9.999"  ";"
        tmp_Enc_Extracto.Tasa_EA  FORMAT "->9.99"   ";"
        tmp_Enc_Extracto.FecMora    ";"
        Pagoinme                    ";"
        tmp_Enc_Extracto.FacDesde   ";"
        tmp_Enc_Extracto.FecHasta   ";"
        Tmp_Enc_Extracto.nom_agen   ";"
        Tmp_Enc_Extracto.seg_cartera SKIP(0).
  
   IF LAST-OF(tmp_Enc_Extracto.agencia)  THEN DO: 
      FIND FIRST tmp_Tot_Extracto WHERE tmp_Tot_Extracto.agencia = tmp_Enc_Extracto.agencia NO-LOCK NO-ERROR.
      IF AVAILABLE(tmp_Tot_Extracto) THEN
         UPDATE 
              /*WHEN tmp_Tot_Extracto.agencia = tmp_Enc_Extracto.agencia*/
         tmp_Tot_Extracto.TotAsoExt  = wcantage                 
         tmp_Tot_Extracto.Tcuptot    = wTcuptot                 
         tmp_Tot_Extracto.TcupDis    = wTcupDis                 
         tmp_Tot_Extracto.TSobrecupo = wTSobrecupo              
         tmp_Tot_Extracto.Tsdo_ant   = wTsdo_ant                
         tmp_Tot_Extracto.TCargos    = wTCargos                 
         tmp_Tot_Extracto.TintCtes   = wTintCtes                
         tmp_Tot_Extracto.TIntMora   = wTintMora                
         tmp_Tot_Extracto.TOtrosCar  = wTOtrosCar               
         tmp_Tot_Extracto.TPagos     = wTPagos                  
         tmp_Tot_Extracto.TNvoSdo    = wTNvoSdo                 
         tmp_Tot_Extracto.TSdoMora   = wTSdoMora                
         tmp_Tot_Extracto.TPagoTotal = wTPagoTotal              
         tmp_Tot_Extracto.TPagoMin   = wTPagoMin
         tmp_Tot_Extracto.TPagoSegC  = wTPagoSegC
         tmp_Tot_Extracto.Tasa_IC    = tmp_Enc_Extracto.Tasa_IC 
         tmp_Tot_Extracto.Tasa_EA    = tmp_Enc_Extracto.Tasa_EA 
         NO-ERROR.
   END. 
  END.
  OUTPUT CLOSE.

  IF w_preliminar THEN 
       LisDetalle = W_PathSpl + "PDetalle-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".
  IF w_oficial THEN
       LisDetalle = W_PathSpl + "Detalle-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".

  OS-DELETE VALUE(LisDetalle).
  OUTPUT TO value(LisDetalle). /* NO-ECHO PAGED PAGE-SIZE 65 */

/*   DEFINE VAR zarcDetalle AS CHARACTER FORMAT "X(40)".                                                                        */
/*   zarcDetalle = "c:\Detalle_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT". */
/*   OUTPUT TO VALUE(zarcDetalle).                                                                                              */
  PUT "nit;Numcredito;fecha;descripcion;comprobante;vlr_Retiro;Abo_Capit" SKIP.
  FOR EACH tmp_Det_Extracto BREAK BY tmp_Det_Extracto.agencia BY tmp_Det_Extracto.nit:
      ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Detal. Cédula : " + tmp_Det_Extracto.Nit + " - Credito : " + STRING(tmp_Det_Extracto.num_credito).
      ASSIGN Msaje.
      PUT tmp_Det_Extracto.Nit          ";"
          tmp_Det_Extracto.num_credito  ";"
          tmp_Det_Extracto.fec_trans    ";"
          tmp_Det_Extracto.descripcion  ";"
          tmp_Det_Extracto.NumDocto     ";"
          tmp_Det_Extracto.CR           ";"
          tmp_Det_Extracto.DB SKIP(0).        
  END.
  OUTPUT CLOSE.

  IF w_preliminar THEN 
       LisEstadis = W_PathSpl + "PEstadisticas-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".
  IF w_oficial THEN
       LisEstadis = W_PathSpl + "Estadisticas-" + STRING(W_Fecha,"99999999") + "-" + STRING(Time) + "-" + STRING(W_Usuario,"999") + ".txt".
  
  OS-DELETE VALUE(LisEstadis).
  OUTPUT TO value(LisEstadis). /* NO-ECHO PAGED PAGE-SIZE 65 */

/*   DEFINE VAR zarcesta AS CHARACTER FORMAT "X(40)".                                                                             */
/*   zarcesta = "c:\Estadisticas_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT". */
/*   OUTPUT TO VALUE(zarcesta).  /*"c:\Est" " .txt". */                                                                           */
  PUT "agencia;Nombre;TotAso;TotAsoExT;Tcuptot;TcupDis;TSobrecupo;Tsdo_ant;TCargos;TintCtes;TIntMora;"
      "TOtrosCar;TPagos;TNvoSdo;TSdoMora;TPagoTotal;TPagoMin;TPagoSegC" SKIP(0).
  
  FOR EACH tmp_Tot_Extracto BY agencia:
      ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Estad. Agencia: " + STRING(tmp_Tot_Extracto.agencia,"999") + " - Registro: " + STRING(tmp_Tot_Extracto.TotAso,"99999").
      ASSIGN Msaje.
      PUT tmp_Tot_Extracto.agencia     ";"
          tmp_Tot_Extracto.Nombre      ";"
          tmp_Tot_Extracto.TotAso      ";"
          tmp_Tot_Extracto.TotAsoExt   ";"
          tmp_Tot_Extracto.Tcuptot     ";"
          tmp_Tot_Extracto.TcupDis     ";"
          tmp_Tot_Extracto.TSobrecupo  ";"
          tmp_Tot_Extracto.Tsdo_ant    ";"
          tmp_Tot_Extracto.TCargos     ";"
          tmp_Tot_Extracto.TintCtes    ";"
          tmp_Tot_Extracto.TIntMora    ";"
          tmp_Tot_Extracto.TOtrosCar   ";"
          tmp_Tot_Extracto.TPagos      ";"
          tmp_Tot_Extracto.TNvoSdo     ";"
          tmp_Tot_Extracto.TSdoMora    ";"
          tmp_Tot_Extracto.TPagoTotal  ";"
          tmp_Tot_Extracto.TPagoMin    ";"
          tmp_Tot_Extracto.TPagoSegC   SKIP(0).    
  END.
  OUTPUT CLOSE.
  
  MESSAGE "Cant.de Extracto de Cupo Rotativo: " wcant SKIP
          "Archivos planos esta en: " LisEncabezado   SKIP
          "                                         " LisDetalle      SKIP  
          "                                         " LisEstadis   
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ASSIGN wcant = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarAnteriorFactura wWin 
PROCEDURE BuscarAnteriorFactura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* CREATE Tmp_Enc_Extracto.                                */
/* ASSIGN Tmp_Enc_Extracto.seg_cartera = wvlrCobSC         */
/*        Tmp_Enc_Extracto.Otroscar = Creditos.honorarios  */
/*        vdfecmora = ?.                                   */
FIND FIRST facturacion WHERE facturacion.nit         = creditos.nit         AND 
                             facturacion.num_credito = creditos.num_credito AND
                             facturacion.estado      = 1 NO-ERROR.
/* 4-Abril-2008*/
/* IF AVAILABLE(facturacion) THEN DO:                                                                                       */
/*    IF (facturacion.val_atraso GT 0 AND facturacion.val_atrasokpen EQ 0) OR                                               */
/*       (facturacion.val_atraso EQ 0 AND facturacion.val_atrasokpen EQ 0) THEN DO:                                         */
/*       ASSIGN Tmp_Enc_Extracto.valatrak = Facturacion.pago_minimo - Facturacion.val_recaudo.                              */
/*              Tmp_Enc_Extracto.Sdomora  = Tmp_Enc_Extracto.valatrak.                                                      */
/*                                                                                                                          */
/*       IF (facturacion.val_atraso GT 0 AND facturacion.val_atrasokpen EQ 0) AND                                           */
/*           facturacion.fec_mora NE ? THEN                                                                                 */
/*           ASSIGN Tmp_Enc_Extracto.fecmora = facturacion.fec_mora.                                                        */
/*                                                                                                                          */
/*       IF Tmp_Enc_Extracto.valatrak LT 0 THEN                                                                             */
/*          ASSIGN Tmp_Enc_Extracto.fecmora  = ?                                                                            */
/*                 Tmp_Enc_Extracto.valatrak = 0                                                                            */
/*                 Tmp_Enc_Extracto.Sdomora  = 0.                                                                           */
/*    END.                                                                                                                  */
/*    ELSE DO:                                                                                                              */
/*      IF (facturacion.val_atraso GT 0 AND facturacion.val_atrasokpen GT 0) THEN DO:                                       */
/*         ASSIGN Tmp_Enc_Extracto.valatrak = Facturacion.pago_minimo - Facturacion.val_recaudo - Facturacion.rec_capital.  */
/*                Tmp_Enc_Extracto.Sdomora  = Tmp_Enc_Extracto.valatrak.                                                    */
/*                                                                                                                          */
/*         IF facturacion.fec_mora NE ? THEN                                                                                */
/*            ASSIGN Tmp_Enc_Extracto.fecmora = facturacion.fec_mora.                                                       */
/*                                                                                                                          */
/*         IF Tmp_Enc_Extracto.valatrak LT 0 THEN                                                                           */
/*            ASSIGN Tmp_Enc_Extracto.fecmora  = ?                                                                          */
/*                   Tmp_Enc_Extracto.valatrak = 0                                                                          */
/*                   Tmp_Enc_Extracto.Sdomora  = 0.                                                                         */
/*                                                                                                                          */
/*      END.                                                                                                                */
/*      ELSE                                                                                                                */
/*         ASSIGN Tmp_Enc_Extracto.fecmora  = ?                                                                             */
/*                Tmp_Enc_Extracto.valatrak = 0                                                                             */
/*                Tmp_Enc_Extracto.Sdomora  = 0.                                                                            */
/*    END.                                                                                                                  */
/* END.                                                                                                                     */
/**********/

IF (AVAILABLE(facturacion) AND NOT facturacion.pagototal) OR
   (AVAILABLE(facturacion) AND facturacion.pagototal AND creditos.honorarios GT 0) THEN DO:
    FIND CURRENT Tmp_Enc_Extracto.
    ASSIGN Tmp_Enc_Extracto.valatrak = Facturacion.cuota - Facturacion.INT_Corrientes -
                                       Facturacion.Rec_capital + Facturacion.val_atrasokpen.

    IF Tmp_Enc_Extracto.valatrak GT 0 THEN DO:
       ASSIGN Tmp_Enc_Extracto.Sdomora = Tmp_Enc_Extracto.valatrak.
       IF facturacion.fec_mora NE ? THEN                                                                               
          ASSIGN Tmp_Enc_Extracto.fecmora = facturacion.fec_mora.     
       ELSE
          ASSIGN Tmp_Enc_Extracto.FecMora = wfec_ANTproxpago.
    END.
    ELSE
       ASSIGN Tmp_Enc_Extracto.fecmora  = ?
              Tmp_Enc_Extracto.valatrak = 0
              Tmp_Enc_Extracto.Sdomora  = 0.
END.

/* IF (AVAILABLE(facturacion) AND NOT facturacion.pagototal) OR                                         */
/*    (AVAILABLE(facturacion) AND facturacion.pagototal AND creditos.honorarios GT 0) THEN DO:          */
/*     ASSIGN /* wfec_proxpago. */                                                                      */
/*            Tmp_Enc_Extracto.valatrak = Facturacion.cuota - Facturacion.INT_Corrientes -              */
/*                                        Facturacion.Rec_capital + Facturacion.val_atrasokpen          */
/*                                        /* Se agrego val_atrasokpen porque son valores anteriores */  */
/*            Tmp_Enc_Extracto.Sdomora  = Tmp_Enc_Extracto.valatrak                                     */
/*                                        /* Facturacion.Pago_minimo - Facturacion.Val_recaudo */       */
/*            /* 4-Abril-2008*/                                                                         */
/*            vdfecmora = Facturacion.fec_mora.                                                         */
/*            /****************/                                                                        */
/*     IF Tmp_Enc_Extracto.valatrak LT 0 THEN                                                           */
/*        ASSIGN Tmp_Enc_Extracto.valatrak = 0                                                          */
/*               Tmp_Enc_Extracto.Sdomora  = 0.                                                         */
/* END.                                                                                                 */

/* Termina Búsqueda de pagos pendientes  */

IF creditos.monto GE creditos.sdo_capital THEN
  ASSIGN wcupdis = creditos.monto - creditos.sdo_capital   wsobcup = 0.
ELSE 
  ASSIGN wcupdis = 0   wsobcup = creditos.sdo_capital - creditos.monto.
/*6-Mayo-2008 - Correspondencia*/
FIND FIRST clientes WHERE 
     clientes.nit EQ creditos.nit
     NO-LOCK NO-ERROR.
IF AVAILABLE Clientes THEN DO:
   FIND FIRST anexos_clientes WHERE 
        anexos_clientes.nit EQ creditos.nit 
        NO-LOCK NO-ERROR.
   IF AVAILABLE anexos_clientes THEN DO: /*Si lo Encuentra*/
      ASSIGN vicorres = anexos_clientes.DIR_correspondencia.
      IF vicorres EQ 3 OR vicorres EQ ? THEN  /* A.A*/
         ASSIGN vicorres = 2.  /* Trabajo*/
      IF vicorres EQ 1 THEN  /* Residencia*/
         ASSIGN wdircorres = Clientes.DIR_residencia 
                wcodciu    = Clientes.lugar_residencia  
                wtel       = Clientes.tel_residencia.
      ELSE
         ASSIGN wdircorres = Clientes.DIR_comercial  
                wcodciu    = Clientes.lugar_comercial
                wtel       = Clientes.tel_comercial.
   END.
   ELSE DO:    /* No Aparece en Anexos_Clientes*/
     CREATE anexos_clientes.
     UPDATE anexos_clientes.nit = clientes.nit.
     IF clientes.DIR_correspondencia THEN DO:     /* Trabajo*/
        UPDATE anexos_clientes.DIR_correspondencia = 2.
        ASSIGN wdircorres = Clientes.DIR_comercial  
               wcodciu    = Clientes.lugar_comercial  
               wtel       = Clientes.tel_comercial.
     END.
     ELSE DO:
        UPDATE anexos_clientes.DIR_correspondencia = 1. /*Residencia*/
        ASSIGN wdircorres = Clientes.DIR_residencia 
               wcodciu    = Clientes.lugar_residencia  
               wtel       = Clientes.tel_residencia.
     END.
   END.
END.
FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ wcodciu NO-ERROR.
IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = trim(Ubicacion.Nombre).

FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,5) NO-ERROR.
IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).

FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,2) NO-ERROR.
IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).
ASSIGN Wnomciu = LC(Wnomciu).
/*************************************/
/* /*correspondencia*/                                                                                                  */
/* FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-ERROR.                                                      */
/* IF AVAILABLE Clientes AND DIR_correspondencia THEN                                                                   */
/*    ASSIGN wdircorres = Clientes.DIR_comercial  wcodciu = clientes.lugar_comercial  wtel = clientes.tel_comercial.    */
/* ELSE                                                                                                                 */
/*   IF AVAILABLE Clientes AND NOT DIR_correspondencia THEN                                                             */
/*    ASSIGN wdircorres = Clientes.DIR_residencia wcodciu = clientes.lugar_residencia  wtel = clientes.tel_residencia.  */
/* /*residencia*/                                                                                                       */
/* FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ wcodciu NO-ERROR.                        */
/* IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = trim(Ubicacion.Nombre).                                                */
/*                                                                                                                      */
/* FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,5) NO-ERROR.     */
/* IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).                                */
/*                                                                                                                      */
/* FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,2) NO-ERROR.     */
/* IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).                                */
/* Wnomciu = LC(Wnomciu).                                                                                               */

/* 9-Abril-2008 Actualiza Int.Moratorios para Nueva Facturación si Tmp_Enc_Extracto.valatrak es igual a 0*/
IF Tmp_Enc_Extracto.valatrak EQ 0 THEN
   ASSIGN wintMor = 0.
ELSE
   ASSIGN wintMor  = Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar.
/***************************/
IF wintMor LT 0 THEN wintMor = 0.

ASSIGN wpagTot  = Creditos.Sdo_capital  + Creditos.INT_Corrientes + wintmor + wTotOtrosC + 
                  Creditos.honorarios   + Creditos.costas + Tmp_Enc_Extracto.seg_cartera - Creditos.INT_anticipado.
ASSIGN wpagomin = ROUND( ( Creditos.sdo_capital -  wsobcup - Tmp_Enc_Extracto.valatrak  )  / 36,0) +  wsobcup +
                  Creditos.INT_Corrientes + wintmor + wTotOtrosC + creditos.honorarios + Creditos.costas + Tmp_Enc_Extracto.seg_cartera + Tmp_Enc_Extracto.valatrak.
ASSIGN wcuota   = ROUND( ( Creditos.sdo_capital -  wsobcup - Tmp_Enc_Extracto.valatrak  )  / 36,0)  + Creditos.INT_Corrientes.

IF wpagomin LE 0 THEN wpagomin = 0.
/* Que hacer con interese anticipados */
IF wpagtot LT 0 THEN  wpagtot = 0.
/* Nuevo 10022008*/
IF wcuota LT 0 THEN  wcuota = 0.
/******************/
ASSIGN vcnomage = "".
FIND FIRST agencias WHERE agencias.agencia = creditos.agencia NO-LOCK NO-ERROR.
IF AVAILABLE(agencias) THEN
   ASSIGN vcnomage = agencias.nombre.
ELSE
   ASSIGN vcnomage = "No Existe...".

FIND CURRENT Tmp_Enc_Extracto.
/* IF wpagtot NE  0 THEN DO: /* Solo Para Pago Total Mayor que Cero*/  /*24-Abril-2008 */ */
   ASSIGN Tmp_Enc_Extracto.agencia    = creditos.agencia
          Tmp_Enc_Extracto.nom_agen   = STRING(creditos.agencia,"999") + " - " + TRIM(vcnomage)
          Tmp_Enc_Extracto.nit        = trim(Creditos.nit)
          Tmp_Enc_Extracto.fecor      = wfecfin
          Tmp_Enc_Extracto.nombre     = trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)
          Tmp_Enc_Extracto.direccion  = TRIM(wdircorres)
          Tmp_Enc_Extracto.telefono   = TRIM(wtel)
          Tmp_Enc_Extracto.ciudad     = TRIM(wnomciu)
          Tmp_Enc_Extracto.numcre     = Creditos.num_credito
          Tmp_Enc_Extracto.cuptot     = Creditos.Monto + Wsobcup
          Tmp_Enc_Extracto.cupDis     = wcupdis
          Tmp_Enc_Extracto.Sobrecupo  = wsobcup
          Tmp_Enc_Extracto.sdo_ant    = wsdoant  
          Tmp_Enc_Extracto.Cargos     = wtotret
          Tmp_Enc_Extracto.intCtes    = Creditos.INT_corrientes
          Tmp_Enc_Extracto.IntMora    = wintmor
          Tmp_Enc_Extracto.OtrosCar   = Tmp_Enc_Extracto.Otroscar + wTotOtrosC
          Tmp_Enc_Extracto.Pagos      = wtotAbok
          Tmp_Enc_Extracto.NvoSdo     = wpagtot
          Tmp_Enc_Extracto.PagoTotal  = wpagtot
          Tmp_Enc_Extracto.PagoMin    = wpagomin
          Tmp_Enc_Extracto.Tasa_IC    = ROUND(creditos.tasa / 12,3)
          Tmp_Enc_Extracto.Tasa_EA    = (EXP( 1 + ( creditos.tasa / 12 / 100 ) , 12) - 1) * 100
          Tmp_Enc_Extracto.FecPlazo   = wfecplazo
          Tmp_Enc_Extracto.FacDesde   = wfecini
          Tmp_Enc_Extracto.FecHasta   = wfecfin
          Tmp_Enc_Extracto.cuota      = wcuota.  /*  round( (wsdoant + wtotret -  wtotAbok)  / 36,0). */
   
   IF Tmp_Enc_Extracto.intCtes LT 0 THEN
      ASSIGN Tmp_Enc_Extracto.intCtes = 0.
   IF Tmp_Enc_Extracto.IntMora LT 0 THEN
      ASSIGN Tmp_Enc_Extracto.IntMora = 0.
          
   IF creditos.dias_atraso GT 0 AND Tmp_Enc_Extracto.valatrak GT 0 THEN DO: /* Para Pago Inmediato */ /*09-Abril-2008*/
      IF creditos.fec_pago EQ wfec_ANTproxpago THEN
         ASSIGN Tmp_Enc_Extracto.FecPlazo = W_Fecha + 3 
                Tmp_Enc_Extracto.FecMora  = creditos.Fec_pago + 1.
      ELSE
          ASSIGN Tmp_Enc_Extracto.FecPlazo = W_Fecha + 3 
                 Tmp_Enc_Extracto.FecMora  = creditos.Fec_pago.
   END.
   ELSE
      /** 4-Abril-2008 **/
      IF creditos.dias_atraso EQ 0 AND Tmp_Enc_Extracto.valatrak GT 0 THEN
         ASSIGN Tmp_Enc_Extracto.FecMora = wfec_ANTproxpago.
   /**********************/
   ASSIGN wcant = wcant + 1.
   
   /* Nuevo Por Agregar 14-Enero-2008*/
   IF (wpagtot EQ 0) AND (wpagtot - wpagomin) LT 0 THEN DO:
      ASSIGN wcuota   = ROUND((Creditos.sdo_capital - wsobcup - 0) / 36,0) +
                               Creditos.INT_Corrientes.
      IF wcuota LT 0 THEN wcuota = 0.
      UPDATE Tmp_Enc_Extracto.fecmora  = ?     /* 4 -Abril-2008*/
             Tmp_Enc_Extracto.valatrak = 0
             Tmp_Enc_Extracto.sdomora  = 0
             Tmp_Enc_Extracto.PagoMin  = wpagtot         
             Tmp_Enc_Extracto.cuota    = wcuota.
   END.
   ELSE
      IF (wpagtot GT 0) AND (wpagtot - wpagomin) LT 0 THEN DO:
         ASSIGN wpagomin = ROUND((Creditos.sdo_capital -  wsobcup - 0) / 36,0) + wsobcup + Creditos.INT_Corrientes +
                           wintmor + wTotOtrosC + creditos.honorarios + Creditos.costas + Tmp_Enc_Extracto.seg_cartera + 0.
         ASSIGN wcuota   = ROUND((Creditos.sdo_capital - wsobcup - 0) / 36,0) +
                                  Creditos.INT_Corrientes.
         IF wpagomin LT 0 THEN wpagomin = 0.
         IF wcuota   LT 0 THEN wcuota = 0.
   
         UPDATE Tmp_Enc_Extracto.fecmora  = ?  /* 4 -Abril-2008*/
                Tmp_Enc_Extracto.valatrak = 0
                Tmp_Enc_Extracto.sdomora  = 0
                Tmp_Enc_Extracto.PagoMin  = wpagomin
                Tmp_Enc_Extracto.cuota    = wcuota.
      END.
/* END.                       */
/* ELSE                       */
/*    ASSIGN vldelenc = TRUE. */

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

  ASSIGN FRAME F-Main:VISIBLE    = TRUE.
/*   Btn_Preliminar:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  */
/*   DISABLE Btn_Preliminar.                               */
/*   Btn_Facturacion:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */
/*   DISABLE Btn_Facturacion.                              */
  IF viPage NE 2 THEN
     DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
        INPUT "add,delete,update,cancel" /* CHARACTER */).

  CASE viPage:
      WHEN 1 THEN DO:
          ASSIGN FRAME FR-Proceso:VISIBLE = FALSE.
          RUN viewObject IN h_dynbrowser-Periodos.
/*           RUN repositionObject IN h_dynbrowser-Periodos ( 6.38 , 7.00 ) NO-ERROR. */
/*           RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 51.00 ) NO-ERROR.    */


          RUN repositionObject IN h_dynbrowser-Periodos ( 6.38 , 4.57 ) NO-ERROR.
          RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 53.00 ) NO-ERROR.


      END.
      WHEN 2 THEN DO:
          ASSIGN FRAME FR-Proceso:VISIBLE = FALSE.
          DYNAMIC-FUNCTION('modifyDisabledActions':U IN h_dyntoolbar, 
             INPUT "REMOVE" /* CHARACTER */,
             INPUT "add,delete,update,cancel" /* CHARACTER */).
          RUN resetTableio IN h_dyntoolbar.
          RUN viewObject IN h_dynbrowser-Periodos.
/*           RUN repositionObject IN h_dynbrowser-Periodos ( 12.31 , 20.00 ) NO-ERROR. */
/*           RUN resizeObject IN h_dynbrowser-Periodos (  4.85 , 51.00 ) NO-ERROR.     */


          RUN repositionObject IN h_dynbrowser-Periodos ( 11.50 , 29.00 ) NO-ERROR.
          RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 53.00 ) NO-ERROR.

      END.
      WHEN 3 THEN DO:
          RUN hideObject IN h_dynbrowser-Periodos.
          ASSIGN FRAME FR-Proceso:VISIBLE = TRUE.
      END.
/*       WHEN 4 THEN DO:                                */
/*           RUN hideObject IN h_dynbrowser-Periodos.   */
/*            ENABLE Btn_Facturacion WITH FRAME f-main. */
/*       END.                                           */
  END CASE.

/*   ASSIGN FRAME F-Main:VISIBLE    = TRUE.                          */
/*   IF DYNAMIC-FUNCTION('getcurrentpage') = 3 THEN DO:              */
/* /*      Btn_Preliminar:HIDDEN IN FRAME {&FRAME-NAME}  = FALSE. */ */
/* /*      Btn_Preliminar:VISIBLE IN FRAME {&FRAME-NAME} = TRUE.  */ */
/*      ENABLE Btn_Preliminar WITH FRAME f-main.                     */
/*   END.                                                            */
/*   ELSE DO:                                                        */
/*      Btn_Preliminar:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         */
/*      DISABLE Btn_Preliminar.                                      */
/*   END.                                                            */
/*   IF DYNAMIC-FUNCTION('getcurrentpage') = 4 THEN DO:              */
/*      ENABLE Btn_Facturacion WITH FRAME f-main.                    */
/*   END.                                                            */
/*   ELSE DO:                                                        */
/*      Btn_Facturacion:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.        */
/*      DISABLE Btn_Facturacion.                                     */
/*   END.                                                            */
/*                                                                   */
/*   /*MESSAGE DYNAMIC-FUNCTION('getcurrentpage')                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/                        */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreaCodigos wWin 
PROCEDURE CreaCodigos :
/*------------------------------------------------------------------------------
  Purpose: Crear Código de Cupo Rotativo     
  Autor  : Félix Vargas
  Fecha  : 08-02-2008      
------------------------------------------------------------------------------*/
CREATE TemCodigo.
UPDATE TemCodigo.cod_credito = 570.
CREATE TemCodigo.
UPDATE TemCodigo.cod_credito = 870.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deletecomplete wWin 
PROCEDURE deletecomplete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('refreshquery':U IN h_dper_facturacion).

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
FOR EACH Creditos WHERE 
    (Creditos.cod_credito  EQ 570 OR creditos.cod_credito EQ 870)  AND 
    ((Creditos.sdo_capital GE 0   AND
    ((Creditos.dias_atraso GT 0   AND Creditos.val_atraso EQ 0) OR
    (Creditos.dias_atraso  EQ 0   AND Creditos.val_atraso GT 0)))   OR
    (Creditos.sdo_capítal  EQ 0   AND Creditos.val_atraso GT Creditos.sdo_capital)):
    UPDATE Creditos.dias_atraso = 0
           Creditos.val_atraso = 0.
END.
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
  DISPLAY W_CmbOfi Msaje 
      WITH FRAME FR-Proceso IN WINDOW wWin.
  ENABLE RECT-3 W_CmbOfi Btn_ActPagos Btn_Preliminar Btn_FactOficial 
      WITH FRAME FR-Proceso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FR-Proceso}
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
ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Seleccionando Registros... Espere Por Favor.".
ASSIGN Msaje.

NvaFactura:
REPEAT TRANSACTION ON ERROR UNDO NvaFactura, LEAVE NvaFactura:
/*   DO TRANSACTION ON ERROR UNDO:  */
   FOR EACH tmp_Enc_Extracto:
      ASSIGN wcontador = wcontador + 1.
      ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " Fact.#: " + STRING(wcontador) + "Cédula: " + STRING(tmp_enc_extracto.nit) + "Credito: " + STRING(tmp_enc_extracto.numcre).
      ASSIGN Msaje.

      CREATE FACTURACION.
      ASSIGN Facturacion.Num_factura       = wcontador
             Facturacion.Per_Factura       = wperNUEVO    
             Facturacion.Nit               = tmp_enc_extracto.nit       
             Facturacion.fec_corte         = tmp_enc_extracto.fecor     
             Facturacion.nombre            = tmp_enc_extracto.nombre    
             Facturacion.direccion         = tmp_enc_extracto.direccion 
             Facturacion.Telefono          = tmp_enc_extracto.telefono  
             Facturacion.ciudad            = tmp_enc_extracto.ciudad    
             Facturacion.Num_Credito       = tmp_enc_extracto.numcre    
             Facturacion.CupoTotal         = tmp_enc_extracto.cuptot    
             Facturacion.CupoDisponible    = tmp_enc_extracto.cupDis    
             Facturacion.Sobrecupo         = tmp_enc_extracto.Sobrecupo 
             Facturacion.Sdo_Anterior      = tmp_enc_extracto.sdo_ant   
             Facturacion.Cargos            = tmp_enc_extracto.Cargos    
             Facturacion.Int_Corrientes    = tmp_enc_extracto.intCtes   
             Facturacion.Int_MorCobrar     = tmp_enc_extracto.IntMora   
             Facturacion.OtrosCargos       = tmp_enc_extracto.OtrosCar  
             Facturacion.Pagos             = tmp_enc_extracto.Pagos     
             Facturacion.Nuevo_Saldo       = tmp_enc_extracto.NvoSdo    
             Facturacion.Sdo_Mora          = tmp_enc_extracto.SdoMora   
             Facturacion.Pago_Total        = tmp_enc_extracto.PagoTotal 
             Facturacion.Pago_Minimo       = tmp_enc_extracto.PagoMin   
             Facturacion.Tasa_Ic           = tmp_enc_extracto.Tasa_IC   
             Facturacion.Tasa_Ea           = tmp_enc_extracto.Tasa_EA   
             Facturacion.Fec_Mora          = tmp_enc_extracto.FecMora   
             Facturacion.Nombre_Agencia    = tmp_enc_extracto.nom_agen 
             Facturacion.Agencia           = tmp_enc_extracto.agencia
             Facturacion.Fec_LimPago       = wfec_proxpago              
             Facturacion.Fec_Inicial       = wfec_inic    
             Facturacion.Fec_Final         = wfec_vcto
             Facturacion.Val_Recaudo       = 0                       
             Facturacion.Rec_Capital       = 0                       
             Facturacion.Rec_IntCorrientes = 0                       
             Facturacion.Rec_IntMora       = 0                       
             Facturacion.Rec_Honorarios    = 0
             Facturacion.Val_Atraso        = 0
             Facturacion.Cuota             = Tmp_Enc_Extracto.cuota
             Facturacion.PagoTotal         = NO
             Facturacion.Estado            = 1
             Facturacion.Val_atrasokpen    = Tmp_Enc_Extracto.valatrak
             Facturacion.Seg_Cartera       = Tmp_Enc_Extracto.seg_cartera.

      FIND FIRST creditos  WHERE Creditos.agencia      EQ Facturacion.agencia                 AND 
                                 Creditos.Tip_credito  EQ 1                                   AND 
                                (Creditos.cod_credito  EQ 570 OR Creditos.cod_credito EQ 870) AND
                                 Creditos.Num_credito  EQ Facturacion.num_credito             AND            
                                 Creditos.nit          EQ Facturacion.nit NO-ERROR.
      IF AVAILABLE(creditos) THEN DO:
        ASSIGN creditos.cuota      = Facturacion.cuota
               Creditos.Honorarios = /* Creditos.honorarios + */ Facturacion.OtrosCargos 
               Creditos.Polizas    = Facturacion.Seg_Cartera.
/*                Creditos.Fec_pago   = Facturacion.Fec_limPago. */
        /** 4-Abril-2008 **/
        IF Facturacion.fec_mora NE ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN DO:
/*            IF Facturacion.fec_mora EQ wfec_proxpago THEN      */
/*               ASSIGN Facturacion.fec_mora = wfec_ANTproxpago. */
           ASSIGN Creditos.Fec_pago    = Facturacion.Fec_Mora
                  Creditos.Val_Atraso  = Facturacion.val_atrasokpen
                  creditos.Dias_Atraso = (w_fecha + 1) - Facturacion.Fec_mora.
        END.
        ELSE
          IF Facturacion.fec_mora EQ ? AND Facturacion.val_atrasokpen GT 0 AND facturacion.sdo_mora GT 0 THEN
/*              ASSIGN Facturacion.fec_mora = wfec_ANTproxpago */
             ASSIGN Creditos.Val_Atraso  = Facturacion.val_atrasokpen
                    creditos.Dias_Atraso = (w_fecha + 1) - Facturacion.Fec_mora.
          ELSE
            IF Facturacion.fec_mora EQ ? AND Facturacion.val_atrasokpen LE 0 AND facturacion.sdo_mora LE 0 THEN
               ASSIGN Creditos.Fec_pago       = Facturacion.Fec_limPago
                      Creditos.Val_Atraso     = 0
                      Creditos.Dias_Atraso    = 0
                      Creditos.Int_MorCobrar  = 0
                      Creditos.Int_MoraDifCob = 0
                      Creditos.Int_DifCobro   = 0.
        /****************/
        IF Facturacion.Pago_Total GT 0 THEN DO:
            FIND FIRST planpagos WHERE   planpagos.agencia       =  creditos.agencia      AND
                                         planpagos.Tip_credito   =  Creditos.Tip_credito  AND
                                         planpagos.cod_credito   =  creditos.cod_credito  AND
                                         planpagos.Num_credito   =  Creditos.Num_credito  AND
                                         planpagos.nit           =  Creditos.nit          AND
                                         planpagos.id_pdo        = 1 NO-ERROR.
            IF AVAILABLE(planpagos) THEN DO:
              ASSIGN PlanPagos.Int_LiqAcum    = 0
                     PlanPagos.Int_LiqPdo     = 0                                                                        
                     PlanPagos.Int_MoraAcum   = 0                                                                         
                     PlanPagos.Int_MoraPdo    = 0                                                                         
                     PlanPagos.Monto_Actual   = 0                                                                         
                     PlanPagos.Fec_Vcto       = wfec_proxpago                                                             
                     PlanPagos.Fec_ProxPago   = wfec_inic                                                                 
                     PlanPagos.Fec_Inic       = wfec_vcto                                                                 
                     PlanPagos.Cuo_Pagas      = 0                                                                         
                     PlanPagos.Cuota          = Facturacion.cuota                                                         
                     PlanPagos.Cargos_Pdo     = Facturacion.Pago_minimo - (Facturacion.Cuota - Facturacion.INT_corrientes)                                                                         
                     PlanPagos.Cargos_Acum    = 0                                                                         
                     PlanPagos.Capital_Pdo    = Facturacion.Cuota - Facturacion.INT_corrientes 
                     PlanPagos.Capital_Acum   = 0.                                                                         
            END.
            ELSE DO:
                MESSAGE "No fue encontrado PlanPagos para la actualizacion" SKIP
                        "de datos de la nueva factura par el Credito Nro.:" Facturacion.Num_Credito SKIP(1)
                        " Cedula  : " Factura.nit     SKIP
                        " Agencia : " Factura.agencia SKIP
                        "Comunique esta inconsistencia al Administrador!"
                    VIEW-AS ALERT-BOX.
                RETURN ERROR.
            END.
        END.
      END.
      ELSE DO:
         MESSAGE "No fue encontrado el producto de créditos para la" SKIP
                 "actualizacion de datos de la nueva factura Credito Nro." Facturacion.Num_Credito SKIP(1)
                 " Cedula  : " Factura.nit     SKIP
                 " Agencia : " Factura.agencia SKIP
                 "Comunique esta inconsistencia al Administrador!"
             VIEW-AS ALERT-BOX.
         RETURN ERROR.
      END.
   END.
   LEAVE.
   EMPTY TEMP-TABLE Tmp_Tot_Extracto.
   EMPTY TEMP-TABLE Tmp_Enc_Extracto.
   EMPTY TEMP-TABLE Tmp_Det_Extracto.
END.  /*Fin Tx*/

END PROCEDURE.

/*          /* IF Facturacion.sdo_mora LE 0 THEN   /* Toda la facturacion con la nueva fecha, por intruccion de OMAR OSORIO      */
/*                                                       AGOSTO 7 DE 2007 */                                                     */
/*             ASSIGN Facturacion.Fec_LimPago       = wfec_proxpago                                                              */
/*                    Facturacion.Fec_Inicial       = wfec_inic                                                                  */
/*                    Facturacion.Fec_Final         = wfec_vcto.                                                                 */
/*          ELSE                                                                                                                 */
/*             ASSIGN Facturacion.Fec_LimPago       = wfec_ANTproxpago                                                           */
/*                    Facturacion.Fec_Inicial       = wfec_ANTinic                                                               */
/*                    Facturacion.Fec_Final         = wfec_ANTvcto.      */                                                      */
/*                                                                                                                               */
/*          FIND FIRST creditos  WHERE creditos.agencia     = Facturacion.agencia     AND                                        */
/*                                     Creditos.Tip_credito = 1                       AND                                        */
/*                                     (creditos.cod_credito = 570 OR creditos.cod_credito = 870)  AND                           */
/*                                     Creditos.Num_credito = Facturacion.num_credito AND                                        */
/*                                     Creditos.nit         = Facturacion.nit NO-ERROR.                                          */
/*          IF AVAILABLE(creditos) THEN DO:                                                                                      */
/*            ASSIGN creditos.cuota      = Facturacion.cuota                                                                     */
/*                   Creditos.Honorarios = /* Creditos.honorarios + */ Facturacion.OtrosCargos                                   */
/*                   Creditos.Polizas    = Facturacion.Seg_Cartera                                                               */
/*                   Creditos.Fec_pago   = Facturacion.Fec_limPago.                                                              */
/*            IF Facturacion.Pago_Total GT 0 THEN DO:                                                                            */
/*                FIND FIRST planpagos WHERE   planpagos.agencia       =  creditos.agencia      AND                              */
/*                                             planpagos.Tip_credito   =  Creditos.Tip_credito  AND                              */
/*                                             planpagos.cod_credito   =  creditos.cod_credito  AND                              */
/*                                             planpagos.Num_credito   =  Creditos.Num_credito  AND                              */
/*                                             planpagos.nit           =  Creditos.nit          AND                              */
/*                                             planpagos.id_pdo        = 1                                                       */
/*                                             NO-ERROR.                                                                         */
/*                IF AVAILABLE(planpagos) THEN DO:                                                                               */
/*                  ASSIGN PlanPagos.Int_LiqAcum    = 0                                                                          */
/*                         PlanPagos.Int_LiqPdo     = 0                                                                          */
/*                         PlanPagos.Int_MoraAcum   = 0                                                                          */
/*                         PlanPagos.Int_MoraPdo    = 0                                                                          */
/*                         PlanPagos.Monto_Actual   = 0                                                                          */
/*                         PlanPagos.Fec_Vcto       = wfec_proxpago                                                              */
/*                         PlanPagos.Fec_ProxPago   = wfec_inic                                                                  */
/*                         PlanPagos.Fec_Inic       = wfec_vcto                                                                  */
/*                         PlanPagos.Cuo_Pagas      = 0                                                                          */
/*                         PlanPagos.Cuota          = Facturacion.cuota                                                          */
/*                         PlanPagos.Cargos_Pdo     = Facturacion.Pago_minimo - (Facturacion.Cuota - Facturacion.INT_corrientes) */
/*                         PlanPagos.Cargos_Acum    = 0                                                                          */
/*                         PlanPagos.Capital_Pdo    = Facturacion.Cuota - Facturacion.INT_corrientes                             */
/*                         PlanPagos.Capital_Acum   = 0.                                                                         */
/*                END.                                                                                                           */
/*                ELSE DO:                                                                                                       */
/*                    MESSAGE "No fue encontrado PlanPagos para la actualizacion" SKIP                                           */
/*                            "de datos de la nueva factura par el Credito Nro.:" Facturacion.Num_Credito SKIP(1)                */
/*                            " Cedula  : " Factura.nit     SKIP                                                                 */
/*                            " Agencia : " Factura.agencia SKIP                                                                 */
/*                            "Comunique esta inconsistencia al Administrador!"                                                  */
/*                    VIEW-AS ALERT-BOX.                                                                                         */
/*                    RETURN ERROR.                                                                                              */
/*                END.                                                                                                           */
/*            END.                                                                                                               */
/*          END.                                                                                                                 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpriPagosFacturacion wWin 
PROCEDURE ImpriPagosFacturacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Nuevo 03-Marzo-2008*/
FIND FIRST Temcreditos NO-ERROR.
IF AVAILABLE(Temcreditos)THEN
   ASSIGN vlgenpagos = TRUE.
ELSE
   ASSIGN vlgenpagos = FALSE.

IF vlgenpagos THEN DO:
    Listado = W_PathSpl + "PagosFacturacion-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".csv".
    OS-DELETE VALUE(Listado).
    OUTPUT TO value(Listado).
    PUT "Agencia;Nit;Credito;Cod.Credito;Fecha;Cpte;Rec_Capital;Rec_Int.Ctes;Rec_Int.Mora;Rec_Honora;Rec_SegCar;Actualizado" SKIP(0).

    FOR EACH Temcreditos BY TAgencia BY TNumcred: 
        PUT TAgencia   ";" 
            TNit       ";" 
            TNumcred   ";" 
            TCodoper   ";"
            TFecha     FORMAT "99/99/9999" ";"
            Tcpte      ";"
            TRecAboK   FORMAT "->>>,>>>,>>>,>>9.99" ";"
            TRecAboIC  FORMAT "->>>,>>>,>>>,>>9.99" ";"
            TRecAboIM  FORMAT "->>>,>>>,>>>,>>9.99" ";"
            TRecHonora FORMAT "->>>,>>>,>>>,>>9.99" ";"
            TRecSegCar FORMAT "->>>,>>>,>>>,>>9.99" ";"
            TActualiza
            SKIP(0).
    END.
    MESSAGE "El archivo plano esta en " listado
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".
    ASSIGN msaje.
    OUTPUT CLOSE.
END.
ELSE
   MESSAGE "No Encontro Registro Para Actualizar.. Ok." 
       VIEW-AS ALERT-BOX.
/****/
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
SUBSCRIBE "deletecomplete" ANYWHERE.

ASSIGN  W_CmbOfi:SCREEN-VALUE IN FRAME FR-Proceso = ("").
W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                    AND Agencias.Agencia GT 0 NO-LOCK BY agencia:
    W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(80)")).
    IF Agencias.Agencia EQ W_Agencia THEN
       ASSIGN W_CmbOfi:SCREEN-VALUE = ("000 CONSOLIDADO").
END.
DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar,
   INPUT 17).
DYNAMIC-FUNCTION('setBGColor':U IN h_folder,
   INPUT 19).
DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar,
   INPUT "add,delete,update,cancel" /* CHARACTER */).   

DISABLE Btn_Preliminar  WITH FRAME FR-Proceso.
DISABLE Btn_FactOficial WITH FRAME FR-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SdoNegativos wWin 
PROCEDURE SdoNegativos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH creditos WHERE 
    (creditos.cod_credito   EQ 570 OR creditos.cod_credito    EQ 870) AND
    (creditos.sdo_capital   LT 0   OR creditos.INT_Corrientes LT 0 OR
    creditos.Int_MoraDifCob LT 0   OR creditos.Int_MorCobrar  LT 0 OR 
    creditos.Int_DifCobro   LT 0   OR Creditos.INT_anticipado LT 0 OR
    creditos.val_atraso     LT 0   OR Creditos.dias_atraso    LT 0):

    IF creditos.sdo_capital LT 0 THEN
       UPDATE creditos.sdo_capital = 0.
    IF creditos.INT_Corrientes LT 0 THEN
       UPDATE creditos.INT_Corrientes = 0.
    IF creditos.Int_MoraDifCob LT 0 THEN
       UPDATE creditos.Int_MoraDifCob = 0
              creditos.Int_MorCobrar  = 0.
    IF creditos.Int_MorCobrar LT 0 THEN
       UPDATE creditos.Int_MorCobrar  = 0
              creditos.Int_MoraDifCob = 0.
    IF creditos.Int_DifCobro LT 0 THEN
       UPDATE creditos.Int_DifCobro  = 0.
    IF creditos.INT_anticipado LT 0 THEN
       UPDATE creditos.INT_anticipado = 0.
    IF creditos.val_atraso LT 0 THEN
       UPDATE creditos.val_atraso = 0.
    IF creditos.dias_atraso LT 0 THEN
       UPDATE creditos.dias_atraso = 0.
END.

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
ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Seleccionando Registros... Espere Por Favor.".
ASSIGN Msaje.
FIND FIRST per_facturacion WHERE estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE(per_facturacion) THEN DO:  /* Facturacion Vigente */
    ASSIGN wfec_ANTproxpago  = per_facturacion.fec_limpago
           wfec_ANTinic      = per_facturacion.fec_inicial
           wfec_ANTvcto      = per_facturacion.fec_final
           wperiodo          = Per_Facturacion.Per_Factura.
    FIND FIRST per_facturacion WHERE per_factura = wperiodo + 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(per_facturacion) THEN  DO: /* Nueva Facturacion */ 
       ASSIGN wfec_proxpago  = per_facturacion.fec_limpago
              wfec_inic      = per_facturacion.fec_inicial
              wfec_vcto      = per_facturacion.fec_final
              wperNUEVO      = Per_Facturacion.Per_Factura.
       IF w_preliminar = TRUE THEN DO:
          MESSAGE "Desea Facturación Preliminar con las siguientes Fechas" SKIP(1)
                  "Periodo de Facturacion: " wperNUEVO SKIP
                  "Fecha Inicial Corte       : " STRING(wfec_inic,"99/99/9999")    SKIP
                  "Fecha Final de Corte    : " STRING(wfec_vcto,"99/99/9999")     SKIP 
                  "Fecha Limite Plazo        : " STRING(wfec_proxpago,"99/99/9999")
                VIEW-AS ALERT-BOX 
                QUESTION BUTTONS YES-NO UPDATE Ask AS LOGICAL.
          IF NOT Ask THEN 
             RETURN ERROR.
       END.
       IF w_oficial = TRUE THEN DO:
          /* Nuevo Fecha y Hora de la Facturacion*/
          ASSIGN vttiempo   = TIME.
          ASSIGN vtsegundos = vttiempo MOD 60.
          ASSIGN vttiempo   = (vttiempo - vtsegundos) / 60.  
          ASSIGN vtminutos  = vttiempo MOD 60.
          ASSIGN vthora     = (vttiempo - vtminutos) / 60.   

          IF (((wfec_vcto    EQ W_Fecha) AND (vthora GE 22 AND vthora LE 24)) OR
             ((wfec_vcto + 1 EQ W_Fecha) AND (vthora GE 0  AND vthora LE 8))) THEN DO:
             MESSAGE "Desea Facturar Oficialmente con las siguientes Fechas" SKIP(1)
                     "Periodo de Facturacion: " wperNUEVO SKIP
                     "Fecha Inicial Corte       : " wfec_inic SKIP
                     "Fecha Final de Corte    : " wfec_vcto SKIP
                     "Fecha Limite Plazo        : " wfec_proxpago VIEW-AS ALERT-BOX
                    QUESTION BUTTONS YES-NO UPDATE Askofi AS LOGICAL.
             IF NOT Askofi THEN
                RETURN ERROR.
          END.
          ELSE DO:
             MESSAGE "NO es Posible Generar la Facturación Oficial" SKIP(1)
                     "Fecha Actual: " STRING(W_Fecha,"99/99/9999") SKIP
                     "Fecha Corte : " STRING(wfec_vcto,"99/99/9999") " Proxima Facturación" SKIP
                     "Hora Actual  : " STRING(vthora) " a.m/p.m " SKIP
                     "La Hora Debe Ser Inferior a las 8 a.m. del Día Siguiente..."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN ERROR.
          END.
       END.
    
       ASSIGN wfecIni   = wfec_inic 
              wfecfin   = wfec_vcto
              wfecplazo = wfec_proxpago.
       /* Nuevo 8-Febrero-2008**/
       FOR EACH mov_creditos WHERE
            mov_creditos.fecha       LT wfec_inic AND
           (mov_creditos.cod_credito EQ 570 OR 
            mov_creditos.cod_credito EQ 870)
           NO-LOCK 
           BREAK BY mov_creditos.num_credito BY mov_creditos.fecha DESCENDING  BY mov_creditos.hora DESCENDING:
           IF FIRST-OF (mov_creditos.num_credito) THEN DO:
              CREATE TemSdoAntCre.
              UPDATE TemSdoAntCre.num_credito = mov_creditos.num_credito
                     TemSdoAntCre.fecha       = mov_creditos.fecha
                     TemSdoAntCre.sdo_capital = mov_creditos.sdo_capital.
           END.
       END.
    
       FOR EACH TemCodigo,
           EACH  mov_creditos WHERE 
                 mov_creditos.fecha       GE wfec_inic             AND
                 mov_creditos.fecha       LE wfec_Vcto             AND
                 mov_creditos.cod_credito EQ TemCodigo.cod_credito
                 NO-LOCK:
           CREATE TemMovCre.
           UPDATE TemMovCre.agencia       = mov_creditos.agencia
                  TemMovCre.nit           = mov_creditos.nit
                  TemMovCre.num_credito   = mov_creditos.num_credito
                  TemMovCre.cod_credito   = mov_creditos.cod_credito
                  TemMovCre.cod_operacion = mov_creditos.cod_operacion
                  TemMovCre.cpte          = mov_creditos.cpte
                  TemMovCre.num_documento = mov_creditos.num_documento
                  TemMovCre.fecha         = mov_creditos.fecha
                  TemMovCre.descrip       = mov_creditos.descrip
                  TemMovCre.nro_auditoria = mov_creditos.num_documento
                  TemMovCre.val_efectivo  = mov_creditos.val_efectivo
                  TemMovCre.val_cheque    = mov_creditos.val_cheque.
           
           FIND FIRST mov_contable WHERE
                mov_contable.agencia       EQ mov_creditos.agencia                AND
                mov_contable.comprobante   EQ mov_creditos.cpte                   AND
                mov_contable.num_documento EQ integer(mov_creditos.num_documento) AND 
                mov_contable.nit           EQ mov_creditos.nit                    AND 
                mov_contable.fec_contable  EQ mov_creditos.fecha                  AND 
               (mov_contable.db + mov_contable.cr) EQ (mov_creditos.val_efectivo + mov_creditos.val_cheque)
               NO-LOCK NO-ERROR.
           IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN
              UPDATE TemMovCre.nro_auditoria = mov_contable.nro_auditoria
                     TemMovCre.detalle       = "Si"
                     TemMovCre.comentario    = mov_contable.comentario.
           ELSE
              UPDATE TemMovCre.detalle       = "No".
       END.
       /*****************************/
    
       FOR EACH creditos WHERE 
          (Creditos.cod_credito EQ 570 OR Creditos.cod_credito EQ 870)    AND
           Creditos.estado      EQ 2  AND Creditos.sdo_capital GT 0 AND
           Creditos.agencia     GE ageini AND Creditos.agencia LE agefin 
          NO-LOCK BREAK BY Creditos.agencia BY Creditos.nit:

          FIND FIRST Clientes WHERE Clientes.Nit EQ creditos.nit AND clientes.estado EQ 2
               NO-LOCK NO-ERROR.
          IF AVAILABLE (clientes) THEN NEXT.

          ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Agencia: " + STRING(creditos.agencia) + " - Cédula: " + STRING(creditos.nit) + "  - Crédito: " + STRING(creditos.num_credito).
          ASSIGN Msaje.
          ASSIGN wvlrOtrosC    = 0   wzswDet   = FALSE  wcupdis    = 0   wsobcup    = 0
                 wcupdis       = 0   wnomciu   = ""     wdircorres = ""
                 wcodciu       = ""  wtel      = ""     wtotabok   = 0   wintmor    = 0
                 wpagtot       = 0   wtotret   = 0      wcuota     = 0   wTotOtrosC = 0  
                 wpagomin      = 0   wpagoSegC = 0      wvlrCobSC  = 0   vicorres   = 1.  
          wcantot = wcantot + 1.
          wcantage = wcantage + 1.
          /* Control Estadistico */
          /* Ojo*/
          ASSIGN wvlrCobSC = creditos.polizas.
          IF LAST-OF(creditos.agencia) THEN DO:
             ASSIGN wcantotage = wcantage wcantage = 0.
             FIND FIRST agencias WHERE agencias.agencia = creditos.agencia NO-LOCK NO-ERROR.
             IF AVAILABLE(agencias) THEN
                DO:
                 CREATE Tmp_Tot_Extracto.
                 ASSIGN                                                   
                 tmp_Tot_Extracto.agencia    =  agencias.agencia  
                 tmp_Tot_Extracto.Nombre     =  agencias.nombre           
                 tmp_Tot_Extracto.TotAso     =  wcantotage                   
                 tmp_Tot_Extracto.TotAsoExt  =  0  
                 tmp_Tot_Extracto.Tcuptot    =  0  
                 tmp_Tot_Extracto.TcupDis    =  0  
                 tmp_Tot_Extracto.TSobrecupo =  0  
                 tmp_Tot_Extracto.Tsdo_ant   =  0  
                 tmp_Tot_Extracto.TCargos    =  0  
                 tmp_Tot_Extracto.TintCtes   =  0  
                 tmp_Tot_Extracto.TIntMora   =  0  
                 tmp_Tot_Extracto.TOtrosCar  =  0  
                 tmp_Tot_Extracto.TPagos     =  0  
                 tmp_Tot_Extracto.TNvoSdo    =  0  
                 tmp_Tot_Extracto.TSdoMora   =  0  
                 tmp_Tot_Extracto.TPagoTotal =  0  
                 tmp_Tot_Extracto.TPagoMin   =  0  
                 tmp_Tot_Extracto.TPagoSegC  =  0  
                 tmp_Tot_Extracto.Tasa_IC    =  0  
                 tmp_Tot_Extracto.Tasa_EA    =  0. 
                END.
          END.
    
          /** Nuevo */
          ASSIGN wsalsegcar = 0.
          /**************/
          /* Saldo inicial de Facturacion */
          /* Nuevo 8-Febrero-2008**/
          FIND FIRST TemSdoAntCre WHERE 
              TemSdoAntCre.num_credito EQ creditos.num_credito 
              NO-LOCK NO-ERROR.
          IF AVAILABLE (TemSdoAntCre) THEN DO:
             ASSIGN wsdoant = TemSdoAntCre.sdo_capital
                    wzswDet = TRUE.
           /***********************/
             /* Nuevo 14-Enero-2008*/
             IF wsdoant LT 0 THEN
                ASSIGN wsdoant = 0.
             /**********************/
          END.           
          ELSE wsdoant = 0.
    
           /* Nuevo 8-Febrero-2008**/
          FOR EACH TemMovCre WHERE 
              TemMovCre.num_credito EQ creditos.num_credito AND
              TemMovCre.nit         EQ creditos.nit 
              NO-LOCK BREAK BY TemMovCre.Fecha BY TemMovCre.descrip :

              FIND FIRST Clientes WHERE Clientes.Nit EQ creditos.nit AND clientes.estado EQ 2
                   NO-LOCK NO-ERROR.
              IF AVAILABLE (clientes) THEN NEXT.

              ASSIGN wvlrRet = 0  wvlrAbok = 0  wvlrAboi = 0  wvlrAboim = 0  wvlrHon = 0 wvlrAboSC = 0.
              IF TemMovCre.cod_operacion = 030303001 THEN NEXT. /* Traslado entre agencias No tener en cuenta */
              /* Acumulo las comisiones por dia */
              IF FIRST-OF(TemMovCre.fecha) THEN /* Se da el total de las comisiones por día y asi no dar largos detalles */ 
                 ASSIGN wvlrOtrosC = 0 wvlrOtrosCMan = 0.
               /* Ojo*/
              IF TemMovCre.cod_operacion = 20102001 AND TemMovCre.descrip BEGINS "RETIRO" THEN
                 ASSIGN wvlrCobSC = ROUND(wvlrCobSC + (((TemMovCre.val_efectivo + TemMovCre.val_cheque) * 1 ) / 100 ),0).
            
              IF TemMovCre.cod_operacion = 20102001 AND TemMovCre.descrip BEGINS "COMISION" THEN
                 ASSIGN wvlrOtrosC = wvlrOtrosC + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  )
                        wTotOtrosC = wTotOtrosC + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  ).
              ELSE DO:
                  CASE TemMovCre.cod_operacion :
                      WHEN 20102001 THEN    /* Retiro / Desembolso de Credito  */
                           wvlrRet = (TemMovCre.val_efectivo + TemMovCre.val_cheque). 
                      WHEN 20101001 THEN   /* Abono A Capital */
                           wvlrAbok = TemMovCre.val_efectivo + TemMovCre.val_cheque.
                      WHEN 20101003 OR WHEN 20102006 THEN  /* Abono a Int. Corrientes y Cargo de Int. Corrientes  */
                           wvlrAboi  = TemMovCre.val_efectivo + TemMovCre.val_cheque.
                      WHEN 20101007 THEN    /* Abono a Honorarios: Comisiones y Sobrecupo */
                           wvlrHon = (TemMovCre.val_efectivo + TemMovCre.val_cheque).
                      WHEN 20101002 OR WHEN 20101004 THEN  /* Abono a Int. Mora  e Int. Dificil Cobro
                                                               para la fact 2 tenerlo  en cuenta 
                                                              al comparar con el pago minimo */
                           wvlrAboiM = TemMovCre.val_efectivo + TemMovCre.val_cheque.
                      WHEN 20101006 THEN    /* Abono de Pólizas / Seguro de Cartera*/
                           wvlrAboSC =TemMovCre.val_efectivo + TemMovCre.val_cheque.
            
                  END CASE.
                  ASSIGN wtotabok = wtotabok + wvlrabok   
                         wtotret  = wtotret + wvlrret.
              END.
              IF TemMovCre.cod_operacion = 10302001 AND TemMovCre.descrip BEGINS "COBRO SOBREGIRO" THEN DO:
                 ASSIGN wvlrOtrosCMan = wvlrOtrosCMan + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  )
                        wtotret  = wtotret + wvlrOtrosCMan.
              END.
              /* C U E R P O   D E L   I N F O R M E  */
              IF  (wvlrRet + wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC) NE  0 THEN DO:
                  /*  PUT TRIM(creditos.nit) ";" creditos.num_credito ";" mov_creditos.fecha  ";" mov_creditos.descrip ";" mov_creditos.num_documento    ";" vlrRet ";"     vlrAbo ";"  creditos.tasa   SKIP. */
                   CREATE Tmp_Det_Extracto.
                   ASSIGN wnro_auditoria = ""  wdetaFact = "".
                   IF TemMovCre.detalle = "Si" THEN DO:
                      /* Corresponde al Detalle de la Factura*/
                      IF TemMovCre.comentario BEGINS "Retiro Red Exter" OR
                         TemMovCre.comentario BEGINS "Reversada Cajero" OR
                         TemMovCre.comentario BEGINS "AJUSTE" THEN
                         wdetaFact = SUBSTRING(mov_contable.comentario,3,17) + SUBSTRING(mov_contable.comentario,39,30).
                      ELSE
                         IF TemMovCre.comentario BEGINS "Retiro Cajero  "  OR
                            TemMovCre.Comentario BEGINS "Retiro Datafono"  THEN
                            wdetaFact = SUBSTRING(TemMovCre.comentario,3,17).
                         ELSE
                            wdetaFact = SUBSTRING(TemMovCre.comentario,3,17) + SUBSTRING(TemMovCre.comentario,39,30).
                      /**********************/
                   END.
                   ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).
                   ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                          Tmp_Det_Extracto.num_credito   = creditos.num_credito
                          Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                          Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                          Tmp_Det_Extracto.CR            = wvlrRet 
                          Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC.
                          /* Ojo*/
                   IF wdetafact NE " "  THEN
                      Tmp_Det_Extracto.descripcion   = wdetafact.
                   ELSE
                      Tmp_Det_Extracto.descripcion   = TemMovCre.descrip.
            
                   IF Tmp_Det_Extracto.descripcion = "Cgo.Capital X Trasl."  OR 
                      Tmp_Det_Extracto.descripcion = "Cgo.Int.Ctes.X Trasl."  THEN
                           ASSIGN Tmp_Det_Extracto.descripcion = 'Traslado Cuenta Ahorros'.
    
                   IF Tmp_Det_Extracto.descripcion = "Pagaré ActivadoCgo.Capital X Trasl." THEN
                      ASSIGN Tmp_Det_Extracto.descripcion = "Traslado CupoRotat Cta Ahorros".
                   /*Nuevo 12-Oct-2007*/
                   IF Tmp_Det_Extracto.descripcion = "Abono para Honorarios-Efectivo" THEN
                      ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Comisión-Efectivo".
                   IF Tmp_Det_Extracto.descripcion = "Abono para Honorarios-Cheque" THEN
                      ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Comisión-Cheque".
                   /* Ojo*/
                  IF Tmp_Det_Extracto.descripcion = "Abono para Polizas-Efectivo" THEN
                     ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Seg.Cartera-Efectivo".             
                  IF Tmp_Det_Extracto.descripcion = "Abono para Polizas-Cheque" THEN
                     ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Seg.Cartera-Cheque".             
                  /*******************/
    
                  ASSIGN wzswDet = TRUE.
              END.
              /* Sumatoria de Comisiones por Dia */
              IF LAST-OF(TemMovCre.fecha) AND wvlrOtrosC > 0 THEN   DO:                                                                          /* Agregado  los  nuevos valores */
                 CREATE Tmp_Det_Extracto.
                 wnro_auditoria = "".
                 ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).
                 ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                        Tmp_Det_Extracto.num_credito   = creditos.num_credito
                        Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                        Tmp_Det_Extracto.descripcion   = "Utilizacion Cajero"
                        Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                        Tmp_Det_Extracto.CR            = wvlrOtrosC 
                        Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi.
                 ASSIGN wzswDet = TRUE.
             END.
             /* Nuevo 04-Mar-2008*/
             IF LAST-OF(TemMovCre.fecha) AND wvlrOtrosCMan > 0 THEN   DO:                                                                          /* Agregado  los  nuevos valores */
                 CREATE Tmp_Det_Extracto.
                 wnro_auditoria = "".
                 ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).
                 ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                        Tmp_Det_Extracto.num_credito   = creditos.num_credito
                        Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                        Tmp_Det_Extracto.descripcion   = "Cobro Transacciones Cajero"
                        Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                        Tmp_Det_Extracto.CR            = wvlrOtrosCMan 
                        Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi.
                 ASSIGN wzswDet = TRUE.
             END.
             /* TERMINA EL CUERPO */
          END.
          IF wzswDet THEN DO:
            CREATE Tmp_Enc_Extracto.
            ASSIGN Tmp_Enc_Extracto.seg_cartera = wvlrCobSC
                   Tmp_Enc_Extracto.Otroscar = Creditos.honorarios.
/*             ASSIGN vldelenc = FALSE. */
            RUN BuscarAnteriorFactura.
            /* 4-Abril-2008**/
/*             IF NOT vldelenc THEN DO: */
               IF Tmp_Enc_Extracto.fecmora NE ? AND Tmp_Enc_Extracto.valatrak GT 0 AND Tmp_Enc_Extracto.sdomora GT 0 THEN DO:
                  IF Tmp_Enc_Extracto.fecmora EQ wfec_proxpago THEN
                     ASSIGN Tmp_Enc_Extracto.fecmora = wfec_ANTproxpago.
               END.
               ELSE
                 IF Tmp_Enc_Extracto.fecmora EQ ? AND Tmp_Enc_Extracto.valatrak GT 0 AND Tmp_Enc_Extracto.sdomora GT 0 THEN
                    ASSIGN Tmp_Enc_Extracto.fecmora = wfec_ANTproxpago.
               /**********************/
/*             END.                                 */
/*             ELSE  /* Sin Pago Total No Factura*/ */
/*                DELETE Tmp_Enc_Extracto.          */
          END.
       END. /* For de Creditos*/
    END.

    ELSE DO:
         MESSAGE "NO Ha Sido Greabado el Próximo Periodo a Facturar..." SKIP
                 "Ingrese el Periodo... Luego Ejecute de Nuevo el Proceso."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN ERROR.
    END.
END. /* Periodo Actual*/
ELSE DO:
     MESSAGE "NO Existe El Último Periódo Facturado...." SKIP
             "Comuníquese con el Administrador."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN ERROR.
END.

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
/* DEFINE VAR wfec_Avcto     AS DATE.                                                                                                                      */
/* DEFINE VAR totfact  AS INTEGER INITIAL 0 NO-UNDO.                                                                                                       */
/* DEFINE VAR totcred  AS INTEGER INITIAL 0 NO-UNDO.                                                                                                       */
/*                                                                                                                                                         */
/* ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Seleccionando Registros... Espere Por Favor.".                                                       */
/* ASSIGN Msaje.                                                                                                                                           */
/* /* Listado = W_PathSpl + "PagosFacturacion-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".csv". */             */
/* /* OS-DELETE VALUE(Listado).                                                                                                                        */  */
/* /* OUTPUT TO value(Listado).                                                                                                                        */  */
/* /* PUT "Agencia;Nit;Credito;Operacion;Val_Cheque;Val_Efec;Fecha;cpte" SKIP(0).                                                                      */  */
/*                                                                                                                                                         */
/* FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.                                                                           */
/* IF AVAILABLE(per_facturacion) THEN DO:                                                                                                                  */
/*    ASSIGN wfec_Avcto      = per_facturacion.fec_final.                                                                                                  */
/*    MESSAGE "Periodo Actual : " per_facturacion.per_factura  SKIP                                                                                        */
/*            "Fecha Corte    : " STRING(wfec_Avcto,"99/99/9999")                                                                                          */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                               */
/* END.                                                                                                                                                    */
/* ELSE DO:                                                                                                                                                */
/*    MESSAGE "Proceso Cancelado. Comuníquese con el Administrado"                                                                                         */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                               */
/*    RETURN.                                                                                                                                              */
/* END.                                                                                                                                                    */
/*                                                                                                                                                         */
/* FOR EACH facturacion WHERE facturacion.estado            EQ 1 AND                                                                                       */
/*                            facturacion.Val_Recaudo       EQ 0 AND                                                                                       */
/*                            facturacion.rec_capital       EQ 0 AND                                                                                       */
/*                            facturacion.Rec_IntCorrientes EQ 0 AND                                                                                       */
/*                            facturacion.Rec_IntMora       EQ 0 AND                                                                                       */
/*                            facturacion.Rec_Honorarios    EQ 0 NO-LOCK BY nit:                                                                           */
/*     ASSIGN totfact = totfact + 1.                                                                                                                       */
/*     FIND FIRST mov_creditos WHERE                                                                                                                       */
/*         (mov_creditos.cod_operacion EQ 20101001   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20101003   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20102006   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20101007   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20101002   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20101004   OR                                                                                                    */
/*          mov_creditos.cod_operacion EQ 20101006)  AND                                                                                                   */
/*          mov_creditos.fecha         GT wfec_Avcto AND /* DATE("06/10/2007") AND /*Fecha de Corte*/*/                                                    */
/*         (mov_creditos.cod_credito   EQ 570        OR mov_creditos.cod_credito   EQ 870) AND                                                             */
/*          mov_creditos.num_credito   EQ facturacion.num_credito AND                                                                                      */
/*          mov_creditos.nit           EQ facturacion.nit                                                                                                  */
/*          NO-LOCK NO-ERROR.                                                                                                                              */
/*     /*IF cod_operacion = 030303001 THEN NEXT.*/                                                                                                         */
/*     IF AVAILABLE(mov_creditos) THEN DO:                                                                                                                 */
/*        ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Cédula : " + mov_creditos.Nit + " - Credito : " + STRING(mov_creditos.num_credito).           */
/*        ASSIGN Msaje.                                                                                                                                    */
/*        FIND FIRST Creditos WHERE (Creditos.cod_credito EQ 570 OR Creditos.cod_credito EQ 870) AND                                                       */
/*                                   Creditos.Nit         EQ Facturacion.Nit                     AND                                                       */
/*                                   Creditos.num_credito EQ Facturacion.num_credito             AND                                                       */
/*                                   Creditos.Fec_UltPago GT wfec_Avcto NO-LOCK NO-ERROR.                                                                  */
/*        IF AVAILABLE (Creditos) THEN DO:                                                                                                                 */
/*            FIND Temcreditos WHERE                                                                                                                       */
/*                 Temcreditos.TNit     EQ mov_creditos.Nit           AND                                                                                  */
/*                 Temcreditos.TNumcred EQ mov_creditos.num_credito   AND                                                                                  */
/*                 Temcreditos.TCodoper EQ mov_creditos.Cod_operacion NO-ERROR.                                                                            */
/*            IF NOT AVAILABLE(Temcreditos) THEN DO:                                                                                                       */
/*               CREATE Temcreditos.                                                                                                                       */
/*               ASSIGN TAgencia   = mov_creditos.Agencia                                                                                                  */
/*                      TNit       = mov_creditos.Nit                                                                                                      */
/*                      TNumcred   = mov_creditos.num_credito                                                                                              */
/*                      TCodoper   = mov_creditos.cod_operacion                                                                                            */
/*                      TValcheque = mov_creditos.Val_Cheque                                                                                               */
/*                      TValefecti = mov_creditos.Val_Efectivo                                                                                             */
/*                      TFecha     = creditos.Fec_UltPago                                                                                                  */
/*                      Tcpte      = mov_creditos.cpte.                                                                                                    */
/*               ASSIGN totcred = totcred + 1.                                                                                                             */
/*              END.                                                                                                                                       */
/*        END.                                                                                                                                             */
/*      END.                                                                                                                                               */
/* END.                                                                                                                                                    */
/* /* Nuevo 10012008*/                                                                                                                                     */
/* FIND FIRST Temcreditos NO-ERROR.                                                                                                                        */
/* IF AVAILABLE(Temcreditos)THEN                                                                                                                           */
/*    ASSIGN vlgenpagos = TRUE.                                                                                                                            */
/* ELSE                                                                                                                                                    */
/*    ASSIGN vlgenpagos = FALSE.                                                                                                                           */
/*                                                                                                                                                         */
/* IF vlgenpagos THEN DO:                                                                                                                                  */
/*     Listado = W_PathSpl + "PagosFacturacion-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".csv".               */
/*     OS-DELETE VALUE(Listado).                                                                                                                           */
/*     OUTPUT TO value(Listado).                                                                                                                           */
/*     PUT "Agencia;Nit;Credito;Operacion;Val_Cheque;Val_Efec;Fecha;cpte" SKIP(0).                                                                         */
/*                                                                                                                                                         */
/*     FOR EACH Temcreditos BY TAgencia BY TNit BY TCodoper:                                                                                               */
/*         PUT TAgencia   ";"                                                                                                                              */
/*             TNit       ";"                                                                                                                              */
/*             TNumcred   ";"                                                                                                                              */
/*             TCodoper   ";"                                                                                                                              */
/*             TValcheque FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                                 */
/*             TValefecti FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                                 */
/*             TFecha    ";"                                                                                                                               */
/*             Tcpte     SKIP(0).                                                                                                                          */
/*     END.                                                                                                                                                */
/*     MESSAGE "El archivo plano esta en " listado                                                                                                         */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                              */
/*     ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".                                                                                                */
/*     ASSIGN msaje.                                                                                                                                       */
/*     OUTPUT CLOSE.                                                                                                                                       */
/* END.                                                                                                                                                    */
/* ELSE                                                                                                                                                    */
/*    MESSAGE "No Encontro Registro Para Actualizar.. Ok."                                                                                                 */
/*        VIEW-AS ALERT-BOX.                                                                                                                               */
/* /****/                                                                                                                                                  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaPagosTotal wWin 
PROCEDURE ValidaPagosTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE vdfecha AS DATE INITIAL ? NO-UNDO.

ASSIGN wfec_Aproxpago  = ?
       wfec_Ainic      = ?
       wfec_Avcto      = ?.

ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Seleccionando Registros... Espere Por Favor.".
ASSIGN Msaje.

FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE(per_facturacion) THEN DO:
   ASSIGN wfec_Aproxpago  = per_facturacion.fec_limpago
          wfec_Ainic      = per_facturacion.fec_inicial
          wfec_Avcto      = per_facturacion.fec_final.
   MESSAGE "Periodo Actual : " per_facturacion.per_factura  SKIP
           "Fecha Corte    : " STRING(wfec_Avcto,"99/99/9999") 
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
   MESSAGE "Proceso Cancelado. Comuníquese con el Administrado"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

FOR EACH mov_creditos WHERE 
    (mov_creditos.cod_credito EQ 570 OR mov_creditos.cod_credito EQ 870) AND 
    mov_creditos.fecha        GT wfec_Avcto
    NO-LOCK BREAK BY mov_creditos.num_credito BY mov_creditos.fecha:

    ASSIGN wvlrHon   = 0  wvlrRet = 0  wvlrAbok = 0  wvlrAboi  = 0  wvlrAboim = 0
           wvlrAboSC = 0.

    IF FIRST-OF (mov_creditos.num_credito) THEN DO:
       ASSIGN vdfecha   = ?
              wtotabok  = 0 
              wtotret   = 0 
              wtothon   = 0 
              wtotAboi  = 0 
              wtotAboim = 0
              wtotpol   = 0.
       ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "  Cédula : " + mov_creditos.Nit + " - Credito : " + STRING(mov_creditos.num_credito).
       ASSIGN Msaje.
    END.

    CASE mov_creditos.cod_operacion :
        WHEN 20101007 THEN  DO:  /* Abono a Honorarios */
             wvlrHon = (mov_creditos.val_efectivo + mov_creditos.val_cheque).
             vdfecha = mov_creditos.fecha.
        END.
        WHEN 20102001 THEN    /* Retiro */
             wvlrRet = (mov_creditos.val_efectivo + mov_creditos.val_cheque). 
        WHEN 20101001 THEN DO:  /* Abono A Capital */
             wvlrAbok = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             vdfecha = mov_creditos.fecha.
        END.
        WHEN 20101003 OR WHEN 20102006 THEN DO: /* Abono a Int. Corrientes y Cargo de Int. Corrientes  */
             wvlrAboi  = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             vdfecha = mov_creditos.fecha.
        END.
        WHEN 20101002 OR WHEN 20101004 THEN DO: /* Abono a Int. Mora   para la fact 2 tenerlo  en cuenta 
                                                al comparar con el pago minimo */
             wvlrAboiM = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             vdfecha = mov_creditos.fecha.
        END.
        WHEN 20101006 THEN DO: /* Abono de polizas/ Seguro de cartera */
             wvlrAboSC = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             vdfecha = mov_creditos.fecha.
        END.

    END CASE.
    ASSIGN wtotabok  = wtotabok  + wvlrabok   wtotret  = wtotret + wvlrret
           wtotaboi  = wtotaboi  + wvlrAboi   wtothon  = wtothon + wvlrhon
           wtotaboim = wtotaboim + wvlrAboim  wtotpol  = wtotpol + wvlrAboSC.

    IF LAST-OF(mov_creditos.num_credito) THEN DO:  /*AND LAST-OF (mov_creditos.fecha) THEN DO:*/
       FIND Temcreditos WHERE 
            Temcreditos.TNit     EQ mov_creditos.Nit           AND 
            Temcreditos.TNumcred EQ mov_creditos.num_credito   
            NO-ERROR.
       IF NOT AVAILABLE(Temcreditos) THEN DO:
          CREATE Temcreditos.
          ASSIGN Temcreditos.TAgencia   = mov_creditos.Agencia                              
                 Temcreditos.TNit       = mov_creditos.Nit                                  
                 Temcreditos.TNumcred   = mov_creditos.num_credito   
                 Temcreditos.TCodoper   = mov_creditos.cod_credito
                 Temcreditos.TFecha     = vdfecha /*mov_creditos.fecha    /*creditos.Fec_UltPago*/    */
                 Temcreditos.Tcpte      = mov_creditos.cpte                                 
                 Temcreditos.TRecAboK   = wtotabok                                          
                 Temcreditos.TRecAboIC  = wtotaboi                                          
                 Temcreditos.TRecAboIM  = wtotaboim                                         
                 Temcreditos.TRecHonora = wtothon                                           
                 Temcreditos.TRecSegCar = wtotpol.                                          
       END.                                                                     
    END.
END.    

FOR EACH Temcreditos WHERE
    Temcreditos.TRecAboK   = 0 AND
    Temcreditos.TRecAboIC  = 0 AND
    Temcreditos.TRecAboIM  = 0 AND
    Temcreditos.TRecHonora = 0 AND
    Temcreditos.TRecSegCar = 0:
    DELETE Temcreditos.
END.


/* /* Nuevo 03-Marzo-2008*/                                                                                                                  */
/* FIND FIRST Temcreditos NO-ERROR.                                                                                                          */
/* IF AVAILABLE(Temcreditos)THEN                                                                                                             */
/*    ASSIGN vlgenpagos = TRUE.                                                                                                              */
/* ELSE                                                                                                                                      */
/*    ASSIGN vlgenpagos = FALSE.                                                                                                             */
/*                                                                                                                                           */
/* IF vlgenpagos THEN DO:                                                                                                                    */
/*     Listado = W_PathSpl + "PagosFacturacion-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".csv". */
/*     OS-DELETE VALUE(Listado).                                                                                                             */
/*     OUTPUT TO value(Listado).                                                                                                             */
/*     PUT "Agencia;Nit;Credito;Cod.Credito;Fecha;Cpte;Rec_Capital;Rec_Int.Ctes;Rec_Int.Mora;Rec_Honora;Rec_SegCar;Actualizado" SKIP(0).     */
/*                                                                                                                                           */
/*     FOR EACH Temcreditos BY TAgencia BY TNumcred:                                                                                         */
/*         PUT TAgencia   ";"                                                                                                                */
/*             TNit       ";"                                                                                                                */
/*             TNumcred   ";"                                                                                                                */
/*             TCodoper   ";"                                                                                                                */
/*             TFecha     FORMAT "99/99/9999" ";"                                                                                            */
/*             Tcpte      ";"                                                                                                                */
/*             TRecAboK   FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                   */
/*             TRecAboIC  FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                   */
/*             TRecAboIM  FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                   */
/*             TRecHonora FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                   */
/*             TRecSegCar FORMAT "->>>,>>>,>>>,>>9.99" ";"                                                                                   */
/*             TActualiza                                                                                                                    */
/*             SKIP(0).                                                                                                                      */
/*     END.                                                                                                                                  */
/*     MESSAGE "El archivo plano esta en " listado                                                                                           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                */
/*     ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = " ".                                                                                  */
/*     ASSIGN msaje.                                                                                                                         */
/*     OUTPUT CLOSE.                                                                                                                         */
/* END.                                                                                                                                      */
/* ELSE                                                                                                                                      */
/*    MESSAGE "No Encontro Registro Para Actualizar.. Ok."                                                                                   */
/*        VIEW-AS ALERT-BOX.                                                                                                                 */
/* /****/                                                                                                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

