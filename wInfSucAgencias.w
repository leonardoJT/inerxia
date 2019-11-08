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

/*     {incluido/Variable.i "SHARED"}. */
/*     {incluido/Varcon.i "SHARED"}.   */

    DEFINE VARIABLE W_Usuario AS CHARACTER INITIAL "339" NO-UNDO.
    DEFINE VARIABLE W_Agencia AS INTEGER  INITIAL 1 NO-UNDO.
    DEFINE VARIABLE W_Entidad AS INTEGER INITIAL 1 NO-UNDO.

    /**********************************************************************/
    DEFINE VARIABLE vcFiltro AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vlCCosto         AS LOGICAL INITIAL YES.
    DEFINE VARIABLE vlOk              AS LOGICAL.
    DEFINE VARIABLE vcAgeIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcAgeFin AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCenIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCenFin AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcComIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcComFin AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCtaIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCtaFin AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNitIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNitFin AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcUsuIni AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcUsuFin AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE W_Naturaleza AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE W_CtrNat AS CHARACTER   NO-UNDO.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CuadroFechas W_CuadroLimite W_CuadroOtros ~
CMB-Agencia CMB-CentroCosto CMB-Comprob F-UsuIni Btn_Informacion F-UsuFin ~
F-FecIni F-FecFin F-CtaIni F-CtaFin F-NitIni F-NitFin Btn_Imprimir ~
Btn_Salir F-Base F-Porcentaje Btn_Ayuda CMB-Nivel 
&Scoped-Define DISPLAYED-OBJECTS CMB-Agencia CMB-CentroCosto CMB-Comprob ~
F-UsuIni F-NmbreUsu1 F-UsuFin F-NmbreUsu2 F-FecIni F-FecFin F-CtaIni ~
F-NmbreCta1 F-CtaFin F-NmbreCta2 F-NitIni F-NomCli1 F-NitFin F-NomCli2 ~
F-Base F-Porcentaje CMB-Nivel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Btn_Ayuda" 
     SIZE 4 BY 1.08
     FONT 4.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62
     FONT 8.

DEFINE BUTTON Btn_Informacion 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Información" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE CMB-Agencia AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 53 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE CMB-CentroCosto AS CHARACTER FORMAT "X(50)":U 
     LABEL "Centros de Costos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 53 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE CMB-Comprob AS CHARACTER FORMAT "X(50)":U 
     LABEL "Comprobantes" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 53 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE CMB-Nivel AS INTEGER FORMAT "9":U INITIAL 8 
     LABEL "Nivel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8" 
     DROP-DOWN-LIST
     SIZE 6 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F-Base AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor de la Base" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CtaFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Final" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CtaIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Trabajo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NitFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Final" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NitIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NmbreCta1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NmbreCta2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NmbreUsu1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NmbreUsu2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NomCli1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NomCli2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Porcentaje AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Porcentaje" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-UsuFin LIKE Usuarios.Usuario
     LABEL "Usuario Final" 
     VIEW-AS FILL-IN 
     SIZE 8.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-UsuIni LIKE Usuarios.Usuario
     LABEL "Usuario Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE W_CuadroFechas
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 2.

DEFINE RECTANGLE W_CuadroLimite
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 6.

DEFINE RECTANGLE W_CuadroOtros
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     CMB-Agencia AT ROW 1.27 COL 26 COLON-ALIGNED HELP
          "Seleccione la Agencia" WIDGET-ID 74
     CMB-CentroCosto AT ROW 2.35 COL 26 COLON-ALIGNED HELP
          "Seleccione el Centro de Costos" WIDGET-ID 76
     CMB-Comprob AT ROW 3.38 COL 26 COLON-ALIGNED HELP
          "Seleccione el Comprobante" WIDGET-ID 78
     F-UsuIni AT ROW 4.5 COL 26 COLON-ALIGNED HELP
          "Cédula del usuario que se habilita para el ingreso al sistema" WIDGET-ID 84
          LABEL "Usuario Inicial" FORMAT "X(4)"
          BGCOLOR 15 
     F-NmbreUsu1 AT ROW 4.5 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     Btn_Informacion AT ROW 4.5 COL 102 WIDGET-ID 68
     F-UsuFin AT ROW 5.58 COL 26 COLON-ALIGNED HELP
          "Cédula del usuario que se habilita para el ingreso al sistema" WIDGET-ID 90
          LABEL "Usuario Final" FORMAT "X(4)"
          BGCOLOR 15 
     F-NmbreUsu2 AT ROW 5.58 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     F-FecIni AT ROW 8 COL 40 COLON-ALIGNED HELP
          "Digite Fecha Inicial" WIDGET-ID 92
     F-FecFin AT ROW 8 COL 68 COLON-ALIGNED HELP
          "Digite Fecha Final" WIDGET-ID 94
     F-CtaIni AT ROW 12.04 COL 31 COLON-ALIGNED HELP
          "Cuenta Inicial" WIDGET-ID 102
     F-NmbreCta1 AT ROW 12.04 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     F-CtaFin AT ROW 13.12 COL 31 COLON-ALIGNED HELP
          "Cuenta Final" WIDGET-ID 106
     F-NmbreCta2 AT ROW 13.12 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     F-NitIni AT ROW 14.19 COL 31 COLON-ALIGNED HELP
          "Nit Inicial" WIDGET-ID 112
     F-NomCli1 AT ROW 14.19 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     F-NitFin AT ROW 15.27 COL 31 COLON-ALIGNED HELP
          "Nit Final" WIDGET-ID 110
     F-NomCli2 AT ROW 15.27 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     Btn_Imprimir AT ROW 15.81 COL 102 WIDGET-ID 70
     Btn_Salir AT ROW 18 COL 102 WIDGET-ID 50
     F-Base AT ROW 18.5 COL 62 COLON-ALIGNED HELP
          "Valor Base" WIDGET-ID 118
     F-Porcentaje AT ROW 19.58 COL 62 COLON-ALIGNED HELP
          "Porcentaje" WIDGET-ID 120
     Btn_Ayuda AT ROW 20 COL 105 WIDGET-ID 72
     CMB-Nivel AT ROW 20.65 COL 62 COLON-ALIGNED HELP
          "Seleccione el Nivel" WIDGET-ID 82
     " Otros Parámetros" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 17.69 COL 19 WIDGET-ID 100
          FGCOLOR 7 FONT 5
     " Límites de Cuentas y NIT'S" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 10.42 COL 18 WIDGET-ID 98
          FGCOLOR 7 FONT 5
     " Escoja la fecha del Informe" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 7.19 COL 19 WIDGET-ID 96
          FGCOLOR 7 FONT 5
     W_CuadroFechas AT ROW 7.5 COL 17 WIDGET-ID 2
     W_CuadroLimite AT ROW 10.88 COL 17 WIDGET-ID 4
     W_CuadroOtros AT ROW 17.96 COL 17 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.14 BY 22.5
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 22.5
         WIDTH              = 114.14
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
/* SETTINGS FOR FILL-IN F-NmbreCta1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NmbreCta2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NmbreUsu1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NmbreUsu2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NomCli1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NomCli2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-UsuFin IN FRAME fMain
   LIKE = bdcentral.Usuarios.Usuario EXP-LABEL EXP-FORMAT EXP-HELP EXP-SIZE */
/* SETTINGS FOR FILL-IN F-UsuIni IN FRAME fMain
   LIKE = bdcentral.Usuarios.Usuario EXP-LABEL EXP-FORMAT EXP-HELP EXP-SIZE */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME fMain /* Imprimir */
DO:
    DEFINE VARIABLE vlOk AS LOGICAL INITIAL FALSE NO-UNDO.
    RUN ValidaParametros(INPUT-OUTPUT vlok).
    IF vlOk THEN  DO:
        RUN wimprime.w ("prMultiSucAgencias.p", "Sucursales y Agencias",
                        vcAgeIni,
                        vcAgeFin,
                        vcCenIni,
                        vcCenFin,
                        vcComIni, /* 20 Campos Char o Numéricos*/
                        vcComFin,
                        vcCtaIni,
                        vcCtaFin,
                        vcNitIni,
                        vcNitFin,
                        vcUsuIni,
                        vcUsuFin,
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        DATE(F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}), /* 8 Campos Fechas - 4 Fechas*/
                        DATE(F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                        ?, ?,
                        ?, ?,
                        ?, ?,
                        ?, ?,  /* 4 Campos Lógicos*/
                        ?, ?).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Informacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Informacion wWin
ON CHOOSE OF Btn_Informacion IN FRAME fMain /* Información */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-Agencia wWin
ON VALUE-CHANGED OF CMB-Agencia IN FRAME fMain /* Agencia */
DO:
    IF vlCCosto THEN DO:
       CMB-CentroCosto:LIST-ITEMS = "".
       vlOk = CMB-CentroCosto:ADD-LAST("000 - Todos los Centros de Costo").
       FOR EACH Cen_Costos WHERE Cen_Costos.Agencia EQ W_Agencia NO-LOCK:
          vlOk = CMB-CentroCosto:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
       END.
       CMB-CentroCosto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "000 - Todos los Centros de Costo".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-Nivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-Nivel wWin
ON LEAVE OF CMB-Nivel IN FRAME fMain /* Nivel */
DO:
    APPLY 'entry' TO Btn_Imprimir.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CtaFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaFin wWin
ON LEAVE OF F-CtaFin IN FRAME fMain /* Cuenta Final */
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
       ASSIGN SELF:SCREEN-VALUE = "99999999999999"
              F-NmbreCta2:SCREEN-VALUE = "Todos las Cuentas".
    ELSE DO:
       FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAILABLE Cuentas THEN
          ASSIGN F-NmbreCta2:SCREEN-VALUE = Cuentas.Nombre.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaFin wWin
ON MOUSE-SELECT-DBLCLICK OF F-CtaFin IN FRAME fMain /* Cuenta Final */
DO:
    DEFINE VAR vcCta LIKE Cuentas.Cuenta.
    DEFINE VAR vcNom LIKE Cuentas.Nombre.
    IF SELF:SCREEN-VALUE EQ "" THEN
       RUN Busca_Cuenta (INPUT SELF:SCREEN-VALUE, OUTPUT vcCta, OUTPUT vcNom).
    ASSIGN SELF:SCREEN-VALUE = vcCta
           F-NmbreCta2:SCREEN-VALUE = vcNom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CtaIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaIni wWin
ON LEAVE OF F-CtaIni IN FRAME fMain /* Cuenta Inicial */
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
       ASSIGN SELF:SCREEN-VALUE = "0"
              F-NmbreCta1:SCREEN-VALUE = "Todas las Cuentas".
    ELSE DO:
       FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAILABLE Cuentas THEN
          ASSIGN F-NmbreCta1:SCREEN-VALUE = Cuentas.Nombre.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CtaIni wWin
ON MOUSE-SELECT-DBLCLICK OF F-CtaIni IN FRAME fMain /* Cuenta Inicial */
DO:
    DEFINE VAR vcCta LIKE Cuentas.Cuenta.
    DEFINE VAR vcNom LIKE Cuentas.Nombre.
    IF SELF:SCREEN-VALUE EQ "" THEN
       RUN Busca_Cuenta (INPUT SELF:SCREEN-VALUE, OUTPUT vcCta, OUTPUT vcNom).
    ASSIGN SELF:SCREEN-VALUE = vcCta
           F-NmbreCta1:SCREEN-VALUE = vcNom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-NitFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-NitFin wWin
ON LEAVE OF F-NitFin IN FRAME fMain /* Nit Final */
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
       ASSIGN SELF:SCREEN-VALUE = "ZZZZZZZZZZZZ"
              F-NomCli2:SCREEN-VALUE = "Todas los Nits".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-UsuFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-UsuFin wWin
ON LEAVE OF F-UsuFin IN FRAME fMain /* Usuario Final */
DO:
    IF F-UsuFin:SCREEN-VALUE NE "999" THEN DO:
       FIND Usuarios WHERE Usuarios.Agencia EQ integer(SUBSTRING(CMB-Agencia:SCREEN-VALUE,1,3)) AND
                           Usuarios.Usuario EQ F-UsuFin:SCREEN-VALUE NO-LOCK NO-ERROR.
       F-NmbreUsu2:SCREEN-VALUE = "Hasta este Numero".
       IF AVAILABLE(Usuarios) THEN
          F-NmbreUsu2:SCREEN-VALUE = Usuarios.Nombre.
    END.
    ELSE F-NmbreUsu2:SCREEN-VALUE = "Consolidado".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-UsuIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-UsuIni wWin
ON LEAVE OF F-UsuIni IN FRAME fMain /* Usuario Inicial */
DO:
    IF F-UsuIni:SCREEN-VALUE NE "0" THEN DO:
       FIND Usuarios WHERE Usuarios.Agencia EQ integer(SUBSTRING(CMB-Agencia:SCREEN-VALUE,1,3)) AND
                           Usuarios.Usuario EQ F-UsuIni:SCREEN-VALUE NO-LOCK NO-ERROR.
       F-NmbreUsu1:SCREEN-VALUE = "A partir de este Numero".
       IF AVAILABLE(Usuarios) THEN
          F-NmbreUsu1:SCREEN-VALUE = Usuarios.Nombre.
    END.
    ELSE F-NmbreUsu1:SCREEN-VALUE = "Consolidado".
END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_Cuenta wWin 
PROCEDURE Busca_Cuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER T_ConsCtai LIKE Cuentas.Cuenta. 
    DEFINE OUTPUT PARAMETER T_ConsCta  LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsNom  LIKE Cuentas.Nombre.
    IF T_ConsCtai NE "" THEN DO:
       FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCtai AND
                          Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(Cuentas) THEN
          ASSIGN T_ConsCta = Cuentas.Cuenta
                 T_ConsNom = Cuentas.Nombre.
    END.
    IF T_ConsCta NE "" THEN DO:
       RUN C-Cuentas.r (OUTPUT T_ConsCta, OUTPUT T_ConsNom, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "T").
       IF T_ConsCta EQ ? THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
          IF NOT AVAILABLE(Cuentas) THEN
            ASSIGN T_ConsCta  = ""
                   T_ConsNom  = "".           
       END.
    END.

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
  DISPLAY CMB-Agencia CMB-CentroCosto CMB-Comprob F-UsuIni F-NmbreUsu1 F-UsuFin 
          F-NmbreUsu2 F-FecIni F-FecFin F-CtaIni F-NmbreCta1 F-CtaFin 
          F-NmbreCta2 F-NitIni F-NomCli1 F-NitFin F-NomCli2 F-Base F-Porcentaje 
          CMB-Nivel 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE W_CuadroFechas W_CuadroLimite W_CuadroOtros CMB-Agencia 
         CMB-CentroCosto CMB-Comprob F-UsuIni Btn_Informacion F-UsuFin F-FecIni 
         F-FecFin F-CtaIni F-CtaFin F-NitIni F-NitFin Btn_Imprimir Btn_Salir 
         F-Base F-Porcentaje Btn_Ayuda CMB-Nivel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HabilitarDesabilitar wWin 
PROCEDURE HabilitarDesabilitar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DISABLE
        CMB-CentroCosto
/*         F-UsuIni */
/*         F-UsuFin */
/*         F-CtaIni */
/*         F-CtaFin */
/*         F-NitIni */
/*         F-NitFin */
        F-Base
        F-Porcentaje
        CMB-Nivel
        WITH FRAME {&FRAME-NAME}.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IniciaVariables wWin 
PROCEDURE IniciaVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
            F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
                                       
/*************************************/
    vlOk = CMB-Agencia:ADD-LAST("000 - Todas las Agencias").
    FOR EACH Agencias NO-LOCK:
      vlOk = CMB-Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
      IF W_Agencia EQ Agencias.Agencia THEN
         CMB-Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
    END.
    vlOk = CMB-Agencia:ADD-LAST("999 - Consolidado x Agencia").
        
    FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE(Entidad) THEN DO:
       IF NOT Entidad.Id_CenCosto THEN ASSIGN vlCCosto = NO.
    END.
    ELSE MESSAGE "No encontro entidad" VIEW-AS ALERT-BOX.
    ASSIGN vlOk = CMB-CentroCosto:ADD-LAST("000 - Entidad No Maneja Cen.Costos")
           CMB-CentroCosto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "000 - Entidad No Maneja Cen.Costos".
    IF vlCCosto THEN DO:
       vlOk = CMB-CentroCosto:ADD-LAST("000 - Todos los Centros de Costo").
       FOR EACH Cen_Costos WHERE Cen_Costos.Agencia EQ W_Agencia NO-LOCK:
          vlOk = CMB-CentroCosto:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
       END.
       CMB-CentroCosto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "000 - Todos los Centros de Costo".
    END.

    vlOk = CMB-Comprob:ADD-LAST("00 - Todos los Comprobantes").
    FOR EACH Comprobantes WHERE Comprobantes.Estado EQ 1 NO-LOCK:
        vlOk = CMB-Comprob:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
    END.
    CMB-Comprob:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00 - Todos los Comprobantes".

    ASSIGN F-UsuIni:SCREEN-VALUE = STRING(0)
           F-UsuFin:SCREEN-VALUE = STRING(999)
           F-NmbreUsu1:SCREEN-VALUE = "Todos los Usuarios"
           F-NmbreUsu2:SCREEN-VALUE = "Todos los Usuarios"
           F-FecIni:SCREEN-VALUE = STRING(TODAY)
           F-FecFin:SCREEN-VALUE = STRING(TODAY)
           CMB-Nivel:SCREEN-VALUE = STRING(8).

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

  /* Code placed here will execute AFTER standard behavior.    */

    RUN IniciaVariables.
    RUN HabilitarDesabilitar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validaParametros wWin 
PROCEDURE validaParametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER vlok AS LOGICAL.
    /* Centro de Costos*/
    ASSIGN vcCenIni = "0"
           vcCenFin = "999".
   /* Agencias*/
    IF SUBSTRING(CMB-Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3) NE "000" THEN 
        ASSIGN vcAgeIni = TRIM(SUBSTRING(CMB-Agencia:SCREEN-VALUE,1,3))
               vcAgeFin = TRIM(SUBSTRING(CMB-Agencia:SCREEN-VALUE,1,3)).
    ELSE
        ASSIGN vcAgeIni = "0"
               vcAgeFin = "999".
    /* Comprobantes*/
    IF SUBSTRING(CMB-Comprob:SCREEN-VALUE,1,2) NE "00" THEN 
        ASSIGN vcComIni = TRIM(SUBSTRING(CMB-Comprob:SCREEN-VALUE,1,2))
               vcComFin = TRIM(SUBSTRING(CMB-Comprob:SCREEN-VALUE,1,2)).
    ELSE
        ASSIGN vcComIni = "0"
               vcComFin = "999".
    /* Cuentas Contables*/
    IF (F-CtaIni:SCREEN-VALUE EQ ""  AND
        F-CtaFin:SCREEN-VALUE EQ "") OR 
       (F-CtaIni:SCREEN-VALUE EQ "0" AND 
        F-CtaFin:SCREEN-VALUE EQ "") THEN
       ASSIGN vcCtaIni = "0"
              vcCtaFin = "99999999999999".
    ELSE
       ASSIGN vcCtaIni = SUBSTRING(F-CtaIni:SCREEN-VALUE,1,14)
               vcCtaFin = SUBSTRING(F-CtaFin:SCREEN-VALUE,1,14).
    /* Nits*/
    IF (F-NitIni:SCREEN-VALUE EQ "" AND
        F-NitFin:SCREEN-VALUE EQ "ZZZZZZZZZZZZ") OR
       (F-NitIni:SCREEN-VALUE EQ "" AND
        F-NitIni:SCREEN-VALUE EQ "") THEN
       ASSIGN vcNitIni = "0"
              vcNitFin = "99999999999999".
    ELSE
       ASSIGN vcNitIni = SUBSTRING(F-NitIni:SCREEN-VALUE,1,12)
              vcNitFin = SUBSTRING(F-NitFin:SCREEN-VALUE,1,12).
    /* Usuarios*/
    ASSIGN vcUsuIni = SUBSTRING(F-UsuIni:SCREEN-VALUE,1,4)
           vcUsuFin = SUBSTRING(F-UsuFin:SCREEN-VALUE,1,4).
    /* Validacion a Fechas */
    IF F-FecIni:SENSITIVE THEN DO:
        IF STRING(F-FecIni:SCREEN-VALUE) EQ "" THEN DO:
            MESSAGE "Digite la fecha Inicial"
                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Datos Incompletos".
            APPLY "ENTRY":U TO F-FecIni.
            RETURN NO-APPLY.
        END.
    END.
    IF F-FecFin:SENSITIVE THEN DO:
        IF STRING(F-FecFin:SCREEN-VALUE) EQ "" THEN DO:
            MESSAGE "Digite la fecha Final"
                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Datos Incompletos".
            APPLY "ENTRY":U TO F-FecFin.
            RETURN NO-APPLY.
        END.
    END.
    IF DATE(F-FecIni:SCREEN-VALUE) > DATE(F-FecFin:SCREEN-VALUE) THEN DO:
        MESSAGE "Fecha Final menor a Fecha Inicial"
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Revise las fechas".
        APPLY "ENTRY":U TO F-FecIni.
        RETURN NO-APPLY.
    END.

    /**************************************/

    ASSIGN vlOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

