&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
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

    {incluido/Variable.i "SHARED"}.


    DEFINE VARIABLE vcArchivo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ProcName AS CHARACTER FORMAT "x(80)".
    DEFINE VARIABLE Archivo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcDptCasa AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCiuCasa AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcDptEmpr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcCiuEmpr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcEmpresa AS CHARACTER   NO-UNDO.
    

    DEFINE TEMP-TABLE TTImp
        FIELD c1    AS CHARACTER FORMAT "X(320)".

    DEFINE TEMP-TABLE TTMarcarGMF
        FIELD   nit     AS CHARACTER    FORMAT "X(15)"
        FIELD   nombres AS CHARACTER    FORMAT "X60)"
        FIELD   descr   AS CHARACTER    FORMAT "X(30)".

    DEFINE TEMP-TABLE TTExcentos
  /*       FIELD tpIdent     AS  INTEGER     COLUMN-LABEL "Tp. Ident" */
        FIELD nit             LIKE    clientes.nit
        FIELD nombre      AS  CHARACTER   COLUMN-LABEL "Nombre Tercero"
        FIELD mensaje     AS  CHARACTER   COLUMN-LABEL "Mensaje"
        FIELD entidad     AS  CHARACTER   COLUMN-LABEL "Entidad"
        FIELD fecini      AS  DATE        COLUMN-LABEL "Ini.Excencion"
        FIELD fecfin      AS  DATE        COLUMN-LABEL "Fin.Excencion"
        FIELD Nagencia    AS  CHARACTER   COLUMN-LABEL "Agencia"
        FIELD Cue_Ahorros     LIKE    ahorros.Cue_Ahorros
        FIELD Fec_Ape         LIKE    ahorros.Fec_Apertura
        FIELD dirCasa     AS  CHARACTER   COLUMN-LABEL "Dirección Casa"
        FIELD telCasa     AS  CHARACTER   COLUMN-LABEL "Teléfono Casa"
        FIELD ciuCasa     AS  CHARACTER   COLUMN-LABEL "Ciudad Casa"
        FIELD dptCasa     AS  CHARACTER   COLUMN-LABEL "Depto. Casa"
        FIELD empresa     AS  CHARACTER   COLUMN-LABEL "Empresa"
        FIELD dirEmpr     AS  CHARACTER   COLUMN-LABEL "Dirección Empresa"
        FIELD telEmpr     AS  CHARACTER   COLUMN-LABEL "Teléfono Empresa"
        FIELD ciuEmpr     AS  CHARACTER   COLUMN-LABEL "Ciudad Empresa"
        FIELD dptEmpr     AS  CHARACTER   COLUMN-LABEL "Depto. Empresa".


    DEFINE TEMP-TABLE TTGMF
        FIELD tpReg       AS  INTEGER     /*Tipo Registro*/   
        FIELD agencia         LIKE    Agencias.agencia
        FIELD tpIdent     AS  INTEGER     COLUMN-LABEL "Tp. Ident"
        FIELD nit             LIKE    clientes.nit
        FIELD nombre      AS  CHARACTER   COLUMN-LABEL "Nombre Tercero"
        FIELD Cue_Ahorros     LIKE    ahorros.Cue_Ahorros
        FIELD Fec_Ape         LIKE    ahorros.Fec_Apertura
        FIELD Fec_Ter         LIKE    Ahorros.Fec_Vencimiento
        FIELD tpCta       AS  INTEGER     /* Tipo de Cuenta */
        FIELD estCta      AS  INTEGER    /* Estado Cuenta  */
        FIELD fecCor          LIKE    ahorros.Fec_Apertura    /* Fecha de corte*/
        FIELD ctaExe      AS INTEGER                          /* Cta de ahorros Exenta GMF*/
        FIELD fecini      AS  DATE   COLUMN-LABEL "Ini.Excencion"
        FIELD fecfin      AS  DATE   COLUMN-LABEL "Fin.Excencion"
        FIELD plzCDT      AS INTEGER                          /* Plazo CDT */
        FIELD NReCDT      AS INTEGER                          /* Número de Renovaciones CDT */
        FIELD dirCasa     AS  CHARACTER   COLUMN-LABEL "Dirección Casa"
        FIELD telCasa     AS  CHARACTER   COLUMN-LABEL "Teléfono Casa"
        FIELD ciuCasa     AS  CHARACTER   COLUMN-LABEL "Ciudad Casa"
        FIELD dptCasa     AS  CHARACTER   COLUMN-LABEL "Depto. Casa"
        FIELD empresa     AS  CHARACTER   COLUMN-LABEL "Empresa"
        FIELD dirEmpr     AS  CHARACTER   COLUMN-LABEL "Dirección Empresa"
        FIELD telEmpr     AS  CHARACTER   COLUMN-LABEL "Teléfono Empresa"
        FIELD ciuEmpr     AS  CHARACTER   COLUMN-LABEL "Ciudad Empresa"
        FIELD dptEmpr     AS  CHARACTER   COLUMN-LABEL "Depto. Empresa"
        FIELD obs         AS  CHARACTER   COLUMN-LABEL "Observaciones".

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-321 RECT-322 RECT-323 RS-Opcion ~
BUTTON-1 BTN-GenPlano Btn_Salir Btn_Ayuda 
&Scoped-Define DISPLAYED-OBJECTS ED-Instruc RS-Opcion F-Descr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-GenPlano 
     LABEL "Procesar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE ED-Instruc AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 83 BY 4
     BGCOLOR 0 FGCOLOR 15 FONT 9 NO-UNDO.

DEFINE VARIABLE F-Descr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY .81
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE RS-Opcion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Generar plano para CIFIN", 1,
"Marcar cuentas en SFG", 2
     SIZE 20 BY 5.12 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 5.65.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.08.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 4.58.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 16.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     ED-Instruc AT ROW 3.5 COL 7.14 NO-LABEL WIDGET-ID 14
     RS-Opcion AT ROW 9.62 COL 18.57 NO-LABEL WIDGET-ID 20
     BUTTON-1 AT ROW 9.62 COL 86 WIDGET-ID 28
     BTN-GenPlano AT ROW 11.77 COL 86 WIDGET-ID 10
     Btn_Salir AT ROW 13.65 COL 86 WIDGET-ID 50
     F-Descr AT ROW 16.08 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     Btn_Ayuda AT ROW 16.08 COL 89 WIDGET-ID 30
     "Seleccione Opción" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.73 COL 17.86 WIDGET-ID 26
          FONT 1
     "MARCADO CUENTAS EXCENTAS GMF" VIEW-AS TEXT
          SIZE 35 BY .5 AT ROW 1.92 COL 31.14 WIDGET-ID 18
          FONT 9
     RECT-1 AT ROW 9.35 COL 17.14 WIDGET-ID 24
     RECT-321 AT ROW 1.62 COL 29.57 WIDGET-ID 52
     RECT-322 AT ROW 3.23 COL 6.14 WIDGET-ID 54
     RECT-323 AT ROW 1 COL 1 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.72 BY 17
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
         TITLE              = "Marcado Cuentas Exentas GMF"
         HEIGHT             = 17
         WIDTH              = 96.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR ED-Instruc IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       ED-Instruc:AUTO-INDENT IN FRAME fMain      = TRUE.

/* SETTINGS FOR FILL-IN F-Descr IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Marcado Cuentas Exentas GMF */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Marcado Cuentas Exentas GMF */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-GenPlano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-GenPlano wWin
ON CHOOSE OF BTN-GenPlano IN FRAME fMain /* Procesar */
DO:
    ASSIGN RS-Opcion.
    CASE RS-Opcion:
        WHEN 1 THEN DO:
            SYSTEM-DIALOG GET-FILE procname
               TITLE      "Escoja el archivo que contiene los Nits ..."
               FILTERS    "Archivos Delimitados ';' (*.csv)"   "*.csv",
                          "Archivos Texto (*.txt)"   "*.txt"
               INITIAL-DIR "C:\"
               MUST-EXIST
               USE-FILENAME.

            IF Procname NE "" THEN DO: 
                ASSIGN Archivo = ProcName.
                RUN importar.
                RUN actualizarGMF.
                RUN actualizar.
    /*             RUN exportarCSV. */
                RUN ExportarCIFIN.
                RUN listaErrores.
            END.                
            ELSE
                MESSAGE "Proceso Cancelado"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Cancelado".

            MESSAGE "Proceso terminado"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        WHEN 2 THEN DO:
            MESSAGE "Para marcar las cuentas como excentas de GMF en SFG," SKIP
                "debe tener como fuente de datos el archivo de confirmación" SKIP
                "de cuentas marcadas en CIFIN." SKIP
                ""SKIP
                "Desea Continuar?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Marcar excentos GMF en SFG"
                UPDATE pregunta AS LOGICAL.
            IF pregunta THEN DO:
                SYSTEM-DIALOG GET-FILE vcArchivo
                    TITLE      "Escoja archivo Enviado por CIFIN"
                    FILTERS    "Archivos GMF (*exn*)"   "*exn*",
                            "Archivos GMF "   "*GMF*"
                    MUST-EXIST
                    USE-FILENAME.

                IF vcArchivo NE "" THEN 
                    RUN marcarGMF.
                ELSE
                    MESSAGE "Proceso Cancelado"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Cancelado".

                MESSAGE "Proceso terminado"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.
        OTHERWISE DO:
            MESSAGE "Seleccione una opción"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY":U TO RS-Opcion.
            RETURN NO-APPLY.
        END.

    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda wWin
ON CHOOSE OF Btn_Ayuda IN FRAME fMain /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "AYUDAS\tesoreri" CONTEXT 25.
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN W-InfDia.w NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizar wWin 
PROCEDURE actualizar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH TTExcentos NO-LOCK:
        ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Actualizando Excentos --> Nit: " + STRING(TTExcentos.nit).
        FIND FIRST ahorros WHERE ahorros.nit EQ TTExcentos.nit AND 
            Ahorros.Tip_ahorro EQ 1 AND
            Ahorros.Cod_ahorro EQ 3 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            FIND FIRST clientes WHERE clientes.nit EQ ahorros.nit NO-LOCK NO-ERROR.
            FIND FIRST agencias WHERE agencias.agencia EQ clientes.agencia NO-LOCK NO-ERROR.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_Residencia,1,2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR. /*Depto Casa*/
            ASSIGN vcDptCasa = ubicacion.nombre.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_Residencia,1,2) AND ubicacion.tipo EQ "C" NO-LOCK NO-ERROR. /*Ciudad Casa*/
            ASSIGN vcCiuCasa = ubicacion.nombre.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_comercial,1,2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR. /*Depto Comercial*/
            ASSIGN vcDptEmpr = ubicacion.nombre.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_comercial,1,2) AND ubicacion.tipo EQ "C" NO-LOCK NO-ERROR. /*Ciudad Comercial*/
            ASSIGN vcCiuEmpr = ubicacion.nombre.
            FIND FIRST empresas WHERE empresas.Cod_Empresa EQ clientes.Cod_Empresa NO-LOCK NO-ERROR.
            IF AVAILABLE(empresas) THEN 
                ASSIGN vcEmpresa = empresas.Alias_Empresa.
            ELSE
                ASSIGN vcEmpresa = "".
    
            UPDATE  TTExcentos.Cue_Ahorros  = ahorros.Cue_Ahorros 
                    TTExcentos.Fec_Ape      = ahorros.Fec_Apertura
                    TTExcentos.Nagencia     = agencias.nombre
                    TTExcentos.dirCasa      = clientes.Dir_Residencia  
                    TTExcentos.telCasa      = clientes.Tel_Residencia
                    TTExcentos.ciuCasa      = vcCiuCasa
                    TTExcentos.dptCasa      = vcDptCasa
                    TTExcentos.empresa      = vcEmpresa
                    TTExcentos.dirEmpr      = Dir_comercial
                    TTExcentos.telEmpr      = Tel_comercial
                    TTExcentos.ciuEmpr      = vcCiuEmpr
                    TTExcentos.dptEmpr      = vcDptEmpr.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizarGMF wWin 
PROCEDURE actualizarGMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH TTGMF NO-LOCK:
        ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Actualizando archivo plano --> Nit: " + STRING(TTGMF.nit).
        FIND FIRST ahorros WHERE ahorros.nit EQ TTGMF.nit AND 
            Ahorros.Tip_ahorro EQ 1 AND
            Ahorros.Cod_ahorro EQ 3 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            FIND FIRST clientes WHERE clientes.nit EQ ahorros.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN  DO:
                UPDATE
                    TTGMF.agencia       = clientes.agencia
                    TTGMF.nombre        = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
                CASE clientes.Tipo_Identificacion:
                    WHEN "C.C" THEN UPDATE TTGMF.tpIdent = 1.
                    WHEN "NIT" THEN UPDATE TTGMF.tpIdent = 2.
                    WHEN "C.E" THEN UPDATE TTGMF.tpIdent = 3.
                    WHEN "T.I" THEN UPDATE TTGMF.tpIdent = 4.
                    WHEN "P.P" THEN UPDATE TTGMF.tpIdent = 5.
                    WHEN "R.C" THEN UPDATE TTGMF.tpIdent = 9.
                END CASE.
                FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_Residencia,1,2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR. /*Depto Casa*/
                ASSIGN vcDptCasa = ubicacion.nombre.
                FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_Residencia,1,2) AND ubicacion.tipo EQ "C" NO-LOCK NO-ERROR. /*Ciudad Casa*/
                ASSIGN vcCiuCasa = ubicacion.nombre.
                FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_comercial,1,2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR. /*Depto Comercial*/
                ASSIGN vcDptEmpr = ubicacion.nombre.
                FIND FIRST ubicacion WHERE SUBSTRING(ubicacion,1,2) EQ SUBSTRING(Lugar_comercial,1,2) AND ubicacion.tipo EQ "C" NO-LOCK NO-ERROR. /*Ciudad Comercial*/
                ASSIGN vcCiuEmpr = ubicacion.nombre.
                FIND FIRST empresas WHERE empresas.Cod_Empresa EQ clientes.Cod_Empresa NO-LOCK NO-ERROR.
                IF AVAILABLE(empresas) THEN 
                    ASSIGN vcEmpresa = empresas.Alias_Empresa.
                ELSE
                    ASSIGN vcEmpresa = "".

                UPDATE
                    TTGMF.tpCta         = 3
                    TTGMF.Cue_Ahorros   = ahorros.Cue_Ahorros 
                    TTGMF.Fec_Ape       = ahorros.Fec_Apertura
                    TTGMF.dirCasa       = clientes.Dir_Residencia  
                    TTGMF.telCasa       = clientes.Tel_Residencia
                    TTGMF.ciuCasa       = vcCiuCasa
                    TTGMF.dptCasa       = vcDptCasa
                    TTGMF.empresa       = vcEmpresa
                    TTGMF.dirEmpr       = Dir_comercial
                    TTGMF.telEmpr       = Tel_comercial
                    TTGMF.ciuEmpr       = vcCiuEmpr
                    TTGMF.dptEmpr       = vcDptEmpr
                    TTGMF.Fec_Ter       = Ahorros.Fec_Vencimient
                    TTGMF.estCta        = 1                     
                    TTGMF.fecCor        = TODAY                 
                    TTGMF.ctaExe        = 1                     
                    TTGMF.fecini        = TODAY  
                    TTGMF.fecfin        = ?                     
                    TTGMF.plzCDT        = 0                     
                    TTGMF.NReCDT        = 0.

            END. /*IF AVAILABLE clientes*/
            ELSE
                UPDATE TTGMF.obs = TTGMF.obs + "IDENT. NO ENCONTRADA - ".
        END. /*IF AVAILABLE ahorros*/
        ELSE DO:
            UPDATE TTGMF.obs = TTGMF.obs + " " + "NO TIENE PROD. AHORROS - ".
            FIND FIRST clientes WHERE clientes.nit EQ TTGMF.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN 
                UPDATE   TTGMF.nombre = clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.nombre.
            ELSE
                UPDATE TTGMF.obs = TTGMF.obs + "IDENT. NO ENCONTRADA - ".            
        END.

    END. /*FOR EACH TTGMF */

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
  DISPLAY ED-Instruc RS-Opcion F-Descr 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-321 RECT-322 RECT-323 RS-Opcion BUTTON-1 BTN-GenPlano 
         Btn_Salir Btn_Ayuda 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarCIFIN wWin 
PROCEDURE exportarCIFIN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE vcRegT1     AS CHARACTER   FORMAT "X(480)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vcRegT2     AS CHARACTER   FORMAT "X(480)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vcRegT9     AS CHARACTER   FORMAT "X(480)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE vdaFecCor   AS DATE  INITIAL TODAY NO-UNDO.
    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.


    ASSIGN vcRegT1  =   "1" + /*Tipo de Registro */
                        "22" + /*Tipo de Producto 22 = GMF */
                        "050" + /* Tip Entidad Asignado por CIFIN = 50 */
                        "0067" + /* Cod. Entidad asignado por CIFIN = 67 */
                        TRIM(STRING(YEAR(vdaFecCor),"9999")) + TRIM(STRING(MONTH(vdaFecCor),"99")) + TRIM(STRING(DAY(vdaFecCor),"99")) + 
                        STRING("3XRPAD","X(15)") + /*Clave de reporte para cargue de informacion asobancaria asignado por CIFIN*/
                        FILL(" ",447).
                       


    OUTPUT TO "C:\Info_juriscoop\GMFPlanoCIFIN.txt".
    /*Registro Tipo 1*/
    PUT vcRegT1 SKIP.

    FOR EACH TTGMF WHERE TTGMF.obs EQ "" NO-LOCK:
        ASSIGN viCnt = viCnt + 1
            F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Creando archivo Plano - Nit: " + STRING(TTGMF.nit) + " Registro número: " + TRIM(STRING(viCnt)) .
        ASSIGN 
            vcRegT2 =   TRIM(STRING(TTGMF.tpReg,"9"))           + 
                        TRIM(STRING(TTGMF.agencia,"999999"))    + 
                        TRIM(STRING(TTGMF.tpIdent,"99"))        +
                        TRIM(STRING(INTEGER(TTGMF.nit),"999999999999999")) +
                        STRING(nombre,"x(60)") + 
                        STRING(Cue_Ahorros,"X(30)") + 
                        TRIM(STRING(YEAR(Fec_Ape),"9999")) + TRIM(STRING(MONTH(Fec_Ape),"99")) + TRIM(STRING(DAY(Fec_Ape),"99")).
        ASSIGN 
            vcRegT2 =   IF Fec_Ter EQ ? THEN 
                            vcRegT2 + FILL(" ",8) 
                        ELSE 
                            vcRegT2 + TRIM(STRING(YEAR(Fec_Ter),"9999")) + TRIM(STRING(MONTH(Fec_Ter),"99")) + TRIM(STRING(DAY(Fec_Ter),"99")).
        ASSIGN 
            vcRegT2 =   vcRegT2 +               
                        TRIM(STRING(TTGMF.tpCta,"99")) + 
                        TRIM(STRING(TTGMF.estCta,"99")) +
                        TRIM(STRING(YEAR(fecCor),"9999")) + TRIM(STRING(MONTH(fecCor),"99")) + TRIM(STRING(DAY(fecCor),"99")) +
                        TRIM(STRING(TTGMF.ctaExe,"99")).
        ASSIGN
            vcRegT2 =   IF fecini EQ ? THEN
                           vcRegT2 + FILL(" ",8)
                        ELSE
                           vcRegT2 + TRIM(STRING(YEAR(fecini),"9999")) + TRIM(STRING(MONTH(fecini),"99")) + TRIM(STRING(DAY(fecini),"99")).
        ASSIGN
            vcRegT2 =   IF fecfin EQ ? THEN
                           vcRegT2 + FILL(" ",8)
                        ELSE
                           vcRegT2 + TRIM(STRING(YEAR(fecfin),"9999")) + TRIM(STRING(MONTH(fecfin),"99")) + TRIM(STRING(DAY(fecfin),"99")).
        ASSIGN
            vcRegT2 =   vcRegT2 +
                        TRIM(STRING(TTGMF.plzCDT,"99")) +
                        TRIM(STRING(TTGMF.NReCDT,"99")) +
                        STRING(TTGMF.dirCasa,"X(60)") + 
                        STRING(TTGMF.telCasa,"X(12)") +
                        STRING(TTGMF.ciuCasa,"X(20)") +
                        STRING(TTGMF.dptCasa,"X(20)") +
                        STRING(TTGMF.empresa,"X(60)") +
                        STRING(TTGMF.dirEmpr,"X(60)") +
                        STRING(TTGMF.telEmpr,"X(12)") +
                        STRING(TTGMF.ciuEmpr,"X(20)") +
                        STRING(TTGMF.dptEmpr,"X(20)") +
                        FILL(" ",32).

        PUT vcRegT2 SKIP.

    END.

    ASSIGN vcRegT9  =   "9" + /* Tipo de Registro*/
                        TRIM(STRING(viCnt,"9999999999")) + 
                        FILL(" ",469).
    PUT vcRegT9 SKIP.
    OUTPUT CLOSE.
    MESSAGE "Se ha generado plano para" SKIP
            "Marcado cuentas GMF." SKIP
            "C:\Info_juriscoop\GMFPlanoCIFIN.txt"
        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Generado Plano CIFIN".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarCSV wWin 
PROCEDURE exportarCSV :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    MESSAGE "Seleccione el archivo a Generar."
        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Generar archivo".

      SYSTEM-DIALOG GET-FILE procname
         TITLE      "Guardar Archivo como"
         FILTERS    "Archivos Delimitados por ';' (*.csv)"   "*.csv"
/*         MUST-EXIST */
          DEFAULT-EXTENSION "csv"
          SAVE-AS
        USE-FILENAME.
 
      IF Procname NE "" THEN Archivo = ProcName.

    OUTPUT TO VALUE(procname).
    PUT "tpIdent ;nit ;nombre ;mensaje ;entidad ;fecini ;fecfin ;agencia ;Cue_Ahorros ;Fec_Ape ;dirCasa ;telCasa ;ciuCasa ;dptCasa ;empresa ;dirEmpr ;telEmpr ;ciuEmpr ;dptEmpr"
        SKIP.
    FOR EACH TTExcentos NO-LOCK:
        EXPORT DELIMITER ";"
/*         PUT */
            nit              /* ";"  */
            nombre           /* ";"  */
            mensaje          /* ";"  */
            entidad          /* ";"  */
            fecini           /* ";"  */
            fecfin           /* ";"  */
            Nagencia         /* ";"  */
            Cue_Ahorros      /* ";"  */
            Fec_Ape          /* ";"  */
            dirCasa          /* ";"  */
            telCasa          /* ";"  */
            ciuCasa          /* ";"  */
            dptCasa          /* ";"  */
            empresa          /* ";"  */
            dirEmpr          /* ";"  */
            telEmpr          /* ";"  */
            ciuEmpr          /* ";"  */
            dptEmpr
            SKIP.
    END.

    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importar wWin 
PROCEDURE importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    INPUT FROM VALUE(Archivo). 
    REPEAT:
        CREATE TTExcentos.
        IMPORT DELIMITER ";" nit /*nombre mensaje entidad fecini fecfin */ NO-ERROR.
        UPDATE  fecini = ? 
                fecfin = ?.
    END.
    INPUT CLOSE.

    FOR EACH TTExcentos NO-LOCK:
        CREATE TTGMF.
        BUFFER-COPY TTExcentos TO TTGMF.
        UPDATE  TTGMF.Fec_Ape = ?
                TTGMF.Fec_Ter = ?
                TTGMF.fecCor  = ?
                TTGMF.tpReg   = 2.
        ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Subiendo Plano - Nit: " + STRING(TTExcentos.nit).
    END.

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

  ASSIGN ED-Instruc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
      "El  plano de  marcado  de cuentas excentas  con  el formato " + 
      "requerido por 'CIFIN', debe tener como fuente un archivo " + 
      "tipo *.txt o *.csv, 'Únicamente' con la lista de las Cédulas. " + 
      "Para  marcar las cuentas exentas de  GMF en SFG, debe tener " + 
      "como fuente de datos el archivo plano devuelto por CIFIN, " + 
      "donde relacionan las cuentas que marcaron como exentas (*exn.txt).".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listaErrores wWin 
PROCEDURE listaErrores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.
    OUTPUT TO "C:\Info_juriscoop\ErroresPlanoGMF.txt".
    FOR EACH TTGMF WHERE obs NE "" NO-LOCK:
        ASSIGN viCnt = viCnt + 1
            F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Creando Lista de Errores - Nit: " + STRING(TTGMF.nit) + " Registro número: " + TRIM(STRING(viCnt)) .
        FORM
            nit     COLUMN-LABEL "Cliente"          FORMAT "X(15)"
            nombre  COLUMN-LABEL "Nombre Cliente"   FORMAT "X(40)"
            obs     COLUMN-LABEL "Inconsistencias"  FORMAT "X(100)"
            WITH FRAME a DOWN COLUMN 1 WIDTH 160
                NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

        DISPLAY
             nit    
             nombre 
             obs    
            WITH FRAME a.
            DOWN WITH FRAME a.        
    END.
    OUTPUT CLOSE.
    IF viCnt NE 0 THEN DO:
        MESSAGE "Se presentaron " viCnt "errores en la generación" SKIP
                "del plano a CIFIN para marcar GMF." SKIP
                "Por favor revise archivo de errores" SKIP
                "C:\Info_juriscoop\ErroresPlanoGMF.txt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "lista de errores".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE marcarGMF wWin 
PROCEDURE marcarGMF :
/*------------------------------------------------------------------------------
  Purpose:    Marcar en ahorros las cuentas excentas de GMF, según el plano enviado por CIFIN
              donde indica que ya han marcado las cuentas
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


INPUT FROM VALUE(vcArchivo). 
REPEAT:
    CREATE TTImp.
    IMPORT DELIMITER ";" c1 /*nombre mensaje entidad fecini fecfin */ NO-ERROR.
END.
INPUT CLOSE.

FOR EACH TTImp NO-LOCK:
    IF SUBSTRING(TTImp.c1,27,1) EQ "2" THEN DO:
        CREATE TTMarcarGMF.
        UPDATE TTMarcarGMF.nit = TRIM(STRING(INTEGER(SUBSTRING(TTImp.c1,32,15)))).
            TTMarcarGMF.descr = SUBSTRING(TTImp.c1,163,30).
        FIND FIRST clientes WHERE clientes.nit EQ TTMarcarGMF.nit NO-LOCK NO-ERROR.
        IF AVAILABLE (Clientes) THEN DO:
            UPDATE TTMarcarGMF.nombre = clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.nombre.
        END.
        ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Importando Archivo de CIFIN - exn - NIT: " + TTMarcarGMF.nit.
    END.
END.

DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.
ASSIGN viCnt = 0.
FOR EACH TTMarcarGMF NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit EQ TTMarcarGMF.nit AND 
            Ahorros.Tip_ahorro EQ 1 AND
            Ahorros.Cod_ahorro EQ 3 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        IF NOT ahorros.Exento_3xm THEN DO:
            UPDATE ahorros.Exento_3xm = TRUE.
            ASSIGN viCnt = viCnt + 1.
            ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Marcando cuentas excentas. --> NIT: " + TTMarcarGMF.nit.
        END.
    END.
END.  

ASSIGN F-Descr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Marcado de cuentas terminado".

MESSAGE "Se han marcado " viCnt "Cuentas" SKIP
    "Como exentas de cobro GMF."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

