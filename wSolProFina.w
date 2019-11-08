&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME NowWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS NowWin 
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
DEF VAR cDtosSlctud AS CHAR NO-UNDO.
{INCLUIDO\VARIABLE.I "SHARED"}

DEF TEMP-TABLE tSlctud NO-UNDO
    FIELD tpo AS INTEGER 
    FIELD iPgna AS INTEGER
    FIELD iLnea AS INTEGER
    FIELD cTxto AS CHAR
    FIELD cVriables AS CHAR
    FIELD coor AS CHAR
    
    INDEX pk iPgna iLnea.
DEF BUFFER btSlctud FOR tSlctud.

DEF VAR GHPARENT AS HANDLE NO-UNDO.
DEF VAR htCrdtos AS HANDLE NO-UNDO.
DEF VAR hClientes AS HANDLE NO-UNDO.
DEF VAR hAnxosClientes AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tCrdtos NO-UNDO
    FIELD agencia AS CHAR
    FIELD CIUDAD AS CHAR
    FIELD CptcionClccion AS CHAR
    FIELD Cuota AS CHAR
    FIELD dstncion AS CHAR
    FIELD Fec_UltActualiza AS CHAR
    FIELD FrmaPgo AS CHAR
    FIELD Grntia AS CHAR
    FIELD Linea AS CHAR
    FIELD Monto AS CHAR
    FIELD NombreAgencia AS CHAR
    FIELD NombreCiudad AS CHAR
    FIELD OtroPrdctoSlctar AS CHAR
    FIELD OtroTpoCliente AS CHAR
    FIELD Plazo AS CHAR
    FIELD PrdctoSlctar AS CHAR
    FIELD reestrctrcion AS CHAR
    FIELD TpoCliente AS CHAR.

DEF VAR tTpoCliente AS CHAR NO-UNDO EXTENT 4.
tTpoCliente[1] = "Otro".
tTpoCliente[2] = "Solicitante".
tTpoCliente[3] = "Codeudor".
tTpoCliente[4] = "Avalista".
DEF VAR tPrdctoSlctar AS CHAR NO-UNDO EXTENT 7.
    tPrdctoSlctar[1] = "Otro".
    tPrdctoSlctar[2] = "Crédito".
    tPrdctoSlctar[3] = "CDAT".
    tPrdctoSlctar[4] = "CDT".
    tPrdctoSlctar[5] = "Ahorro Programado".
    tPrdctoSlctar[6] = "Ahorro Permanente".
    tPrdctoSlctar[7] = "Ahorro Vista".

DEF VAR tTposVvienda AS CHAR NO-UNDO EXTENT 4.
tTposVvienda[1] = "Propia".
tTposVvienda[2] = "Arrendada".
tTposVvienda[3] = "Familiar".
tTposVvienda[4] = "Asignada Emp".

DEF VAR tDirCrrspndncia AS CHAR NO-UNDO EXTENT 3.
tDirCrrspndncia[1] = "Residencia".
tDirCrrspndncia[2] = "Oficina".   
tDirCrrspndncia[3] = "A.A.".

DEF BUFFER BCnygue FOR clientes.    
DEF BUFFER BCnygueAnxos FOR anexos_clientes.    

    DEF VAR cln AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS NitCliente RECT-326 BUTTON-1 BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS NitCliente cNombreCliente 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fciiu NowWin 
FUNCTION fciiu RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCntra NowWin 
FUNCTION fCntra RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCoordndas NowWin 
FUNCTION fCoordndas RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEmprsa NowWin 
FUNCTION fEmprsa RETURNS CHARACTER
  (iCdgo AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstrctra NowWin 
FUNCTION fEstrctra RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFrmto NowWin 
FUNCTION fFrmto RETURNS CHARACTER
  (c AS CHAR,l AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNCiudad NowWin 
FUNCTION fNCiudad RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNmbreUsuario NowWin 
FUNCTION fNmbreUsuario RETURNS CHAR(cCdgo AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTTalActvos NowWin 
FUNCTION fTTalActvos RETURNS DECIMAL
  (cnit AS char /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fttalpsvos NowWin 
FUNCTION fttalpsvos RETURNS DECIMAL
  (cnit AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVr NowWin 
FUNCTION fVr RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVrios NowWin 
FUNCTION fVrios RETURNS CHARACTER
  (iTpo AS INTEGER,icdgo AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR NowWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dsolprofina AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsolprofinawi AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U NO-FOCUS
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE cPgna1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 30 BY 4 NO-UNDO.

DEFINE VARIABLE cNombreCliente AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE NitCliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Nit Cliente" NO-UNDO.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 13 BY 8.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Consulta AT ROW 8.54 COL 116 WIDGET-ID 4
     NitCliente AT ROW 1 COL 3 COLON-ALIGNED WIDGET-ID 10
     cNombreCliente AT ROW 1 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     BUTTON-1 AT ROW 2.38 COL 116 WIDGET-ID 6
     BUTTON-2 AT ROW 8.54 COL 116 WIDGET-ID 8
     cPgna1 AT ROW 7.19 COL 1 NO-LABEL WIDGET-ID 14
     RECT-326 AT ROW 2.08 COL 115 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127 BY 10.19
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW NowWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SOLICITUD DE PRODUCTOS FINANCIEROS Y ACTUALIZACION DE DATOS"
         HEIGHT             = 10.19
         WIDTH              = 127
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB NowWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW NowWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       FRAME fMain:PRIVATE-DATA     = 
                "00) Identificación Cliente".

/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Consulta:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN cNombreCliente IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR cPgna1 IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cPgna1:HIDDEN IN FRAME fMain           = TRUE
       cPgna1:PRIVATE-DATA IN FRAME fMain     = 
                "JURISCOOP                                                                                           PERSONAS NATURALES[]
PROGRESO SOLIDARIO                                         SOLICITUD DE PRODUCTOS FINANCIEROS Y ACTUALIZACION DE DATOS[]
NIT.860.075.780-9                                                                                 www.juriscoop.com.co[]
[]
Seccional: _______________________________ Ciudad: ___________________________________  Fecha de Entrega: ___________ [tCrdtos.NombreAgencia,tCrdtos.NombreCiudad,clientes.Fec_UltActualiza]
[]
                                             1. DATOS DE LA SOLICITUD[]
[]
Tipo de Cliente: ___________________  Otro: ______________________________________  Producto a Solicitar: ___________ [tCrdtos.TpoCliente,tCrdtos.CptcionClccion,tCrdtos.CptcionClccion]
Producto a Solicitar: ____________________________________________________________________________  OTRO: ___________ [tCrdtos.PrdctoSlctar,tCrdtos.OtroPrdctoSlctar]
Crédito Personal: Monto $________________                 Plazo: _________________              Garantía: ___________ [tCrdtos.Monto,tCrdtos.Plazo,tCrdtos.Grntia]
Línea: _______________________________________ Reestructuración: _________________________ Forma de Pago: ___________ [tCrdtos.Linea,tCrdtos.reestrctrcion,tCrdtos.FrmaPgo]
Destino Específico del Crédito: ____________________________________________________________________________________  [tCrdtos.dstncion]
[]
                                             2. INFORMACION PERSONAL[]
Primer Apellido:            Segundo Apellido:            Primer Nombre:                 Segundo Nombre:               []
________________________    _________________________    _________________________      _______________________       [Clientes.Apellido1,Clientes.Apellido2,Clientes.Nombre]
Tipo De Documento De Identidad:              Número De Documento De Identidad:      Lugar Y Fecha De Expedición:      []
______________________________               ________________________________       __________________ ______________ [Clientes.Tipo_Identificacion,clientes.nit,Clientes.Lugar_expedicion,Clientes.Fec_expedicion]
Lugar Y Fecha De Nacimiento:                 Nacionalidad:                          Sexo:[]
______________________________________       ________________________________       _________________________________ []
Estado Civil:                                Número De Hijos Por Edades:  0-10     11-18     Mayores 18[]
_________________________________                                         ____     _____             ____[]                                                                                      
Nivel de Educación:                          Profesión:                             Número de Personas a Cargo[]
_________________________________            _________________________________      _______[]
Dirección Residencia:                        Estrato:                               Barrio:
_________________________________            _________________________________      ___________________________[]
Ciudad y Departamento de Residencia:       Teléfono de Residencia:     Teléfono Celular:       Correo Electrónico:[]
___________________________________        ______________________      _________________       _____________________[]
Clase De Vivienda:                           Valor del Arriendo:                    Permanencia:[]
___________________________________          ___________________                    ____Años ____Meses[]
Envio de Correspondencia: _________________                            A.A.: ____________________________[]
[]
                                  Información del Cónyuge o Compañero(a) permanente[]".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(NowWin)
THEN NowWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME NowWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NowWin NowWin
ON END-ERROR OF NowWin /* SOLICITUD DE PRODUCTOS FINANCIEROS Y ACTUALIZACION DE DATOS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NowWin NowWin
ON WINDOW-CLOSE OF NowWin /* SOLICITUD DE PRODUCTOS FINANCIEROS Y ACTUALIZACION DE DATOS */
DO:
    IF NOT GHPARENT = ? 
    THEN DO:
        RUN pActvaVntna IN GHPARENT.
        RUN hideObject IN THIS-PROCEDURE.    
        RETURN NO-APPLY.
    END.
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta NowWin
ON CHOOSE OF Btn_Consulta IN FRAME fMain /* Button 3 */
DO:
/*                                                                                                                                  */
/*     DO WITH FRAME F_Clientes:                                                                                                    */
/*         ASSIGN WWin:SENSITIVE = FALSE.                                                                                           */
/*         RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).            */
/*         ASSIGN WWin:SENSITIVE = TRUE.                                                                                            */
/*         WWin:MOVE-TO-TOP().                                                                                                      */
/*                                                                                                                                  */
/*         IF P_Nit NE ""                                                                                                           */
/*         THEN DO:                                                                                                                 */
/*             FIND Clientes                                                                                                        */
/*                 WHERE                                                                                                            */
/*                     Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit                                                       */
/*                 AND Clientes.Tipo_Vinculo LT 3 NO-ERROR.                                                                         */
/*             IF AVAILABLE(Clientes)                                                                                               */
/*             THEN DO:                                                                                                             */
/*                 Wk_Edad = (DECIMAL(TODAY) - DECIMAL(Clientes.Fec_Nacimiento)) / 365.                                             */
/*                 IF Clientes.Tipo_Cliente EQ 2 AND Wk_Edad GE 18                                                                  */
/*                 THEN DO:                                                                                                         */
/*                     MESSAGE "El Cliente esta identificado como menor de edad" SKIP                                               */
/*                             "sin embargo por fecha de nacimiento ya es mayor de edad" SKIP                                       */
/*                             "Favor solicitar actualizar documento de identificación." SKIP                                       */
/*                     VIEW-AS ALERT-BOX ERROR.                                                                                     */
/*                 END.                                                                                                             */
/*                 RUN Mostrar_Cliente.                                                                                             */
/*                 FIND Agencias WHERE Agencias.Agencia EQ P_AgeCli NO-LOCK NO-ERROR.                                               */
/*                 IF AVAILABLE(Agencias) THEN Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre. */
/*             END.                                                                                                                 */
/*             ELSE DO:                                                                                                             */
/*                 MESSAGE "Este persona o empresa no es un cliente de" SKIP                                                        */
/*                         "la cooperativa." SKIP(1)                                                                                */
/*                         "Si se desea cambiar su estado a cliente. deberá" SKIP                                                   */
/*                         "hacerse como un ingreso normal" VIEW-AS ALERT-BOX INFORMATION TITLE "Persona No vinculada".             */
/*             END.                                                                                                                 */
/*         END.                                                                                                                     */
/*     END.                                                                                                                         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 NowWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 NowWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    RUN onChoose IN h_dyntoolbar("save").
    DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.
    Listado = W_Pathspl + "Lst_Relaciones.lst".
    {incluido/imprimir.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK NowWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

ON 'leave','return' OF  NitCliente
DO:
    ASSIGN {&SELF-NAME}.
    IF NOT CAN-FIND(FIRST clientes WHERE clientes.nit = SELF:SCREEN-VALUE) 
    THEN DO:
        MESSAGE "ERROR: Nit No Existe"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
    FIND FIRST clientes NO-LOCK
        WHERE
            clientes.nit = SELF:SCREEN-VALUE NO-ERROR.
    DO WITH FRAME {&FRAME-NAME}:
        cNombreCliente:SCREEN-VALUE = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
    END.
    IF NOT CAN-FIND(FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit)
    THEN DO:
        CREATE anexos_clientes.
        ASSIGN  anexos_clientes.nit = clientes.nit.
        RELEASE anexos_clientes.
    END.
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects NowWin  _ADM-CREATE-OBJECTS
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
             INPUT  'dsolprofina.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsolprofinaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsolprofina ).
       RUN repositionObject IN h_dsolprofina ( 1.00 , 72.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vsolprofinawi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsolprofinawi ).
       RUN repositionObject IN h_vsolprofinawi ( 2.08 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 8.42 , 114.29 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHideyesDisabledActionsFlatButtonsyesMenunoShowBordernoToolbaryesActionGroupsTableioTableIOTypeSaveSupportedLinksTableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsUPDATE,Add,Copy,Delete,UndoChange,Reset,CancelHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 83.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 0.92 , 14.57 ) NO-ERROR.

       /* Links to SmartDataViewer h_vsolprofinawi. */
       RUN addLink ( h_dsolprofina , 'Data':U , h_vsolprofinawi ).
       RUN addLink ( h_vsolprofinawi , 'Update':U , h_dsolprofina ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vsolprofinawi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             cNombreCliente:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_vsolprofinawi ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI NowWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(NowWin)
  THEN DELETE WIDGET NowWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI NowWin  _DEFAULT-ENABLE
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
  DISPLAY NitCliente cNombreCliente 
      WITH FRAME fMain IN WINDOW NowWin.
  ENABLE NitCliente RECT-326 BUTTON-1 BUTTON-2 
      WITH FRAME fMain IN WINDOW NowWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW NowWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject NowWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject NowWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    RUN SUPER.
    /* Code placed here will execute AFTER standard behavior.    */
    DYNAMIC-FUNCTION('fEstrctra':U). /* carga la estructura del formulario */
    /*RUN hideObject IN h_dyntoolbar.*/
    htCrdtos = DYNAMIC-FUNCTION('fHndle':U IN h_dsolprofina).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE o_pFrmlrio NowWin 
PROCEDURE o_pFrmlrio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR cc AS CHAR NO-UNDO.
    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cCmpo AS CHAR NO-UNDO.
    DEF VAR ctbla AS CHAR NO-UNDO.
    DEF VAR ccoor AS CHAR NO-UNDO.
    DEF VAR pi AS INTEGER NO-UNDO.
    DEF VAR lng AS INTEGER NO-UNDO.
    DEF VAR cVlor AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        /*
        ESTRUCTURA DEL MENSAJE 
        AGENCIACIUDADCptcionClccionCuotadstncionFec_UltActualizaFrmaPgoGrntiaLineaMonto
        NombreAgenciaNombreCiudadOtroPrdctoSlctarOtroTpoClientePlazoPrdctoSlctarreestrctrcionTpoCliente
        */
        cDtosSlctud = DYNAMIC-FUNCTION('fEnviaDtos':U IN h_vsolprofinawi).
        htcrdtos = BUFFER tCrdtos:HANDLE.
        CREATE tCrdtos.
        ASSIGN  tCrdtos.AGENCIA           = entry(1,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.CIUDAD            = entry(2,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.CptcionClccion    = entry(3,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Cuota             = entry(4,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.dstncion          = entry(5,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Fec_UltActualiza  = entry(6,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.FrmaPgo           = entry(7,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Grntia            = entry(8,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Linea             = entry(9,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Monto             = entry(10,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.NombreAgencia     = entry(11,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.NombreCiudad      = entry(12,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.OtroPrdctoSlctar  = entry(13,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.OtroTpoCliente    = entry(14,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Plazo             = entry(15,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.PrdctoSlctar      = entry(16,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.reestrctrcion     = entry(17,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.TpoCliente        = entry(18,entry(2,cDtosSlctud,chr(10)),CHR(1)).

        FOR EACH tSlctud
            WHERE
                NOT tSlctud.tpo = 0:
            DELETE tSlctud.
        END. /* FOR EACH tSlctud */
        FOR FIRST clientes NO-LOCK
            WHERE
                clientes.nit = NitCliente:SCREEN-VALUE,
            FIRST anexos_clientes NO-LOCK
                WHERE
                    anexos_clientes.nit = clientes.nit:
            FOR EACH tSlctud 
                WHERE
                    tslctud.tpo = 0
                BY iPgna
                BY iLnea:
                CREATE btSlctud.
                BUFFER-COPY tSlctud TO btSlctud.
                btSlctud.tpo = 1. 
                DO i = 1 TO NUM-ENTRIES(tSlctud.cVriable,","):
                    cc= "".
                    cCmpo = entry(2,ENTRY(i,tSlctud.cVriable,","),".").
                    cTbla = entry(1,ENTRY(i,tSlctud.cVriable,","),".").
                    CASE cTbla:
                        WHEN "tCrdtos" THEN h = htcrdtos.
                        WHEN "clientes" THEN h = BUFFER clientes:HANDLE.
                        WHEN "anexos_clientes" THEN h = BUFFER anexos_clientes:HANDLE.
                    END CASE. /* CASE cTbla: */
                    
                    IF VALID-HANDLE(h)
                    THEN DO:
                        DO j = 1 TO h:num-FIELDS:
                            h1 = h:BUFFER-FIELD(j).
                            IF VALID-HANDLE(h1)
                            THEN DO:
                                IF h1:NAME = cCmpo
                                THEN DO:
                                    ccoor = ENTRY(i,btSlctud.coor,CHR(1)).    
                                    pi = INTEGER(ENTRY(1,ccoor,CHR(2))).
                                    lng = INTEGER(ENTRY(2,ccoor,CHR(2))).
                                    cVlor = FILL(" ",lng).
                                    OVERLAY(cvlor,1,MINIMUM(lng,LENGTH(h1:BUFFER-VALUE))) = IF NOT h1:BUFFER-VALUE = ? THEN h1:BUFFER-VALUE ELSE " ".
                                    OVERLAY(btslctud.ctxto,pi,lng) = cVlor.
                                END.
                            END.
                        END.
                    END. /* IF VALID-HANDLE(h) */
                END. /* DO i = 1 TO NUM-ENTRIES(tSlctud.cVriable,","): */
                PUT UNFORMATTED 
                    btSlctud.cTxto SKIP.
    /*                 tSlctud.cVriables SKIP */
    /*                 tSlctud.coor.          */
            END. /* FOR EACH tSlctud  */
        END. /* FOR FIRST clientes NO-LOCK */
    END. /* DO WITH FRAME {&FRAME-NAME}: */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmlrio NowWin 
PROCEDURE pFrmlrio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR cc AS CHAR NO-UNDO.
    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cCmpo AS CHAR NO-UNDO.
    DEF VAR ctbla AS CHAR NO-UNDO.
    DEF VAR ccoor AS CHAR NO-UNDO.
    DEF VAR pi AS INTEGER NO-UNDO.
    DEF VAR lng AS INTEGER NO-UNDO.
    DEF VAR cVlor AS CHAR NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        /*
        ESTRUCTURA DEL MENSAJE 
        AGENCIACIUDADCptcionClccionCuotadstncionFec_UltActualizaFrmaPgoGrntiaLineaMonto
        NombreAgenciaNombreCiudadOtroPrdctoSlctarOtroTpoClientePlazoPrdctoSlctarreestrctrcionTpoCliente
        */
        cDtosSlctud = DYNAMIC-FUNCTION('fEnviaDtos':U IN h_vsolprofinawi).
        htcrdtos = BUFFER tCrdtos:HANDLE.
        CREATE tCrdtos.
        ASSIGN  tCrdtos.AGENCIA           = entry(1,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.CIUDAD            = entry(2,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.CptcionClccion    = entry(3,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Cuota             = entry(4,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.dstncion          = entry(5,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Fec_UltActualiza  = entry(6,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.FrmaPgo           = entry(7,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Grntia            = entry(8,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Linea             = entry(9,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Monto             = entry(10,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.NombreAgencia     = entry(11,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.NombreCiudad      = entry(12,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.OtroPrdctoSlctar  = entry(13,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.OtroTpoCliente    = entry(14,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.Plazo             = entry(15,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.PrdctoSlctar      = entry(16,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.reestrctrcion     = entry(17,entry(2,cDtosSlctud,chr(10)),CHR(1))
                tCrdtos.TpoCliente        = entry(18,entry(2,cDtosSlctud,chr(10)),CHR(1)).
        FOR FIRST clientes NO-LOCK
            WHERE
                clientes.nit = NitCliente:SCREEN-VALUE,
            FIRST anexos_clientes NO-LOCK
                WHERE
                    anexos_clientes.nit = clientes.nit:

            cln = "".
            cln = fCntra("----- SOLICITUD DE PRODUCTOS FINANCIEROS Y ACTUALIZACION DE DATOS -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("----- USO EXCLUSIVO DE JURISCOOP -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Seccional ".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = STRING(clientes.agencia) + " " + tCrdtos.NombreAgencia.
            OVERLAY(cln,60,18)  = "Ciudad".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,38)  = tCrdtos.CIUDAD + " " + tCrdtos.NombreCiudad.
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "Fecha de Entrega ".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(TODAY,"99/99/9999").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("----- 1. DATOS DE LA SOLICITUD -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Tipo de Cliente".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tTpoCliente[INTEGER(tCrdtos.TpoCliente) + 1].
            OVERLAY(cln,60,18)  = "Otro".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = OtroTpoCliente.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Producto a solicitar".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tCrdtos.CptcionClccion.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Producto a solicitar".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tPrdctoSlctar[integer(tCrdtos.PrdctoSlctar) + 1].
            OVERLAY(cln,60,18)  = "Otro".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = tCrdtos.OtroPrdctoSlctar.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Crédito Personal: Monto".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tCrdtos.Monto.
            OVERLAY(cln,60,18)  = "Plazo".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = tCrdtos.plazo + " MESES".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Garantía".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tCrdtos.grntia.
            PUT UNFORMATTED cln SKIP.
        
            cln = "".
            OVERLAY(cln,1,18)   = "Línea".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tCrdtos.linea.
            OVERLAY(cln,60,18)  = "Reestructuración".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = tCrdtos.reestrctrcion.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Forma De Pago".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tCrdtos.frmapgo.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1)   = "Destino Específico Del Crédito: " + tCrdtos.Dstncion.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            cln = fCntra("----- 2. INFORMACION PERSONAL -----").
            PUT UNFORMATTED cln SKIP SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Primer Apellido".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.Apellido1.
            OVERLAY(cln,60,18)  = "Segundo Apellido".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = Clientes.Apellido2.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Nombres".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.nombre.
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "T. Doc. Identidad".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.Tipo_Identificacion.
            OVERLAY(cln,60,18)  = "Número".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = clientes.nit.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1)   = "L. y F. Expedición : " + fNciudad(clientes.Lugar_expedicion) +
                ", " + (IF Clientes.Fec_expedicion <> ? THEN STRING(Clientes.Fec_expedicion,"99/99/9999") ELSE "").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1)   = "L. y F. Nacimiento : " + fNciudad(clientes.Lugar_Nacimiento) +
                ", " + (IF Clientes.Fec_Nacimiento <> ? THEN STRING(Clientes.Fec_Nacimiento,"99/99/9999") ELSE "").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Nacionalidad".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Anexos_Clientes.Nacionalidad.
            OVERLAY(cln,60,18)  = "Sexo".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82,18)  = IF Clientes.Sexo = 1 THEN "Masculino" ELSE "Femenino".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Estado Civil".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.Est_Civil.
            OVERLAY(cln,60,18)  = "Número de Hijos".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)  = "0-10 " + string(Clientes.Num_Hijos) + " 11-18 " + string(Anexos_Clientes.Num_Hijos_11) +
                                    " Mayores 18 " + string(Anexos_Clientes.Num_Hijos_18).
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Nivel de Educación".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.Niv_Educativo.
            OVERLAY(cln,60,18)  = "Profesión".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = fvrios(1,clientes.cod_profesion).
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Número Personas a Cargo".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = string(Clientes.Per_Acargo).
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Dirección Residencia".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = Clientes.Dir_Residencia.
            OVERLAY(cln,60,18)  = "Estrato".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = string(Clientes.Estrato).
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Barrio".
            OVERLAY(cln,20,1)   = ":".
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Ciudad y Departamento Residencia".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = fNciudad(clientes.Lugar_Residencia).
            OVERLAY(cln,60,18)  = "Teléfono Residencia".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = (IF Anexos_clientes.Ind_Cli <> 0 THEN "(" + STRING(Anexos_clientes.Ind_Cli) + ") " ELSE "") + Clientes.Tel_Residencia.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Celular".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Clientes.Celular.
            OVERLAY(cln,60,18)  = "Correo Electrónico".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = Clientes.Email.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Clase Vivienda".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tTposVvienda[Clientes.Tipo_Vivienda].
            OVERLAY(cln,60,18)  = "Valor Arriendo".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(Anexos_Clientes.Val_arriendo).
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1,18)   = "Permanencia".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22)  = STRING(Anexos_Clientes.Per_Anos) + " Años " + 
                STRING(Anexos_Clientes.Per_Meses) + " Meses".
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "Nombre Arrendador".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = Clientes.Nom_Arrendatario.
            OVERLAY(cln,60,18)  = "Dirección".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = Anexos_Clientes.Dir_Arrendatario.
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "Teléfono".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = (IF Anexos_clientes.Ind_Arr <> 0 THEN "(" + STRING(Anexos_clientes.Ind_Arr) + ") " ELSE "") + Clientes.Tel_Arrendatario.
            PUT UNFORMATTED cln SKIP.
            
            
            cln = "".
            OVERLAY(cln,1)   = "Sí en la vivienda actual lleva menos de dos años, favor indique su vivienda anterior".
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "Dirección".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = Anexos_Clientes.Dir_Ant_Residencia.
            OVERLAY(cln,60,18)  = "Teléfono".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = (IF Anexos_clientes.Ind_VAnt <> 0 THEN "(" + STRING(Anexos_clientes.Ind_VAnt) + ") " ELSE "") + Anexos_Clientes.Tel_Ant_Residencia.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Permanencia:".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22)     = string(Anexos_Clientes.Per_Ant_Anos) + " Años " +
                string(Anexos_Clientes.Per_Ant_Meses) + " Meses".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Envío de Correspondencia".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = tDirCrrspndncia[Anexos_Clientes.Dir_Correspondencia].
            OVERLAY(cln,60,18)  = "No.".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = Anexos_Clientes.AA_Cliente.
            PUT UNFORMATTED cln SKIP.


            cln = "".
            cln = fCntra("----- Información Del Cónyuge o Compañero(a) permanente -----").
            PUT UNFORMATTED cln SKIP SKIP.

            /* INFORMACION CONYUGUE */
            
            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 1 
                AND Relaciones.Descripcion  = "Conyuge" 
                AND Relaciones.Estado = 1,
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion,
                FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit:
                
                cln = "".
                OVERLAY(cln,1,18)   = "Primer Apellido".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Apellido1.
                OVERLAY(cln,60,18)  = "Segundo Apellido".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Apellido2.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombres".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = BCnygue.nombre.
                PUT UNFORMATTED cln SKIP.


                cln = "".
                OVERLAY(cln,1,18)   = "T. De Doc. Identidad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Tipo_Identificacion.
                OVERLAY(cln,60,18)  = "No Documento de Identidad".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.nit.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Sexo".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = IF BCnygue.Sexo = 1 THEN "Masculino" ELSE "Femenino".
                OVERLAY(cln,60,18)  = "Nacionalidad".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygueAnxos.nacionalidad.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1)   = "L. y F. Nacimiento : " + fNciudad(BCnygue.Lugar_Nacimiento) +
                    ", " + (IF BCnygue.Fec_Nacimiento <> ? THEN STRING(BCnygue.Fec_Nacimiento,"99/99/9999") ELSE "").
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nivel de Educación".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Niv_Educativo.
                OVERLAY(cln,60,18)  = "Profesión".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = fvrios(1,BCnygue.cod_profesion).
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Ocupación Principal".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Tipo_Actividad.
                OVERLAY(cln,60,18)  = "Especifique".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygueAnxos.Especifique_Ocupacion.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Cargo Actual/Actividad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = Relaciones.Rel_Cargo_Acti.
                OVERLAY(cln,60,18)  = "Nombre de la Empresa donde Trabaja".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = relaciones.Rel_Nom_Empresa.
                PUT UNFORMATTED cln SKIP.


                cln = "".
                OVERLAY(cln,1,18)   = "Dirección de la Empresa".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Dir_comercial.
                OVERLAY(cln,60,18)  = "Ciudad y Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = fNCiudad(BCnygue.Lugar_comercial).
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.Tel_Comercial.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Total Ingresos".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = string(Relaciones.Tot_Ing_Conyuge,"$>>>,>>>,>>9").
                OVERLAY(cln,60,18)  = "Total Egresos".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = string(Relaciones.Tot_Egr_Conyug,"$>>>,>>>,>>9").
                PUT UNFORMATTED cln SKIP.

            END. /* FOR EACH relaciones NO-LOCK */
            /* INFORMACION CONYUGUE */

            cln = "".
            cln = fCntra("----- Espacio para diligenciar por el representante, si el titular es menor de edad -----").
            PUT UNFORMATTED cln SKIP SKIP.
            RUN pFrmlrio1.
            RUN pFrmlrio2.
            RUN pFrmlrio3.
            RUN pFrmlrio4.
        END. /* FOR FIRST clientes NO-LOCK */
    END. /* DO WITH FRAME {&FRAME-NAME}: */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmlrio1 NowWin 
PROCEDURE pFrmlrio1 :
/*------------------------------------------------------------------------------
  Purpose: pFrmlrio1    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
            /* INFORMACION TUTOR */
            
            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 3 
                AND Relaciones.Estado = 1,
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion,
                FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit:
                
                cln = "".
                OVERLAY(cln,1,18)   = "Primer Apellido".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Apellido1.
                OVERLAY(cln,60,18)  = "Segundo Apellido".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Apellido2.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombres".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = BCnygue.nombre.
                PUT UNFORMATTED cln SKIP.
                
                cln = "".
                OVERLAY(cln,1,18)   = "No Documento de Identidad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.nit.
                OVERLAY(cln,60,18)  = "Dirección Residencia".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Dir_Residencia.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Ciudad". /* esto donde se captura???? */
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = fNciudad(BCnygue.Lugar_Residencia).
                OVERLAY(cln,60,18)  = "Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = "".
                PUT UNFORMATTED cln SKIP.
            
                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.tel_Residencia.
                PUT UNFORMATTED cln SKIP.
            END. /* FOR EACH relaciones NO-LOCK */
            /* INFORMACION TUTOR */

            cln = "".
            cln = fCntra("----- 3. INFORMACION ACTIVIDAD ECONOMICA -----").
            PUT UNFORMATTED cln SKIP SKIP.
            
            cln = "".
            OVERLAY(cln,1,18)   = "Exento Retención".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = IF anexos_clientes.exento_retencion THEN "SI" ELSE "NO".
            OVERLAY(cln,60,18)  = "Exento GMF".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = IF anexos_clientes.exento_gmf THEN "SI" ELSE "NO".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Declara Renta".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = IF anexos_clientes.declara_renta THEN "SI" ELSE "NO".
            OVERLAY(cln,60,18)  = "Adm. Rec. Públicos".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = IF anexos_clientes.adm_recursos_publ THEN "SI" ELSE "NO".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Ocupación".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  =  IF Clientes.Tipo_Actividad = ? THEN " " ELSE Clientes.Tipo_Actividad.
            OVERLAY(cln,60,18)  = "Estado del Cargo".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = 
                IF Clientes.Tipo_Actividad = "Empleado Público" 
                THEN anexos_clientes.estado_cargo 
                ELSE 
                    IF Clientes.Tipo_Actividad = "Empleado Privado" 
                    THEN (IF Clientes.Tip_contrato = 1 THEN "Término Indefinido" ELSE "Término Fijo")
                    ELSE " ".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Empresa donde Trabaja".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = femprsa(clientes.cod_empresa).
            OVERLAY(cln,60,18)  = "Cargo Actual/Actividad".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = fvrios(2,clientes.cod_cargo).
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Fecha de Ingreso".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.fec_ingempresa,"99/99/9999").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Actividad Económica Empresa".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = fciiu(anexos_Clientes.acti_economica_emp).
            OVERLAY(cln,60,18)  = "Teléfono".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = (IF Anexos_clientes.Ind_Comercial <> 0 THEN "(" + STRING(Anexos_clientes.Ind_Comercial) + ") " ELSE "") + clientes.tel_comercial.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Dirección de la Empresa".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = clientes.DIR_comercial.
            OVERLAY(cln,60,18)  = "Ciudad y Departamento".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = fNCiudad(Clientes.Lugar_Comercial).
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Teléfono (fax)".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = (IF Anexos_clientes.ind_fax_comercial <> 0 THEN "(" + STRING(Anexos_clientes.ind_fax_comercial) + ") " ELSE "") + Anexos_Clientes.Tel_Fax_Comercial.
            OVERLAY(cln,60,18)  = "Ext.".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(anexos_clientes.extencion_comercial).
            PUT UNFORMATTED cln SKIP.
            IF Clientes.Tipo_Actividad = "Independiente o Empleado Socio" 
            THEN DO:
                cln = "".
                OVERLAY(cln,1)   = "Independiente o Empleado Socio:".
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombre de la Entidad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = femprsa(clientes.cod_empresa).
                OVERLAY(cln,60,18)  = "Nit".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = anexos_clientes.nit_independiente.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Código CIIU".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = STRING(Clientes.Grupo) + "-" + STRING(Clientes.Subgrupo) + "-" + STRING(Clientes.Codigo_Ciiu).
                OVERLAY(cln,60,18)  = "Tiempo de Actividad".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = STRING(anexos_clientes.tiempo_actividad) + " Años " + string(anexos_clientes.cam_int1) + " Meses".
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Actividad Económica Entidad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = fciiu(string(Clientes.Grupo) + "-" + string(Clientes.Subgrupo) + "-" + string(Clientes.Codigo_Ciiu)).
                OVERLAY(cln,60,18)  = "Cargo Actual".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = fvrios(2,clientes.cod_cargo).
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Dirección de la Empresa".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = clientes.DIR_comercial.
                OVERLAY(cln,60,18)  = "Ciudad y Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = fNCiudad(Clientes.Lugar_Comercial).
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF Anexos_clientes.ind_comercial <> 0 THEN "(" + STRING(Anexos_clientes.ind_comercial) + ") " ELSE "") +  STRING(clientes.tel_comercial).
                PUT UNFORMATTED cln SKIP.

            END. /* IF Clientes.Tipo_Actividad = "Independiente o Empleado Socio"  */

            IF Clientes.Tipo_Actividad = "Pensionado" 
            THEN DO:
                cln = "".
                OVERLAY(cln,1)   = "Pensionado:".
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombre de la Entidad".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = femprsa(clientes.cod_empresa).
                OVERLAY(cln,60,18)  = "Pensionado desde".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = STRING(anexos_clientes.Fec_pensionado,"99/99/9999").
                PUT UNFORMATTED cln SKIP.
            END. /* IF Clientes.Tipo_Actividad = "Independiente o Empleado Socio"  */

            IF Clientes.Tipo_Actividad = "otro" 
            THEN DO:
                cln = "".
                OVERLAY(cln,1)   = "otro:".
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1)   = Anexos_Clientes.Especifique_Ocupacion.
                PUT UNFORMATTED cln SKIP.
            END. /* IF Clientes.Tipo_Actividad = "Independiente o Empleado Socio"  */
            
            cln = "".
            cln = fCntra("----- 4. INFORMACION FINANCIERA(en pesos) -----").
            PUT UNFORMATTED cln SKIP SKIP.
            
            cln = "".
            OVERLAY(cln,1,30)   = "INGRESO MENSUAL(Certificados)".
            OVERLAY(cln,60,30)  = "EGRESO MENSUAL".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Salario/Pensión".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.salario,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "Descuento de Nómina".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(clientes.gto_obligacion,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Honorarios".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.ing_honorarios,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "Arriendo/Cuota Hip.".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(clientes.gto_arriendo,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Comisiones".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.ing_financieros,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "Cuota de Préstamo".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(clientes.sdo_obligaciones,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Arriendos".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.ing_arriendos,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "Cuota Tarj. Credito".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(Anexos_Clientes.Gto_TargetaCredito,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Otros Ingresos".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(Clientes.Ing_Otros,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "Gastos".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(Clientes.Gto_familiar,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "Especificar".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,37)  = Anexos_Clientes.Especifique_OtrosIng.
            OVERLAY(cln,60,18)  = "Otras Deudas".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(Anexos_Clientes.Gto_otros,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "TOTAL INGRESOS".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = STRING(clientes.salario + clientes.ing_honorarios + clientes.ing_financieros + clientes.ing_arriendos + Clientes.Ing_otros,"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "TOTAL EGRESOS".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = STRING(clientes.gto_obligacion + clientes.gto_arriendo + clientes.sdo_obligaciones + Anexos_Clientes.Gto_TargetaCredito + Clientes.Gto_familiar + Anexos_Clientes.Gto_otros,"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1,18)   = "TOTAL ACTIVOS".
            OVERLAY(cln,20,1)   = ":".
            OVERLAY(cln,22,18)  = string(fTTalActvos(clientes.nit),"$>>>,>>>,>>9").
            OVERLAY(cln,60,18)  = "TOTAL PASIVOS".
            OVERLAY(cln,80,1)   = ":".
            OVERLAY(cln,82)     = string(fTTalPsvos(clientes.nit),"$>>>,>>>,>>9").
            PUT UNFORMATTED cln SKIP.

                /* Verifica si registra algun pasivo */
            IF CAN-FIND(FIRST Act_Pasivos WHERE Act_pasivos.nit = clientes.nit
                                            AND Act_Pasivos.Cod_Tipo = 4) THEN DO:
                cln = "".
                cln = fCntra("----- Descripción de Pasivos(Préstamos) -----").
                PUT UNFORMATTED cln SKIP SKIP.

                cln = "".
                OVERLAY(cln,1,20)   = "ENTIDAD".
                OVERLAY(cln,22,18)  = "VALOR INICIAL". 
                OVERLAY(cln,42,18)  = "CUOTA".
                OVERLAY(cln,60,18)  = "SALDO".
                PUT UNFORMATTED fcntra(cln) SKIP.
                FOR EACH Act_Pasivos NO-LOCK
                    WHERE
                        act_pasivos.nit = clientes.nit
                    AND Act_Pasivos.Cod_Tipo = 4:
                    cln = "".
                    OVERLAY(cln,1,20)   = Act_Pasivos.Nombre.
                    OVERLAY(cln,22,18)  = STRING(Act_Pasivos.Val_Comercial,"$>>>,>>>,>>9"). 
                    OVERLAY(cln,42,18)  = STRING(Act_Pasivos.Val_Cuota,"$>>>,>>>,>>9").
                    OVERLAY(cln,60,18)  = STRING(Act_Pasivos.Val_Saldo,"$>>>,>>>,>>9").
                    PUT UNFORMATTED fcntra(cln) SKIP.
                END.
            END.
               /* Verifica si tiene algun Activo   */
            IF CAN-FIND(FIRST Act_Pasivos WHERE act_pasivos.nit = clientes.nit
                                            AND Act_Pasivos.Cod_Tipo >= 1
                                            AND Act_Pasivos.Cod_Tipo <= 3) THEN DO:
                cln = "".
                cln = fCntra("----- Detalle De Activos -----").
                PUT UNFORMATTED cln SKIP SKIP.

                IF CAN-FIND(FIRST Act_Pasivos WHERE act_pasivos.nit = clientes.nit
                                                AND Act_Pasivos.Cod_Tipo = 2) THEN DO:   /* Si posee  Vehiculos */
                    cln = "".
                    cln = fCntra("----- VEHICULOS -----").
                    PUT UNFORMATTED cln SKIP SKIP.
                    cln = "".
                    OVERLAY(cln,1)   = "                      Marca                Modelo               Placa         Valor Comercial     Prenda a favor de ".
                    PUT UNFORMATTED cln SKIP.
                    FOR EACH Act_Pasivos NO-LOCK
                        WHERE
                            act_pasivos.nit = clientes.nit
                        AND Act_Pasivos.Cod_Tipo = 2:
                        cln = "".
                        OVERLAY(cln,1,20)   = Act_Pasivos.Nombre.
                        OVERLAY(cln,22,18)  = Act_Pasivos.modelo. 
                        OVERLAY(cln,42,18)  = Act_Pasivos.placa.
                        OVERLAY(cln,60,18)  = string(Act_Pasivos.Val_Comercial,"$>>>,>>>,>>9").
                        PUT UNFORMATTED fcntra(cln) SKIP.
                    END.
                END.

                IF CAN-FIND(FIRST Act_Pasivos WHERE act_pasivos.nit = clientes.nit
                                AND Act_Pasivos.Cod_Tipo = 1) THEN DO:   /* Si posee  Bienes Raices */
                    cln = "".
                    cln = fCntra("----- BIENES RAICES -----").
                    PUT UNFORMATTED cln SKIP SKIP.

                    cln =   "      Tipo                                                  Valor         Matricula        Hipoteca" + CHR(10) +
                            "      de Bien     Dirección     Ciudad     Departamento     Comercial     Inmobiliaria     a Favor de".

                    PUT UNFORMATTED cln SKIP.
                    FOR EACH Act_Pasivos NO-LOCK
                        WHERE
                            act_pasivos.nit = clientes.nit
                        AND Act_Pasivos.Cod_Tipo = 1:
                        cln = "".
                        OVERLAY(cln,1,15)   = Act_Pasivos.nombre.
                        OVERLAY(cln,17,17)  = Act_Pasivos.Dir_Bienes.
                        OVERLAY(cln,35,29)  = entry(2,Act_Pasivos.lugar_bienes + "-","-") + "-" + entry(3,Act_Pasivos.lugar_bienes + "--","-").
                        OVERLAY(cln,65,16)  = STRING(Act_Pasivos.val_comercial,"$>>>,>>>,>>9").
                        OVERLAY(cln,81,16)  = Act_Pasivos.matricula_inmobiliaria.
                        OVERLAY(cln,98,15)  = Act_Pasivos.Prenda_hipoteca.
                        PUT UNFORMATTED fcntra(cln) SKIP.
                    END.
                END.


                IF CAN-FIND(FIRST Act_Pasivos WHERE act_pasivos.nit = clientes.nit
                                AND Act_Pasivos.Cod_Tipo = 3) THEN DO:   /* Otros Activos */
                    cln = "".
                    cln = fCntra("----- OTROS ACTIVOS -----").
                    PUT UNFORMATTED cln SKIP SKIP.

                    cln =   "               Tipo de Bien                       valor comercial                        Detalle    ".

                    PUT UNFORMATTED cln SKIP.
                    FOR EACH Act_Pasivos NO-LOCK
                        WHERE
                            act_pasivos.nit = clientes.nit
                        AND Act_Pasivos.Cod_Tipo = 3:
                        cln = "".
                        OVERLAY(cln,1,30)   = Act_Pasivos.nombre.
                        OVERLAY(cln,36,35)  = STRING(Act_Pasivos.val_comercial,"$>>>,>>>,>>9").
                        OVERLAY(cln,75,20)  = Act_Pasivos.Detalle.
                        PUT UNFORMATTED fcntra(cln) SKIP.
                    END.
                END.
            END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmlrio2 NowWin 
PROCEDURE pFrmlrio2 :
/*------------------------------------------------------------------------------
  Purpose: pFrmlrio2   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
            
            
            cln = "".
            cln = fCntra("----- 5. REFERENCIAS -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("----- FAMILIAR -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("(Persona que NO VIVA con usted)").
            PUT UNFORMATTED cln SKIP SKIP.

            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 1 
                AND Relaciones.Estado = 1 
                AND Relaciones.Descripcion  <> "Conyuge",  /* Se ecluye el  Conyuge  */
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion,
                FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit:
                
                cln = "".
                OVERLAY(cln,1,18)   = "Primer Apellido".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Apellido1.
                OVERLAY(cln,60,18)  = "Segundo Apellido".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Apellido2.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombres".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22)  = BCnygue.nombre.
                PUT UNFORMATTED cln SKIP.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Parentesco".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = relaciones.descripcion.
                OVERLAY(cln,60,18)  = "Dirección Residencia".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Dir_Residencia.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Ciudad". /* esto donde se captura???? */
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = fNciudad(BCnygue.Lugar_Residencia).
                OVERLAY(cln,60,18)  = "Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = "".
                PUT UNFORMATTED cln SKIP.
            
                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.tel_Residencia.
                PUT UNFORMATTED cln SKIP.
            END. /* FOR EACH relaciones NO-LOCK */
            

            cln = "".
            cln = fCntra("----- PERSONAL -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("(no familiar)").
            PUT UNFORMATTED cln SKIP SKIP.

            
            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 9 
                AND Relaciones.Estado = 1,
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion:
                IF NOT CAN-FIND(FIRST BCnygueAnxos WHERE BCnygueAnxos.nit = bCnygue.nit)
                THEN DO:
                    CREATE BCnygueAnxos.
                    BCnygueAnxos.nit = bCnygue.nit.
                END.
                FIND FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit NO-ERROR.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Primer Apellido".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = BCnygue.Apellido1.
                OVERLAY(cln,60,18)  = "Segundo Apellido".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Apellido2.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Nombres".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22)     = BCnygue.nombre.
                PUT UNFORMATTED cln SKIP.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Parentesco".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = relaciones.descripcion.
                OVERLAY(cln,60,18)  = "Dirección Residencia".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Dir_Residencia.
                PUT UNFORMATTED cln SKIP.

                cln = "".
                OVERLAY(cln,1,18)   = "Ciudad". /* esto donde se captura???? */
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = fNciudad(BCnygue.Lugar_Residencia).
                OVERLAY(cln,60,18)  = "Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = "".
                PUT UNFORMATTED cln SKIP.
            
                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.tel_Residencia.
                PUT UNFORMATTED cln SKIP.
                PUT  SKIP(1).
            END. /* FOR EACH relaciones NO-LOCK */


            cln = "".
            cln = fCntra("----- COMERCIAL -----").
            PUT UNFORMATTED cln SKIP SKIP.

            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 2 
                AND Relaciones.Estado = 1,
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion:
                IF NOT CAN-FIND(FIRST BCnygueAnxos WHERE BCnygueAnxos.nit = bCnygue.nit)
                THEN DO:
                    CREATE BCnygueAnxos.
                    BCnygueAnxos.nit = bCnygue.nit.
                END.
                FIND FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit NO-ERROR.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Establecimiento".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = BCnygue.nombre.
                OVERLAY(cln,60,18)  = "Dirección".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Dir_Residencia.
                PUT UNFORMATTED cln SKIP.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Ciudad". 
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = fNciudad(BCnygue.Lugar_Residencia).
                OVERLAY(cln,60,18)  = "Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = "".
                PUT UNFORMATTED cln SKIP.
            
                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.tel_Residencia.
                PUT UNFORMATTED cln SKIP.
                PUT  SKIP(1).
            END. /* FOR EACH relaciones NO-LOCK */

            cln = "".
            cln = fCntra("----- FINANCIERA -----").
            PUT UNFORMATTED cln SKIP SKIP.

            
            FOR EACH relaciones NO-LOCK
                WHERE
                    relaciones.nit = clientes.nit
                AND Relaciones.Cod_relacion = 2 
                AND Relaciones.Estado = 1,
                FIRST BCnygue NO-LOCK WHERE bCnygue.nit = Relaciones.Nit_relacion:
                IF NOT CAN-FIND(FIRST BCnygueAnxos WHERE BCnygueAnxos.nit = bCnygue.nit)
                THEN DO:
                    CREATE BCnygueAnxos.
                    BCnygueAnxos.nit = bCnygue.nit.
                END.
                FIND FIRST BCnygueAnxos NO-LOCK WHERE BCnygueAnxos.nit = BCnygue.nit NO-ERROR.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Establecimiento".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = BCnygue.nombre.
                OVERLAY(cln,60,18)  = "Dirección".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = BCnygue.Dir_Residencia.
                PUT UNFORMATTED cln SKIP.
                
                cln = "".
                OVERLAY(cln,1,18)   = "Ciudad". 
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,37)  = fNciudad(BCnygue.Lugar_Residencia).
                OVERLAY(cln,60,18)  = "Departamento".
                OVERLAY(cln,80,1)   = ":".
                OVERLAY(cln,82)     = "".
                PUT UNFORMATTED cln SKIP.
            
                cln = "".
                OVERLAY(cln,1,18)   = "Teléfono".
                OVERLAY(cln,20,1)   = ":".
                OVERLAY(cln,22,18)  = (IF BCnygueAnxos.Ind_Cli <> 0 THEN "(" + STRING(BCnygueAnxos.Ind_Cli) + ") " ELSE "") + BCnygue.tel_Residencia.
                PUT UNFORMATTED cln SKIP.
                PUT  SKIP(1).
            END. /* FOR EACH relaciones NO-LOCK */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmlrio3 NowWin 
PROCEDURE pFrmlrio3 :
/*------------------------------------------------------------------------------
  Purpose: pFrmlrio3   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
            
            
            cln = "".
            cln = fCntra("----- 6. TRANSACCIONES EN MONEDA EXTRANJERA -----").
            PUT UNFORMATTED cln SKIP SKIP.
            
            cln = "".
            OVERLAY(cln,1) = "Transaciones que normalmente realiza: " + anexos_clientes.Transac_Mod_Ext.
            PUT UNFORMATTED cln SKIP.
            
            cln = "".
            OVERLAY(cln,1) = "Posee cuenta en moneda extranjera: " + 
                IF Anexos_Clientes.Maneja_Cta_Extanjera THEN "SI" ELSE "NO".
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1) = "Tipo de Moneda: " + Anexos_Clientes.Tipo_Moneda_Divisa.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1) = "Banco: " + Anexos_Clientes.Nom_Banco_extranjero.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1) = "Número de Cuenta: " + Anexos_Clientes.Num_cta_extranjera.
            PUT UNFORMATTED cln SKIP.

            cln = "".
            OVERLAY(cln,1) = "Ciudad y Pais: " + Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.
            PUT UNFORMATTED cln SKIP.
            

            cln = "".
            cln = fCntra("----- 7. Declaración Voluntaria de origen de fondos -----").
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "Obrando en nombre propio de manera voluntaria o en representación de ___________________________________________" + CHR(10) +
                  "y dando certeza que todo lo aquí consignado es cierto, realizo la siguiente declaración de origen de fondos y " + CHR(10) +
                  "bienes en cumplimiento de lo señalado en el estatuto Orgánico del Sistema Financiero (decreto 663 de 1993), la" + CHR(10) +
                  "Circular Básica Jurídica 007 de 1996  (CE 046 de 2002, 025 de 2003 y la 034 de 2004) expedidas por la " + CHR(10) +
                  "Superintendencia Financiera y demás normas legales concordantes para la apertura y manejo de productos bancarios.".
            PUT UNFORMATTED cln SKIP SKIP.
                        
            cln = 
                "7.1. Declaro que mis activos, ingresos, recursos que entrego y entregaré en depósito, en garantía o para cancelar"  + CHR(10) +
                "obligaciones a mi nombre provienen de ___________________________  ___________________________________________"  + CHR(10) +
                'conforme a lo señalado en la sección de "Información Actividad Económica".'  + CHR(10) +
                "7.2. Declaro que no admitiré que terceros manejen depósitos a mis cuentas o cancelen obligaciones a mi nombre para"  + CHR(10) +
                "con Juriscoop, con fondos provenientes de actividades ilícitas o aparentemente lícitas. Ni efectuaré transacciones "  + CHR(10) +
                "destinadas a tales actividades o a favor de personas aparente o efectivamente relacionadas con las mismas."     + CHR(10) +
                "7.3. Destinaré los fondos que procedan de cualquier financiación que me otorgue Juriscoop, a los fines específicos"  + CHR(10) +
                "para los que hayan sido concedidos y en ningún caso para la realización de actividades ilícitas."  + CHR(10) +
                "7.4. Autorizo a Juriscoop, para saldar cuentas de cualquier tipo que mantenga y para declarar plazo vencido a las "  + CHR(10) +
                "obligaciones a mi cargo, en caso de infracción de cualquiera de los numerales contenidos en este documento,"  + CHR(10) +
                "eximiendo a Juriscoop, de toda responsabilidad que se deriva por información errónea, falsa o inexacta que yo"  + CHR(10) +
                "hubiere proporcionado en este documento o la violación de los compromisos aquí adquiridos.".
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("----- 8. Autorizaciones -----").
            PUT UNFORMATTED cln SKIP SKIP.



            cln =
            "8.1. CONSULTA Y REPORTE A CENTRALES DE RIESGO"  + CHR(10) +
            "Autorizo expresa e irrevocablemente, con carácter permanente a Juriscoop o a quien represente sus derechos u ostente"  + CHR(10) +
            "en el futuro la calidad de acreedor, para que con fines estadísticos de información comercial y de evaluación de "  + CHR(10) +
            "riesgos, en la realización de negocios financieros y de operaciones activas de crédito, reporte, procese, solicite, "  + CHR(10) +
            "consulte y divulgue a la Asociación Bancaria y de Entidades Financieras de Colombia, a cualquier otra entidad que "  + CHR(10) +
            "maneje o administre bases de datos con los mismos fines o a cualquier institución sometida al control y vigilancia "  + CHR(10) +
            "de la Superintendencia Financiera, todo lo relativo al nacimiento, modificación y extinción de obligaciones que "  + CHR(10) +
            "directa o indirectamente  tenga contraídas o vigentes hasta la total extinción de las obligaciones a mi cargo por "  + CHR(10) +
            "cualquier medio legal y después de ello durante el plazo máximo que para el efecto autoricen la ley o la "  + CHR(10) +
            "jurisprudencia."  + CHR(10) +
            "La presente autorización se extiende a favor de aquellas entidades que otorguen garantías para respaldar obligaciones "  + CHR(10) +
            "adquiridas por mí con Juriscoop."  + CHR(10) +
            "8.2. ACTUALIZACIÓN Y VERIFICACIÓN DE LA INFORMACIÓN"  + CHR(10) +
            "8.2.1. Certifico que toda la información suministrada es verídica, exacta y que en el evento de cualquier cambio, éste "  + CHR(10) +
            "le será comunicado a Juriscoop, a través suyo o de aquella entidad de acuerdo con los convenios existentes.                                                                                                                                                        "  + CHR(10) +
            "8.2.2. Me obligo a mantener actualizada la información suministrada, para lo cual me comprometo a reportar por lo "  + CHR(10) +
            "menos una vez al año los cambios que se hayan generado respecto a la información aquí contenida.                                                                                                                                                   "  + CHR(10) +
            "8.2.3. Juriscoop procederá a saldar las cuentas abiertas a mi favor si la información aquí suministrada no se puede "  + CHR(10) +
            "verificar y/o cuando no cumpla con la obligación de actualizar debidamente por lo menos una vez al año la referida "  + CHR(10) +
            "información."  + CHR(10) +
            "8.2.4. Autorizo irrevocablemente a Juriscoop a través suyo o de aquella entidad de acuerdo con los convenios "  + CHR(10) +
            "existentes, para verificar a través de las entidades que dicha instituciones designen, la información suministrada "  + CHR(10) +
            "en el presente documento."  + CHR(10) +
            "8.3. SUMINISTRO DE INFORMACIÓN"  + CHR(10) +
            "La información general aquí contenida la suministro para efectos de mi vinculación, actualización o contratación "  + CHR(10) +
            "de productos con Juriscoop y/o demás entidades del grupo solidario al cual pertenece."  + CHR(10) +
            "8.4. CLÁUSULA DE ACEPTACIÓN DE REGLAMENTOS"  + CHR(10) +
            "En el evento que alguno de los productos relacionados en la presente solicitud sea aprobado en fecha posterior, "  + CHR(10) +
            "Juriscoop entregará a EL CLIENTE un ejemplar del respectivo contrato o reglamento del producto, si transcurridos "  + CHR(10) +
            "cinco (5) días hábiles desde la fecha de entrega del respectivo contrato o reglamento a EL CLIENTE, Juriscoop no "  + CHR(10) +
            "recibe manifestación escrita alguna al respecto de los términos de dicho contrato o reglamento, se presume que "  + CHR(10) +
            "EL CLIENTE lo acepta en todas sus partes, EL CLIENTE acepta desde ahora todas las modificaciones o adiciones "  + CHR(10) +
            "efectuadas al mencionado reglamento siempre y cuando las mismas sean debidamente notificadas por parte de "  + CHR(10) +
            "Juriscoop a través del medio que éste considere mas adecuado, con cinco (5) días hábiles de antelación al momento "  + CHR(10) +
            "en que dichas modificaciones entren en rigor, entendiéndose que si una vez notificada la modificación o adición "  + CHR(10) +
            "EL CLIENTE continúa haciendo uso del respectivo producto, dichas modificaciones o adiciones se entienden aceptadas "  + CHR(10) +
            "por mí aún cuando no haya transcurrido el plazo estipulado."  + CHR(10) +
            "8.5. DESTRUCCIÓN DE DOCUMENTOS"  + CHR(10) +
            "Autorizo irrevocablemente a Juriscoop para que en caso que la solicitud sea negada, destruya los documentos que he "  + CHR(10) +
            "aportado, siendo por ende responsable Juriscoop de efectuar tal destrucción."  + CHR(10) +
            "8.6. CLÁUSULA DE ACEPTACIÓN DE TÉRMINOS"  + CHR(10) +
            "Manifiesto que he entendido y diligenciado de manera voluntaria y veraz, toda la información contenida en la "  + CHR(10) +
            "presente solicitud de productos."  + CHR(10) +
            "8.7. COBRO DE CENTRALES DE RIESGO"  + CHR(10) +
            "Autorizo a Juriscoop para efectuar el cobro de las consultas a las centrales de riesgo y que sean debitadas de mi "  + CHR(10) +
            "cuenta"  + CHR(10) +
            "8.8. COMPROMISO DE CUMPLIMIENTO DE ACTUALIZACIÓN DE INFORMACIÓN"  + CHR(10) +
            "Me obligo con Juriscoop a actualizar mi información y documentación respectiva mínimo una vez al año, en "  + CHR(10) +
            "concordancia con la facultad que tienen los establecimientos financieros de saldar los productos en caso de "  + CHR(10) +
            "incumplimiento de esta obligación."  + CHR(10) +
            "Declaro conocer y aceptar los términos de los contratos y reglamentos vigentes en la cooperativa Juriscoop."  + CHR(10) +
            "8.9. REQUISITOS DE ASEGURABILIDAD." + CHR(10) +
            "Se le ha diagnosticado o sufre en el momento de una enfermedad terminal       SI __   NO __"  + CHR(10) +
            "8.10. DESEMBOLSO"  + CHR(10) +
            "Autorizo que es desembolso del presente crédito se realice en mi cuenta de ahorro a la vista"  + CHR(10) +
            "8.11. DEBITO AUTOMATICO"  + CHR(10) +
            "8.12. PRIORIDAD DESCUENTO".
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "Declaro conocer y aceptar los términos de los contratos y reglamentos vigentes en la cooperativa Juriscoop." +
                    FILL(CHR(10),5).
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "Fecha de Diligenciamiento     ___________________________________________" + CHR(10) +
                STRING(DAY(TODAY),"  99  ") + STRING(MONTH(TODAY),"  99  ") + STRING(YEAR(TODAY),"  9999")    +
                "            FIRMA SOLICITANTE  /  CODEUDOR   / AVALISTA" + CHR(10) +
                "  DD    MM    AAAA                   Documento de Identidad No.                         HUELLA INDICE DERECHO".
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            cln = fCntra("----- Documentación Requerida -----").
            PUT UNFORMATTED cln SKIP SKIP.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmlrio4 NowWin 
PROCEDURE pFrmlrio4 :
/*------------------------------------------------------------------------------
  Purpose: pFrmlrio3   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
            
            
            cln = "".
            cln = 
                
                "DOCUMENTOS                                                                                               CHEQUEO" + CHR(10) +
                "Formato Único de Vinculación" + CHR(10) +
                "Fotocopia Ampliada del Documento de Identidad (CC, TI, CE, CD, PAS)" + CHR(10) +
                "Fotocopia de las dos (2) últimas nóminas o mesada pensional" + CHR(10) +
                "Certificado Laboral (con cargo, sueldo, tipo contrato, antigüedad) expedición menor a 30 días" + CHR(10) +
                "Fotocopia de Tarjeta de propiedad de (los) vehículo(s) o moto(s)" + CHR(10) +
                "Factura proforma para vehículo o moto nuevo" + CHR(10) +
                "Peritaje para vehículo usado" + CHR(10) +
                "Certificado original de tradición y libertad del inmueble no mayor a 30 días" + CHR(10) +
                "Declaración de renta o constancia si no esta obligado (2 últimos años)" + CHR(10) +
                "Copia del contrato de promesa de compra venta o de la minuta del contrato de compra venta" + CHR(10) +
                "Paz y salvo de Impuesto Predial" + CHR(10) +
                "Avalúo Comercial y estudio de títulos" + CHR(10) +
                "Factura de compra venta que acrediten las transacciones comerciales" + CHR(10) +
                "Certificado de ingresos, retenciones otros ingresos u honorarios" + CHR(10) +
                "Cert. de Cámara de Comercio, Licencia de Sanidad o Bomberos. (vigencia no mayor a 90 días)" + CHR(10) +
                "Extractos bancarios  de los últimos tres (3) meses" + CHR(10) +
                "Estados Financieros certificados de los 2 últimos años y al corte más reciente" + CHR(10) +
                "Prueba de destinación para el Crédito educativo" + CHR(10) +
                "Flujo de caja del proyecto a financiar" + CHR(10) +
                "" + CHR(10) +
                fcntra("Convenciones") + CHR(10) +
                "                          A:  Asalariados        I:  Independientes" + CHR(10) +
                "                          *" + CHR(10) +
                "                          CC:  Cédula de Ciudadanía                        CE:  Cédula de Extranjería" + CHR(10) +
                "                          TI:  Tarjeta de Identidad                        CD:  Carné Diplomático" + CHR(10) +
                "                          NIT:  Número de Identificación Tributaria        PAS:  Pasaporte" + CHR(10) +
                "                          NUIP:  Número Único de Identificación Personal".
            PUT UNFORMATTED cln SKIP SKIP.

            cln = "".
            PUT UNFORMATTE fcntra("ESPACIO EXCLUSIVO PARA JURISCOOP") SKIP.


            cln =   "Nombre Asesor Comercial                                    Código del Asesor            Fecha y Hora" + CHR(10) +
                    "________________________________                           _______________           " +  STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").
            PUT UNFORMATTED cln SKIP.

            cln = "Certifico que he cumplido con las políticas y procedimientos establecidos en el manual de SARLAFT " + CHR(10) +
                  "(Sistema de Administración del Riesgo de Lavado de Activos y de la Financiación del Terrorismo), para el" + CHR(10) +
                  "conocimiento y vinculación del cliente, así como la entrevista o visita presencial.".
            PUT UNFORMATTED cln SKIP.
            cln = "Condiciones Especiales: ________________________________________________________________________________" + CHR(10) +
                  "________________________________________________________________________________________________________" + CHR(10) +
                  "________________________________________________________________________________________________________".
            PUT UNFORMATTED cln SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrntHndle NowWin 
PROCEDURE pPrntHndle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER h AS HANDLE NO-UNDO.
    GHPARENT = h.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRcbeDtos NowWin 
PROCEDURE pRcbeDtos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DEF VAR cNit AS CHAR NO-UNDO.
    cNit = ENTRY(1,c,CHR(1)).
    DO WITH FRAME {&FRAME-NAME}:
        NitCliente:SCREEN-VALUE = cNit.
        APPLY "leave" TO NitCliente.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir NowWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pFrmlrio.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fciiu NowWin 
FUNCTION fciiu RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    c = c + FILL("-",2).
    DEF VAR cRtrno AS CHAR NO-UNDO.
    cRtrno = "ERROR: Código Ciiu".
    FOR FIRST ciiu NO-LOCK
        WHERE
            ciiu.grupo      = integer(ENTRY(1,c,"-"))
        AND ciiu.subgrupo   = integer(ENTRY(2,c,"-"))
        AND ciiu.codigo     = integer(ENTRY(3,c,"-")):
        cRtrno = ciiu.descripcion.
    END.
    RETURN crtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCntra NowWin 
FUNCTION fCntra RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR l AS INTEGER NO-UNDO.
    l = 118.
    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    i = TRUNCATE((l - LENGTH(TRIM(c))) / 2,0).
    OVERLAY(c1,i) = c.
    RETURN c1.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCoordndas NowWin 
FUNCTION fCoordndas RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR l AS LOGICAL NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    IF INDEX(c,"_") = 0 THEN RETURN "".
    l = FALSE.
    DO i = 1 TO LENGTH(c):
        IF NOT l 
        THEN DO:
            IF SUBSTRING(c,i,1) = "_" 
            THEN do:
                l = TRUE.
                j = i.
            END.
        END.
        ELSE DO:
            IF SUBSTRING(c,i,1) = " "
            THEN DO:
                l = FALSE.
                c1 = c1 + STRING(j) + CHR(2) + STRING(i - j) + CHR(1).    
            END.
        END.
        
    END.
    RETURN c1.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEmprsa NowWin 
FUNCTION fEmprsa RETURNS CHARACTER
  (iCdgo AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def VAR cRtrno AS CHAR NO-UNDO.
    cRtrno = "ERRROR: Código Empresa".
        
    FOR FIRST empresas NO-LOCK
        WHERE
            empresas.Cod_Empresa = iCdgo:
        cRtrno = Empresas.Alias_Empresa.
    END.
    RETURN cRtrno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstrctra NowWin 
FUNCTION fEstrctra RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR iP AS INTEGER NO-UNDO.
    EMPTY TEMP-TABLE tSlctud.
    ip = 1.
    DO WITH FRAME {&FRAME-NAME}:
        c = cPgna1:PRIVATE-DATA.
        c = REPLACE(c,"]",CHR(10)).
        DO i = 1 TO NUM-ENTRIES(c,CHR(10)):
            CREATE tSlctud.
            ASSIGN
                tslctud.tpo         = 0
                tSlctud.iPgna       = ip
                tSlctud.iLnea       = i
                tSlctud.cTxto       = entry(1,ENTRY(i,c,CHR(10)) + "[","[")
                tSlctud.cVriables   = entry(2,ENTRY(i,c,CHR(10)) + "[","[").
            tSlctud.coor        = fCoordndas(tSlctud.cTxto).
        END.
    END.
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFrmto NowWin 
FUNCTION fFrmto RETURNS CHARACTER
  (c AS CHAR,l AS INTEGER /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    c = TEXTO A FORMATEAR
    l = Ancho Máximo De La Línea        
    ------------------------------------------------------------------------------*/


    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    i = l.
    j = 1.

    DO WHILE i <= LENGTH(c):
        DO WHILE SUBSTRING(c,i,1) <> " " AND SUBSTRING(c,i,1) <> CHR(10) AND i > 1:
            i = i - 1.
        END.
        IF NOT i >= j * l 
        THEN DO:
            SUBSTRING(c,i,1) = CHR(10).
            j = j + 1.
            i = j * l.
        END.
        ELSE i = i - 1.
    END.
    RETURN c.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNCiudad NowWin 
FUNCTION fNCiudad RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR cRtrno AS CHAR NO-UNDO.
    cRtrno = "ERROR: Ciudad de Expedición".
    FOR FIRST ubicacion no-lock
        WHERE 
            ubicacion.ubicacion = c:
        cRtrno = ubicacion.nombre.
    END.

    RETURN cRtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNmbreUsuario NowWin 
FUNCTION fNmbreUsuario RETURNS CHAR(cCdgo AS CHAR):
    DEF VAR cRtrno AS CHAR NO-UNDO.
    cRtrno = "ERROR: " + cCdgo + " NO Definido".
    FOR FIRST usuarios NO-LOCK
            WHERE
                usuarios.usuario = cCdgo:
        crtrno = usuario.nombre.
    END.
    RETURN cRtrno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTTalActvos NowWin 
FUNCTION fTTalActvos RETURNS DECIMAL
  (cnit AS char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fttalpsvos NowWin 
FUNCTION fttalpsvos RETURNS DECIMAL
  (cnit AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVr NowWin 
FUNCTION fVr RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.

    i = LOOKUP(c,entry(1,cDtosSlctud,CHR(10)),CHR(1)).
    IF i = 0 THEN RETURN "".
    
    RETURN ENTRY(i,ENTRY(2,cDtosSlctud,CHR(10)),CHR(1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVrios NowWin 
FUNCTION fVrios RETURNS CHARACTER
  (iTpo AS INTEGER,icdgo AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cRtrno AS CHAR NO-UNDO.
  cRtrno = "ERROR: Código Profesión".
  FOR FIRST varios NO-LOCK
      WHERE
            varios.tipo = iTpo
        AND varios.codigo = icdgo:
    cRtrno =  Varios.Descripcion.
  END.
  RETURN cRtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

