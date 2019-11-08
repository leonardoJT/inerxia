&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
DEFINE INPUT  PARAMETER PNit LIKE Clientes.Nit.
DEFINE INPUT  PARAMETER PMen AS CHARACTER FORMAT "X(5)".
DEFINE INPUT  PARAMETER PMenM AS CHARACTER FORMAT "X(5)".
DEFINE OUTPUT PARAMETER PCod LIKE BorradorSipla.CodAutoriza.

DEFINE VAR XOk AS LOGICAL INITIAL NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FSipla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Mensaje Btn_Abortar BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Mensaje CodAutoriza 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Autorizar" 
     SIZE 19 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON Btn_Abortar DEFAULT 
     LABEL "&Abortar Operacion" 
     SIZE 18 BY 1.35
     BGCOLOR 8 .

DEFINE VARIABLE Mensaje AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 58 BY 7
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CodAutoriza AS INTEGER FORMAT "99999":U INITIAL 0 
     LABEL "Codigo de Autorizacion" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FSipla
     Mensaje AT ROW 1.27 COL 2 NO-LABEL
     CodAutoriza AT ROW 8.54 COL 51 COLON-ALIGNED
     Btn_Abortar AT ROW 9.88 COL 21
     BtnDone AT ROW 9.88 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.43 BY 10.73
         BGCOLOR 17 FGCOLOR 0 FONT 5
         DEFAULT-BUTTON Btn_Abortar.


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
         TITLE              = "Control del Lavado de Activos"
         HEIGHT             = 10.73
         WIDTH              = 60.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
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
IF NOT wWin:LOAD-ICON("imagenes/magnify0.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/magnify0.ico"
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
/* SETTINGS FOR FRAME FSipla
                                                                        */
/* SETTINGS FOR FILL-IN CodAutoriza IN FRAME FSipla
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Control del Lavado de Activos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Control del Lavado de Activos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  /*APPLY "choose" TO Btn_Abortar IN FRAME FSipla.*/
  APPLY "choose" TO Btn_Abortar IN FRAME FSipla.  
  /*APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FSipla /* Autorizar */
DO:
  IF AVAILABLE BorradorSipla THEN DO:
     BorradorSipla.Estado = YES.
     /*RUN ActTaq_Sipla.*/
  END.
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


&Scoped-define SELF-NAME Btn_Abortar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Abortar wWin
ON CHOOSE OF Btn_Abortar IN FRAME FSipla /* Abortar Operacion */
DO:
  PCod = 0.
  MESSAGE "Transaccion Cancelada por el Usuario" VIEW-AS ALERT-BOX.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActTaqSipla wWin 
PROCEDURE ActTaqSipla :
/*si se desea seguir con el esquema que prende las banderas en la tabla de taquilla
  se debe descomentariar este modulo*/
/*IF PMen EQ "CSDIA" OR PMen EQ "RTDIA" THEN DO:
   FOR EACH Taquilla WHERE
            Taquilla.Nit EQ PNit AND
            Taquilla.Fec_Transaccion EQ W_Fecha:
       ASSIGN Taquilla.Fec_GNU = W_Fecha
              Taquilla.Id_NUD  = YES
              Taquilla.Usu_GNU = BorradorSipla.Usuario.
   END.
END.
IF PMenM EQ "CSMES" OR PMenM EQ "RTMES" THEN DO:
   FOR EACH Taquilla WHERE
            Taquilla.Nit EQ PNit AND
            Taquilla.Fec_Transaccion EQ W_Fecha:
       ASSIGN Taquilla.Fec_GNUM = W_Fecha
              Taquilla.Id_NUM   = YES
              Taquilla.Usu_GNUM = BorradorSipla.Usuario.
   END.
END.*/
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
  DISPLAY Mensaje CodAutoriza 
      WITH FRAME FSipla IN WINDOW wWin.
  ENABLE Mensaje Btn_Abortar BtnDone 
      WITH FRAME FSipla IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FSipla}
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
DO WITH FRAME FSipla:
  CASE PMen:
   WHEN "CSDIA" THEN
       ASSIGN mensaje = "Con esta consignacion se excede el maximo de operaciones en"
              mensaje = mensaje + " efectivo de un dia. Por favor remita al cliente"
              mensaje = mensaje + " donde la persona encargada de la generacion del"
              mensaje = mensaje + " codigo de autorizacion para esta operacion."
              mensaje = mensaje + " (Entre operaciones de Credito y Ahorro)".
   WHEN "RTDIA" THEN
       ASSIGN mensaje = "Con este retiro se excede el maximo de operaciones en"
              mensaje = mensaje + " efectivo de un dia. Por favor remita al cliente"
              mensaje = mensaje + " donde la persona encargada de la generacion del"
              mensaje = mensaje + " codigo de autorizacion para esta operacion."
              mensaje = mensaje + " (Entre operaciones de Credito y Ahorro)".
  END CASE.
  CASE PMenM:
   WHEN "CSMES" THEN
     ASSIGN mensaje = Mensaje + ">> Con esta consignacion se excede el maximo de operaciones en"
            mensaje = mensaje + " efectivo de un Mes. Por favor remita al cliente"
            mensaje = mensaje + " donde la persona encargada de la generacion del"
            mensaje = mensaje + " codigo de autorizacion para esta operacion."
            mensaje = mensaje + " (Entre operaciones de Credito y Ahorro)".
   WHEN "RTMES" THEN
     ASSIGN mensaje = Mensaje + ">> Con este retiro se excede el maximo de operaciones en"
            mensaje = mensaje + " efectivo de un Mes. Por favor remita al cliente"
            mensaje = mensaje + " donde la persona encargada de la generacion del"
            mensaje = mensaje + " codigo de autorizacion para esta operacion."
            mensaje = mensaje + " (Entre operaciones de Credito y Ahorro)".
  END CASE.
  FIND Clientes WHERE Clientes.Nit EQ PNit NO-LOCK NO-ERROR.
  IF Clientes.Id_ExoneradoSipla AND Clientes.FecIni_NoSipla LE W_Fecha AND
     Clientes.FecFin_NoSipla GE W_Fecha THEN DO:
     FIND LAST BorradorSipla WHERE
          BorradorSipla.Agencia     EQ W_Agencia AND
          BorradorSipla.Nit         EQ PNit      AND
          BorradorSipla.Fecha       EQ W_Fecha   AND
          BorradorSipla.CodAutoriza NE 0         AND
          BorradorSipla.Estado      EQ NO NO-ERROR.
     IF NOT AVAILABLE BorradorSipla THEN DO:
         CREATE BorradorSipla.
         ASSIGN BorradorSipla.Agencia        = W_Agencia                   
                BorradorSipla.Nit            = PNit                        
                BorradorSipla.Fecha          = W_Fecha                     
                BorradorSipla.Hora           = TIME                        
                BorradorSipla.Usuario        = W_Usuario                   
                BorradorSipla.CodAutoriza    = NEXT-VALUE(Sec_Autorizacion).
         IF PMen  NE "" THEN BorradorSipla.Id_Nud  = YES.
         IF PMenM NE "" THEN BorradorSipla.Id_Num  = YES.
     END. 
     ASSIGN BorradorSipla.Descripcion    = "[Exonerado del control, Reportado automatico]"
            BorradorSipla.Id_Exonerada   = YES
            CodAutoriza                  = BorradorSipla.CodAutoriza
            PCod                         = BorradorSipla.CodAutoriza
            BtnDone:LABEL                = "Autorizar".
  END.
  ELSE DO:
      FIND LAST BorradorSipla WHERE
           BorradorSipla.Agencia     EQ W_Agencia AND
           BorradorSipla.Nit         EQ PNit      AND
           BorradorSipla.Fecha       EQ W_Fecha   AND
           BorradorSipla.CodAutoriza NE 0         AND
           BorradorSipla.Estado      EQ NO NO-ERROR.
      IF AVAILABLE BorradorSipla THEN DO:
          ASSIGN CodAutoriza   = BorradorSipla.CodAutoriza
                 PCod          = BorradorSipla.CodAutoriza
                 BtnDone:LABEL = "Autorizar".
      END.
      ELSE BtnDone:LABEL = "Salir".
  END.
  RUN SUPER.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

