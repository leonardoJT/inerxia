&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE TEMP-TABLE HVida LIKE Hoja_Vida.
DEFINE VARIABLE W_Cr AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_HojaVida

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-281 RECT-282 BUTTON-139 ED_Observacion ~
Btn_Ingresar Btn_Salvar Btn_Cancelar Btn_Salir BUTTON-137 
&Scoped-Define DISPLAYED-FIELDS Hoja_Vida.Nit Hoja_Vida.Usuario ~
Hoja_Vida.Asunto_Cumplido Hoja_Vida.DoctoRefer Hoja_Vida.Fec_Grabacion 
&Scoped-define DISPLAYED-TABLES Hoja_Vida
&Scoped-define FIRST-DISPLAYED-TABLE Hoja_Vida
&Scoped-Define DISPLAYED-OBJECTS Cmb_Codigos NomNit NomUsuario HV_Hora ~
ED_Observacion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_Codigos Hoja_Vida.Nit Hoja_Vida.Asunto_Cumplido ~
Hoja_Vida.DoctoRefer 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 12 BY 1.35.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 12 BY 1.35.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 12 BY 1.35.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.38.

DEFINE BUTTON BUTTON-137 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 137" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-139 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 139" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Cmb_Codigos AS CHARACTER FORMAT "X(50)":U INITIAL "00000 - No Asignado" 
     LABEL "Código" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 61 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ED_Observacion AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP MAX-CHARS 300 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 60.86 BY 5.96
     BGCOLOR 15 .

DEFINE VARIABLE HV_Hora AS CHARACTER FORMAT "X(15)":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 7.54.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 2.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_HojaVida
     Cmb_Codigos AT ROW 1.27 COL 8 COLON-ALIGNED
     BUTTON-139 AT ROW 1.54 COL 74
     Hoja_Vida.Nit AT ROW 2.35 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     NomNit AT ROW 2.35 COL 22 COLON-ALIGNED NO-LABEL
     Hoja_Vida.Usuario AT ROW 3.31 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     NomUsuario AT ROW 3.31 COL 22 COLON-ALIGNED NO-LABEL
     Hoja_Vida.Asunto_Cumplido AT ROW 4.23 COL 10 HELP
          ""
          LABEL "Cumplido"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     Hoja_Vida.DoctoRefer AT ROW 4.23 COL 56.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.43 BY .81
          BGCOLOR 15 
     Hoja_Vida.Fec_Grabacion AT ROW 5.19 COL 56.72 COLON-ALIGNED
          LABEL "Fecha de Grabación"
          VIEW-AS FILL-IN 
          SIZE 12.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     HV_Hora AT ROW 6.12 COL 56.72 COLON-ALIGNED
     ED_Observacion AT ROW 7.73 COL 10 HELP
          "Observaciones al asunto referenciado" NO-LABEL
     Btn_Ingresar AT ROW 8.15 COL 74
     Btn_Salvar AT ROW 9.5 COL 74
     Btn_Cancelar AT ROW 10.88 COL 74
     Btn_Salir AT ROW 12.23 COL 74
     BUTTON-137 AT ROW 13.77 COL 78
     "Observación" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 6.92 COL 10
          FGCOLOR 7 
     RECT-281 AT ROW 7.73 COL 73
     RECT-282 AT ROW 1.27 COL 73
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.29 BY 14.54
         BGCOLOR 17 FONT 5.


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
         TITLE              = "SFG - Actualización de la Hoja de Vida"
         COLUMN             = 19
         ROW                = 6.42
         HEIGHT             = 14.54
         WIDTH              = 87.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
/* SETTINGS FOR FRAME F_HojaVida
                                                                        */
/* SETTINGS FOR TOGGLE-BOX Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR COMBO-BOX Cmb_Codigos IN FRAME F_HojaVida
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Hoja_Vida.DoctoRefer IN FRAME F_HojaVida
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Hoja_Vida.Fec_Grabacion IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN HV_Hora IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Nit IN FRAME F_HojaVida
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN NomNit IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Usuario IN FRAME F_HojaVida
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _Query            is NOT OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Actualización de la Hoja de Vida */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Actualización de la Hoja de Vida */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_HojaVida /* Cancelar */
DO:
  DO WITH FRAME F_HojaVida:
     DISABLE Btn_Salvar {&List-1}.
     RUN Inicializar_Campos.
     IF AVAILABLE HVida THEN
       ASSIGN Hoja_Vida.Usuario                    = HVida.Usuario
              Hoja_Vida.Asunto_Cumplido            = HVida.Asunto_Cumplido
              Hoja_Vida.Nit                        = HVida.Nit
              Hoja_Vida.Observacion                = HVida.Observacion
              Hoja_Vida.Fec_Grabacion              = HVida.Fec_Grabacion
              Hoja_Vida.Hora                       = HVida.Hora
              Hoja_Vida.DoctoRefer                 = HVida.DoctoRefer
              Hoja_Vida.Codigo                     = HVida.Codigo
              Hoja_Vida.Tipo                       = HVida.Tipo.
     FOR EACH HVida: DELETE HVida. END.
     ENABLE Btn_Ingresar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_HojaVida /* Ingresar */
DO:
  DO WITH FRAME F_HojaVida:
     W_Cr = YES.
     IF AVAILABLE Hoja_Vida THEN BUFFER-COPY Hoja_Vida TO HVida.
     RUN Inicializar_Campos.
     IF W_NitGlobal NE "" THEN DO:
        ASSIGN Hoja_vida.Nit:SCREEN-VALUE = W_NitGlobal.
        FIND Clientes WHERE Clientes.Nit EQ W_NitGlobal NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN 
           NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     END.
     ENABLE {&List-1} Btn_Salvar.
     DISABLE Btn_Ingresar.
     APPLY "entry" TO Cmb_Codigos.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_HojaVida /* Salir */
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


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_HojaVida /* Salvar */
DO:
  DO WITH FRAME F_HojaVida:
   IF Ed_Observacion:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "El crear un registro de la hoja de vida" SKIP
              "exige entrar la observación que se le" SKIP
              "hará a esta. entre la Observación!!!" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO Ed_Observacion.
      RETURN NO-APPLY.
   END.
   IF W_Cr THEN DO: 
     CREATE Hoja_Vida.
     W_Cr = NO.
   END.

   ASSIGN Hoja_Vida.Usuario                    = Hoja_Vida.Usuario:SCREEN-VALUE
          Hoja_Vida.Asunto_Cumplido           
          Hoja_Vida.Nit                        = Hoja_Vida.Nit:SCREEN-VALUE
          Hoja_Vida.Observacion                = Ed_Observacion:SCREEN-VALUE
          Hoja_Vida.Fec_Grabacion              = DATE(Hoja_Vida.Fec_Grabacion:SCREEN-VALUE)
          Hoja_Vida.Hora_Grabacion             = TIME
          Hoja_Vida.DoctoRefer                 = DECIMAL(Hoja_Vida.DoctoRefer:SCREEN-VALUE)
          Hoja_Vida.Codigo                     = INTEGER(SUBSTRING(Cmb_Codigos:SCREEN-VALUE,1,5))
          Hoja_Vida.Tipo                       = 18.
    
    DISABLE Btn_Salvar.
    ENABLE Btn_Ingresar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-139
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-139 wWin
ON CHOOSE OF BUTTON-139 IN FRAME F_HojaVida /* Button 139 */
DO:
  RUN W-InfDia.R NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_Observacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_Observacion wWin
ON LEAVE OF ED_Observacion IN FRAME F_HojaVida /* Observación */
DO:
  ASSIGN Ed_Observacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Hoja_Vida.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Nit wWin
ON LEAVE OF Hoja_Vida.Nit IN FRAME F_HojaVida /* Nit */
DO:
  DO WITH FRAME F_HojaVida:
     FIND Clientes WHERE Clientes.Nit EQ Hoja_Vida.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        MESSAGE "No existe algún cliente con este número" SKIP
                "de indicación. Se cancela la operación" VIEW-AS ALERT-BOX WARNING.
        APPLY "choose" TO Btn_Cancelar.
     END.
  END.
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
  DISPLAY Cmb_Codigos NomNit NomUsuario HV_Hora ED_Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Nit Hoja_Vida.Usuario Hoja_Vida.Asunto_Cumplido 
          Hoja_Vida.DoctoRefer Hoja_Vida.Fec_Grabacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE RECT-281 RECT-282 BUTTON-139 ED_Observacion Btn_Ingresar Btn_Salvar 
         Btn_Cancelar Btn_Salir BUTTON-137 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos wWin 
PROCEDURE Inicializar_Campos :
DO WITH FRAME F_HojaVida:
   ASSIGN Hoja_Vida.Usuario:SCREEN-VALUE         = W_Usuario
          Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no"
          Hoja_Vida.Nit:SCREEN-VALUE             = ""
          Ed_Observacion:SCREEN-VALUE            = ""
          Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(TODAY)
          HV_Hora:SCREEN-VALUE                   = STRING(TIME,"hh:mm:ss am")
          Hoja_Vida.DoctoRefer:SCREEN-VALUE      = "0"
          Cmb_Codigos:SCREEN-VALUE               = "00000 - No Asignado"
          NomNit:SCREEN-VALUE                    = "".
   FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_HojaVida:
    FOR EACH Varios WHERE Varios.Tipo EQ 18 NO-LOCK:
      W_Ok = Cmb_Codigos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
    END.
  END.
  RUN SUPER.
  IF W_NitGlobal NE "" THEN 
     APPLY "choose" TO Btn_Ingresar.
  ELSE DO:
    /*FIND FIRST Hoja_Vida WHERE Hoja_Vida.Tipo EQ 18 NO-ERROR.
    IF AVAILABLE Hoja_Vida THEN RUN Mostrar_Registro.
    ELSE*/
    RUN Inicializar_Campos.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
DO WITH FRAME F_HojaVida:
   ASSIGN Hoja_Vida.Usuario:SCREEN-VALUE         = Hoja_Vida.Usuario
          Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido)
          Hoja_Vida.Nit:SCREEN-VALUE             = Hoja_Vida.Nit
          ED_Observacion:SCREEN-VALUE            = Hoja_Vida.Observacion
          Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(Hoja_Vida.Fec_Grabacion)
          HV_Hora:SCREEN-VALUE                   = STRING(Hoja_Vida.Hora,"hh:mm:ss am")
          Hoja_Vida.DoctoRefer:SCREEN-VALUE      = STRING(Hoja_Vida.DoctoRefer).
   FIND Varios WHERE Varios.Tipo EQ 18 AND Varios.Codigo EQ Hoja_Vida.Codigo NO-LOCK NO-ERROR.
   IF AVAILABLE Varios THEN Cmb_Codigos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
   
   FIND Clientes WHERE Clientes.Nit EQ Hoja_Vida.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN
      NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      
   FIND Usuarios WHERE Usuarios.Usuario EQ Hoja_Vida.Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

