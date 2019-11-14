&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-zona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-zona 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {incluido/variable.i "SHARED"}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_termconsulta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-zona AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_zonas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_zonas AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_termconsulta 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y vuelva a la pantalla anterior".

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-56 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 56" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra información de sesión y mensajes del usuario activo".

DEFINE RECTANGLE RECT-222
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-56 AT ROW 1.81 COL 102
     Btn_Impresion AT ROW 3.42 COL 102 HELP
          "Permite Generar e Imprimir informes"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Permite la Consulta de Informaci¢n de Zonas"
     Btn_Ayuda AT ROW 20.38 COL 105 HELP
          "Muestra la Ayuda pertinente a la pantalla"
     RECT-222 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.35
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-Consulta
     Btn_termconsulta AT ROW 11.5 COL 34 HELP
          "Termina la consulta de zonas"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 71.29 ROW 6.77
         SIZE 41.86 BY 13.04
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Zonas".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-zona ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración de Zonas"
         HEIGHT             = 21.35
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.35
         VIRTUAL-WIDTH      = 114.29
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
IF NOT W-zona:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-zona 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-zona
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-zona)
THEN W-zona:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-zona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-zona W-zona
ON END-ERROR OF W-zona /* Configuración de Zonas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-zona W-zona
ON WINDOW-CLOSE OF W-zona /* Configuración de Zonas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-zona
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "ayudas\configur" CONTEXT 11.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-zona
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
    DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
    VIEW FRAME F-Consulta.
    ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-zona
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_Pathspl + "L-Zona.lst".
    {incluido/imprimir.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_termconsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_termconsulta W-zona
ON CHOOSE OF Btn_termconsulta IN FRAME F-Consulta /* Terminar Consulta */
DO:
    HIDE FRAME F-Consulta.
    ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-56
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-56 W-zona
ON CHOOSE OF BUTTON-56 IN FRAME F-Main /* Button 56 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-zona 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */ 


{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-zona  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_zonas.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_zonas ).
       RUN set-position IN h_b-cf_zonas ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_zonas ( 9.96 , 39.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_zonas.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_zonas ).
       RUN set-position IN h_v-cf_zonas ( 9.35 , 29.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.65 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.62 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.23 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_zonas. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_zonas , 'Record':U , h_v-cf_zonas ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_zonas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_zonas ,
             Btn_termconsulta:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_zonas ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             h_v-cf_zonas , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-zona  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-zona  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-zona)
  THEN DELETE WIDGET W-zona.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-zona  _DEFAULT-ENABLE
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
  ENABLE BUTTON-56 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-222 
      WITH FRAME F-Main IN WINDOW W-zona.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_termconsulta 
      WITH FRAME F-Consulta IN WINDOW W-zona.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-zona.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-zona 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-zona 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Linea AS CHARACTER FORMAT "X(86)" INITIAL "".
  DEFINE VAR Raya AS CHARACTER INITIAL "_".

  DEFINE FRAME F-Informe
    Zonas.Cod_zona         AT 1 
    Zonas.Nombre           AT 10
  WITH DOWN WIDTH 100.
     
    DEFINE FRAME F-Encabezado
     HEADER
      "COOPERATIVA BELEN"             AT 1
      "Pagina: "             AT 68 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
      "Listado de Zonas"     AT 35
      "Fecha : "             AT 68
      TODAY                  AT 76 FORMAT "99/99/9999" SKIP(1)
      "Zona"                AT 1
      "Nombre de Zona"      AT 10
       Linea                AT 1  FORMAT "X(50)"
     WITH DOWN WIDTH 100 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.          
  
    DEFINE FRAME f-ftr
      HEADER 
      W_Nom_Agencia    AT 28 FORMAT "X(30)"
      "Hora: "         AT 72 STRING(TIME,"HH:MM:SS")
    WITH DOWN WIDTH 100 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    Linea = FILL(Raya,50).
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Zonas NO-LOCK:
      DISPLAY Zonas.Cod_zona
              Zonas.Nombre
      WITH WIDTH 100 FRAME F-Informe USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-zona  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-zona 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

