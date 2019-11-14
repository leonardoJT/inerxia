&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-entidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-entidad 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {incluido/Variable.i "SHARED"}.

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
&Scoped-Define ENABLED-OBJECTS Btn_Terminarcon 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-entidad AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_organizacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_organizacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Terminarcon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Terminar Consulta" 
     SIZE 6 BY 1.62.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mpresion" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la pantalla de Información de Sesión y mensajes del usuario activo".

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-2 AT ROW 1.54 COL 102
     Btn_Impresion AT ROW 3.15 COL 102 HELP
          "Permite generar la Impresion de Entidades"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite generar la consulta de Entidades"
     Btn_Ayuda AT ROW 20.65 COL 105 HELP
          "Permite generar la Ayuda de la pantalla"
     RECT-229 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.43 BY 21.08
         BGCOLOR 17 .

DEFINE FRAME F-Consulta
     Btn_Terminarcon AT ROW 9.62 COL 61 HELP
          "Terminar la consulta de Entidades"
     " Nota:" VIEW-AS TEXT
          SIZE 6 BY .77 AT ROW 9.62 COL 4
          FGCOLOR 7 FONT 5
     "Escoja el tipo de filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 47 BY .81 AT ROW 9.62 COL 10
     "digite la palabra clave que desea buscar. Luego presione ENTER." VIEW-AS TEXT
          SIZE 46 BY .5 AT ROW 10.42 COL 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 43.72 ROW 5.19
         SIZE 67.72 BY 11.38
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Entidades".


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
  CREATE WINDOW W-entidad ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Organizaciones"
         HEIGHT             = 21.08
         WIDTH              = 113.43
         MAX-HEIGHT         = 21.12
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.12
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
IF NOT W-entidad:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-entidad 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-entidad
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-entidad)
THEN W-entidad:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-entidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-entidad W-entidad
ON END-ERROR OF W-entidad /* SFG - Configuración de Organizaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-entidad W-entidad
ON WINDOW-CLOSE OF W-entidad /* SFG - Configuración de Organizaciones */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-entidad
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONFIGUR" CONTEXT 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-entidad
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Consulta.
  ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-entidad
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Impresion */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  
  Listado = w_Pathspl + "L-ENTIDA.lst".
  {incluido\IMPRIMIR.I "listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Terminarcon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminarcon W-entidad
ON CHOOSE OF Btn_Terminarcon IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-entidad
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-entidad 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-entidad  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_organizacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_organizacion ).
       RUN set-position IN h_v-cf_organizacion ( 1.27 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.46 , 96.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_organizacion.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_organizacion ).
       RUN set-position IN h_b-cf_organizacion ( 1.27 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_organizacion ( 7.81 , 64.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 10.69 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.15 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_organizacion. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_organizacion , 'Record':U , h_v-cf_organizacion ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_organizacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_organizacion ,
             BUTTON-2:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_organizacion ,
             Btn_Terminarcon:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-entidad  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-entidad  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-entidad)
  THEN DELETE WIDGET W-entidad.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-entidad  _DEFAULT-ENABLE
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
  ENABLE BUTTON-2 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-229 
      WITH FRAME F-Main IN WINDOW W-entidad.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Terminarcon 
      WITH FRAME F-Consulta IN WINDOW W-entidad.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-entidad.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-entidad 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 4.
 E_Fila      = "003Ent012NitInsti.   030Nombre                        002CC".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
 FOR EACH Entidad NO-LOCK:
      IF Entidad.Id_CenCosto THEN
          W_IdCC = "Si".
      ELSE
          W_IdCC = "No".
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Entidad.Entidad,"999")
                  + "012" + STRING(Entidad.Nit,"X(12)")
                  + "030" + STRING(Entidad.Nombre,"X(30)")
                  + "003" + STRING(W_IdCC,"X(2)").
      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-entidad 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-entidad 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

  DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
  DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".

  W_Reporte   = "REPORTE   : ENTIDADES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 /*                 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
  W_EncColumna = "ENT NIT          NOMBRE                          CC".
  
  DEFINE FRAME F-Datosinf
    Entidad.Entidad             AT 1
    Entidad.Nit   AT 5
    Entidad.Nombre              AT 18 FORMAT "X(30)"
    Nom_Cencosto                AT 50 FORMAT "X(15)"
  WITH DOWN WIDTH 132.
     
          
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Entidad NO-LOCK:
      Nom_Cencosto = "".
      IF Entidad.Id_CenCosto EQ YES THEN
        Nom_Cencosto = "Si".
      ELSE
        Nom_Cencosto = "No".
      IF Entidad.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo".         
      DISPLAY Entidad.Entidad
              Entidad.Nit
              Entidad.Nombre
              Nom_Cencosto
      WITH WIDTH 132 FRAME F-Datosinf USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-entidad  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-entidad 
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

