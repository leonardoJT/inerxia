&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-ubica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ubica 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  {incluido/variable.i "Shared"}.
  DEFINE VAR W_Deptoinf  AS CHAR FORMAT "X(2)".
  DEFINE VAR W_Ciudadinf AS CHAR FORMAT "X(3)".
  DEFINE VAR W_Barrioinf AS CHAR FORMAT "X(3)".

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
&Scoped-Define ENABLED-OBJECTS W_terminacon 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ubica AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_localizacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_localizacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON W_terminacon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Terminar Consulta" 
     SIZE 9 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla activa"
     FONT 4.

DEFINE BUTTON Btn_Aceptar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Ace&ptar" 
     SIZE 9.72 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)".

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Ca&ncelar" 
     SIZE 9.72 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla activa".

DEFINE VARIABLE W_RConsulta AS CHARACTER INITIAL "G" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "General", "G",
"Departamento", "D",
"Ciudad", "C",
"Barrio", "B"
     SIZE 20 BY 4.04
     BGCOLOR 17 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "C&onsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-55 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 55" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra información de sesión y mensajes del usuario activo".

DEFINE RECTANGLE RECT-231
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-55 AT ROW 1.81 COL 102
     Btn_Impresion AT ROW 3.42 COL 102 HELP
          "Permite Generar la Impresion de Ubicacion"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Permite Generar la consulta de Ubicacion"
     Btn_Ayuda AT ROW 20.38 COL 105 HELP
          "Permite obtener la Ayuda de esta pantalla"
     RECT-231 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.31
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-Impresion
     W_RConsulta AT ROW 1.54 COL 4 HELP
          "Seleccione el tipo de Ubicacion para listar" NO-LABEL
     Btn_Aceptar AT ROW 1.81 COL 31 HELP
          "Permite Generar la impresion de Ubicacion"
     Btn_Cancelar AT ROW 4.23 COL 31 HELP
          "Permite Salir de la impresion"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 68.29 ROW 6.31
         SIZE 43.57 BY 6.35
         BGCOLOR 17 FONT 5
         TITLE "Reporte de Localizaciones".

DEFINE FRAME F-Consulta
     W_terminacon AT ROW 15.27 COL 50 HELP
          "Permite Regresar a la Captura de Información"
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 15.54 COL 8
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 16.35 COL 8
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 15.54 COL 2
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 52.57 ROW 2.96
         SIZE 60.29 BY 17.08
         BGCOLOR 17 FONT 4
         TITLE BGCOLOR 7 "Consulta de Información".


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
  CREATE WINDOW W-ubica ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración de Localizaciones"
         HEIGHT             = 21.31
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.31
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.31
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
IF NOT W-ubica:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ubica 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ubica
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Impresion
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Impresion:HIDDEN           = TRUE
       FRAME F-Impresion:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ubica)
THEN W-ubica:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ubica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ubica W-ubica
ON END-ERROR OF W-ubica /* Configuración de Localizaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ubica W-ubica
ON WINDOW-CLOSE OF W-ubica /* Configuración de Localizaciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Impresion
&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar W-ubica
ON CHOOSE OF Btn_Aceptar IN FRAME F-Impresion /* Aceptar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  listado = W_PathSpl + "L-ubica.Lst".
  {incluido/Imprimir.I "listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ubica
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "ayudas\configur" CONTEXT 13.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Impresion
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-ubica
ON CHOOSE OF Btn_Cancelar IN FRAME F-Impresion /* Cancelar */
DO:
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  HIDE FRAME F-Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ubica
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Consulta.
  ENABLE ALL WITH FRAME F-Consulta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ubica
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Impresion.     
  ENABLE ALL WITH FRAME F-Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-55
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-55 W-ubica
ON CHOOSE OF BUTTON-55 IN FRAME F-Main /* Button 55 */
DO:
  RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Impresion
&Scoped-define SELF-NAME W_RConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RConsulta W-ubica
ON VALUE-CHANGED OF W_RConsulta IN FRAME F-Impresion
DO:
  ASSIGN W_RConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME W_terminacon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_terminacon W-ubica
ON CHOOSE OF W_terminacon IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ubica 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-ubica  _ADM-CREATE-OBJECTS
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
             INPUT  'b-cf_localizacion.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_localizacion ).
       RUN set-position IN h_b-cf_localizacion ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_localizacion ( 13.46 , 58.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_localizacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_localizacion ).
       RUN set-position IN h_v-cf_localizacion ( 9.35 , 14.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.92 , 62.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 10.15 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.96 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_localizacion. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_localizacion , 'Record':U , h_v-cf_localizacion ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_localizacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_localizacion ,
             W_terminacon:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_localizacion ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             h_v-cf_localizacion , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-ubica  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ubica  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ubica)
  THEN DELETE WIDGET W-ubica.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ubica  _DEFAULT-ENABLE
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
  ENABLE BUTTON-55 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-231 
      WITH FRAME F-Main IN WINDOW W-ubica.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE W_terminacon 
      WITH FRAME F-Consulta IN WINDOW W-ubica.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  DISPLAY W_RConsulta 
      WITH FRAME F-Impresion IN WINDOW W-ubica.
  ENABLE W_RConsulta Btn_Aceptar Btn_Cancelar 
      WITH FRAME F-Impresion IN WINDOW W-ubica.
  {&OPEN-BROWSERS-IN-QUERY-F-Impresion}
  VIEW W-ubica.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirDetalle W-ubica 
PROCEDURE ImprimirDetalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  W_DeptoInf =  SUBSTRING(Ubicacion.Ubicacion,1,2).
  W_Ciudadinf = SUBSTRING(Ubicacion.Ubicacion,3,3).
  W_barrioinf = SUBSTRING(Ubicacion.Ubicacion,6,3).    
  DISPLAY
    W_Deptoinf             AT 1 
    W_Ciudadinf            AT 20
    W_Barrioinf            AT 40  
    Ubicacion.Nombre       AT 60
  WITH WIDTH 132 FRAME USE-TEXT STREAM-IO NO-BOX NO-LABELS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-ubica 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 4.
 E_Fila      = "002" + "Dp"
             + "003" + "Ciu"
             + "003" + "Bar"
             + "020" + "Nombre              ".
    
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
DEFINE VAR tipo AS CHARACTER FORMAT "X(15)".
DEFINE VAR Est  AS CHARACTER FORMAT "X(8)".
    FOR EACH Ubicacion NO-LOCK:
      W_DeptoInf =  SUBSTRING(Ubicacion.Ubicacion,1,2).
      W_Ciudadinf = SUBSTRING(Ubicacion.Ubicacion,3,3).
      W_barrioinf = SUBSTRING(Ubicacion.Ubicacion,6,3).    
      E_Fila2     = "".
      E_Fila2     = "002" + STRING(W_Deptoinf,"X(2)")
                  + "003" + STRING(W_Ciudadinf,"X(3)")
                  + "003" + STRING(W_Barrioinf,"X(3)")
                  + "020" + STRING(Ubicacion.Nombre,"X(20)").
      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-ubica 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ubica 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Linea AS CHARACTER FORMAT "X(124)" INITIAL "".
  DEFINE VAR Raya AS CHARACTER INITIAL "_".
  
    DEFINE FRAME F-Encabezado
      HEADER
        "REDECOOP"          AT 1
        "Sistema de Informaci¢n Financiero Cooperativo"  AT 39
        "Pagina: "          AT 106 PAGE-NUMBER FORMAT ">>>9"
        "Listado de Departamentos,Ciudades y Barrios"  AT 40 
        "Fecha : "          AT 106
         TODAY              AT 114 FORMAT "99/99/9999" SKIP(1)
        "Departamento"      AT 1
        "Ciudad"            AT 20
        "Barrio"            AT 40  
        "Nombre"            AT 60
         Linea              AT 1  FORMAT "X(124)"
     WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

    DEFINE FRAME f-ftr
      HEADER 
        W_Nom_Agencia       AT  47 FORMAT "X(30)"
        "Hora: "            AT 108 STRING(TIME,"HH:MM:SS")
      WITH DOWN WIDTH 132 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    Linea = FILL(Raya,124).
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    IF W_RConsulta = "G" THEN
       FOR EACH Ubicacion NO-LOCK BY Ubicacion.Nombre:
           RUN ImprimirDetalle.
       END.  
    ELSE
       FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Tipo = W_RConsulta BY Ubicacion.Nombre:
           RUN ImprimirDetalle.
       END.
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-ubica  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-ubica 
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

