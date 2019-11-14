&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-progra 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
   DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
   DEFINE SHARED VAR W_Manija         AS   HANDLE.
   DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.
   DEFINE VAR W_Rpta                  AS LOGICAL.
   DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.
   DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0.


DEFINE VAR W_Alm AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cencosto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-progra AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_cencostos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_cencostos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cencosto 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Terminar Consulta" 
     SIZE 9 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior"
     FONT 4.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4.43 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "I&mpresión" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 1" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantallade información de Sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.54 COL 103
     Btn_Impresion AT ROW 3.15 COL 103 HELP
          "Permite Generar la Impresion de Centro de Costos"
     Btn_Consulta AT ROW 4.77 COL 103 HELP
          "Permite Generar la Consulta de Centro de Costos"
     Btn_Ayuda AT ROW 20.12 COL 106 HELP
          "Permite Obtener la ayuda de esta Pantalla"
     RECT-274 AT ROW 1.27 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 114 BY 21.23
         BGCOLOR 17 .

DEFINE FRAME F-Consulta
     Btn_Cencosto AT ROW 12.31 COL 55 HELP
          "Termina la consulta de Centro de Costos"
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 12.31 COL 4
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la búsqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 12.31 COL 9
          FGCOLOR 0 FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 13.12 COL 9
          FGCOLOR 0 FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23.72 ROW 2.62
         SIZE 65.29 BY 13.88
         BGCOLOR 17 FGCOLOR 7 FONT 5
         TITLE "Consulta de Centro de Costos".


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
  CREATE WINDOW W-progra ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Centros de Costos"
         HEIGHT             = 21.27
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-progra 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-progra
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
THEN W-progra:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON END-ERROR OF W-progra /* SFG - Configuración de Centros de Costos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON WINDOW-CLOSE OF W-progra /* SFG - Configuración de Centros de Costos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. *//*
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-progra
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONFIGUR" CONTEXT 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Cencosto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cencosto W-progra
ON CHOOSE OF Btn_Cencosto IN FRAME F-Consulta /* Terminar Consulta */
DO:    
       
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  HIDE FRAME F-Consulta.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-progra
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
    DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
    ENABLE ALL WITH FRAME F-Consulta.
    VIEW FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-progra
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Impresión */
DO:
        /*
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_CENCOS.Lst".
  {incluido\ImpMil.i "Listado" Tamano}
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-progra
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  /* RUN W-Infdia.r. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-progra 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-progra  _ADM-CREATE-OBJECTS
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
             INPUT  'b-cf_cencostos.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_cencostos ).
       RUN set-position IN h_b-cf_cencostos ( 1.27 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_cencostos ( 10.50 , 61.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_cencostos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_cencostos ).
       RUN set-position IN h_v-cf_cencostos ( 8.81 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.77 , 74.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.62 , 102.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.23 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_cencostos. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_cencostos , 'Record':U , h_v-cf_cencostos ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_cencostos ).

       /* Links to SmartPanel h_P-UPDSAV. */
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'costo':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_cencostos ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             h_v-cf_cencostos , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-progra  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-progra  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
  THEN DELETE WIDGET W-progra.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-progra  _DEFAULT-ENABLE
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
  ENABLE RECT-274 BUTTON-1 Btn_Impresion Btn_Consulta Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Cencosto 
      WITH FRAME F-Consulta IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW W-progra.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-progra 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 4.
 E_Fila      = "003Age003CCo040Nombre                                  010Estado    ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 
/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 DEFI VAR W_est AS CHARACTER FORMAT "X(10)".
 FOR EACH Cen_costos NO-LOCK:
      IF Cen_Costos.Estado EQ 1 THEN
          W_Est = "Activo".
      ELSE
          W_Est = "Inactivo".
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Cen_Costos.Agencia,"999")
                  + "003" + STRING(Cen_Costos.Cen_Costo,"999")
                  + "040" + STRING(Cen_Costos.Nombre,"X(40)")
                  + "010" + STRING(W_Est,"X(10)").
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-progra 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-progra 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
def var w_alm as char.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    /* 
    FIND FIRST Entidad NO-LOCK NO-ERROR.
     IF NOT Entidad.Id_CenCosto THEN DO:
        RUN get-link-handle IN adm-broker-hdl
           (INPUT THIS-PROCEDURE,
            INPUT 'costo':U,
            OUTPUT W_Alm).
        RUN local-deshabilita IN WIDGET-HANDLE(W_Alm).
     END.  
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-progra 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    

    W_Reporte    = "REPORTE   : CENTROS DE COSTO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =       "AGENCIA  CC.   NOMBRE                         ESTADO".


  DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "".  

  DEFINE FRAME F-Datosinf
    Cen_Costos.Agencia       AT 2
    Cen_Costos.Cen_Costos    AT 10
    Cen_Costos.Nombre        AT 16 
    W_Estadoinf              AT 47
  WITH DOWN WIDTH 90.       

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    
    FOR EACH Cen_costos NO-LOCK:
      IF Cen_Costos.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo".
      DISPLAY Cen_Costos.Agencia
              Cen_Costos.Cen_Costos
              Cen_Costos.Nombre    FORMAT "X(30)"
              W_EstadoInf
      WITH WIDTH 90 FRAME F-Datosinf USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-progra  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-progra 
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

