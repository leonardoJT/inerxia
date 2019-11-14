&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Indicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Indicadores 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE SHARED VAR W_Nom_Agencia AS CHARACTER FORMAT "X(50)".
  DEFINE SHARED VAR W_Nom_Entidad AS CHARACTER FORMAT "X(50)".
  DEFINE NEW SHARED VAR W_Ind     LIKE Indicadores.Indicador.
  DEFINE NEW SHARED VAR W_Rango   LIKE Indicadores.Rango.
  DEFINE NEW SHARED VAR W_Est     LIKE Indicadores.Estado.
  DEFINE NEW SHARED VAR W_Fec     LIKE Indicadores.Fecha.
  DEFINE NEW SHARED VAR W_FecVcto LIKE Indicadores.FecVcto.
  DEFINE SHARED VAR W_Agencia     LIKE Agencias.Agencia.
  DEFINE SHARED VAR W_Pathspl     LIKE Entidad.Dir_Spl.
  DEFINE SHARED VAR W_Manija      AS HANDLE.
  DEFINE VAR W_Ventana            AS CHARACTER.

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
DEFINE VAR W-Indicadores AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_B-CF_Indicadores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-CF_RanInteres AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-CF_indicadores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-CF_RanInteres AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Terminarcon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y muestra la pantalla anterior".

DEFINE BUTTON Btn-Rangos 
     LABEL "Rangos" 
     SIZE 10 BY 1.65.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mpresion" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-51 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 51" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de inicio de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-233
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.

DEFINE BUTTON Btn-SalirRang 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir &Rangos" 
     SIZE 10 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-51 AT ROW 1.54 COL 102
     Btn_Impresion AT ROW 3.15 COL 102 HELP
          "Permite Generar la Impresion de Indicadores"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite Generar la Consulta de Indicadores"
     Btn-Rangos AT ROW 8 COL 102
     Btn_Ayuda AT ROW 20.38 COL 105 HELP
          "Permite Obtener la ayuda de esta Pantalla"
     RECT-233 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 113.86 BY 21.27
         BGCOLOR 17 .

DEFINE FRAME F-Rangos
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5.57 ROW 7.92
         SIZE 90.29 BY 13.88
         BGCOLOR 17 FONT 4
         TITLE BGCOLOR 18 FGCOLOR 15 "Rangos de Intereses para este Indicador".

DEFINE FRAME F-RanInt
     Btn-SalirRang AT ROW 8.54 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.14 ROW 10.31
         SIZE 92.57 BY 10.23
         BGCOLOR 17 FONT 5
         TITLE "Mantenimiento Rangos de Intereses".

DEFINE FRAME F-Consulta
     Btn_Terminarcon AT ROW 15 COL 57 HELP
          "Termina la consulta de Indicadores"
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 15 COL 10
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 15.81 COL 10
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 15 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 47.14 ROW 3.38
         SIZE 64.43 BY 16.85
         BGCOLOR 17 FONT 4
         TITLE BGCOLOR 18 FGCOLOR 15 "Consulta de Indicadores".


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
  CREATE WINDOW W-Indicadores ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración de Indicadores"
         HEIGHT             = 21.38
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
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
IF NOT W-Indicadores:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Indicadores 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Indicadores
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON Btn-Rangos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F-Rangos
                                                                        */
/* SETTINGS FOR FRAME F-RanInt
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-RanInt:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Indicadores)
THEN W-Indicadores:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Indicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Indicadores W-Indicadores
ON END-ERROR OF W-Indicadores /* Configuración de Indicadores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Indicadores W-Indicadores
ON WINDOW-CLOSE OF W-Indicadores /* Configuración de Indicadores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn-Rangos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Rangos W-Indicadores
ON CHOOSE OF Btn-Rangos IN FRAME F-Main /* Rangos */
DO:
  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ W_Ind
                           AND Indicadores.Rangos EQ TRUE 
                           AND Indicadores.Estado EQ W_Est NO-LOCK NO-ERROR.
  IF AVAILABLE(Indicadores) THEN DO:                          
     FRAME F-Main:SENSITIVE = FALSE.  
     VIEW FRAME F-RanInt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-RanInt
&Scoped-define SELF-NAME Btn-SalirRang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-SalirRang W-Indicadores
ON CHOOSE OF Btn-SalirRang IN FRAME F-RanInt /* Salir Rangos */
DO:
  HIDE FRAME F-RanInt.
  FRAME F-Main:SENSITIVE = TRUE.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinVI':U,OUTPUT W_Ventana).
  RUN P-LlegaRango IN WIDGET-HANDLE (W_Ventana).
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinB':U,OUTPUT W_Ventana).
  RUN P-HabRangos IN WIDGET-HANDLE (W_Ventana) (INPUT FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Indicadores
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 26.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Indicadores
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
  VIEW FRAME F-Consulta.
  ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-Indicadores
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Impresion */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  listado = W_PathSpl + "L_Indica.Lst".
  {incluido/Imprimir.I "listado"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Terminarcon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminarcon W-Indicadores
ON CHOOSE OF Btn_Terminarcon IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-51
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-51 W-Indicadores
ON CHOOSE OF BUTTON-51 IN FRAME F-Main /* Button 51 */
DO:
  RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Indicadores 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */ 

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Indicadores  _ADM-CREATE-OBJECTS
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
             INPUT  'B-CF_RanInteres.w':U ,
             INPUT  FRAME F-Rangos:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-CF_RanInteres ).
       RUN set-position IN h_b-CF_RanInteres ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-CF_RanInteres ( 12.38 , 88.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-CF_RanInteres.w':U ,
             INPUT  FRAME F-RanInt:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-CF_RanInteres ).
       RUN set-position IN h_v-CF_RanInteres ( 1.27 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.27 , 90.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-CF_Indicadores.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-CF_indicadores ).
       RUN set-position IN h_v-CF_indicadores ( 1.81 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.38 , 90.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'B-CF_Indicadores.W':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_B-CF_Indicadores ).
       RUN set-position IN h_B-CF_Indicadores ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_B-CF_Indicadores ( 13.19 , 62.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.88 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.96 , 12.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-CF_RanInteres. */
       RUN add-link IN adm-broker-hdl ( h_v-CF_indicadores , 'Record':U , h_b-CF_RanInteres ).
       RUN add-link IN adm-broker-hdl ( h_b-CF_RanInteres , 'LinkWinB':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v-CF_RanInteres. */
       RUN add-link IN adm-broker-hdl ( h_b-CF_RanInteres , 'LinkWinBB':U , h_v-CF_RanInteres ).
       RUN add-link IN adm-broker-hdl ( h_b-CF_RanInteres , 'LinkWinR':U , h_v-CF_RanInteres ).
       RUN add-link IN adm-broker-hdl ( h_b-CF_RanInteres , 'Record':U , h_v-CF_RanInteres ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'LinkWinVB':U , h_v-CF_RanInteres ).
       RUN add-link IN adm-broker-hdl ( h_v-CF_RanInteres , 'LinkWinVR':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v-CF_indicadores. */
       RUN add-link IN adm-broker-hdl ( h_B-CF_Indicadores , 'Record':U , h_v-CF_indicadores ).
       RUN add-link IN adm-broker-hdl ( h_b-CF_RanInteres , 'LinkWinV':U , h_v-CF_indicadores ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-CF_indicadores ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'LinkWinW':U , h_v-CF_indicadores ).
       RUN add-link IN adm-broker-hdl ( h_v-CF_indicadores , 'LinkWinVI':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-CF_RanInteres ,
             Btn-SalirRang:HANDLE IN FRAME F-RanInt , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-CF_indicadores ,
             BUTTON-51:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_B-CF_Indicadores ,
             Btn_Terminarcon:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn-Rangos:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Indicadores  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Indicadores  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Indicadores)
  THEN DELETE WIDGET W-Indicadores.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Indicadores  _DEFAULT-ENABLE
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
  ENABLE BUTTON-51 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-233 
      WITH FRAME F-Main IN WINDOW W-Indicadores.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Terminarcon 
      WITH FRAME F-Consulta IN WINDOW W-Indicadores.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  VIEW FRAME F-Rangos IN WINDOW W-Indicadores.
  {&OPEN-BROWSERS-IN-QUERY-F-Rangos}
  ENABLE Btn-SalirRang 
      WITH FRAME F-RanInt IN WINDOW W-Indicadores.
  {&OPEN-BROWSERS-IN-QUERY-F-RanInt}
  VIEW W-Indicadores.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Indicadores 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 8.
 E_Fila      =      "005" + "Cod  "
                  + "030" + "Nombre                        "
                  + "015" + "Valor          "
                  + "006" + "Tasa  "
                  + "015" + "Base           "
                  + "010" + "Fecha     "
                  + "010" + "FecVcto   "
                  + "008" + "Est.Inf ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

    FOR EACH Indicadores NO-LOCK:
        W_Estadoinf = "".
        IF Indicadores.Estado = 1 THEN
           ASSIGN W_EstadoInf = "Activo".
        ELSE
           ASSIGN W_EstadoInf = "Inactivo". 
      E_Fila2     = "".
      E_Fila2     = "005" + STRING(Indicadores.Indicador,"99999")
                  + "030" + STRING(Indicadores.Nombre,"X(30)")
                  + "015" + STRING(Indicadores.Valor,">>>,>>>,>>>,>>9")
                  + "006" + STRING(Indicadores.Tasa,">>>.99")
                  + "015" + STRING(Indicadores.Base,">>>,>>>,>>>,>>9")
                  + "010" + STRING(Indicadores.Fecha,"99/99/9999")
                  + "010" + STRING(Indicadores.FecVcto,"99/99/9999")
                  + "008" + STRING(W_EstadoInf,"X(8)").

      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Indicadores 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE P-DesHRango W-Indicadores 
PROCEDURE P-DesHRango :
/*------------------------------------------------------------------------------
  Purpose:     Recibe de v-CF_Indicadores para habilitar o deshab.Btn-Rangos.
  Parameters:  <none>
  Notes:       Agregó Pcto GAER el 22 septbre/98. 
------------------------------------------------------------------------------*/
  Btn-Rangos:SENSITIVE IN FRAME F-Main = FALSE.
  FIND FIRST Indicadores WHERE Indicadores.Rangos EQ TRUE
                           AND Indicadores.Indicador EQ W_Ind 
                           AND Indicadores.Estado EQ W_Est NO-LOCK NO-ERROR.
  IF AVAILABLE(Indicadores) THEN               
     Btn-Rangos:SENSITIVE IN FRAME F-Main = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE P-IngRangos W-Indicadores 
PROCEDURE P-IngRangos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME F-MAIN:SENSITIVE = FALSE.
    VIEW FRAME F-RanInt.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinVR':U,OUTPUT W_Ventana).
    RUN P-VRangos IN WIDGET-HANDLE (W_Ventana).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Indicadores 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  GAER   -  Se estandarizó informe el 22 septbre/98  
------------------------------------------------------------------------------*/
  DEFINE VAR Linea         AS CHARACTER FORMAT "X(110)" INITIAL "".
  DEFINE VAR W_EstadoInf   AS CHARACTER FORMAT "X(8)"   INITIAL "".
  DEFINE VAR Raya          AS CHARACTER INITIAL "_".

  DEFINE FRAME F-Datosinf
    Indicadores.Indicador     AT 1   FORMAT "99999"
    Indicadores.Nombre     AT 8   FORMAT "X(30)"
    Indicadores.Valor      AT 40  FORMAT ">>>,>>>,>>>,>>9"
    Indicadores.Tasa       AT 57  FORMAT ">>>.99"
    Indicadores.Base       AT 64  FORMAT ">>>,>>>,>>>,>>9"
    Indicadores.Fecha      AT 81  FORMAT "99/99/9999"
    Indicadores.FecVcto    AT 92  FORMAT "99/99/9999"
    W_EstadoInf            AT 103 FORMAT "X(8)"
  WITH DOWN WIDTH 132.
     
  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad            AT 40
      "Página: "               AT 92 PAGE-NUMBER FORMAT ">>>9" SKIP
      "Informe de Indicadores" AT 40 SKIP(1)
      "Código"                 AT 1
      "Nombre"                 AT 8
      "Valor Indicador"        AT 40
      "Tasa"                   AT 59
      "Valor de Base"          AT 66
      "Fecha Inic"             AT 81
      "Fecha Vcto"             AT 92
      "Estado"                 AT 103 SKIP
       Linea                   AT 1  FORMAT "X(110)"
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
          
 DEFINE FRAME F-Ftr
    HEADER 
      "Fecha:"      AT  3
      TODAY         AT 10 FORMAT "99/99/9999"
      W_Nom_Agencia AT 35 FORMAT "X(50)"
      "Hora: "      AT 92 STRING(TIME,"HH:MM:SS")
    WITH DOWN WIDTH 132 FRAME F-Ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    Linea = FILL(Raya,110).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    FOR EACH Indicadores NO-LOCK:
        W_Estadoinf = "".
        IF Indicadores.Estado = 1 THEN
           ASSIGN W_EstadoInf = "Activo".
        ELSE
           ASSIGN W_EstadoInf = "Inactivo".         
        DISPLAY Indicadores.Indicador
                Indicadores.Nombre
                Indicadores.Valor 
                Indicadores.Tasa
                Indicadores.Base
                Indicadores.Fecha 
                Indicadores.FecVcto
                W_EstadoInf
        WITH WIDTH 132 FRAME F-Datosinf USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Indicadores  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Indicadores 
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

