&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-plan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-plan 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.I "SHARED"}

  DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
  DEFINE VAR Linea     AS CHARACTER.
  DEFINE VAR Linea2    AS CHARACTER.
  DEFINE VAR Linea3    AS CHARACTER.
  DEFINE VAR Linea4    AS CHARACTER.
  DEFINE VAR Linea5    AS CHARACTER.
  DEFINE VAR Linea6    AS CHARACTER.
  DEFINE VAR Linea7    AS CHARACTER.
  DEFINE VAR Linea8    AS CHARACTER.
  DEFINE VAR Raya      AS CHARACTER INITIAL "-".
  DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
  DEFINE VAR Op_salida AS CHARACTER INITIAL "P".
  DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
  DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
  DEFINE VAR W_Str     AS CHARACTER FORMAT "X(15)".
  DEFINE VAR W_AuxRet  AS CHARACTER FORMAT "X(8)" INITIAL "".
  DEFINE VAR W_AuxEst  AS INTEGER.

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
&Scoped-Define ENABLED-OBJECTS Btn_Termina 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-plan AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_cuentas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_cuentas AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Aceptar 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Ace&ptar" 
     SIZE 8 BY 1.35.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Ca&ncelar" 
     SIZE 8 BY 1.35.

DEFINE VARIABLE R_Organiza AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuenta", 1,
"Nombre", 2
     SIZE 11 BY 1.88
     BGCOLOR 17 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 2.69.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Buscar 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de Informaciòn de la pantalla en uso".

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)".

DEFINE BUTTON BUTTON-54 
     IMAGE-UP FILE "imagenes\informacion":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 54" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-269
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Plan
     BUTTON-54 AT ROW 1.81 COL 102
     Btn_Impresion AT ROW 3.42 COL 102 HELP
          "Permite generar la impresión del Plan Contable"
     Btn_Buscar AT ROW 5.04 COL 102 HELP
          "Permite generar la consulta de Cuentas"
     Btn_Ayuda AT ROW 20.65 COL 104 HELP
          "Permite generar la ayuda de la Pantalla"
     RECT-269 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.43 BY 21.12
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-Consulta
     Btn_Termina AT ROW 18.77 COL 53 HELP
          "Termina la consulta del Plan Contable"
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 18.77 COL 3
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 18.77 COL 9
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 19.58 COL 9
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 51.14 ROW 1.73
         SIZE 61.57 BY 20.31
         BGCOLOR 17 FONT 4
         TITLE "Consulta de una Cuenta".

DEFINE FRAME F-Imprimir
     Btn_Aceptar AT ROW 1.54 COL 30 HELP
          "Permite seleccionar el dispositivo de salida del informe"
     R_Organiza AT ROW 2.08 COL 4 NO-LABEL
     Btn_Cancelar AT ROW 2.88 COL 30 HELP
          "Termina y regresa a la pantalla de cuentas"
     " Organizado por" VIEW-AS TEXT
          SIZE 15 BY 1.15 AT ROW 1 COL 3
          FGCOLOR 7 
     RECT-20 AT ROW 1.54 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 71.86 ROW 5.08
         SIZE 39.86 BY 4.54
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Elección de parámetros para el informe".


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
  CREATE WINDOW w-plan ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Cuentas Contables (PUC)"
         HEIGHT             = 21.12
         WIDTH              = 113.43
         MAX-HEIGHT         = 21.15
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.15
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-plan 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-plan
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Imprimir:HIDDEN           = TRUE
       FRAME F-Imprimir:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Plan
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-plan)
THEN w-plan:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-plan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-plan w-plan
ON END-ERROR OF w-plan /* SFG - Configuración de Cuentas Contables (PUC) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-plan w-plan
ON WINDOW-CLOSE OF w-plan /* SFG - Configuración de Cuentas Contables (PUC) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Imprimir
&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar w-plan
ON CHOOSE OF Btn_Aceptar IN FRAME F-Imprimir /* Aceptar */
DO:
   DEFINE VAR Listado AS CHAR INITIAL "L_Plan.Lst".
   
   CASE R_Organiza:SCREEN-VALUE IN FRAME F-Imprimir:
     WHEN "1" THEN
          W_Titulo = "Organizado por Número de la Cuenta".
     WHEN "2" THEN
          W_Titulo = "Organizado por Orden Alfabético".
  END CASE.
  ASSIGN {&list-1}.
  Listado = W_Path + Listado.
  {Incluido\Imprimir.i "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Plan
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda w-plan
ON CHOOSE OF Btn_Ayuda IN FRAME F-Plan /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 22.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Buscar w-plan
ON CHOOSE OF Btn_Buscar IN FRAME F-Plan /* Consultar */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Plan.
  VIEW FRAME F-Consulta.
  FRAME {&FRAME-NAME}:SENSITIVE = FALSE.
  ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Imprimir
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar w-plan
ON CHOOSE OF Btn_Cancelar IN FRAME F-Imprimir /* Cancelar */
DO:
  HIDE FRAME F-Imprimir.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Plan.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Plan
&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion w-plan
ON CHOOSE OF Btn_Impresion IN FRAME F-Plan /* Imprimir */
DO:
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Plan.
  VIEW FRAME F-Imprimir.
  ENABLE ALL WITH FRAME F-Imprimir.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina w-plan
ON CHOOSE OF Btn_Termina IN FRAME F-Consulta /* Terminar Consulta */
DO:
  HIDE FRAME F-Consulta.
  ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Plan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Plan
&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 w-plan
ON CHOOSE OF BUTTON-54 IN FRAME F-Plan /* Button 54 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-plan 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-plan  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_cuentas.w':U ,
             INPUT  FRAME F-Plan:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_cuentas ).
       RUN set-position IN h_v-cf_cuentas ( 1.54 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.96 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_cuentas.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_cuentas ).
       RUN set-position IN h_b-cf_cuentas ( 1.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_cuentas ( 17.23 , 59.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Plan:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV-2 ).
       RUN set-position IN h_P-UPDSAV-2 ( 9.62 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV-2 ( 10.69 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_cuentas. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_cuentas , 'Record':U , h_v-cf_cuentas ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV-2 , 'TableIO':U , h_v-cf_cuentas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_cuentas ,
             BUTTON-54:HANDLE IN FRAME F-Plan , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_cuentas ,
             Btn_Termina:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV-2 ,
             Btn_Buscar:HANDLE IN FRAME F-Plan , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-plan  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar w-plan 
PROCEDURE Asignar :
/*------------------------------------------------------------------------------
  Objetivo:     Asigna a impresion
------------------------------------------------------------------------------*/
     DISPLAY
     Cuentas.Cuenta                AT 1 NO-LABEL
     Cuentas.Nombre                AT 16 NO-LABEL  FORMAT "X(50)"
     Cuentas.Naturaleza            AT 70 NO-LABEL
     Cuentas.Nivel                 AT 82 NO-LABEL
     Cuentas.Tipo                  AT 93 NO-LABEL
     W_Str                         AT 100 NO-LABEL
     WITH WIDTH 300 USE-TEXT STREAM-IO font 2 NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-plan  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-plan)
  THEN DELETE WIDGET w-plan.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-plan  _DEFAULT-ENABLE
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
  ENABLE RECT-269 BUTTON-54 Btn_Impresion Btn_Buscar Btn_Ayuda 
      WITH FRAME F-Plan IN WINDOW w-plan.
  {&OPEN-BROWSERS-IN-QUERY-F-Plan}
  ENABLE Btn_Termina 
      WITH FRAME F-Consulta IN WINDOW w-plan.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  DISPLAY R_Organiza 
      WITH FRAME F-Imprimir IN WINDOW w-plan.
  ENABLE RECT-20 Btn_Aceptar R_Organiza Btn_Cancelar 
      WITH FRAME F-Imprimir IN WINDOW w-plan.
  {&OPEN-BROWSERS-IN-QUERY-F-Imprimir}
  VIEW w-plan.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel w-plan 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
  /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
  

  E_NumFila = 1.
  E_NumColumn = 8.
  E_Fila      = "014Cuenta        040Nombre                                  002Nt002Nv002TP003DOC003CCO003Nit".
  RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 
  DEFINE VAR DC AS CHARACTER FORMAT "X(3)".
  DEFINE VAR NI AS CHARACTER FORMAT "X(3)".
  DEFINE VAR CC AS CHARACTER FORMAT "X(3)".
 /* launch Excel so it is visible to the user */
 chExcelApp:Visible = TRUE.

 /* create a new Workbook */
 chWorkbook = chExcelApp:Workbooks:Add().

 /* get the active Worksheet */
 chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH Cuentas NO-LOCK BY Cuentas.Cuenta:
       IF Cuentas.Id_Doc THEN DC = " SI". ELSE DC = " NO".
       IF Cuentas.Id_CenCostos THEN CC = " SI". ELSE CC = " NO".
       IF Cuentas.Id_Nit THEN NI = " SI". ELSE NI = " NO".
                                 
       E_Fila2     = "".
       E_Fila2     = "014" + STRING(Cuentas.Cuenta,"X(14)")
                   + "040" + STRING(Cuentas.Nombre,"X(40)")
                   + "002" + STRING(Cuentas.Naturaleza,"X(2)")
                   + "002" + STRING(Cuentas.Nivel,"99")
                   + "002" + STRING(Cuentas.Tipo,"99")
                   /*  + "003" + STRING(Cuentas.Id_Presupuesto,"999").*/
                   + "003" + DC
                   + "003" + CC
                   + "003" + NI.


       {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-plan 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir w-plan 
PROCEDURE ProcesoImprimir :
/*
    Modificado: GCamacho
    Fecha:  Abr/09/08
    Objetivo: Dar mas espacio a campo cuenta
*/
    
  DEFINE VAR W_Nit AS CHARACTER FORMAT "X(2)".
    DEFINE VAR W_Nat AS CHARACTER FORMAT "X(2)".
    
  {INCLUIDO\RepEncabezado.I}    
  DEFINE VAR W_Titulo2 AS CHARACTER FORMAT "X(120)".
    W_Reporte    = "REPORTE   : PLAN DE CUENTAS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
              
/*     W_EncColumna = "CUENTA            NOMBRE                         NAT NIV TIP NIT PPT DOC CCO".  */  /*GCamacho 04/09/08*/
    W_EncColumna = "".

  
  DEFINE FRAME f-reportes0
    WITH WIDTH 200 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  DEFINE FRAME f-reportes1
    WITH WIDTH 200 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

      
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-ftr.  
    FIND FIRST cuentas NO-LOCK NO-ERROR.
    FORM 
       Cuentas.Cuenta          COLUMN-LABEL "Cuenta"     FORMAT "X(22)"    
       Cuentas.Nombre          COLUMN-LABEL "Nombre"     FORMAT "X(50)"    
       Cuentas.Naturaleza      COLUMN-LABEL "Nat"                       
       Cuentas.Nivel           COLUMN-LABEL "Niv"        FORMAT ">9"               
       Cuentas.Tipo            COLUMN-LABEL "Tip"                       
       Cuentas.Id_Nit          COLUMN-LABEL "Nit"       FORMAT "Si/No"                 
       Cuentas.Id_Presupuesto  COLUMN-LABEL "PPT"       FORMAT "Si/No"                
       Cuentas.Id_Doc          COLUMN-LABEL "Doc"       FORMAT "Si/No"                
       Cuentas.Id_CenCostos    COLUMN-LABEL "CCo"       FORMAT "Si/No"                
    WITH FRAME f-repoCtas DOWN COLUMN 1 WIDTH 200
    NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
            
    IF R_Organiza:SCREEN-VALUE IN FRAME F-Imprimir = "1" THEN
       FOR EACH Cuentas NO-LOCK BY Cuentas.Cuenta:
           DISPLAY Cuentas.Cuenta                /* AT 1  FORMAT "X(22)"        */
                   Cuentas.Nombre                /* AT 20 FORMAT "X(30)"        */
                   Cuentas.Naturaleza            /* AT 51                       */
                   Cuentas.Nivel                 /* AT 55                       */
                   Cuentas.Tipo                  /* AT 59                       */
                   Cuentas.Id_Nit                /* AT 63                       */
                   Cuentas.Id_Presupuesto        /* AT 67                       */
                   Cuentas.Id_Doc                /* AT 71                       */
                   Cuentas.Id_CenCostos          /* AT 75                       */
                   WITH FRAME f-repoCtas.
            DOWN WITH FRAME f-repoCtas.
       END.
    ELSE
       FOR EACH Cuentas NO-LOCK BY Cuentas.Nombre:
           DISPLAY Cuentas.Cuenta                /* AT 1  FORMAT "X(22)"    */
                   Cuentas.Nombre                /* AT 20 FORMAT "X(30)"    */
                   Cuentas.Naturaleza            /* AT 51                   */
                   Cuentas.Nivel                 /* AT 55                   */
                   Cuentas.Tipo                  /* AT 59                   */
                   Cuentas.Id_Nit                /* AT 63                   */
                   Cuentas.Id_Presupuesto        /* AT 67                   */
                   Cuentas.Id_Doc                /* AT 71                   */
                   Cuentas.Id_CenCostos          /* AT 75                   */
                   WITH FRAME F-reportes1.     
            DOWN WITH FRAME f-repoCtas.
       END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-plan  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-plan 
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

