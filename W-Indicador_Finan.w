&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WinGraph
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WinGraph 
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
DEFINE VARIABLE W_F1 AS LOGICAL INITIAL NO.
DEFINE VARIABLE W_F2 AS LOGICAL INITIAL NO.
DEFINE VARIABLE W_Frame AS LOGICAL.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Frame1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B2 B3 B4 B5 R-2 R-3 R-4 R-5 R-6 R1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WinGraph AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B2 
     LABEL "Liquidez" 
     SIZE 16 BY 1.12
     FONT 4.

DEFINE BUTTON B3 
     LABEL "Rotación" 
     SIZE 16.14 BY 1.12
     FONT 4.

DEFINE BUTTON B4 
     LABEL "Endeudamiento" 
     SIZE 16.29 BY 1.12
     FONT 4.

DEFINE BUTTON B5 
     LABEL "Rentabilidad" 
     SIZE 15 BY 1.12
     FONT 4.

DEFINE RECTANGLE R-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY .08
     FGCOLOR 0 .

DEFINE BUTTON B10 
     LABEL "Cuentas x Pagar" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B11 
     LABEL "Autonomía" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B12 
     LABEL "Endeudamiento" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B13 
     LABEL "Endeud. Futuro" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B14 
     LABEL "Ventas" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B15 
     LABEL "Patrimonio" 
     SIZE 21 BY 1.35
     FGCOLOR 0 FONT 4.

DEFINE BUTTON B16 
     LABEL "Activos" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B6 
     LABEL "Corriente de Fuego" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B7 
     LABEL "Materia Prima" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B8 
     LABEL "Producto Terminado" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE BUTTON B9 
     LABEL "Cuentas x Cobrar" 
     SIZE 21 BY 1.35
     FONT 4.

DEFINE RECTANGLE R-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE RECTANGLE R-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .14 BY 1.23
     FGCOLOR 0 .

DEFINE VARIABLE Def1T AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 47 BY 13.19
     BGCOLOR 17 FGCOLOR 0 FONT 8 NO-UNDO.

DEFINE BUTTON B1 
     LABEL "Razones Financieras" 
     SIZE 23 BY 1.62
     FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Ppal
     B1 AT ROW 1.27 COL 46.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.54
         BGCOLOR 17 .

DEFINE FRAME Frame2
     B6 AT ROW 2.19 COL 15
     B7 AT ROW 2.19 COL 38.14
     B11 AT ROW 2.19 COL 60.29
     B14 AT ROW 2.19 COL 82
     B8 AT ROW 4.58 COL 38.14
     B12 AT ROW 4.58 COL 60.29
     B15 AT ROW 4.58 COL 82
     B9 AT ROW 6.92 COL 38.14
     B13 AT ROW 6.92 COL 60.29
     B16 AT ROW 6.92 COL 82
     B10 AT ROW 9.31 COL 38.14
     R-10 AT ROW 5.77 COL 48.43
     R-11 AT ROW 8.15 COL 48.43
     R-12 AT ROW 1 COL 70.57
     R-13 AT ROW 3.42 COL 70.57
     R-14 AT ROW 5.85 COL 70.57
     R-15 AT ROW 1 COL 92.29
     R-16 AT ROW 3.42 COL 92.29
     R-17 AT ROW 5.73 COL 92.29
     R-7 AT ROW 1 COL 25.43
     R-8 AT ROW 1 COL 48.43
     R-9 AT ROW 3.38 COL 48.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 6.46
         SIZE 113.86 BY 16.08
         BGCOLOR 17 FGCOLOR 0 .

DEFINE FRAME Frame2a
     Def1T AT ROW 1 COL 1 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 42 ROW 1
         SIZE 48 BY 14
         BGCOLOR 17 FGCOLOR 0 FONT 8
         TITLE FGCOLOR 12 "".

DEFINE FRAME Frame1
     B2 AT ROW 3.42 COL 9.72
     B3 AT ROW 3.42 COL 32.29
     B4 AT ROW 3.42 COL 54.43
     B5 AT ROW 3.42 COL 76.72
     R-2 AT ROW 1.04 COL 49
     R-3 AT ROW 2.23 COL 17.72
     R-4 AT ROW 2.23 COL 40.29
     R-5 AT ROW 2.23 COL 62.43
     R-6 AT ROW 2.23 COL 84.14
     R1 AT ROW 2.23 COL 17.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 9 ROW 2.88
         SIZE 98 BY 3.77
         BGCOLOR 17 .


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
  CREATE WINDOW WinGraph ASSIGN
         HIDDEN             = YES
         TITLE              = "Indicadores"
         HEIGHT             = 21.54
         WIDTH              = 113.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 17
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB WinGraph 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WinGraph
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME Frame1:FRAME = FRAME Ppal:HANDLE
       FRAME Frame2:FRAME = FRAME Ppal:HANDLE
       FRAME Frame2a:FRAME = FRAME Frame2:HANDLE.

/* SETTINGS FOR FRAME Frame1
                                                                        */
ASSIGN 
       FRAME Frame1:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Frame2
                                                                        */
ASSIGN 
       FRAME Frame2:HIDDEN           = TRUE.

ASSIGN 
       B6:HIDDEN IN FRAME Frame2           = TRUE
       B6:RESIZABLE IN FRAME Frame2        = TRUE.

ASSIGN 
       B7:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       B8:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       B9:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-10:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-11:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-12:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-13:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-14:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-15:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-16:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-17:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-7:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-8:HIDDEN IN FRAME Frame2           = TRUE.

ASSIGN 
       R-9:HIDDEN IN FRAME Frame2           = TRUE.

/* SETTINGS FOR FRAME Frame2a
                                                                        */
ASSIGN 
       FRAME Frame2a:HIDDEN           = TRUE.

ASSIGN 
       Def1T:HIDDEN IN FRAME Frame2a           = TRUE.

/* SETTINGS FOR FRAME Ppal
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME Frame1:MOVE-AFTER-TAB-ITEM (B1:HANDLE IN FRAME Ppal)
       XXTABVALXX = FRAME Frame1:MOVE-BEFORE-TAB-ITEM (FRAME Frame2:HANDLE)
/* END-ASSIGN-TABS */.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WinGraph)
THEN WinGraph:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WinGraph
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WinGraph WinGraph
ON END-ERROR OF WinGraph /* Indicadores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WinGraph WinGraph
ON WINDOW-CLOSE OF WinGraph /* Indicadores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Ppal
&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 WinGraph
ON CHOOSE OF B1 IN FRAME Ppal /* Razones Financieras */
DO:
  RUN apagar(INPUT 1, INPUT 0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame2
&Scoped-define SELF-NAME B10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B10 WinGraph
ON MOUSE-SELECT-CLICK OF B10 IN FRAME Frame2 /* Cuentas x Pagar */
DO:
 FRAME Frame2a:TITLE = 'Cuentas por Pagar = 26 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Permite interpretar las deudas que se tienen en el periodo'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B7 B8 B9 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
    ENABLE B7 B8 B9 WITH FRAME Frame2.
    HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B11 WinGraph
ON MOUSE-SELECT-CLICK OF B11 IN FRAME Frame2 /* Autonomía */
DO:
 FRAME Frame2a:TITLE = 'Autonomia = 58 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Refleja la autonimía économica del periodo y su incremento determina la solidez de la empresa.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
     DISABLE B12 B13 WITH FRAME Frame2. 
     VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B12 B13 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B12 WinGraph
ON MOUSE-SELECT-CLICK OF B12 IN FRAME Frame2 /* Endeudamiento */
DO:
   FRAME Frame2a:TITLE = 'Endeudamiento = 75 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Es la capacidad de endeudamiento según las utilidades del periodo más el porcentage de autonomía.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
     DISABLE B11 B13 WITH FRAME Frame2. 
     VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B11 B13 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B13 WinGraph
ON CHOOSE OF B13 IN FRAME Frame2 /* Endeud. Futuro */
DO:
 FRAME Frame2a:TITLE = 'Endeudamiento Futuro = 35 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Refleja el posible endeudamiento que se podría tener si el parametro anterior baja.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
     DISABLE B11 B12 WITH FRAME Frame2. 
     VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B11 B12 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B14 WinGraph
ON MOUSE-SELECT-CLICK OF B14 IN FRAME Frame2 /* Ventas */
DO:
 FRAME Frame2a:TITLE = 'Ventas = 87 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Estado de Ventas Netas en el Periodo.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B15 B16 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B15 B16 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B15 WinGraph
ON CHOOSE OF B15 IN FRAME Frame2 /* Patrimonio */
DO:
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B14 B16 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B14 B16 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B16 WinGraph
ON MOUSE-SELECT-CLICK OF B16 IN FRAME Frame2 /* Activos */
DO:
 FRAME Frame2a:TITLE = 'Activos = 40 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Total Activos en ejercicio.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B14 B15 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B14 B15 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame1
&Scoped-define SELF-NAME B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B2 WinGraph
ON CHOOSE OF B2 IN FRAME Frame1 /* Liquidez */
DO:
  RUN apagar(INPUT 2, INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B3 WinGraph
ON CHOOSE OF B3 IN FRAME Frame1 /* Rotación */
DO:
  RUN apagar(INPUT 2, INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B4 WinGraph
ON CHOOSE OF B4 IN FRAME Frame1 /* Endeudamiento */
DO:
  RUN apagar(INPUT 2, INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B5 WinGraph
ON CHOOSE OF B5 IN FRAME Frame1 /* Rentabilidad */
DO:
  RUN apagar(INPUT 2, INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame2
&Scoped-define SELF-NAME B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B6 WinGraph
ON MOUSE-SELECT-CLICK OF B6 IN FRAME Frame2 /* Corriente de Fuego */
DO:
  Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Porcentaje de liquide variable en        Activos' + FILL(' ',165) + '-----------------' + FILL(' ',162) + 'Pasivos' + FILL(' ',100) +  'Según normas generales del Gobierno'. 
  W_Frame = FRAME Frame2a:HIDDEN.
  RUN Colores(INPUT 6).
   IF W_Frame = YES THEN
     VIEW FRAME Frame2a.
    ELSE
     HIDE FRAME Frame2a.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B7 WinGraph
ON MOUSE-SELECT-CLICK OF B7 IN FRAME Frame2 /* Materia Prima */
DO:
 FRAME Frame2a:TITLE = 'Materia Prima = 14 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Rendimiento de Materias Primas en el periodo es igual al valor de venta menos el costo dividido el tiempo de proceso'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B8 B9 B10 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B8 B9 B10 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B8 WinGraph
ON MOUSE-SELECT-CLICK OF B8 IN FRAME Frame2 /* Producto Terminado */
DO:
 FRAME Frame2a:TITLE = 'Producto Terminado = 28 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Este proceso tiene como prioridad mostrar el margen de productividad en la planta de ensamble y determina según su tamaño el tiempo obtenido en producción general mensualmente.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B7 B9 B10 WITH FRAME Frame2.  
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B7 B9 B10 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B9 WinGraph
ON MOUSE-SELECT-CLICK OF B9 IN FRAME Frame2 /* Cuentas x Cobrar */
DO:
 FRAME Frame2a:TITLE = 'Cuentas por Cobrar = 34 %'.
 Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Este porcentage es obtenido mediante el total de facturas vencidas y refleja la perdida de utilidad del periodo.'.
 W_Frame = FRAME Frame2a:HIDDEN.
 IF W_Frame = YES THEN DO:
    DISABLE B7 B8 B10 WITH FRAME Frame2.
    VIEW FRAME Frame2a.
   END.
  ELSE DO:
   ENABLE B7 B8 B10 WITH FRAME Frame2.
   HIDE FRAME Frame2a.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WinGraph 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects WinGraph  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apagar WinGraph 
PROCEDURE Apagar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER W_Nframe AS INTEGER.
DEFINE INPUT PARAMETER W_Col    AS INTEGER.
CASE W_Nframe:
    WHEN 1 THEN DO:
     W_F1 = FRAME Frame1:HIDDEN.
     IF W_F1 = YES THEN
         VIEW FRAME Frame1.
      ELSE 
       IF W_F2 = NO THEN 
        HIDE FRAME Frame1.
    END.
    WHEN 2 THEN DO:
      W_F2 = FRAME Frame2:HIDDEN.
      IF W_F2 = YES THEN DO:
         VIEW FRAME Frame2.
         HIDE FRAME Frame2a.
        END.
       ELSE 
        HIDE FRAME Frame2.
  END.
END CASE.
IF W_Col > 0 THEN DO:
 ENABLE B2 B3 B4 B5 WITH FRAME Frame1.
 HIDE R-12 R-13 R-14 R-15 R-16 R-17 IN FRAME Frame2.
 HIDE B6 B7 B8 B9 B10 B11 B12 B13 B14 B15 B16 IN FRAME Frame2.
 HIDE R-7 R-8 R-9 R-10 R-11 R-17 IN FRAME Frame2. 
 IF W_f2 = YES THEN
 CASE W_Col:
    WHEN 1 THEN DO:
     VIEW B6 R-7 IN FRAME Frame2.
     ENABLE B6 WITH FRAME Frame2.
     FRAME Frame2a:COLUMN = 36.
     FRAME Frame2a:ROW = 2.19.
     DISABLE B3 B4 B5 WITH FRAME Frame1. 
    END.
    WHEN 2 THEN DO:
     VIEW B7 R-8 B8 R-9 B9 R-10 B10 R-11 IN FRAME Frame2.
     ENABLE B7 B8 B9 B10 WITH FRAME Frame2.
     FRAME Frame2a:COLUMN = 59.
     FRAME Frame2a:ROW = 2.19.
     DISABLE B2 B4 B5 WITH FRAME Frame1. 
    END.
    WHEN 3 THEN DO:
     VIEW B11 R-12 B12 R-13 B13 R-14 IN FRAME Frame2.
     ENABLE B11 B12 B13 WITH FRAME Frame2.
     FRAME Frame2a:COLUMN = 12.14.
     FRAME Frame2a:ROW = 2.19.
     DISABLE B2 B3 B5 WITH FRAME Frame1. 
    END.
    WHEN 4 THEN DO:
     VIEW B14 R-15 B15 R-16 B16 R-17 IN FRAME Frame2.
     ENABLE B14 B15 B16 WITH FRAME Frame2.
     FRAME Frame2a:COLUMN = 33.86.
     FRAME Frame2a:ROW = 2.19.
     DISABLE B2 B3 B4 WITH FRAME Frame1. 
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Colores WinGraph 
PROCEDURE Colores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER W_Boton AS INTEGER.
/*DEFINE INPUT PARAMETER W_Color AS INTEGER.*/

CASE W_Boton:
    WHEN 6 THEN DO:
     B6:FGCOLOR IN FRAME Frame2 = 2.

    END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WinGraph  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WinGraph)
  THEN DELETE WIDGET WinGraph.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WinGraph  _DEFAULT-ENABLE
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
  ENABLE B1 
      WITH FRAME Ppal IN WINDOW WinGraph.
  {&OPEN-BROWSERS-IN-QUERY-Ppal}
  DISPLAY Def1T 
      WITH FRAME Frame2a IN WINDOW WinGraph.
  ENABLE Def1T 
      WITH FRAME Frame2a IN WINDOW WinGraph.
  VIEW FRAME Frame2a IN WINDOW WinGraph.
  {&OPEN-BROWSERS-IN-QUERY-Frame2a}
  ENABLE B2 B3 B4 B5 R-2 R-3 R-4 R-5 R-6 R1 
      WITH FRAME Frame1 IN WINDOW WinGraph.
  VIEW FRAME Frame1 IN WINDOW WinGraph.
  {&OPEN-BROWSERS-IN-QUERY-Frame1}
  ENABLE B6 B7 B11 B14 B8 B12 B15 B9 B13 B16 B10 R-10 R-11 R-12 R-13 R-14 R-15 
         R-16 R-17 R-7 R-8 R-9 
      WITH FRAME Frame2 IN WINDOW WinGraph.
  VIEW FRAME Frame2 IN WINDOW WinGraph.
  {&OPEN-BROWSERS-IN-QUERY-Frame2}
  VIEW WinGraph.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject WinGraph 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generacion WinGraph 
PROCEDURE Generacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
B6:LABEL IN FRAME Frame2 = 'Cor Fuego = 18 %'. 
B7:LABEL IN FRAME Frame2 = 'Mat Pri = 14 %'. 
B8:LABEL IN FRAME Frame2 = 'Prod Term = 28 %'. 
B9:LABEL IN FRAME Frame2 = 'CxC = 34 %'. 
B10:LABEL IN FRAME Frame2 = 'CxP = 26 %'. 
B11:LABEL IN FRAME Frame2 = 'Auton = 58 %'.
B12:LABEL IN FRAME Frame2 = 'Endeuda = 75 %'.
B13:LABEL IN FRAME Frame2 = 'Ende Fut = 35 %'.
B14:LABEL IN FRAME Frame2 = 'Ventas = 87 %'.
B16:LABEL IN FRAME Frame2 = 'Activos = 40 %'. 
RUN Patrimonio.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject WinGraph 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.
  HIDE FRAME Frame1.
  HIDE FRAME Frame2.
  HIDE FRAME Frame2a.
  HIDE B6 B7 B8 B9 B10 B11 B12 B13 B14 B15 B16.
  HIDE R-7 R-8 R-9 R-10 R-11.
  HIDE R-12 R-13 R-14 R-15 R-16 R-17.
  RUN Generacion.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Patrimonio WinGraph 
PROCEDURE Patrimonio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE W_TotAct AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_TotPas AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_TotPor AS DECIMAL INITIAL 0.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,1,'CHARACTER') = '1' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta(INPUT Cuentas.Cuenta, INPUT 2003, INPUT 001, INPUT Cuentas.Naturaleza, INPUT 09, OUTPUT W_TotPor).
 W_TotAct = W_TotAct + W_TotPor.
END.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,1,'CHARACTER') = '2' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta(INPUT Cuentas.Cuenta, INPUT 2003, INPUT 001, INPUT Cuentas.Naturaleza, INPUT 01, OUTPUT W_TotPor).
 W_TotPas = W_TotPas + W_TotPor.
END.
IF W_TotAct = 0 OR W_TotPas = 0 THEN
  W_TotPor = 0.
 ELSE
  W_TotPor = W_TotPas / W_TotAct.
  FRAME Frame2a:TITLE = 'Patrimonio = ' + STRING(W_TotPor,'->99.99') + '%'.
Def1T:SCREEN-VALUE IN FRAME Frame2a = 'Patrimonio en el periodo.' + FILL(' ',20) + 'Resultado de la suma los pasivos dividido la suma los activos que es igual a ' + STRING(W_TotPor,'->99.99') + '%'.  
IF W_TotPor < 10  THEN
  Def1T:FGCOLOR = 12.
 ELSE
  IF W_TotPor < 50 THEN
    Def1T:FGCOLOR = 14.
   ELSE
    Def1T:FGCOLOR = 7. 
B15:LABEL IN FRAME Frame2 = 'Patrimonio = ' + STRING(W_TotPor,'->99.99') + '%'.
B15:FGCOLOR = 12.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaldosCuenta WinGraph 
PROCEDURE SaldosCuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER W_CtaSal  LIKE Sal_Cuenta.Cuenta.
DEFINE INPUT PARAMETER W_Ano     AS   INTEGER.
DEFINE INPUT PARAMETER W_AgenSal LIKE Sal_Cuenta.Agencia.
DEFINE INPUT PARAMETER W_NatCta LIKE Cuentas.Naturaleza.
DEFINE INPUT PARAMETER W_MesCor  AS INTEGER.
DEFINE OUTPUT PARAMETER W_Salcta  AS DECIMAL.

DEFINE VARIABLE i AS INTEGER.
W_Salcta = 0.
IF W_NatCta = ' ' THEN DO:
 FIND FIRST Cuentas WHERE Cuentas.Cuenta = W_CtaSal NO-LOCK NO-ERROR.
 IF AVAILABLE Cuentas THEN
  W_NatCta = Naturaleza.
END.
FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Cuenta = W_CtaSal AND Sal_Cuenta.Ano = W_Ano AND Sal_Cuenta.Agencia = W_AgenSal NO-LOCK NO-ERROR.
IF AVAILABLE Sal_Cuenta THEN DO:
 IF AVAILABLE Cuentas THEN DO:
  ASSIGN W_SalCta = Sal_Cuenta.Sal_Inicial.
  DO i = 1 TO W_MesCor:
    IF W_NatCta = 'Db' THEN
      ASSIGN W_SalCta = W_SalCta + (Sal_Cuenta.Db[i] - Sal_Cuenta.Cr[i]).
     ELSE
      ASSIGN W_SalCta = W_SalCta + (Sal_Cuenta.Cr[i] - Sal_Cuenta.Db[i]). 
  END.
 END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

