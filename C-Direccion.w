&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

 DEFINE OUTPUT PARAMETER P_Dir AS CHARACTER FORMAT "X(50)".

/*  DEFINE VAR W_Ok AS LOGICAL. */
 DEF VAR p_cod AS CHAR. 

/*   DEF VAR p_dir AS CHAR. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Direcciones

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Dir_Nomenclatura Numero Retorno 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Dir_Nomenclatura Numero Direccion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Retorno 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Dir_Nomenclatura AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nomenclatura" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "AG-Agrupación","AP-Apartamento","AUTOP-Autopista","AV-Avenida","AC-Avenida Calle","AK-Avenida Carrera","AV JMZ-Avenida Jiménez","BRR-Barrio","BL-Bloque","BG-Bodega","BS-Bosa","CL-Calle","CN-Camino","KR-Carrera","CT-Carretera","CA-Casa","CEL-Célula","CAN-Centro Administrativo Nacional","CUAN-Centro Urbano Antonio Nariño","CONJ-Conjunto","CS-Consultorio","DP-Depósito","DG-Diagonal","ED-Edificio","ENGT-Engativa","EN-Entrada","ESQ-Esquina","ES-Este","ET-Etapa","GJ-Garaje","GS-Garaje Sótano","IN-Interior","KNDY-Kennedy","KM-Kilómetro","LC-Local","LT-Lote","MZ-Manzana","MN-Mezzanine","MD-Módulo","N-Norte","OCC-Occidente","OE-Oeste","OF-Oficina","PA-Parcela","PO-Paseo","PH-Penthouse","PI-Piso","PD-Predio","PN-Puente","PT-Puesto","SC-Salón Comunal","SR-Sector","SS-Semisótano","SL-Solar","SM-Supermanzana","SUR-Sur","TO-Torre","TV-Transversal","UN-Unidad","UR-Unidad Residencial","URB-Urbanización" 
     DROP-DOWN-LIST
     SIZE 23.57 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Direccion AS CHARACTER FORMAT "X(90)":U 
     LABEL "Direccion" 
     VIEW-AS FILL-IN 
     SIZE 53 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE Numero AS CHARACTER FORMAT "X(40)":U 
     LABEL "Numero" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Direcciones
     Cmb_Dir_Nomenclatura AT ROW 1.81 COL 14 COLON-ALIGNED
     Numero AT ROW 2.92 COL 14 COLON-ALIGNED
     Direccion AT ROW 4.12 COL 10.29 COLON-ALIGNED
     Retorno AT ROW 5.38 COL 60 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71 BY 6.58
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Asignacion de Direcciones"
         HEIGHT             = 6.58
         WIDTH              = 71
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME Direcciones
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Direccion IN FRAME Direcciones
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Asignacion de Direcciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Asignacion de Direcciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Dir_Nomenclatura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Dir_Nomenclatura wWin
ON LEAVE OF Cmb_Dir_Nomenclatura IN FRAME Direcciones /* Nomenclatura */
DO:
  ASSIGN p_cod = " ".

  ASSIGN p_cod = Cmb_Dir_nomenclatura:SCREEN-VALUE.

  RUN codigo-nom. 
  
  ASSIGN p_dir = p_dir + " " + p_cod 
         direccion:SCREEN-VALUE = p_dir.

  DISABLE Direccion.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Numero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Numero wWin
ON LEAVE OF Numero IN FRAME Direcciones /* Numero */
DO:
  ASSIGN p_cod = " ".

  ASSIGN p_cod = Numero:SCREEN-VALUE.

  ASSIGN p_dir = p_dir + " " + p_cod 
         direccion:SCREEN-VALUE = p_dir
         numero:SCREEN-VALUE = "".
  
  DISABLE Direccion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Retorno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Retorno wWin
ON CHOOSE OF Retorno IN FRAME Direcciones /* Button 101 */
DO:

  IF P_Dir = " "  THEN DO:
     MESSAGE "No se esta enviando ninguna Direccion" SKIP
             "al registro. Rectifique" VIEW-AS ALERT-BOX.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE codigo-nom wWin 
PROCEDURE codigo-nom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR mm AS INT.
 DEF VAR cc AS INT.
 DEF VAR p_cod2 AS CHAR.

 ASSIGN p_cod2 = p_cod.

 ASSIGN cc = 0.
 DO mm = 1 TO LENGTH(p_cod2):
   
   IF SUBSTRING(p_cod2,mm,1)= '-' THEN
     ASSIGN cc = mm.

   IF cc <> 0 THEN NEXT.
 END.
 
 
 ASSIGN p_cod = SUBSTRING(P_cod2,1,(cc - 1)) + " ".

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
  DISPLAY Cmb_Dir_Nomenclatura Numero Direccion 
      WITH FRAME Direcciones IN WINDOW wWin.
  ENABLE Cmb_Dir_Nomenclatura Numero Retorno 
      WITH FRAME Direcciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Direcciones}
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

