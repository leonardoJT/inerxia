&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
 {Incluido\VARIABLE.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR WEncabezado AS CHARACTER FORMAT "X(480)".
DEFINE VAR AgeWk LIKE Agencias.Agencia.


DEFINE TEMP-TABLE Personas
 FIELD Nit LIKE Clientes.Nit
 FIELD Nom AS CHARACTER FORMAT "X(50)"
 FIELD NCr LIKE Creditos.Num_Credito
 FIELD NPg LIKE Creditos.Pagare
 FIELD Val LIKE Creditos.Val_Atraso
 FIELD Dif LIKE Creditos.INT_DifCobro
 FIELD IMo LIKE Creditos.INT_MorCobrar
 FIELD Dia LIKE Creditos.Dias_Atraso.

DEFINE TEMP-TABLE TCode 
    FIELD TCod LIKE Clientes.Nit
    FIELD TNit LIKE Clientes.Nit
    FIELD TCre LIKE Creditos.Num_Credito.

DEFINE TEMP-TABLE Plano
    FIELD PCon AS INTEGER FORMAT "99999"
    FIELD PAgn AS INTEGER FORMAT "999"
    FIELD PNom AS CHARACTER FORMAT "X(40)"
    FIELD PDir AS CHARACTER FORMAT "X(50)"
    FIELD PMun AS CHARACTER FORMAT "X(20)"
    FIELD PTel AS CHARACTER FORMAT "X(15)".

DEFINE VAR Numero AS INTEGER FORMAT "99999".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FCer
&Scoped-define BROWSE-NAME BPersonas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Personas

/* Definitions for BROWSE BPersonas                                     */
&Scoped-define FIELDS-IN-QUERY-BPersonas Nit Nom NCr Val Dif Imo Dia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BPersonas   
&Scoped-define SELF-NAME BPersonas
&Scoped-define QUERY-STRING-BPersonas FOR EACH Personas WHERE     (Personas.Val GT 0 OR Personas.Dif GT 0 OR Personas.Imo GT 0) NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BPersonas OPEN QUERY {&SELF-NAME} FOR EACH Personas WHERE     (Personas.Val GT 0 OR Personas.Dif GT 0 OR Personas.Imo GT 0) NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BPersonas Personas
&Scoped-define FIRST-TABLE-IN-QUERY-BPersonas Personas


/* Definitions for FRAME FCer                                           */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FCer ~
    ~{&OPEN-QUERY-BPersonas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-171 WFirma WCargo WCC Cmb_Doc ~
Cmb_Agencias BUTTON-177 Cmb_TipoPersonas Btn_Query BUTTON-172 BuscarNit ~
BPersonas ROrganiza BUTTON-148 BUTTON-149 RNumero RECT-283 RECT-285 ~
RECT-286 
&Scoped-Define DISPLAYED-OBJECTS WFirma WCargo WCC Cmb_Doc Cmb_Agencias ~
Cmb_TipoPersonas BuscarNit ROrganiza RNumero 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Query 
     LABEL "Ejecutar" 
     SIZE 8 BY .81
     FONT 4.

DEFINE BUTTON BUTTON-148 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 14 BY 1.88
     FONT 5.

DEFINE BUTTON BUTTON-149 AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 14 BY 1.88.

DEFINE BUTTON BUTTON-171 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 171" 
     SIZE 14 BY 1.88.

DEFINE BUTTON BUTTON-172 
     LABEL "Ver todos los de la categoría" 
     SIZE 24 BY .81
     FONT 4.

DEFINE BUTTON BUTTON-177 
     LABEL "Filtrar Información" 
     SIZE 24 BY .81
     FONT 4.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 45 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Doc AS CHARACTER FORMAT "X(80)":U 
     LABEL "Tipos" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TipoPersonas AS CHARACTER FORMAT "X(35)":U INITIAL "E Categoria" 
     LABEL "Tipos de Personas" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Clientes Atrasados","A Categoria ","B Categoria ","C Categoria ","D Categoria ","E Categoria","Compromisos No Cumplidos" 
     DROP-DOWN-LIST
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE BuscarNit AS CHARACTER FORMAT "X(14)":U 
     LABEL "Buscar un Nit" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WCargo AS CHARACTER FORMAT "X(40)":U 
     LABEL "Cargo" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WCC AS CHARACTER FORMAT "X(100)":U 
     LABEL "C.C" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WFirma AS CHARACTER FORMAT "X(45)":U 
     LABEL "Firma" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RNumero AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Imprimir Carta al Cliente seleccionado", 1,
"Imprimir Todas las cartas de todos los Clientes", 2
     SIZE 79 BY .81 NO-UNDO.

DEFINE VARIABLE ROrganiza AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Nit", 1,
"Nombre", 2,
"Valor Atraso", 3,
"Interes Dif.Cobro", 4,
"Interes de Mora", 5,
"Días Atraso", 6
     SIZE 15 BY 4.85
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.96.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19 BY 5.65.

DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.62.

DEFINE BUTTON BUTTON-176 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 176" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-181 
     LABEL "Tomar Valores Predeterminados" 
     SIZE 29 BY 1.12.

DEFINE VARIABLE AtrasoMayorA AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor atraso mayor a" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DiaMorFin AS INTEGER FORMAT "9999":U INITIAL 9999 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DiaMorIni AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Entre" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 2.15.

DEFINE BUTTON BUTTON-182 
     LABEL "Salir" 
     SIZE 9 BY .81.

DEFINE VARIABLE F1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BPersonas FOR 
      Personas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BPersonas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BPersonas wWin _FREEFORM
  QUERY BPersonas NO-LOCK DISPLAY
      Nit
 Nom FORMAT "X(40)" LABEL "Nombre Deudor"
 NCr LABEL "Num.Crédito"
 Val LABEL "Valor Atraso"
 Dif FORMAT ">>>,>>>,>>9" LABEL "Int.Dif.Cobro"
 Imo FORMAT ">>>,>>>,>>9" LABEL "Int.Mora"
 Dia FORMAT ">>>,>>>,>>9" LABEL "Dias Atraso"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 12.12
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .45 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FCer
     BUTTON-171 AT ROW 1.81 COL 95
     WFirma AT ROW 2.08 COL 10 COLON-ALIGNED
     WCargo AT ROW 2.08 COL 50 COLON-ALIGNED
     WCC AT ROW 3.15 COL 10 COLON-ALIGNED
     Cmb_Doc AT ROW 3.15 COL 50 COLON-ALIGNED
     Cmb_Agencias AT ROW 4.77 COL 10 COLON-ALIGNED
     BUTTON-177 AT ROW 4.96 COL 67
     Cmb_TipoPersonas AT ROW 5.85 COL 19 COLON-ALIGNED
     Btn_Query AT ROW 5.96 COL 58
     BUTTON-172 AT ROW 5.96 COL 67
     BuscarNit AT ROW 6.92 COL 19 COLON-ALIGNED
     BPersonas AT ROW 8 COL 3
     ROrganiza AT ROW 8.54 COL 95 NO-LABEL
     BUTTON-148 AT ROW 16.08 COL 95
     BUTTON-149 AT ROW 18.23 COL 95
     RNumero AT ROW 20.85 COL 8.29 NO-LABEL
     RECT-283 AT ROW 1.54 COL 3
     RECT-285 AT ROW 8 COL 93
     RECT-286 AT ROW 20.38 COL 3
     " Entre la Información de Firmas y Copia a" VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 1.23 COL 4.72
          FGCOLOR 7 
     "Organizar Información" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 7.73 COL 95
          FGCOLOR 7 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 21.12
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Compromisos
     F1 AT ROW 1.27 COL 10 COLON-ALIGNED
     F2 AT ROW 1.27 COL 31 COLON-ALIGNED
     BUTTON-182 AT ROW 1.27 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 3.42
         SIZE 56 BY 2.15
         BGCOLOR 17 FONT 4
         TITLE "Rango de Fechas".

DEFINE FRAME Filtros
     AtrasoMayorA AT ROW 1.27 COL 31 COLON-ALIGNED
     DiaMorIni AT ROW 3.69 COL 14 COLON-ALIGNED
     DiaMorFin AT ROW 3.69 COL 29 COLON-ALIGNED
     BUTTON-176 AT ROW 5.31 COL 40
     BUTTON-181 AT ROW 5.58 COL 6
     RECT-284 AT ROW 2.88 COL 2
     " Filtrar días atrasados entre los siguientes limites" VIEW-AS TEXT
          SIZE 42 BY .81 AT ROW 2.54 COL 4
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 43 ROW 8
         SIZE 49 BY 7
         BGCOLOR 17 FONT 4
         TITLE "Filtros de Información".


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
         TITLE              = "SFG - Impresión de Certificados"
         HEIGHT             = 21.12
         WIDTH              = 113
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
/* REPARENT FRAME */
ASSIGN FRAME Filtros:FRAME = FRAME FCer:HANDLE
       FRAME F_Compromisos:FRAME = FRAME FCer:HANDLE.

/* SETTINGS FOR FRAME FCer
                                                                        */
/* BROWSE-TAB BPersonas BuscarNit FCer */
/* SETTINGS FOR FRAME Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Compromisos
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Compromisos:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BPersonas
/* Query rebuild information for BROWSE BPersonas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Personas WHERE
    (Personas.Val GT 0 OR Personas.Dif GT 0 OR Personas.Imo GT 0) NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BPersonas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Impresión de Certificados */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Impresión de Certificados */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Query
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Query wWin
ON CHOOSE OF Btn_Query IN FRAME FCer /* Ejecutar */
DO: 
    DEFINE VAR XOk AS LOGICAL INITIAL NO.
    RUN _SetCurs.r ("WAIT").
    FOR EACH Personas: DELETE Personas. END.
    ASSIGN FRAME FCer Cmb_Agencias Cmb_TipoPersonas.
    ASSIGN FRAME F_Compromisos F1 F2.
    ASSIGN FRAME Filtros Diamorini Diamorfin Atrasomayora.
    CLOSE QUERY BPersonas.
    AgeWk = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).


  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN DO:
      IF Cmb_TipoPersonas:SCREEN-VALUE EQ "Clientes Atrasados" THEN DO: 
          FOR EACH Creditos WHERE
                   Creditos.Estado     EQ 2 AND
                   Creditos.FOR_Pago   NE 2 AND
                   Creditos.Val_Atraso GT 0 AND 
                   Creditos.Sdo_Capital GT 0 AND
                   Creditos.Abogado    EQ NO NO-LOCK
                   BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              FIND FIRST Cobros WHERE 
                   Cobros.Num_Credito EQ Creditos.Num_Credito AND   
                   Cobros.Nit         EQ Creditos.Nit AND           
                   Cobros.Estado      EQ 1 AND                      
                   MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND
                   YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND
                   Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.   
              IF AVAILABLE Cobros THEN NEXT.
              /* Procesa todos - Excepto los que tienen compromisos */
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred.       
          END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "A" THEN DO:
         FOR EACH Creditos WHERE 
                     Creditos.FOR_Pago    NE 2 AND
                     Creditos.Estado      EQ 2 AND 
                     Creditos.Dias_Atraso GT 0 AND
                     Creditos.Categoria   EQ "A" AND 
                     Creditos.Sdo_Capital GT 0 AND
                     Creditos.Val_Atraso  GT 0 AND 
                     Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
                IF FIRST-OF(Creditos.Nit) THEN DO:
                   FIND Clientes WHERE 
                        Clientes.Nit EQ Creditos.Nit AND 
                        Clientes.Fec_Fallecido EQ ?
                        NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE Clientes THEN NEXT.
                END.
                FIND FIRST Cobros WHERE                                   
                     Cobros.Num_Credito EQ Creditos.Num_Credito AND       
                     Cobros.Nit         EQ Creditos.Nit AND               
                     Cobros.Estado      EQ 1 AND                          
                     MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND    
                     YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND    
                     Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.       
                IF AVAILABLE Cobros THEN NEXT.                            
                /* Procesa todos - Excepto los que tienen compromisos */  
                RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
                IF XOk THEN DO: XOk = NO. NEXT. END.
                RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "B" THEN DO:
          FOR EACH Creditos WHERE 
                   Creditos.FOR_Pago    NE 2 AND
                   Creditos.Estado      EQ 2 AND
                   Creditos.Dias_Atraso GT 0 AND
                   Creditos.Categoria   EQ "B" AND 
                   Creditos.Sdo_Capital GT 0 AND
                   Creditos.Val_Atraso  GT 0 AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              FIND FIRST Cobros WHERE                                   
                   Cobros.Num_Credito EQ Creditos.Num_Credito AND       
                   Cobros.Nit         EQ Creditos.Nit AND               
                   Cobros.Estado      EQ 1 AND                          
                   MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND    
                   YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND    
                   Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.       
              IF AVAILABLE Cobros THEN NEXT.                            
              /* Procesa todos - Excepto los que tienen compromisos */  
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred. 
          END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "C" THEN DO:
         FOR EACH Creditos WHERE
                     Creditos.FOR_Pago    NE 2 AND
                     Creditos.Estado      EQ 2 AND 
                     Creditos.Dias_Atraso GT 0 AND
                     Creditos.Sdo_Capital GT 0 AND
                     Creditos.Categoria   EQ "C" AND 
                     Creditos.Val_Atraso  GT 0 AND 
                     Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
                IF FIRST-OF(Creditos.Nit) THEN DO:
                   FIND Clientes WHERE 
                        Clientes.Nit EQ Creditos.Nit AND 
                        Clientes.Fec_Fallecido EQ ?
                        NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE Clientes THEN NEXT.
                END.
                FIND FIRST Cobros WHERE                                   
                     Cobros.Num_Credito EQ Creditos.Num_Credito AND       
                     Cobros.Nit         EQ Creditos.Nit AND               
                     Cobros.Estado      EQ 1 AND                          
                     MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND    
                     YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND    
                     Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.       
                IF AVAILABLE Cobros THEN NEXT.                            
                /* Procesa todos - Excepto los que tienen compromisos */  
                RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
                IF XOk THEN DO: XOk = NO. NEXT. END.
                RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "D" THEN DO:
         FOR EACH Creditos WHERE 
                     Creditos.Sdo_Capital GT 0 AND
                     Creditos.FOR_Pago    NE 2 AND
                     Creditos.Estado      EQ 2 AND 
                     Creditos.Dias_Atraso GT 0 AND
                     Creditos.Categoria   EQ "D" AND 
                     Creditos.Val_Atraso  GT 0  AND 
                     Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
                IF FIRST-OF(Creditos.Nit) THEN DO:
                   FIND Clientes WHERE 
                        Clientes.Nit EQ Creditos.Nit AND 
                        Clientes.Fec_Fallecido EQ ?
                        NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE Clientes THEN NEXT.
                END.
                FIND FIRST Cobros WHERE                                   
                     Cobros.Num_Credito EQ Creditos.Num_Credito AND       
                     Cobros.Nit         EQ Creditos.Nit AND               
                     Cobros.Estado      EQ 1 AND                          
                     MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND    
                     YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND    
                     Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.       
                IF AVAILABLE Cobros THEN NEXT.                            
                /* Procesa todos - Excepto los que tienen compromisos */  
                RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
                IF XOk THEN DO: XOk = NO. NEXT. END.
                RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "E" THEN DO:
         FOR EACH Creditos WHERE 
                    Creditos.Sdo_Capital GT 0 AND
                    Creditos.FOR_Pago    NE 2 AND
                    Creditos.Estado      EQ 2 AND  
                    Creditos.Dias_Atraso GT 0 AND
                    Creditos.Categoria   EQ "E" AND
                    Creditos.Val_Atraso  GT 0  AND 
                    Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
             IF FIRST-OF(Creditos.Nit) THEN DO:
                FIND Clientes WHERE 
                     Clientes.Nit EQ Creditos.Nit AND 
                     Clientes.Fec_Fallecido EQ ?
                     NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Clientes THEN NEXT.
             END.
             FIND FIRST Cobros WHERE                                   
                  Cobros.Num_Credito EQ Creditos.Num_Credito AND       
                  Cobros.Nit         EQ Creditos.Nit AND               
                  Cobros.Estado      EQ 1 AND                          
                  MONTH(Cobros.Fec_Compromiso) = MONTH(w_fecha) AND    
                  YEAR(Cobros.Fec_Compromiso)  = YEAR(w_fecha)  AND    
                  Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.       
             IF AVAILABLE Cobros THEN NEXT.                            
             /* Procesa todos - Excepto los que tienen compromisos */  
             RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
             IF XOk THEN DO: XOk = NO. NEXT. END.
             RUN QueryCred. 
         END.
      END.
      IF Cmb_TipoPersonas EQ "Compromisos No Cumplidos" THEN DO:
         FOR EACH Personas: DELETE Personas. END.
         FOR EACH Creditos WHERE 
                  Creditos.Sdo_Capital GT 0 AND
                  Creditos.FOR_Pago    NE 2 AND
                  Creditos.Estado      EQ 2 AND  
                  Creditos.Dias_Atraso GT 0 AND
                  Creditos.Val_Atraso  GT 0 AND 
                  Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
            IF FIRST-OF(Creditos.Nit) THEN DO:
               FIND Clientes WHERE 
                    Clientes.Nit EQ Creditos.Nit AND 
                    Clientes.Fec_Fallecido EQ ?
                    NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Clientes THEN NEXT.
            END.
            FIND FIRST Cobros WHERE 
                       Cobros.Num_Credito EQ Creditos.Num_Credito AND
                       Cobros.Nit         EQ Creditos.Nit AND
                       Cobros.Estado      EQ 1     AND 
                       Creditos.Abogado   EQ NO    AND 
                       Cobros.Fec_Compromiso GE F1 AND
                       Cobros.Fec_Compromiso LE F2 AND
                       Cobros.Fec_Cumplimiento EQ ?
                       NO-LOCK NO-ERROR.
            IF AVAILABLE Cobros THEN RUN QueryCred. 
         END.
      END.
  END.
  ELSE DO:
      IF Cmb_TipoPersonas:SCREEN-VALUE EQ "Clientes Atrasados" THEN DO:
         FOR EACH Creditos WHERE 
                   Creditos.Agencia     EQ AgeWk AND
                   Creditos.Sdo_Capital GT 0 AND
                   Creditos.FOR_Pago    NE 2 AND
                   Creditos.Estado      EQ 2 AND
                   Creditos.Dias_Atraso GT 0 AND
                   Creditos.Val_Atraso  GT 0 AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred.       
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "A" THEN DO:
         FOR EACH Creditos WHERE 
                   Creditos.Agencia     EQ AgeWk AND
                   Creditos.Sdo_Capital GT 0   AND
                   Creditos.FOR_Pago    NE 2   AND
                   Creditos.Estado      EQ 2   AND 
                   Creditos.Dias_Atraso GT 0   AND
                   Creditos.Categoria   EQ "A" AND 
                   Creditos.Val_Atraso  GT 0   AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "B" THEN DO:
         FOR EACH Creditos WHERE 
                   Creditos.Agencia     EQ AgeWk AND
                   Creditos.Sdo_Capital GT 0   AND
                   Creditos.FOR_Pago    NE 2   AND
                   Creditos.Estado      EQ 2   AND
                   Creditos.Categoria   EQ "B" AND 
                   Creditos.Dias_Atraso GT 0   AND
                   Creditos.Val_Atraso  GT 0   AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "C" THEN DO:
         FOR EACH Creditos WHERE 
                   Creditos.Agencia     EQ AgeWk AND
                   Creditos.Sdo_Capital GT 0   AND
                   Creditos.FOR_Pago    NE 2   AND
                   Creditos.Estado      EQ 2   AND
                   Creditos.Categoria   EQ "C" AND 
                   Creditos.Dias_Atraso GT 0   AND
                   Creditos.Val_Atraso  GT 0   AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "D" THEN DO:
         FOR EACH Creditos WHERE 
                   Creditos.Agencia     EQ AgeWk AND
                   Creditos.Sdo_Capital GT 0     AND
                   Creditos.FOR_Pago    NE 2     AND
                   Creditos.Estado      EQ 2     AND
                   Creditos.Categoria   EQ "D"   AND 
                   Creditos.Dias_Atraso GT 0     AND
                   Creditos.Val_Atraso  GT 0     AND 
                   Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
              IF FIRST-OF(Creditos.Nit) THEN DO:
                 FIND Clientes WHERE 
                      Clientes.Nit EQ Creditos.Nit AND 
                      Clientes.Fec_Fallecido EQ ?
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Clientes THEN NEXT.
              END.
              RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
              IF XOk THEN DO: XOk = NO. NEXT. END.
              RUN QueryCred. 
         END.
      END.
      IF SUBSTRING(Cmb_TipoPersonas:SCREEN-VALUE,1,1) EQ "E" THEN DO:
         FOR EACH Creditos WHERE 
                    Creditos.Agencia     EQ AgeWk AND
                    Creditos.Sdo_Capital GT 0     AND
                    Creditos.FOR_Pago    NE 2     AND
                    Creditos.Estado      EQ 2     AND
                    Creditos.Categoria   EQ "E"   AND
                    Creditos.Dias_Atraso GT 0 AND
                    Creditos.Val_Atraso  GT 0 AND 
                    Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
             IF FIRST-OF(Creditos.Nit) THEN DO:
                FIND Clientes WHERE 
                     Clientes.Nit EQ Creditos.Nit AND 
                     Clientes.Fec_Fallecido EQ ?
                     NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Clientes THEN NEXT.
             END.
             RUN Verificar_Compromisos_MesActual(OUTPUT XOk).
             IF XOk THEN DO: XOk = NO. NEXT. END.
             RUN QueryCred. 
         END.
      END.
      IF Cmb_TipoPersonas EQ "Compromisos No Cumplidos" THEN DO:
             FOR EACH Personas: DELETE Personas. END.
             FOR EACH Creditos WHERE 
                      Creditos.Agencia     EQ AgeWk AND
                      Creditos.Sdo_Capital GT 0 AND
                      Creditos.FOR_Pago    NE 2 AND
                      Creditos.Estado      EQ 2 AND  
                      Creditos.Dias_Atraso GT 0 AND
                      Creditos.Val_Atraso  GT 0 AND 
                      Creditos.Abogado     EQ NO NO-LOCK BREAK BY Creditos.Nit:
                  IF FIRST-OF(Creditos.Nit) THEN DO:
                     FIND Clientes WHERE 
                          Clientes.Nit EQ Creditos.Nit AND 
                          Clientes.Fec_Fallecido EQ ?
                          NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE Clientes THEN NEXT.
                  END.
                  FIND FIRST Cobros WHERE 
                           Cobros.Num_Credito EQ Creditos.Num_Credito AND
                           Cobros.Nit         EQ Creditos.Nit AND
                           Cobros.Estado      EQ 1 AND
                           Cobros.Fec_Compromiso GE F1 AND
                           Cobros.Fec_Compromiso LE F2 AND
                           Cobros.Fec_Cumplimiento EQ ? NO-LOCK NO-ERROR.
                  IF AVAILABLE Cobros THEN 
                     RUN QueryCred. 
             END.
      END.
  END.
  OPEN QUERY BPersonas FOR EACH Personas WHERE 
                                Personas.Val GT AtrasoMayorA AND
                                Personas.Dia GE DiaMorIni AND
                                Personas.Dia LE DiaMorFin NO-LOCK INDEXED-REPOSITION.
  RUN _SetCurs.r ("ARROW").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BuscarNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BuscarNit wWin
ON LEAVE OF BuscarNit IN FRAME FCer /* Buscar un Nit */
DO:
  OPEN QUERY BPersonas FOR EACH Personas WHERE Personas.Nit EQ SELF:SCREEN-VALUE
       NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-148 wWin
ON CHOOSE OF BUTTON-148 IN FRAME FCer /* Imprimir */
DO:
ASSIGN FRAME FCer WFirma WCC WCargo Cmb_Doc Cmb_TipoPersonas RNumero.
/*FOR EACH Personas:
   RUN F-Documento.r (INPUT INPUT Cmb_Doc, INPUT Personas.Nit,INPUT WFirma, INPUT WCargo, INPUT WCC).
END.*/
 
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  FOR EACH Plano: DELETE Plano. END.
  Listado = w_Pathspl + "L-ENTIDA.lst".
  {incluido\IMPRIMIR_CARTA.I "listado"}.  
  DEFINE VAR W_NomCar AS CHARACTER FORMAT "X(40)".
  DEFINE VAR OkPressed AS LOGICAL.
W_NomCar = W_Pathspl + "\" + Cmb_TipoPersonas:SCREEN-VALUE + ".txt".
MESSAGE "Desea generar un archivo plano con la información" SKIP
        "de las cartas generadas?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
IF choice THEN DO:
  SYSTEM-DIALOG GET-FILE W_NomCar
     TITLE      "Escoja el directorio destino ..."
     INITIAL-DIR W_Pathspl
     FILTERS    "Archivos Texto (*.txt)"   "*.txt"
     RETURN-TO-START-DIR
     UPDATE OKpressed.
  
    OUTPUT TO VALUE(W_NomCar).
    FOR EACH Plano WHERE
        LENGTH(Plano.PDir) GE 10 AND
        LENGTH(Plano.PTel) GE 7  AND
        Plano.PMun NE "":
        DISPLAY Plano.PCon ";" Plano.PAgn ";" Plano.PNom ";" 
                Plano.PDir ";" Plano.PMun ";" Plano.PTel
        WITH FRAME F_Envios WIDTH 300 NO-LABELS USE-TEXT NO-UNDERLINE NO-BOX STREAM-IO.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE(W_NomCar + "_i").
    FOR EACH Plano WHERE
        LENGTH(Plano.PDir) LT 10 OR
        LENGTH(Plano.PTel) LT 7  OR
        Plano.PMun EQ "":
        DISPLAY Plano.PCon ";" Plano.PAgn ";" Plano.PNom ";" 
                Plano.PDir ";" Plano.PMun ";" Plano.PTel
        WITH FRAME F_Enviosi WIDTH 300 NO-LABELS USE-TEXT NO-UNDERLINE NO-BOX STREAM-IO.
    END.
    OUTPUT CLOSE.

END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME FCer /* Salir */
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


&Scoped-define SELF-NAME BUTTON-171
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-171 wWin
ON CHOOSE OF BUTTON-171 IN FRAME FCer /* Button 171 */
DO:
    RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-172
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-172 wWin
ON CHOOSE OF BUTTON-172 IN FRAME FCer /* Ver todos los de la categoría */
DO:
  APPLY "choose" TO Btn_Query IN FRAME FCer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Filtros
&Scoped-define SELF-NAME BUTTON-176
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-176 wWin
ON CHOOSE OF BUTTON-176 IN FRAME Filtros /* Button 176 */
DO:
  ASSIGN FRAME Filtros AtrasoMayorA DiaMorIni DiaMorFin.
  OPEN QUERY BPersonas
       FOR EACH Personas WHERE
                Personas.Val GT AtrasoMayorA AND
                Personas.Dia GE DiaMorIni AND
                Personas.Dia LE DiaMorFin
                NO-LOCK INDEXED-REPOSITION.
  HIDE FRAME Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCer
&Scoped-define SELF-NAME BUTTON-177
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-177 wWin
ON CHOOSE OF BUTTON-177 IN FRAME FCer /* Filtrar Información */
DO:
  VIEW FRAME Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Filtros
&Scoped-define SELF-NAME BUTTON-181
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-181 wWin
ON CHOOSE OF BUTTON-181 IN FRAME Filtros /* Tomar Valores Predeterminados */
DO:
    APPLY "choose" TO Btn_Query IN FRAME FCer.
    HIDE FRAME Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME BUTTON-182
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-182 wWin
ON CHOOSE OF BUTTON-182 IN FRAME F_Compromisos /* Salir */
DO:
  ASSIGN FRAME F_Compromisos F1 F2.
  HIDE FRAME F_compromisos.
  IF F1 EQ ? OR F2 EQ ? THEN DO:
     MESSAGE "Para poder consultar los creditos que tienen compromisos" SKIP
             "entre una fecha, deben digitarse las fechas primero" SKIP
             "Rectifique el rango de fechas!!!." VIEW-AS ALERT-BOX INFORMATION.
     VIEW FRAME F_Compromisos.
     APPLY "entry" TO F1 IN FRAME F_Compromisos.
     RETURN NO-APPLY.
  END.
  APPLY "choose" TO Btn_Query IN FRAME FCer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCer
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME FCer /* Agencia */
DO:
  APPLY "choose" TO Btn_Query IN FRAME FCer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TipoPersonas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipoPersonas wWin
ON VALUE-CHANGED OF Cmb_TipoPersonas IN FRAME FCer /* Tipos de Personas */
DO:
  IF SELF:SCREEN-VALUE EQ "Compromisos No Cumplidos" THEN DO:
     ASSIGN F1 = ? F2 = ?.
     VIEW FRAME F_Compromisos.
     DISPLAY F1 F2 WITH FRAME F_Compromisos.
     APPLY "entry" TO F1 IN FRAME F_Compromisos.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ROrganiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ROrganiza wWin
ON VALUE-CHANGED OF ROrganiza IN FRAME FCer
DO:
  ASSIGN FRAME Fcer ROrganiza.
  CASE ROrganiza:
      WHEN 1 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Nit INDEXED-REPOSITION.
      WHEN 2 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Nom INDEXED-REPOSITION.
      WHEN 3 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Val DESCENDING INDEXED-REPOSITION.
      WHEN 4 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Dif DESCENDING INDEXED-REPOSITION.
      WHEN 5 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Imo DESCENDING INDEXED-REPOSITION.
      WHEN 6 THEN  OPEN QUERY BPersonas
                   FOR EACH Personas NO-LOCK BY Personas.Dia DESCENDING INDEXED-REPOSITION.

  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BPersonas
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carta wWin 
PROCEDURE Carta :
DEFINE INPUT PARAMETER WNit LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER WCod LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER WCre LIKE Creditos.Num_Credito.
 
 DEFINE VAR WCiu        AS CHARACTER FORMAT "X(30)".
 DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
 DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WMItad      AS INTEGER.
 DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(11)".
 DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(20)".

 DEFINE VAR puntero AS ROWID.

 DEFINE VAR WCar AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Cargo*/
 DEFINE VAR WEmp AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Empresa*/
 DEFINE VAR WPro AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Profesion*/


 DEFINE VAR W_NroDoc           AS   INTEGER INITIAL 0.
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_PrimerCom        AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario2      AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Nomofi           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Rpta             AS   LOGICAL. 

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".
  

  IF WCod NE "" THEN
     FIND FIRST Clientes WHERE Clientes.nit EQ WCod NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Clientes WHERE Clientes.nit EQ WNit NO-LOCK NO-ERROR.

  IF AVAILABLE(Clientes) THEN DO:
     W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     IF DIR_Correspondencia THEN DO: /*mandar a la oficina*/
        FIND Ubicacion WHERE Ubicacion.Ubicacion BEGINS 
             SUBSTRING(Clientes.Lugar_comercial,1,5) AND 
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN WCiu = Ubicacion.Nombre.
        ELSE WCiu = "".
     END.
     ELSE DO:
        FIND Ubicacion WHERE Ubicacion.Ubicacion BEGINS
             SUBSTRING(Clientes.Lugar_Residencia,1,5) AND
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN WCiu = Ubicacion.Nombre.
        ELSE WCiu = "".
     END.
     FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN WPro = Varios.Descripcion.
     FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN WCar = Varios.Descripcion.
     puntero = ROWID(Clientes).
     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE Empresas THEN DO:
        FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN WEmp = Clientes.Nombre + " " + Clientes.Apellido1  + " " + Clientes.Apellido2.
        FIND Clientes WHERE ROWID(Clientes) EQ Puntero NO-ERROR.
     END.
  END.
  
  FIND Agencias WHERE Agencias.Agencia = W_Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(Agencias) THEN DO:
   W_NomOfi = Agencias.Nombre.
   FIND Entidad WHERE Entidad.Entidad EQ Agencia.Entidad NO-LOCK NO-ERROR.
   IF AVAILABLE(Entidad) THEN 
    ASSIGN W_NomEntidad = Entidad.Nombre 
           W_NitEnti    = Entidad.Nit
           w_ConcatEnti = TRIM(W_NomEntidad) + " " + "  Nit: " + w_NitEnti.
  END.
  
  ASSIGN FRAME FCer Cmb_Doc.
  IF LENGTH(Cmb_Doc:SCREEN-VALUE IN FRAME FCer) GT 0 THEN
     DISPLAY Cmb_Doc AT 8 WITH FRAME FDoc NO-LABELS WIDTH 132.
  
  IF LENGTH(WEncabezado) GT 0 THEN
     DISPLAY WEncabezado AT 8 VIEW-AS EDITOR SIZE 97 BY 4 WITH FRAME FEnc1 NO-LABELS WIDTH 132.

  IF Documentos.Id_EnviaCodeudor AND WCod NE "" THEN DO:
      DISPLAY 
        W_NomCli                       AT 7                                    SKIP(1)
        "Identificación del Cliente: " AT 7  WCod                              SKIP
        "Ciudad                    : " AT 7  WCiu                              SKIP
        "Dirección                 : " AT 7  Clientes.DIR_Residencia           SKIP
        "Teléfono                  : " AT 7  Clientes.Tel_Residencia           SKIP
        "Codeudor de la Obligación : " AT 7  Personas.NPg                      SKIP
        "Cliente Deudor            : " AT 7  WNit " - " Personas.Nom          
      WITH FRAME F_EncNom1 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
  END.
  ELSE DO:
      DISPLAY 
        W_NomCli                       AT 7                                   SKIP(1)
        "Ciudad                    : " AT 7 WCiu                              SKIP
        "Identificación del Cliente: " AT 7 WNit                              SKIP
        "Dirección                 : " AT 7 Clientes.DIR_Residencia           SKIP
        "Teléfono                  : " AT 7 Clientes.Tel_Residencia           
      WITH FRAME F_EncNom2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
  END.
  Numero = Numero + 1.
  CREATE Plano.
  ASSIGN Plano.PCon = Numero
         Plano.PAgn = Clientes.Agencia
         Plano.PNom = W_nomCli
         Plano.PDir = Clientes.DIR_Residencia
         Plano.PMun = WCiu
         Plano.PTel = Clientes.Tel_Residencia.

  IF LENGTH(Documentos.Cuerpo) GT 0 THEN
     DISPLAY SKIP(2)
            Documentos.Cuerpo AT 8 VIEW-AS EDITOR SIZE 97 BY 4 WITH FRAME FCue NO-LABELS WIDTH 132.
            
  IF Documentos.Id_Ahorros AND WCod EQ "" THEN DO:
     FIND FIRST Ahorros WHERE Ahorros.Nit EQ WNit NO-LOCK NO-ERROR.
     IF AVAILABLE Ahorros THEN DO:
         DISPLAY SKIP(1)
         "TipAhorro   Num.Cuenta         Apertura  Fec.UltTrans Plazo     Tasa        Saldo" AT 7
         WITH FRAME FAhoEnc WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
         FOR EACH Ahorros WHERE Ahorros.Nit EQ WNit AND
                  Ahorros.Estado EQ 1 NO-LOCK BREAK BY Ahorros.Tip_Ahorro:
            CASE Ahorros.Tip_Ahorro:
              WHEN 1 THEN WTipAho = "A la Vista".
              WHEN 2 THEN WTipAho = "Contractual".
              WHEN 3 THEN WTipAho = "A Termino".
              WHEN 4 THEN WTipAho = "Aportes".
            END CASE.
            DISPLAY 
                    WTipAho  AT 7               
                    Ahorros.Cue_Ahorros     
                    Ahorros.Fec_Apertura     
                    Ahorros.Fec_UltTransaccion
                    Ahorros.Plazo             
                    Ahorros.Tasa              
                    TRIM(STRING(Ahorros.Sdo_Disponible,">>,>>>,>>>,>>9")) 
            WITH FRAME FCUE1 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
         END.
     END.
  END.

  IF Documentos.Id_Creditos THEN DO:
     FIND FIRST Creditos WHERE
                Creditos.Nit         EQ WNit AND
                Creditos.Num_Credito EQ WCre AND
                Creditos.Estado      EQ 2    AND
                Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.

     IF AVAILABLE Creditos THEN DO:
         FIND agencia WHERE agencia.agencia EQ creditos.agencia NO-LOCK NO-ERROR.
         DISPLAY SKIP(1)
             "Agencia     Pagare  Fec.Desembolso  Pla    Tasa         Vlr. Cuota            Saldo Capital" AT 7 SKIP(1) 
             SUBSTR(TRIM(Agencia.Nombre),1,11) AT  7
             Creditos.Pagare                   AT 19 FORMAT "X(8)"
             Creditos.Fec_Desembolso           AT 30 FORMAT "99/99/9999"
             Creditos.Plazo                    AT 42 FORMAT "zzz9"
             Creditos.Tasa                     AT 47
             Creditos.Cuota                    AT 59 FORMAT "zzz,zzz,zz9.99"
             Creditos.Sdo_Capital              AT 77
            WITH FRAME FCUE2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
            IF Documentos.Id_Atrasos THEN
               RUN Mostrar_Atrasos.
            IF Documentos.Id_GarAdmisible THEN
               RUN Mostrar_Admisibles.
            IF Documentos.Id_GarPersonal AND WCod EQ "" THEN
               RUN Mostrar_Codeudores.
       /*     IF Cmb_TipoPersonas EQ "Compromisos No Cumplidos" THEN DO:
               DISPLAY "COMPROMISOS NO CUMPLIDOS."  AT 7 SKIP
                       "-------------------------------------------------------------------------------------------" AT 7 SKIP
                       "Compromiso    Acuerdo  Valor Acuerdo" AT 7 SKIP
                       WITH FRAME F_EncCompro WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
               FOR EACH Cobros WHERE 
                        Cobros.Nit         EQ Creditos.Nit AND
                        Cobros.Num_Credito EQ Creditos.Num_Credito AND
                        Cobros.Estado      EQ 1 AND 
                        Cobros.Fec_Compromiso GE F1 AND 
                        Cobros.Fec_Compromiso LE F2 NO-LOCK:
                   DISPLAY Cobros.Fec_Compromiso AT 7 Cobros.Fec_Acuerdo Cobros.Val_Compromiso
                       WITH FRAME F_MovCompro WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
               END.
            END.   */
           /*"Pagare       Fec.Desembolso  Pla    Tasa                MtoInicial                    Saldo" AT 7 SKIP(1)  */
     END.
  END.
  
  
  IF Documentos.Id_Cliente THEN DO:
     DISPLAY SKIP(1)
             " INFORMACION DEL CLIENTE --------------------------------------------------------------- " AT 8 SKIP(1)
             "Dirección Residencia    : "     AT 8
              Clientes.DIR_Residencia         AT 37
             "Telefono  Residencia    : "     AT 8
              Clientes.Tel_Residencia         AT 37
             "Dirección Comercial     : "     AT 8
              Clientes.DIR_Comercial          AT 37
             "Telefono  Comercial     : "     AT 8
              Clientes.Tel_Comercial          AT 37
             "Telefono  Movil         : "     AT 8
              Clientes.Celular                AT 37
             "Correo Electronico      : "     AT 8
              Clientes.email                  AT 37
             "Estado Civil            : "     AT 8
              Clientes.Est_Civil              AT 37
             "Personas a Cargo        : "     AT 8
              Clientes.Per_Acargo             AT 37
             "Número de Hijos         : "     AT 8
              Clientes.Num_Hijos              AT 37
             "Nivel Educativo         : "     AT 8 
              Clientes.Niv_Educativo          AT 37
             "Profesión               : "     AT 8
              WPro                            AT 37
             "Empresas donde Trabaja  : "     AT 8
              WEmp                            AT 37
             "Cargo que Desempeña     : "     AT 8
              WCar                            AT 37
             "Salario                 : "     AT 8
              Clientes.Salario                AT 37
     WITH FRAME FCUE4 WIDTH 97 NO-LABELS USE-TEXT NO-BOX.
  END.


  IF LENGTH(Documentos.Pie_Pagina) GT 0 THEN
     DISPLAY SKIP(2)
            Documentos.Pie_Pagina AT 7 VIEW-AS EDITOR SIZE 97 BY 4 WITH FRAME FPie NO-LABELS WIDTH 132.

  IF WFirma NE "" THEN
     DISPLAY SKIP(4)
             "_______________________________________" AT 8 SKIP(1)
            WFirma AT 8 
            WCargo AT 8 SKIP(1)
            "C.C:" AT 8
            WCC    AT 17 WITH FRAME FPie USE-TEXT NO-BOX NO-LABELS WIDTH 132.
    
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearCodeudores wWin 
PROCEDURE CrearCodeudores :
FOR EACH Relaciones WHERE 
             Relaciones.Nit            EQ Creditos.Nit         AND
             INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
             Relaciones.Clase_Producto EQ 2                    AND
             Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
             Relaciones.Cod_Relacion   EQ 11 NO-LOCK BREAK BY Relaciones.Nit_Relacion:
        FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
           CREATE TCode. ASSIGN TCode.TCod = Relaciones.Nit_Relacion
                                TCode.TNit = Creditos.Nit
                                TCode.TCre = Creditos.Num_Credito.
        END.
    END.

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
  DISPLAY WFirma WCargo WCC Cmb_Doc Cmb_Agencias Cmb_TipoPersonas BuscarNit 
          ROrganiza RNumero 
      WITH FRAME FCer IN WINDOW wWin.
  ENABLE BUTTON-171 WFirma WCargo WCC Cmb_Doc Cmb_Agencias BUTTON-177 
         Cmb_TipoPersonas Btn_Query BUTTON-172 BuscarNit BPersonas ROrganiza 
         BUTTON-148 BUTTON-149 RNumero RECT-283 RECT-285 RECT-286 
      WITH FRAME FCer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCer}
  DISPLAY F1 F2 
      WITH FRAME F_Compromisos IN WINDOW wWin.
  ENABLE F1 F2 BUTTON-182 
      WITH FRAME F_Compromisos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Compromisos}
  DISPLAY AtrasoMayorA DiaMorIni DiaMorFin 
      WITH FRAME Filtros IN WINDOW wWin.
  ENABLE AtrasoMayorA DiaMorIni DiaMorFin BUTTON-176 BUTTON-181 RECT-284 
      WITH FRAME Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Filtros}
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
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  Cmb_Doc:LIST-ITEMS IN FRAME FCer = "".
     FOR EACH Documentos NO-LOCK:
        W_Ok = Cmb_Doc:ADD-LAST(Documentos.Nombre) IN FRAME FCer.
     END.
  IF Cmb_Doc:LIST-ITEMS IN FRAME FCer NE "" THEN
     Cmb_Doc:SCREEN-VALUE IN FRAME FCer = Cmb_Doc:ENTRY(1).
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
  END.
  APPLY "entry" TO WFirma IN FRAME FCer.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Admisibles wWin 
PROCEDURE Mostrar_Admisibles :
DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
 DEFINE VAR WEncabezado AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WMItad      AS INTEGER.
 DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(20)".
 DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(12)".
 FIND FIRST Garantias WHERE
            Garantias.Num_Credito EQ Creditos.Num_Credito AND
            Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
 IF AVAILABLE Garantias THEN DO:
     DISPLAY "GARANTIAS ADMISIBLES."  AT 7 SKIP
             "----------------------------------------------------------------------------------" AT 7 SKIP
             "Tip.Garantia Fec.Matric Identifiac   Nombre.Garantia                Valor.Garantia" AT 7 SKIP
             WITH FRAME F_EncGarAdm WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
             
     FOR EACH Garantias WHERE
              Garantias.Num_Credito EQ Creditos.Num_Credito AND
              Garantias.Estado EQ 1 NO-LOCK
              BREAK BY Garantias.Tip_Credito BY Garantias.Cod_Credito BY Garantias.Num_Credito:
         IF Garantias.Tipo_Garantia EQ 1 THEN WTipGar = "Propiedad".
         IF Garantias.Tipo_Garantia EQ 2 THEN WTipGar = "Vehiculo".
         IF Garantias.Tipo_Garantia EQ 3 THEN WTipGar = "Inversion".
         IF Garantias.Tipo_Garantia EQ 4 THEN WTipGar = "No Admisible".
         DISPLAY WTipGar                AT 7  
                 Garantias.Fec_Creacion   
                 Garantias.Identificacion_Bien 
                 Garantias.Nom_Bien FORMAT "X(30)"
                 Garantias.Val_Bien        
           WITH FRAME FCUE5 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Atrasos wWin 
PROCEDURE Mostrar_Atrasos :
DEFINE VAR TotMora  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SdoVdo   AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SdoDeuda AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

SdoDeuda = Creditos.Honorarios + Creditos.Polizas + Creditos.Costas + Creditos.INT_MorCobrar +
           Creditos.Int_MoraDifCob + Creditos.INT_Corrientes + Creditos.INT_DifCobro +
           Creditos.Sdo_Capital - Creditos.INT_Anticipado.

SdoVdo   = Creditos.Honorarios + Creditos.Polizas + Creditos.Costas + Creditos.INT_MorCobrar +
           Creditos.INT_MoraDifCob.

IF Creditos.Capital_Acum GT Creditos.Sdo_CapPag THEN
   SdoVdo = SdoVdo + (Creditos.Capital_Acum - Creditos.Sdo_CapPag).

IF Creditos.INT_LiqAcum GT Creditos.Sdo_IntPag THEN
   SdoVdo = SdoVdo + (Creditos.INT_LiqAcum - Creditos.Sdo_IntPag).

IF SdoVdo GT SdoDeuda THEN DO:
    SdoVdo = SdoDeuda.
    DISPLAY SKIP(1)
            "                                                          Honorarios :     "  AT 7 
            Creditos.Honorarios FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                              Costas :     "   AT 7 
            Creditos.Costas FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                             Polizas :     "   AT 7 
            Creditos.Polizas FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                     Interes de Mora :     "   AT 7 
            Creditos.INT_MorCobrar FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                           Interes Mora Dificl Cobro :     "   AT 7 
            Creditos.INT_MoraDifCob FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                   Interes Corriente :     "   AT 7 
            Creditos.Int_Corrientes FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                               Interes Dificil Cobro :     "   AT 7 
            Creditos.Int_DifCobro FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                       Saldo Capital :     "   AT 7 
            Creditos.Sdo_Capital FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                              (-) Interes Anticipado :      "   AT 7 
            Creditos.INT_Anticipado FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                  -----------------------------------------" AT 7 SKIP
            "                                                               Total :     "                 AT 7
            SdoDeuda FORMAT ">>>,>>>,>>>,>>9"
    WITH FRAME FAtraso WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
END.
ELSE DO:
    DISPLAY SKIP(1)
            "                                                          Honorarios :     "  AT 7 
            Creditos.Honorarios FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                              Costas :     "   AT 7 
            Creditos.Costas FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                             Polizas :     "   AT 7 
            Creditos.Polizas FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                     Interes de Mora :     "   AT 7 
            Creditos.INT_MorCobrar FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                           Interes Mora Dificl Cobro :     "   AT 7 
            Creditos.INT_MoraDifCob FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                            Capital Acumulado - Saldo Capital Pagado :     "   AT 7 
            (Creditos.Capital_Acum - Creditos.Sdo_CapPag) FORMAT ">>>,>>>,>>>,>>9" SKIP
            "               Interes Liquidado Acumulado - Saldo de Interes Pagado :     "   AT 7 
            (Creditos.INT_LiqAcum - Creditos.Sdo_IntPag) FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                  -----------------------------------------" AT 7 SKIP
            "                                                               Total :     "                 AT 7
            SdoVdo FORMAT ">>>,>>>,>>>,>>9"
    WITH FRAME FAtraso2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.

END.

/*    TotMora = Creditos.Val_Atraso + Creditos.INT_DifCobro + Creditos.INT_MorCobrar + Creditos.Int_Corrientes.
    DISPLAY SKIP(1)
            "                                                        Dias de Mora :                "  AT 7 
            Creditos.Dias_Atraso  FORMAT "9999" SKIP
            "                                                       Saldo en Mora :     "   AT 7 
            Creditos.Val_Atraso   FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                   Int.Dificil.Cobro :     "   AT 7 
            Creditos.INT_DifCobro FORMAT ">>>,>>>,>>>,>>9"  SKIP
            "                                                     Interes de Mora :     "   AT 7 
            Creditos.INT_MorCobrar FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                   Interes Corriente :     "   AT 7 
            Creditos.Int_Corrientes FORMAT ">>>,>>>,>>>,>>9" SKIP
            "                                                  -----------------------------------------" AT 7 SKIP
            "                                                               Total :     "                 AT 7
            TotMora FORMAT ">>>,>>>,>>>,>>9"
    WITH FRAME FAtraso WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Codeudores wWin 
PROCEDURE Mostrar_Codeudores :
FOR EACH TCode: DELETE TCode. END.
DEFINE VAR Wnom AS CHARACTER FORMAT "X(40)".
FIND FIRST Relaciones WHERE 
         Relaciones.Nit            EQ Creditos.Nit         AND
         INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
         Relaciones.Clase_Producto EQ 2                    AND
         Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
         Relaciones.Cod_Relacion   EQ 11 NO-LOCK NO-ERROR.
IF AVAILABLE Relaciones THEN DO:
    DISPLAY SKIP(1)
            "GARANTIAS PERSONALES." AT 7 SKIP
            "Nit          Nombre                                  Telefono" AT 7 SKIP
            "-------------------------------------------------------------" AT 7
    WITH FRAME F_EncCodeudores WIDTH 97 NO-LABELS USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE.
    FOR EACH Relaciones WHERE 
             Relaciones.Nit            EQ Creditos.Nit         AND
             INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
             Relaciones.Clase_Producto EQ 2                    AND
             Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
             Relaciones.Cod_Relacion   EQ 11 NO-LOCK BREAK BY Relaciones.Nit_Relacion:
        FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
           WNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
           DISPLAY Clientes.Nit AT 7 WNom Clientes.Tel_Residencia
           WITH FRAME F_MovCodeudores WIDTH 97 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
           CREATE TCode. ASSIGN TCode.TCod = Relaciones.Nit_Relacion
                                TCode.TNit = Creditos.Nit
                                TCode.TCre = Creditos.Num_Credito.
        END.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*{Incluido\RepEncabezado.i}*/
  
 DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
 DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WMItad      AS INTEGER.
 DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(20)".
 DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(20)".


 DEFINE VAR puntero AS ROWID.

 DEFINE VAR WCar AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Cargo*/
 DEFINE VAR WEmp AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Empresa*/
 DEFINE VAR WPro AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Profesion*/


 DEFINE VAR W_NroDoc           AS   INTEGER INITIAL 0.
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario2      AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Rpta             AS   LOGICAL. 

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".
 DEFINE VAR NitIni    LIKE Clientes.Nit.
 DEFINE VAR NitFin    LIKE Clientes.Nit.
 
 FIND Documentos WHERE Documentos.Nombre EQ Cmb_Doc:SCREEN-VALUE IN FRAME FCer NO-LOCK NO-ERROR.
 ASSIGN WEncabezado = Documentos.Encabezado.

    
IF RNumero EQ 1 THEN
   ASSIGN NitIni = Personas.Nit:SCREEN-VALUE IN BROWSE BPersonas
          NitFin = NitIni.
ELSE
   ASSIGN NitIni = ""
          NitFin = "9999999999999".

Numero = 0.
FOR EACH Personas WHERE
         Personas.Nit GE NitIni       AND
         Personas.Nit LE NitFin       AND
         Personas.Val GT AtrasoMayorA AND
         Personas.Dia GE DiaMorIni    AND
         Personas.Dia LE DiaMorFin    BREAK BY Personas.Nit:
  FOR EACH TCode: DELETE TCode. END.
  RUN Carta(INPUT Personas.Nit, INPUT "", INPUT Personas.NCr).
  IF Documentos.Id_EnviaCodeudor THEN DO:
     FIND FIRST TCode NO-ERROR.
     IF NOT AVAILABLE TCode THEN RUN CrearCodeudores.
     FOR EACH TCode WHERE
              TCode.TCre EQ Personas.NCr:
         RUN Carta(INPUT TCode.TNit, INPUT TCode.TCod, INPUT TCode.TCre).
     END.
  END.
END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryCred wWin 
PROCEDURE QueryCred :
FIND Personas WHERE Personas.Nit EQ Creditos.Nit         AND 
                    Personas.NCr EQ Creditos.Num_Credito NO-ERROR.
  IF NOT AVAILABLE Personas THEN DO:                                                                
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-ERROR.                                             
     IF AVAILABLE Clientes THEN DO:                                                                            
        CREATE Personas.                                                                                       
        ASSIGN Personas.Nit = Clientes.Nit                                                                     
               Personas.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               Personas.NCr = Creditos.Num_Credito
               Personas.Val = Creditos.Val_Atraso
               Personas.Dif = Creditos.INT_DifCobro
               Personas.IMo = Creditos.INT_MorCobrar
               Personas.Dia = Creditos.Dias_Atraso
               Personas.NPg = Creditos.Pagare.  
     END.
  END.                                                                                                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Compromisos_MesActual wWin 
PROCEDURE Verificar_Compromisos_MesActual :
DEFINE OUTPUT PARAMETER XOk AS LOGICAL.
FIND FIRST Cobros WHERE 
           Cobros.Num_Credito EQ Creditos.Num_Credito AND
           Cobros.Nit         EQ Creditos.Nit AND
           Cobros.Estado      EQ 1     AND 
           Creditos.Abogado   EQ NO    AND 
           Cobros.Fec_Compromiso GE DATE("01/" + STRING(MONTH(W_Fecha),"99") + "/" + STRING(YEAR(W_Fecha),"99")) AND
           Cobros.Fec_Compromiso LE W_Fecha AND
           Cobros.Fec_Cumplimiento EQ ?
           NO-LOCK NO-ERROR.
IF AVAILABLE Cobros THEN XOk = YES. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

