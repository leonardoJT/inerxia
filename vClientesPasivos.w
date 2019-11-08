&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dact_pasivos.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
DEF VAR cAlfbto AS CHAR NO-UNDO INITIAL " ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".
DEF VAR cDgtos AS CHAR NO-UNDO INITIAL "1,2,3,4,5,6,7,8,9,0".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dact_pasivos.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Nombre RowObject.Val_Comercial ~
RowObject.Val_Cuota RowObject.Val_Saldo 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-306 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nombre RowObject.Val_Comercial ~
RowObject.Val_Cuota RowObject.Val_Saldo 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fchar vTableWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDgto vTableWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTxtoCmpo vTableWin 
FUNCTION fTxtoCmpo RETURNS CHARACTER
  (h AS HANDLE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 53 BY 4.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nombre AT ROW 1.19 COL 10 COLON-ALIGNED
          LABEL "Entidad"
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .81
          BGCOLOR 15 
     RowObject.Val_Comercial AT ROW 2.23 COL 10 COLON-ALIGNED WIDGET-ID 32
          LABEL "Valor Inicial"
          VIEW-AS FILL-IN 
          SIZE 25.43 BY .81
          BGCOLOR 15 
     RowObject.Val_Cuota AT ROW 3.31 COL 10 COLON-ALIGNED WIDGET-ID 34
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 25.43 BY .81
          BGCOLOR 15 
     RowObject.Val_Saldo AT ROW 4.35 COL 10 COLON-ALIGNED WIDGET-ID 36
          LABEL "Saldo"
          VIEW-AS FILL-IN 
          SIZE 25.43 BY .81
          BGCOLOR 15 
     RECT-306 AT ROW 1 COL 1 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 53 BY 4.58
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dact_pasivos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dact_pasivos.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 4.58
         WIDTH              = 53.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Comercial IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Cuota IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Saldo IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

    ON 'any-printable':U OF RowObject.Nombre
    DO:
        IF NOT fchar(KEYLABEL(LASTKEY))
        THEN RETURN NO-APPLY.
        RETURN.
    END.
/*
    /* SOLO NUMERICO */
    ON 'any-printable':U OF 
    DO:
        IF NOT fdgto(KEYLABEL(LASTKEY))
        THEN RETURN NO-APPLY.
        RETURN.
    END.
    /* CONVIERTE A MAYUSCULAS */
    ON 'leave':U OF nacionalidad,clientes.nom_arrendatario,R_Apellido2 in FRAME f_conyuge,R_Empresa  in FRAME f_conyuge
    DO:
        SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
        RETURN.
    END.

    /* INDICATIVOS TELEFONICOS */
    ON 'leave':U OF anexos_clientes.ind_cli,anexos_clientes.ind_arr,R_Tel_Empresa-Indctvo IN FRAME f_conyuge,anexos_clientes.ind_comercial IN FRAME f_ubicacion
    DO:
        IF length(LEFT-TRIM(SELF:SCREEN-VALUE,"0")) < 2
        THEN DO:
            MESSAGE "Indicativo Incorrecto"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.

        RETURN.
    END.
    /* TELEFONOS */
    ON 'leave':U OF clientes.tel_residencia,clientes.tel_arrendatario,R_Tel_Empresa IN FRAME f_conyuge,clientes.tel_comercial IN FRAME F_Ubicacion
    DO:
        IF LENGTH(LEFT-TRIM(SELF:SCREEN-VALUE)) < 6
        THEN DO:
            MESSAGE "Número Telefónico Incorrecto"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    /*
      DEFI VAR K AS INTEG FORM "99".

      ASSIGN Tel_Numerico = DEC(Clientes.Tel_Resid:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Solo Numèrico ...Corrija por favor."
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.

      IF LENGTH(Clientes.Tel_Resid:SCREEN-VALUE) < 7 THEN DO:
        MESSAGE "Debe contener minimo 7 caracteres.... Corriha por Favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
      END.
    */  
    END.
    ON 'leave':U OF clientes.nombre,clientes.apellido1,R_Nombre IN FRAME f_conyuge,R_Apellido1 IN FRAME f_conyuge
    DO:
        SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
        IF LENGTH(trim(SELF:SCREEN-VALUE)) = 0
        THEN DO:
            MESSAGE fTxtoCmpo(SELF) " Incorrecto"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ASSIGN Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes = CAPS(Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes)
             NomLN = Clientes.Nombre:SCREEN-VALUE.
        /*IF Clientes.Tipo_Identificacion:SCREEN-VALUE = "Nit" THEN DO:
         FIND FIRST ListaNegra WHERE ListaNegra.Nombre EQ NomLN NO-LOCK NO-ERROR.
         IF AVAILABLE ListaNegra THEN DO:
            MESSAGE "Esta empresa se encuentra en las listas negras" SKIP
                    "de la cooperativa. y se identifica con el nit: " ListaNegra.Nit SKIP
                    "La operacion se cancelara!" SKIP(1)
                    "Reporte este incidente al departamento de CI" VIEW-AS ALERT-BOX WARNING TITLE "Lista Negra".
            APPLY "choose" TO btn_cancelar.
         END.
        END.*/
    END.

    ON 'leave':U OF clientes.salario IN FRAME f_economica,clientes.ing_honorarios IN FRAME f_economica,clientes.ing_financieros IN FRAME f_economica,clientes.ing_arriendos IN FRAME f_economica,clientes.ing_otros IN FRAME f_economica,clientes.gto_obligacion IN FRAME f_economica,clientes.gto_arriendo IN FRAME f_economica,clientes.sdo_obligaciones IN FRAME f_economica,anexos_clientes.gto_targetacredito IN FRAME f_economica,clientes.gto_familiar IN FRAME f_economica,anexos_clientes.gto_otros IN FRAME f_economica
    DO:
        IF decimal(SELF:SCREEN-VALUE) > 0
        THEN IF decimal(SELF:SCREEN-VALUE) < 1000
        THEN DO:
            MESSAGE "Valor Incorrecto"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        RUN Totales_Economica.
        DO WITH FRAME F_Economica:
            IF INT(Clientes.ing_otros:SCREEN-VALUE) <> 0 
            THEN DO:
                ENABLE  Anexos_Clientes.Especifique_OtrosIng.
                        Anexos_Clientes.Especifique_OtrosIng:BGCOLOR = 15.
                        Anexos_Clientes.Especifique_OtrosIng:FGCOLOR = 0.
                END.
                ELSE DO:
                    ASSIGN  Anexos_Clientes.Especifique_OtrosIng:SCREEN-VALUE = '0'.
                    DISABLE Anexos_Clientes.Especifique_OtrosIng.
                            Anexos_Clientes.Especifique_OtrosIng:BGCOLOR = 18.
                            Anexos_Clientes.Especifique_OtrosIng:FGCOLOR = 15.
            END.
        END.
    END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FBusqueda_Pasivos vTableWin 
PROCEDURE FBusqueda_Pasivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DEFINE INPUT PARAMETER P_Nit LIKE Act_Pasivos.Nit.

  DO WITH FRAME {&FRAME-NAME}:
  
    FIND FIRST Act_Pasivos WHERE Act_Pasivos.Nit = p_nit
                            AND Act_Pasivos.Cod_Tipo = 4 
    NO-LOCK NO-ERROR.
    IF AVAILABLE act_pasivos THEN DO:
/*      DISPLAY Act_Pasivos.Nit @  RowObject.Nit                    */
/*              Act_Pasivos.Cod_Tipo @ RowObject.Cod_Tipo           */
/*              Act_Pasivos.Cod_relacion @  RowObject.Cod_relacion. */

    END.

  END. /*Fin del do*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fchar vTableWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fchar
    Notes:  
------------------------------------------------------------------------------*/
    RETURN CAN-DO(cAlfbto,LC(c)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDgto vTableWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: fDgto  
------------------------------------------------------------------------------*/

    RETURN CAN-DO(cdgtos,c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTxtoCmpo vTableWin 
FUNCTION fTxtoCmpo RETURNS CHARACTER
  (h AS HANDLE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fTxtoCmpo
    Notes:  
------------------------------------------------------------------------------*/
    RETURN
    "'" +
    (IF NOT h:LABEL = ?
    THEN h:LABEL
    ELSE
    IF NOT h:HELP = ?
    THEN h:HELP
    ELSE h:NAME) +
    "'".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

