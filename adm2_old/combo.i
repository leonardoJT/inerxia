&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*********************************************************************
* Copyright (C) 2005 by Progress Software Corporation. All rights    *
* reserved.  Prior versions of this work may contain portions        *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*-------------------------------------------------------------------------
    File        : combo.i
    Purpose     : Basic Method Library for the ADMClass combo.
  
    Syntax      : {src/adm2/combo.i}

    Description :
  
    Modified    : 08/14/2001
-------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF "{&ADMClass}":U = "":U &THEN
  &GLOB ADMClass combo
&ENDIF

&IF "{&ADMClass}":U = "combo":U &THEN
  {src/adm2/combprop.i}
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 8
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm2/lookupfield.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

&IF DEFINED(ADM-EXCLUDE-STATIC) = 0 &THEN
  /* Starts super procedure */
  IF NOT {&ADM-LOAD-FROM-REPOSITORY} THEN
  DO:
    RUN start-super-proc("adm2/combo.p":U).
    /* Subscribe to viewer events  */

    RUN modifyListProperty(THIS-PROCEDURE, "ADD":U, 
                      "ContainerSourceEvents":U,"getComboQuery":U).
    RUN modifyListProperty(THIS-PROCEDURE, "ADD":U, 
                      "ContainerSourceEvents":U,"displayCombo":U).
    RUN modifyListProperty(THIS-PROCEDURE, "ADD":U, 
                      "DataSourceEvents":U,"queryOpened":U).

    /* New API */
    RUN modifyListProperty(THIS-PROCEDURE, "ADD":U, 
                      "ContainerSourceEvents":U,"prepareField":U).
    RUN modifyListProperty(THIS-PROCEDURE, "ADD":U, 
                      "ContainerSourceEvents":U,"displayField":U).
  END.
  /* _ADM-CODE-BLOCK-START _CUSTOM _INCLUDED-LIB-CUSTOM CUSTOM */

  {src/adm2/custom/combocustom.i}

  /* _ADM-CODE-BLOCK-END */
  
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


