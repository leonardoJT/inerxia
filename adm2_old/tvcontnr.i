&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*-------------------------------------------------------------------------
    File        : tvcontnr.i
    Purpose     : Basic Method Library for the ADMClass tvcontnr.
  
    Syntax      : {src/adm2/tvcontnr.i}

    Description :
  
    Modified    : 03/18/2002
-------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF "{&ADMClass}":U = "":U &THEN
  &GLOB ADMClass tvcontnr
&ENDIF
/* If this is a SmartFrame with no Frame, then re-identify it as a 'virtual'
   -- i.e, non-visual -- container. */
&IF "{&ADM-CONTAINER}":U = "FRAME":U AND "{&FRAME-NAME}":U = "":U &THEN
  &SCOP ADM-CONTAINER VIRTUAL
&ENDIF

/* make the window's max size the session size, but do it no-error 
   as window-name may be current-window with no window (serverside sbo) */     
&IF '{&WINDOW-NAME}' <> '':U &THEN
  {&WINDOW-NAME}:MAX-WIDTH  = SESSION:WIDTH - 1 NO-ERROR.
  {&WINDOW-NAME}:MAX-HEIGHT = SESSION:HEIGHT - 1 NO-ERROR.       
&ENDIF

&IF "{&ADMClass}":U = "tvcontnr":U &THEN
  {src/adm2/tvcoprop.i}
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

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

&IF DEFINED(ADM-EXCLUDE-STATIC) = 0 &THEN
  /* Starts super procedure */
  IF NOT {&ADM-LOAD-FROM-REPOSITORY} THEN
    RUN start-super-proc("adm2/tvcontnr.p":U).
  
  /* _ADM-CODE-BLOCK-START _CUSTOM _INCLUDED-LIB-CUSTOM CUSTOM */

  {src/adm2/custom/tvcontnrcustom.i}

  /* _ADM-CODE-BLOCK-END */
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


