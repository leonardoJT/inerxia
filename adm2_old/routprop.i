&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
/*--------------------------------------------------------------------------
    File        : routprop.i
    Purpose     : Defines basic properties.
    Syntax      : {src/adm2/routprop.i}

    Description :

    Modified    : 05/16/2000
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(ADM-EXCLUDE-STATIC) = 0 &THEN
  /* Custom instance definition file */
  {src/adm2/custom/routerdefscustom.i}

&IF "{&xcInstanceProperties}":U NE "":U &THEN
  &GLOB xcInstanceProperties {&xcInstanceProperties},
&ENDIF
&GLOB xcInstanceProperties {&xcInstanceProperties}~
ExternalRefList,InternalRefList

/* This is the procedure to execute to set InstanceProperties at design time. */
&IF DEFINED (ADM-PROPERTY-DLG) = 0 &THEN
  &SCOP ADM-PROPERTY-DLG adm2/support/routerd.w
&ENDIF

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
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 8
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

  /* Include the file which defines prototypes for all of the super
     procedure's entry points. 
     And skip including the prototypes if we are *any* super procedure. */
&IF DEFINED(ADM-EXCLUDE-PROTOTYPES) = 0 &THEN
  &IF "{&ADMSuper}":U EQ "":U &THEN
    {src/adm2/routprto.i}
  &ENDIF
&ENDIF

  /* Put your xp{&Property} preprocessor definitions here.
     Use the following format, e.g.,
     &GLOBAL-DEFINE xpMyProperty
     These preprocessors tell at compile time which properties can
     be retrieved directly from the temp-table */

  &GLOBAL-DEFINE xpExternalRefList
  &GLOBAL-DEFINE xpInternalRefList
  &GLOBAL-DEFINE xpRouterSource
  {src/adm2/b2bprop.i}

&IF DEFINED(ADM-EXCLUDE-STATIC) = 0 &THEN
IF NOT {&ADM-PROPS-DEFINED} THEN
DO:
&IF "{&ADMSuper}":U = "":U &THEN
  /* Put your property field definitions here.
     Use the following syntax, e.g.,
     ghADMProps:ADD-NEW-FIELD('MyProperty':U, 'CHAR':U, 0,'X(20)':U, 'Hi':U). */
   
   ghADMProps:ADD-NEW-FIELD('ExternalRefList':U, 'CHARACTER':U, 0, ?, '':U).
   ghADMProps:ADD-NEW-FIELD('InternalRefList':U, 'CHARACTER':U, 0, ?, '':U).
   ghADMProps:ADD-NEW-FIELD('RouterSource':U, 'CHARACTER':U, 0, ?, '':U).
&ENDIF

  {src/adm2/custom/routpropcustom.i}
END.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


