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
/* updtabledef.i -- define RowObjUpd temp-table for an SBO */
 
 /* The default is to define the TT with no-undo, but tables can be defined 
    without no-undo by defining &SCOP update-tables-undo . */
 &IF DEFINED(update-tables-undo) <> 0 &THEN
    &SCOP update-tables-no-undo  
 &ELSE 
    &SCOP update-tables-no-undo NO-UNDO 
 &ENDIF
 
 
 &IF '{&SDOInclude{1}}':U NE "":U &THEN
  DEFINE TEMP-TABLE {&UpdTable{1}} {&update-tables-no-undo} {{&SDOInclude{1}}} {src/adm2/rupdflds.i}.
  ghUpdTables[{1}] = TEMP-TABLE {&UpdTable{1}}:HANDLE.
 &ENDIF
