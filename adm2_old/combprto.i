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
/*
 * Prototype include file: src/adm2/combprto.i
 * Created from procedure: src/adm2/combo.p at 13:21:17 on 08/14/2001
 * by the PROGRESS PRO*Tools Prototype Include File Generator
 */




PROCEDURE anyKey IN SUPER:
END PROCEDURE.

PROCEDURE destroyObject IN SUPER:
END PROCEDURE.

PROCEDURE disableField IN SUPER:
END PROCEDURE.

PROCEDURE disable_UI IN SUPER:
END PROCEDURE.

PROCEDURE displayCombo IN SUPER:
  DEFINE INPUT PARAMETER TABLE FOR ttDCombo.
END PROCEDURE.

PROCEDURE enableField IN SUPER:
END PROCEDURE.

PROCEDURE endMove IN SUPER:
END PROCEDURE.

PROCEDURE enterCombo IN SUPER:
END PROCEDURE.

PROCEDURE getComboQuery IN SUPER:
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttDCombo.
END PROCEDURE.

PROCEDURE hideObject IN SUPER:
END PROCEDURE.

PROCEDURE initializeCombo IN SUPER:
END PROCEDURE.

PROCEDURE leaveCombo IN SUPER:
END PROCEDURE.

PROCEDURE resizeObject IN SUPER:
  DEFINE INPUT PARAMETER pidHeight AS DECIMAL.
  DEFINE INPUT PARAMETER pidWidth AS DECIMAL.
END PROCEDURE.

PROCEDURE valueChanged IN SUPER:
END PROCEDURE.

PROCEDURE viewObject IN SUPER:
END PROCEDURE.

FUNCTION createLabel RETURNS HANDLE
  (INPUT pcLabel AS CHARACTER) IN SUPER.

FUNCTION destroyCombo RETURNS LOGICAL IN SUPER.

FUNCTION getDataValue RETURNS CHARACTER IN SUPER.

FUNCTION getDisplayDataType RETURNS CHARACTER IN SUPER.

FUNCTION getDisplayedField RETURNS CHARACTER IN SUPER.

FUNCTION getDisplayFormat RETURNS CHARACTER IN SUPER.

FUNCTION getFieldLabel RETURNS CHARACTER IN SUPER.

FUNCTION getFieldToolTip RETURNS CHARACTER IN SUPER.

FUNCTION getKeyDataType RETURNS CHARACTER IN SUPER.

FUNCTION getKeyField RETURNS CHARACTER IN SUPER.

FUNCTION getKeyFormat RETURNS CHARACTER IN SUPER.

FUNCTION getLabelHandle RETURNS HANDLE IN SUPER.

FUNCTION getComboHandle RETURNS HANDLE IN SUPER.

FUNCTION getQueryTables RETURNS CHARACTER IN SUPER.

FUNCTION getSDFFileName RETURNS CHARACTER IN SUPER.

FUNCTION getSDFTemplate RETURNS CHARACTER IN SUPER.

FUNCTION getParentField RETURNS CHARACTER IN SUPER.

FUNCTION getParentFilterQuery RETURNS CHARACTER IN SUPER.

FUNCTION getInnerLines RETURNS INTEGER IN SUPER.

FUNCTION getComboFlag RETURNS CHARACTER IN SUPER.

FUNCTION getFlagValue RETURNS CHARACTER IN SUPER.

FUNCTION getDescSubstitute RETURNS CHARACTER IN SUPER.

FUNCTION getObjectType RETURNS CHARACTER IN SUPER.

FUNCTION getBuildSequence RETURNS INTEGER IN SUPER.

FUNCTION getSecured RETURNS LOGICAL IN SUPER.

FUNCTION getPhysicalTableNames RETURNS CHARACTER IN SUPER.

FUNCTION getTempTables RETURNS CHARACTER IN SUPER.

FUNCTION newQueryString RETURNS CHARACTER
  (INPUT pcColumns AS CHARACTER,
   INPUT pcValues AS CHARACTER,
   INPUT pcDataTypes AS CHARACTER,
   INPUT pcOperators AS CHARACTER,
   INPUT pcQueryString AS CHARACTER,
   INPUT pcAndOr AS CHARACTER) IN SUPER.

FUNCTION newWhereClause RETURNS CHARACTER
  (INPUT pcBuffer AS CHARACTER,
   INPUT pcExpression AS CHARACTER,
   INPUT pcWhere AS CHARACTER,
   INPUT pcAndOr AS CHARACTER) IN SUPER.

FUNCTION setBaseQueryString RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setDataValue RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setDisplayDataType RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setDisplayedField RETURNS LOGICAL
  (INPUT pcDisplayedField AS CHARACTER) IN SUPER.

FUNCTION setDisplayFormat RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setFieldLabel RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setFieldToolTip RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setKeyDataType RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setKeyField RETURNS LOGICAL
  (INPUT pcKeyField AS CHARACTER) IN SUPER.

FUNCTION setKeyFormat RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setLabelHandle RETURNS LOGICAL
  (INPUT phValue AS HANDLE) IN SUPER.

FUNCTION setComboHandle RETURNS LOGICAL
  (INPUT phValue AS HANDLE) IN SUPER.

FUNCTION setQueryTables RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setSDFFileName RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setSDFTemplate RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setParentField RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setParentFilterQuery RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setInnerLines RETURNS LOGICAL
  (INPUT piInnerLines AS INTEGER) IN SUPER.

FUNCTION setComboFlag RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setFlagValue RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setDescSubstitute RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setBuildSequence RETURNS LOGICAL
  (INPUT piSequence AS INTEGER) IN SUPER.

FUNCTION setPhysicalTableNames RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setTempTables RETURNS LOGICAL
  (INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION setSecured RETURNS LOGICAL
  (INPUT plFieldIsSecured AS LOGICAL) IN SUPER.
