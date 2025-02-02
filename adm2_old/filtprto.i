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
 * Prototype include file: S:\astra\object\admenh\dev\src\adm2\filtprto.i
 * Created from procedure: S:\astra\object\admenh\dev\src\adm2\filter.p at 14:20 on 22/03/01
 * by the PROGRESS PRO*Tools Prototype Include File Generator
 */

PROCEDURE applyFilter IN SUPER:
END PROCEDURE.

PROCEDURE blankFields IN SUPER:
END PROCEDURE.

PROCEDURE dataAvailable IN SUPER:
  DEFINE INPUT PARAMETER pcRelative AS CHARACTER.
END PROCEDURE.

PROCEDURE disableFields IN SUPER:
END PROCEDURE.

PROCEDURE enableFields IN SUPER:
END PROCEDURE.

PROCEDURE initializeObject IN SUPER:
END PROCEDURE.

PROCEDURE removeSpace IN SUPER:
END PROCEDURE.

PROCEDURE resetFields IN SUPER:
END PROCEDURE.

PROCEDURE unBlankLogical IN SUPER:
  DEFINE INPUT PARAMETER phField AS HANDLE.
END PROCEDURE.

FUNCTION assignColumnFormat RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT pcFormat AS CHARACTER) IN SUPER.

FUNCTION assignColumnHelpId RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT piHelpId AS INTEGER) IN SUPER.

FUNCTION assignColumnLabel RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT pcLabel AS CHARACTER) IN SUPER.

FUNCTION assignColumnOperatorStyle RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT pcStyle AS CHARACTER) IN SUPER.

FUNCTION assignColumnTooltip RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT pcTooltip AS CHARACTER) IN SUPER.

FUNCTION assignColumnWidth RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER,
   INPUT pdWidth AS DECIMAL) IN SUPER.

FUNCTION blankField RETURNS LOGICAL
  (INPUT phField AS HANDLE,
   INPUT phRangeField AS HANDLE,
   INPUT phOperator AS HANDLE) IN SUPER.

FUNCTION blankFillIn RETURNS LOGICAL
  (INPUT phFillIn AS HANDLE) IN SUPER.

FUNCTION columnDataType RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnFilterTarget RETURNS HANDLE
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnFormat RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnHelpId RETURNS INTEGER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnLabel RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnLabelDefault RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnOperatorStyle RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnStyleDefault RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnTooltip RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnWidth RETURNS DECIMAL
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION columnWidthDefault RETURNS LOGICAL
  (INPUT pcColumn AS CHARACTER) IN SUPER.

FUNCTION createField RETURNS HANDLE
  (INPUT phFrame AS HANDLE,
   INPUT pcName AS CHARACTER,
   INPUT pcDataType AS CHARACTER,
   INPUT pcViewAs AS CHARACTER,
   INPUT pcFormat AS CHARACTER,
   INPUT plEnable AS LOGICAL,
   INPUT pcTooltip AS CHARACTER,
   INPUT piHelpid AS INTEGER,
   INPUT pdRow AS DECIMAL,
   INPUT pdCol AS DECIMAL,
   INPUT pdHeight AS DECIMAL,
   INPUT pdWidth AS DECIMAL) IN SUPER.

FUNCTION createLabel RETURNS HANDLE
  (INPUT phField AS HANDLE,
   INPUT pcLabel AS CHARACTER) IN SUPER.

FUNCTION createOperator RETURNS HANDLE
  (INPUT phField AS HANDLE,
   INPUT pcType AS CHARACTER,
   INPUT pcValues AS CHARACTER,
   INPUT pdCol AS DECIMAL,
   INPUT pdWidth AS DECIMAL) IN SUPER.

FUNCTION dataValue RETURNS CHARACTER
  (INPUT pcColumn AS CHARACTER,
   INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION deleteObjects RETURNS LOGICAL IN SUPER.

FUNCTION getDataObject RETURNS CHARACTER IN SUPER.

FUNCTION getDefaultCharWidth RETURNS DECIMAL IN SUPER.

FUNCTION getDefaultEditorLines RETURNS DECIMAL IN SUPER.

FUNCTION getDefaultLogical RETURNS CHARACTER IN SUPER.

FUNCTION getDefaultWidth RETURNS DECIMAL IN SUPER.

FUNCTION getDisplayedFields RETURNS CHARACTER IN SUPER.

FUNCTION getEnabledFields RETURNS CHARACTER IN SUPER.

FUNCTION getFieldColumn RETURNS DECIMAL IN SUPER.

FUNCTION getFieldFormats RETURNS CHARACTER IN SUPER.

FUNCTION getFieldHelpIds RETURNS CHARACTER IN SUPER.

FUNCTION getFieldLabels RETURNS CHARACTER IN SUPER.

FUNCTION getFieldOperatorStyles RETURNS CHARACTER IN SUPER.

FUNCTION getFieldToolTips RETURNS CHARACTER IN SUPER.

FUNCTION getFieldWidths RETURNS CHARACTER IN SUPER.

FUNCTION getFilterTarget RETURNS CHARACTER IN SUPER.

FUNCTION getFilterTargetEvents RETURNS CHARACTER IN SUPER.

FUNCTION getOperator RETURNS CHARACTER IN SUPER.

FUNCTION getOperatorLongValues RETURNS CHARACTER IN SUPER.

FUNCTION getOperatorStyle RETURNS CHARACTER IN SUPER.

FUNCTION getOperatorViewAs RETURNS CHARACTER IN SUPER.

FUNCTION getUseBegins RETURNS LOGICAL IN SUPER.

FUNCTION getUseContains RETURNS LOGICAL IN SUPER.

FUNCTION getViewAsFields RETURNS CHARACTER IN SUPER.

FUNCTION getVisualBlank RETURNS CHARACTER IN SUPER.

FUNCTION setDataObject RETURNS LOGICAL
  (INPUT pcDataObject AS CHARACTER) IN SUPER.

FUNCTION setDefaultCharWidth RETURNS LOGICAL
  (INPUT pdDefaultCharWidth AS DECIMAL) IN SUPER.

FUNCTION setDefaultEditorLines RETURNS LOGICAL
  (INPUT piLines AS INTEGER) IN SUPER.

FUNCTION setDefaultLogical RETURNS LOGICAL
  (INPUT pcDefaultLogical AS CHARACTER) IN SUPER.

FUNCTION setDefaultWidth RETURNS LOGICAL
  (INPUT pdDefaultWidth AS DECIMAL) IN SUPER.

FUNCTION setDisplayedFields RETURNS LOGICAL
  (INPUT pcDisplayedFields AS CHARACTER) IN SUPER.

FUNCTION setEnabledFields RETURNS LOGICAL
  (INPUT pcEnabledFields AS CHARACTER) IN SUPER.

FUNCTION setFieldColumn RETURNS LOGICAL
  (INPUT pcFieldColumn AS DECIMAL) IN SUPER.

FUNCTION setFieldFormats RETURNS LOGICAL
  (INPUT pcFieldFormats AS CHARACTER) IN SUPER.

FUNCTION setFieldHelpIds RETURNS LOGICAL
  (INPUT pcFieldHelpIds AS CHARACTER) IN SUPER.

FUNCTION setFieldLabels RETURNS LOGICAL
  (INPUT pcFieldLabels AS CHARACTER) IN SUPER.

FUNCTION setFieldOperatorStyles RETURNS LOGICAL
  (INPUT pcFieldOperatorStyles AS CHARACTER) IN SUPER.

FUNCTION setFieldToolTips RETURNS LOGICAL
  (INPUT pcFieldToolTips AS CHARACTER) IN SUPER.

FUNCTION setFieldWidths RETURNS LOGICAL
  (INPUT pcFieldWidths AS CHARACTER) IN SUPER.

FUNCTION setFilterTarget RETURNS LOGICAL
  (INPUT pcTarget AS CHARACTER) IN SUPER.

FUNCTION setFilterTargetEvents RETURNS LOGICAL
  (INPUT pcEvents AS CHARACTER) IN SUPER.

FUNCTION setOperator RETURNS LOGICAL
  (INPUT pcOperator AS CHARACTER) IN SUPER.

FUNCTION setOperatorStyle RETURNS LOGICAL
  (INPUT pcOperatorStyle AS CHARACTER) IN SUPER.

FUNCTION setOperatorViewAs RETURNS LOGICAL
  (INPUT pcOperatorViewAs AS CHARACTER) IN SUPER.

FUNCTION setUseBegins RETURNS LOGICAL
  (INPUT plUseBegins AS CHARACTER) IN SUPER.

FUNCTION setUseContains RETURNS LOGICAL
  (INPUT plUseContains AS CHARACTER) IN SUPER.

FUNCTION setViewAsFields RETURNS LOGICAL
  (INPUT pcViewAsFields AS CHARACTER) IN SUPER.

FUNCTION setVisualBlank RETURNS LOGICAL
  (INPUT pcVisual AS CHARACTER) IN SUPER.

FUNCTION showDataMessages RETURNS CHARACTER IN SUPER.

FUNCTION unBlankFillin RETURNS LOGICAL
  (INPUT phField AS HANDLE) IN SUPER.

FUNCTION getObjectType RETURNS CHARACTER IN SUPER.

