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
 * Prototype include file: S:\astra\object\admenh\dev\src\adm2\b2bprto.i
 * Created from procedure: S:\astra\object\admenh\dev\src\adm2\b2b.p at 14:22 on 22/03/01
 * by the PROGRESS PRO*Tools Prototype Include File Generator
 */

PROCEDURE characterValue IN SUPER:
  DEFINE INPUT PARAMETER pcPath AS CHARACTER.
  DEFINE INPUT PARAMETER pcValue AS CHARACTER.
END PROCEDURE.

PROCEDURE destroyObject IN SUPER:
END PROCEDURE.

PROCEDURE endDocument IN SUPER:
END PROCEDURE.

PROCEDURE endElement IN SUPER:
  DEFINE INPUT PARAMETER pcPath AS CHARACTER.
  DEFINE INPUT PARAMETER pcNameSpace AS CHARACTER.
  DEFINE INPUT PARAMETER pcName AS CHARACTER.
  DEFINE INPUT PARAMETER pcQualName AS CHARACTER.
END PROCEDURE.

PROCEDURE initializeObject IN SUPER:
END PROCEDURE.

PROCEDURE processMappings IN SUPER:
END PROCEDURE.

PROCEDURE processMessages IN SUPER:
  DEFINE INPUT PARAMETER pcMessages AS CHARACTER.
END PROCEDURE.

PROCEDURE produceAttributes IN SUPER:
  DEFINE INPUT PARAMETER piParentSchemaNode AS INTEGER.
  DEFINE INPUT PARAMETER pdOwnerNode AS DECIMAL.
  DEFINE INPUT PARAMETER phDataSource AS HANDLE.
END PROCEDURE.

PROCEDURE produceChildren IN SUPER:
  DEFINE INPUT PARAMETER piParentSchemaNode AS INTEGER.
  DEFINE INPUT PARAMETER pdParentNode AS DECIMAL.
  DEFINE INPUT PARAMETER phDataSource AS HANDLE.
END PROCEDURE.

PROCEDURE produceDocument IN SUPER:
END PROCEDURE.

PROCEDURE receiveHandler IN SUPER:
  DEFINE INPUT PARAMETER phMessage AS HANDLE.
END PROCEDURE.

PROCEDURE sendHandler IN SUPER:
  DEFINE INPUT PARAMETER phMsgHandler AS HANDLE.
END PROCEDURE.

PROCEDURE sendMessage IN SUPER:
END PROCEDURE.

PROCEDURE startElement IN SUPER:
  DEFINE INPUT PARAMETER pcPath AS CHARACTER.
  DEFINE INPUT PARAMETER pcNameSpace AS CHARACTER.
  DEFINE INPUT PARAMETER pcName AS CHARACTER.
  DEFINE INPUT PARAMETER pcQualName AS CHARACTER.
END PROCEDURE.

PROCEDURE targetProcedure IN SUPER:
  DEFINE OUTPUT PARAMETER phHandle AS HANDLE.
END PROCEDURE.

FUNCTION callOutParams RETURNS LOGICAL
  (INPUT pdNode AS DECIMAL) IN SUPER.

FUNCTION createSchemaAttributes RETURNS HANDLE
  (INPUT piParentNode AS INTEGER) IN SUPER.

FUNCTION createSchemaChildren RETURNS HANDLE
  (INPUT piParentNode AS INTEGER) IN SUPER.

FUNCTION createSchemaPath RETURNS HANDLE
  (INPUT pcPath AS CHARACTER) IN SUPER.

FUNCTION dataSource RETURNS HANDLE
  (INPUT pcName AS CHARACTER) IN SUPER.

FUNCTION defineMapping RETURNS LOGICAL
  (INPUT pcName AS CHARACTER,
   INPUT pcColumns AS CHARACTER,
   INPUT pcValues AS CHARACTER) IN SUPER.

FUNCTION findDataRow RETURNS LOGICAL
  (INPUT phObject AS HANDLE,
   INPUT pcKeyValues AS CHARACTER) IN SUPER.

FUNCTION getConsumerSchema RETURNS CHARACTER IN SUPER.

FUNCTION getDestinationList RETURNS CHARACTER IN SUPER.

FUNCTION getDirectionList RETURNS CHARACTER IN SUPER.

FUNCTION getDocTypeList RETURNS CHARACTER IN SUPER.

FUNCTION getDTDPublicIdList RETURNS CHARACTER IN SUPER.

FUNCTION getDTDSystemIdList RETURNS CHARACTER IN SUPER.

FUNCTION getLoadedByRouter RETURNS LOGICAL IN SUPER.

FUNCTION getMapNameProducer RETURNS CHARACTER IN SUPER.

FUNCTION getMapObjectProducer RETURNS CHARACTER IN SUPER.

FUNCTION getMapTypeProducer RETURNS CHARACTER IN SUPER.

FUNCTION getNameList RETURNS CHARACTER IN SUPER.

FUNCTION getNameSpaceHandle RETURNS HANDLE IN SUPER.

FUNCTION getReplyReqList RETURNS CHARACTER IN SUPER.

FUNCTION getReplySelectorList RETURNS CHARACTER IN SUPER.

FUNCTION getSchemaHandle RETURNS HANDLE IN SUPER.

FUNCTION getSchemaList RETURNS CHARACTER IN SUPER.

FUNCTION getSchemaManager RETURNS HANDLE IN SUPER.

FUNCTION getTargetNameSpace RETURNS CHARACTER IN SUPER.

FUNCTION getTypeName RETURNS CHARACTER IN SUPER.

FUNCTION loadProducerSchema RETURNS LOGICAL IN SUPER.

FUNCTION loadSchema RETURNS LOGICAL
  (INPUT pcSchema AS CHARACTER) IN SUPER.

FUNCTION mapNode RETURNS DECIMAL
  (INPUT pdNode AS DECIMAL,
   INPUT phDataSource AS HANDLE,
   INPUT pcMapType AS CHARACTER,
   INPUT pcMapName AS CHARACTER,
   INPUT pcConversion AS CHARACTER,
   INPUT pcMapParameter AS CHARACTER,
   INPUT pcNodeType AS CHARACTER,
   INPUT pcNodeName AS CHARACTER) IN SUPER.

FUNCTION NotFoundError RETURNS CHARACTER
  (INPUT phDataSource AS HANDLE,
   INPUT pcKeyValues AS CHARACTER) IN SUPER.

FUNCTION numParameters RETURNS INTEGER
  (INPUT phProc AS HANDLE,
   INPUT pcMethod AS CHARACTER) IN SUPER.

FUNCTION rowNotFoundError RETURNS CHARACTER
  (INPUT phDataSource AS HANDLE,
   INPUT pcKeyValues AS CHARACTER) IN SUPER.

FUNCTION setDestinationList RETURNS LOGICAL
  (INPUT pcDestinationList AS CHARACTER) IN SUPER.

FUNCTION setDirectionList RETURNS LOGICAL
  (INPUT pcDirectionList AS CHARACTER) IN SUPER.

FUNCTION setDocTypeList RETURNS LOGICAL
  (INPUT pcDocTypeList AS CHARACTER) IN SUPER.

FUNCTION setDTDPublicIdList RETURNS LOGICAL
  (INPUT pcDTDPublicIdList AS CHARACTER) IN SUPER.

FUNCTION setDTDSystemIdList RETURNS LOGICAL
  (INPUT pcDTDSystemIdList AS CHARACTER) IN SUPER.

FUNCTION setLoadedByRouter RETURNS LOGICAL
  (INPUT plLoadedByRouter AS LOGICAL) IN SUPER.

FUNCTION setMapNameProducer RETURNS LOGICAL
  (INPUT pcMapNameProducer AS CHARACTER) IN SUPER.

FUNCTION setMapObjectProducer RETURNS LOGICAL
  (INPUT pcMapObjectProducer AS CHARACTER) IN SUPER.

FUNCTION setMapTypeProducer RETURNS LOGICAL
  (INPUT pcMapTypeProducer AS CHARACTER) IN SUPER.

FUNCTION setNameList RETURNS LOGICAL
  (INPUT pcNameList AS CHARACTER) IN SUPER.

FUNCTION setReplyReqList RETURNS LOGICAL
  (INPUT pcReplyReqList AS CHARACTER) IN SUPER.

FUNCTION setReplySelectorList RETURNS LOGICAL
  (INPUT pcReplySelectorList AS CHARACTER) IN SUPER.

FUNCTION setSchemaHandle RETURNS LOGICAL
  (INPUT phSchema AS HANDLE) IN SUPER.

FUNCTION setSchemaList RETURNS LOGICAL
  (INPUT pcSchemaList AS CHARACTER) IN SUPER.

FUNCTION setTypeName RETURNS LOGICAL
  (INPUT pcName AS CHARACTER) IN SUPER.

FUNCTION startDataRow RETURNS LOGICAL
  (INPUT phDataSource AS HANDLE,
   INPUT pcAction AS CHARACTER) IN SUPER.

FUNCTION storeNodeValue RETURNS LOGICAL
  (INPUT phDataSource AS HANDLE,
   INPUT pcColumnName AS CHARACTER,
   INPUT pcNodeValue AS CHARACTER) IN SUPER.

FUNCTION storeParameterNode RETURNS LOGICAL
  (INPUT phDataSource AS HANDLE,
   INPUT pcMethod AS CHARACTER,
   INPUT pdNode AS DECIMAL,
   INPUT piNum AS INTEGER,
   INPUT piNumParam AS INTEGER) IN SUPER.

FUNCTION storeParameterValue RETURNS LOGICAL
  (INPUT phDataSource AS HANDLE,
   INPUT pcMethod AS CHARACTER,
   INPUT piNum AS INTEGER,
   INPUT piNumParam AS INTEGER,
   INPUT pcValue AS CHARACTER) IN SUPER.

FUNCTION getObjectType RETURNS CHARACTER IN SUPER.

