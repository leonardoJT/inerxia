/******************************************************************************

  Program:      pdf_pre.i
  
  Written By:   Gordon Campbell
  Written On:   July 6, 2005
  
  Description:  Preprocessor defintions for PDFinclude
  
******************************************************************************/

&GLOBAL-DEFINE PDFDIR 

&IF OPSYS = "UNIX" &THEN
  &GLOBAL-DEFINE zlib          /lib/libz.so.1
&ELSE
  &GLOBAL-DEFINE zlib          D:/SFG/Desarrollo/Prog/pdf/dll/zlib1.dll
&ENDIF

&GLOBAL-DEFINE MD5LIB          D:/SFG/Desarrollo/Prog/pdf/dll/md5.exe
&GLOBAL-DEFINE pdfencryptlib   D:/SFG/Desarrollo/Prog/pdf/dll/procryptlib.dll

/* end of pdf_pre.i */
