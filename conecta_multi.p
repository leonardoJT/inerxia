/* Conecta base de datos Multiactiva */ 
IF NOT CONNECTED("multi") THEN 
   CONNECT VALUE("-db multi -H 172.16.31.82 -N tcp -S 5160 -Mm 4096 -ld multi -U desa106 -P nelson123") NO-ERROR.

