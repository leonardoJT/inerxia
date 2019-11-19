set PATH=C:\Progress\OpenEdge\BIN;%PATH%
set LIB=C:\Progress\OpenEdge\LIB;%LIB%
d:
cd d:\sps\soportes\fodun\cpa
proserve pruebas -H servidor-hp -N tcp -S 5000 -L 100000
cd ..
