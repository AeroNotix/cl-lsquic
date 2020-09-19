%feature("intern_function", "swig-lispify");
%feature("export");
%insert("lisphead") %{
(in-package :udp)
%}

%include "udp.h"
