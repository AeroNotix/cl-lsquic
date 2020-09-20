%feature("intern_function", "swig-lispify");
%feature("export");
%typemap(cin) size_t ":unsigned-int";
%insert("lisphead") %{
(in-package :udp)
%}

%include "udp.h"
