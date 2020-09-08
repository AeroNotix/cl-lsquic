%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :lsquic)
%}

%include "dns.h"
