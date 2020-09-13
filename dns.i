%feature("intern_function", "lispify");

%typemap(cin) u_short ":unsigned-short";

%insert("lisphead") %{
(in-package :lsquic)
%}

%include "dns.h"
