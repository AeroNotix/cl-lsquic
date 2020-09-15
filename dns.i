%feature("intern_function", "swig-lispify");
%feature("export");
%typemap(cin) u_short ":unsigned-short";

%insert("lisphead") %{
    (defpackage :dns
     (:use :cl :cffi))
(in-package :dns)
%}

%include "dns.h"
