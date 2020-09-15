%feature("intern_function", "swig-lispify");
%feature("export");
%insert("lisphead") %{
    (defpackage :udp
     (:use :cl :cffi))
(in-package :udp)
%}

%include "udp.h"
