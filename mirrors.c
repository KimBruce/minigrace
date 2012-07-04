#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "gracelib.h"

// mirrors.c defines a Grace module "mirrors" which can be compiled for
// dynamic or static linking. The module contains one method:
//   reflect(obj : Any) -> Mirror
// Mirrors support two methods:
//   methods -> List<MirrorMethod>
//   getMethod(name : String) -> MirrorMethod
// MirrorMethods support two methods:
//   name -> String
//   request(*arglists : List) -> Any
// A sample use might be:
//   mirrors.reflect(1).getMethod("+").request([2]) == 3

Object mirrors_module = NULL;
ClassData MirrorClass;
ClassData MirrorMethodClass;

Method *findmethodsimple(Object self, const char *name);

struct MirrorObject {
    int32_t flags;
    ClassData class;
    Object obj;
};

struct MirrorMethodObject {
    int32_t flags;
    ClassData class;
    Method *method;
    Object obj;
};

Object MirrorMethod_asString(Object self, int nparams, int *argcv, Object *argv,
        int flags) {
    struct MirrorMethodObject *s = (struct MirrorMethodObject*)self;
    char buf[strlen(s->method->name) + 12];
    strcpy(buf, "<Method \"");
    strcat(buf, s->method->name);
    strcat(buf, "\">");
    return alloc_String(buf);
}

Object MirrorMethod_name(Object self, int nparams, int *argcv, Object *argv,
        int flags) {
    struct MirrorMethodObject *s = (struct MirrorMethodObject*)self;
    return alloc_String(s->method->name);
}

Object MirrorMethod_partcount(Object self, int nparams, int *argcv,
        Object *argv, int flags) {
    struct MirrorMethodObject *s = (struct MirrorMethodObject*)self;
    if (!s->method->type)
        return alloc_Float64(-1);
    return alloc_Float64(s->method->type->nparts);
}

Object MirrorMethod_paramcounts(Object self, int nparams, int *argcv,
        Object *argv, int flags) {
    struct MirrorMethodObject *s = (struct MirrorMethodObject*)self;
    int i;
    if (!s->method->type)
        return alloc_none();
    gc_pause();
    Object l = alloc_List();
    int cargcv[] = {1};
    Object carg;
    for (i=0; i<s->method->type->nparts; i++) {
        carg = alloc_Float64(s->method->type->argcv[i]);
        callmethod(l, "push", 1, cargcv, &carg);
    }
    gc_unpause();
    return l;
}

Object MirrorMethod_request(Object self, int nparams, int *argcv, Object *argv,
        int flags) {
    struct MirrorMethodObject *s = (struct MirrorMethodObject*)self;
    int cargc = argcv[0];
    int i;
    int size = 0;
    int cargcv[nparams];
    for (i=0; i<nparams; i++) {
        cargcv[i] = integerfromAny(callmethod(argv[i], "size", 0, NULL, NULL));
        size += cargcv[i];
    }
    Object cargv[size];
    int j = 0;
    for (i=0; i<nparams; i++) {
        Object iter = callmethod(argv[i], "iter", 0, NULL, NULL);
        while (istrue(callmethod(iter, "havemore", 0, NULL, NULL))) {
            Object o = callmethod(iter, "next", 0, NULL, NULL);
            cargv[j] = o;
            j++;
        }
    }
    Object rv = callmethod(s->obj, s->method->name, nparams, cargcv,
            cargv);
    return rv;
}

Object alloc_MirrorMethod(Method *method, Object obj) {
    if (MirrorMethodClass == NULL) {
        MirrorMethodClass = alloc_class("MirrorMethod", 5);
        add_Method(MirrorMethodClass, "request", &MirrorMethod_request);
        add_Method(MirrorMethodClass, "name", &MirrorMethod_name);
        add_Method(MirrorMethodClass, "asString", &MirrorMethod_asString);
        add_Method(MirrorMethodClass, "partcount", &MirrorMethod_partcount);
        add_Method(MirrorMethodClass, "paramcounts", &MirrorMethod_paramcounts);
    }
    Object o = alloc_obj(sizeof(struct MirrorMethodObject)
            - sizeof(struct Object), MirrorMethodClass);
    struct MirrorMethodObject *p = (struct MirrorMethodObject*)o;
    p->obj = obj;
    p->method = method;
    return o;
}

Object Mirror_getMethod(Object self, int nparams, int *argcv, Object *argv,
        int flags) {
    struct MirrorObject *s = (struct MirrorObject*)self;
    Object o = s->obj;
    Method *m = findmethodsimple(o, grcstring(argv[0]));
    return alloc_MirrorMethod(m, o);
}

Object Mirror_methods(Object self, int nparams, int *argcv, Object *args,
        int flags) {
    struct MirrorObject *s = (struct MirrorObject*)self;
    Object o = s->obj;
    ClassData c = o->class;
    Method *m;
    gc_pause();
    Object l = alloc_List();
    Object arg;
    int tmp = 1;
    int i;
    for (i=0; i<c->nummethods; i++) {
        m = &c->methods[i];
        arg = alloc_MirrorMethod(m, o);
        callmethod(l, "push", 1, &tmp, &arg);
    }
    gc_unpause();
    return l;
}

Object alloc_mirror(Object obj) {
    if (MirrorClass == NULL) {
        MirrorClass = alloc_class("Mirror", 2);
        add_Method(MirrorClass, "methods", &Mirror_methods);
        add_Method(MirrorClass, "getMethod", &Mirror_getMethod);
    }
    Object o = alloc_obj(sizeof(Object), MirrorClass);
    struct MirrorObject *p = (struct MirrorObject*)o;
    p->obj = obj;
    return o;
}

Object mirrors_reflect(Object self, int nparams, int *argcv, Object *args,
        int flags) {
    if (nparams != 1)
        gracedie("mirrors.reflect requires one argument");
    return alloc_mirror(args[0]);
}

// Create and return a Grace object with all the above functions as methods.
Object module_mirrors_init() {
    if (mirrors_module != NULL)
        return mirrors_module;
    ClassData c = alloc_class("Module<mirrors>", 12);
    add_Method(c, "reflect", &mirrors_reflect);
    Object o = alloc_newobj(0, c);
    mirrors_module = o;
    gc_root(mirrors_module);
    return o;
}

