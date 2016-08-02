// defines a Grace module "curl", which is a thin wrapper
// of a subset of the libcurl API. The module contains one method:
//     easy -> CurlEasy
// This method wraps curl_easy_init, returning a CurlEasy object
// representing a CURL* handle.

native "c" header ‹#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <curl/curl.h>
Object curl_module = NULL;
ClassData CurlEasy;
extern ClassData Octets;

struct CurlModuleObject {
    OBJECT_HEADER;
};

struct CurlEasyObject {
    OBJECT_HEADER;
    CURL *handle;
};

size_t CurlEasy__receive(char *ptr, size_t size, size_t nmemb,
        void *blk) {
    size_t bsz = size * nmemb;
    char buf[bsz];
    int tmp = 1;
    Object arg = alloc_Octets(ptr, bsz);
    callmethod(blk, "apply(1)", 1, &tmp, &arg);
    return bsz;
}
size_t CurlEasy__receiveHeader(char *ptr, size_t size, size_t nmemb,
        void *blk) {
    size_t bsz = size * nmemb;
    char buf[bsz];
    int tmp = 1;
    Object arg = alloc_Octets(ptr, bsz);
    callmethod(blk, "apply(1)", 1, &tmp, &arg);
    return bsz;
}

void CurlEasy__mark(struct CurlEasyObject *r) {

}

void CurlEasy__release(struct CurlEasyObject *r) {
    curl_easy_cleanup(r->handle);
}

Object alloc_CurlEasy() {
    if (!CurlEasy) {
        CurlEasy = alloc_class3("CurlEasy", 0, (void*)&CurlEasy__mark,
                (void*)&CurlEasy__release);
    }
    Object o = alloc_obj(sizeof(struct CurlEasyObject)
            - sizeof(struct Object), CurlEasy);
    struct CurlEasyObject *r = (struct CurlEasyObject *)o;
    r->handle = curl_easy_init();
    curl_easy_setopt(r->handle, CURLOPT_PROTOCOLS, CURLPROTO_HTTP | CURLPROTO_HTTPS | CURLPROTO_FTP);
    curl_easy_setopt(r->handle, CURLOPT_MAXREDIRS, 10);
    curl_easy_setopt(r->handle, CURLOPT_USERAGENT, "Minigrace HTTP Library");
    return o;
}
›

type Octets = Unknown
type CurlEasy = type {
    onReceive(blk: Block1) -> Done
    url:= (url: String) -> Done
    perform -> Done
    onHeader(blk: Block1) -> Done
    escape(s: String | Octets) -> String
    unescape(s: String | Octets) -> Octets
    responseCode -> Number
    effectiveUrl -> String
    includeResponseHeader:=(b: Boolean) -> Done
    followLocation:=(b: Boolean) -> Done
}

class easy {
    var dataBlock
    var myUrl
    var headerBlock
    var curlObject := native "c" code ‹
        result = alloc_CurlEasy();›

    method onReceive(blk: Block1) -> Done {
        // Set the block to be executed when data is received in response
        // to a request. The block is passed the received data as an Octets
        // object. Wraps CURLOPT_WRITEFUNCTION.
        native "c" code ‹
            struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_setopt(r->handle, CURLOPT_WRITEFUNCTION, &CurlEasy__receive);
            curl_easy_setopt(r->handle, CURLOPT_WRITEDATA, args[0]);
            uo->data[1] = args[0];
            return alloc_done();›
    }

    method url -> String {
        // Answers the URL being requested
        myUrl
    }

    method url:= (aUrl: String) -> Done {
        // Set the URL to be requested. Wraps CURLOPT_URL.
        native "c" code ‹
            struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_setopt(r->handle, CURLOPT_URL, grcstring(args[0]));
            uo->data[2] = args[0];
            return alloc_done();›
    }

    method perform -> Done {
        // Make the request. Wraps curl_easy_perform.
        native "c" code ‹
            struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_perform(r->handle);
            return self;›
    }

    method onHeader(blk: Block1) -> Done {
        // Set the block to be executed when a header is received in response
        // to a request. The block is passed the received header as an Octets
        // object. Wraps CURLOPT_HEADERFUNCTION.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_setopt(r->handle, CURLOPT_HEADERFUNCTION, &CurlEasy__receiveHeader);
            curl_easy_setopt(r->handle, CURLOPT_HEADERDATA, args[0]);
            uo->data[3] = args[0];
            return alloc_done();›
    }

    method escape(s: String | Octets) -> String { 
        // URL-encodes s. Wraps curl_easy_escape.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            char *inp;
            int len = 0;
            if (args[0]->class == Octets) {
                struct OctetsObject *oct = (struct OctetsObject *)args[0];
                inp = oct->body;
                len = oct->blen;
            } else {
                inp = grcstring(args[0]);
            }
            char *c = curl_easy_escape(r->handle, inp, len);
            Object o = alloc_String(c);
            curl_free(c);
            return o;›
    }

    method unescape(s: String | Octets) -> Octets { 
        // URL-decodes s. Wraps curl_easy_unescape.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            char *inp;
            int len = 0;
            if (args[0]->class == Octets) {
                struct OctetsObject *oct = (struct OctetsObject *)args[0];
                inp = oct->body;
                len = oct->blen;
            } else {
                inp = grcstring(args[0]);
            }
            char *c = curl_easy_unescape(r->handle, inp, len, &len);
            Object o = alloc_Octets(c, len);
            curl_free(c);
            return o;›
    }

    method responseCode -> Number { 
        // Returns the result code of the most recent request performed.
        // Wraps CURLINFO_RESPONSE_CODE.

        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];            long l;
            curl_easy_getinfo(r->handle, CURLINFO_RESPONSE_CODE, &l);
            return alloc_Float64(l);›
    }

    method effectiveUrl -> String {
        // Returns the effective URL, after any redirects, of the most
        // recent request performed. Wraps CURLINFO_EFFECTIVE_URL.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            char *c;
            curl_easy_getinfo(r->handle, CURLINFO_EFFECTIVE_URL, &c);
            return alloc_String(c);›
    }

    method includeResponseHeader:=(b: Boolean) -> Done {
        // Sets whether to include response headers in the returned data.
        // Wraps CURLOPT_HEADER.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_setopt(r->handle, CURLOPT_HEADER, istrue(args[0]));
            return alloc_done();›
    }

    method followLocation:=(b: Boolean) -> Done { 
        // Sets whether to follow any Location: headers that appear in.
        // Wraps CURLOPT_FOLLOWLOCATION.
        native "c" code ‹struct UserObject *uo = (struct UserObject *)self;
            struct CurlEasyObject *r = (struct CurlEasyObject *)uo->data[4];
            curl_easy_setopt(r->handle, CURLOPT_FOLLOWLOCATION, istrue(args[0]));
            return alloc_done();›
    }
}
