---
layout: default
title: Comparison of Common Lisp Json Libraries (2016)
---
- [Comparing cl-json, st-json, yason, jsown, jonathan and com.gigamonkeys.json](#a882a4e5-4e83-4302-a555-d9f4ec783744)
- [Quick Summary](#quick-summary)
- [Libraries](#libraries)
  - [Some Postgresql results](#1f87972a-83a6-42f4-8286-b3362b1e6a74)
    - [cl-json](#cl-json)
    - [st-json](#st-json)
    - [yason](#yason)
    - [jsown](#jsown)
    - [jonathan](#jonathan)
    - [gigamonkeys.json](#gigamonkeys.json)
    - [Some test data.](#dcb7f24a-e09d-4e83-8b83-bb3e8ca8d923)
  - [Quick JSON syntax refresher](#1b5d183a-bcb7-4847-9b77-fa603ae996e6)
  - [Security](#security)
  - [Mapping Data Structures from JSON to lisp](#f31d81ac-4c32-4c6e-9046-fc70c3ab45d3)
  - [Mapping Data Structures from lisp to JSON](#cb52878f-5771-418b-9c82-4c5aaf25a6fe)
  - [Encodes Correctly using sbcl 1.2.13 (Some of these results have changed](#fd75b757-6074-483e-87b1-0b9e6ed6dd4c)
    - [Encoding a hash-table](#encoding-a-hash-table)
- [Mapping library functions against each other](#2f81bcf5-cf14-4f88-968d-74bbdecbae95)
- [Testing with Unexpected Data](#testing-with-unexpected-data)
- [Benchmarking](#benchmarking)
  - [Encoding Overview](#bb9d4e06-2c5b-4850-8a48-41a480d872e0)
  - [Decoding Overview](#decoding)
  - [[cl-json](https://www.google.com/url?q=https%3A%2F%2Fsites.google.com%2F&sa=D&sntz=1&usg=AFQjCNHxY5wfuxXyAi0nLvqSnNdtxyHyWw)](#cl-json-1)
    - [Encoding](#encoding)
    - [Decoding](#decoding-1)
    - [Nested JSON Objects](#nested-json-objects)
    - [Other Information](#other-information)
  - [st-json](#st-json-1)
    - [Encoding](#encoding-1)
    - [Decoding](#decoding-2)
    - [Other Information](#other-information-1)
    - [Nested JSON Objects](#nested-json-objects-1)
  - [[Yason](file:///)](#yason-1)
    - [Encoding](#encoding-2)
    - [Decoding](#decoding-3)
    - [Nested JSON Objects](#nested-json-objects-2)
    - [Other Information](#other-information-2)
  - [[Jsown](file:///)](#jsown-1)
    - [Encoding](#encoding-3)
    - [Nested JSON Objects](#nested-json-objects-3)
  - [Jonathan](#jonathan-1)
    - [Encoding](#encoding-4)
    - [Decoding](#decoding-4)
    - [Other attributes](#other-attributes)
  - [[Json-streams](https://www.google.com/url?q=https%3A%2F%2Fgithub.com%2Fcopyleft%2Fjson-streams&sa=D&sntz=1&usg=AFQjCNHNholdktw4j7XPK4_JLIHhE56mzQ)](#json-streams)
    - [Encoding](#encoding-5)
    - [Decoding (Incomplete)](#decoding-incomplete)
    - [Other Comments](#other-comments)
  - [Com.gigamonkeys.json](#com.gigamonkeys.json)
    - [Encoding](#ea11f064-801c-4dce-899d-f4d94abb052b)
    - [Decoding](#decoding-5)


<a id="a882a4e5-4e83-4302-a555-d9f4ec783744"></a>

# Comparing cl-json, st-json, yason, jsown, jonathan and com.gigamonkeys.json

JSON (JavaScript Object Notation) is a lightweight data-interchange format, codified in [rfc4627](file:///). Common lisp currently has six libraries that address importing and exporting json data. (For purposes of this comparison, I will refer to "encoding" as converting from common lisp to JSON and "decoding" as converting from JSON to common lisp. This note compares cl-json, st-json jsown,com.gigamonkeys.json, jonathan and json-streams.

Cl-json has by far the biggest feature list, allowing for library internal mapping between JSON objects and clos objects. Yason was created as a lighter weight alternative. St-json was also created to be simpler than cl-json but also to be more precise about types (distinguishing between boolean false, the empty array and the empty object. Jonathan is head and shoulders above everything else in encoding but has some issues with decoding simple-strings (such as returned from postgresql queries). On the other hand, Jsown appears to be incredibly good at decoding. If you use the generic encoding function (jsown:to-json), it is slower at encoding, but if you use the non-generic function (jsown:to-json\*) it is competitive. The generic function is likely to be faster if you specialize the generic functions for your data. All the libraries write to streams; cl-json and yason generally allowing the stream specification as an optional parameter and st-json requiring that you specify the stream. Jsown encodes to string and it is your job to get it to whatever stream you desire. com.monkeylib.json encodes to a stream or a string. Obviously, any of these libraries can read from a file using the normal common-lisp with-open-file macro.


<a id="quick-summary"></a>

# Quick Summary

As can be expected, the libraries do much of the same if you have basic needs. However, significant differences exist and should be considered in choosing which library is right for any particular project. Many applications are asymmetric in how they will use these libraries. If you need to encode data, go with Jonathan. If you are mostly getting JSON data from somewhere else, parsing it and using it, then jsown will likely be your best choice. It is focused on decoding JSON data and is an order of magnitude faster than any other library at this task. The next version will be adding new features, some of them noted below. If you do not want to write your own methods for generic functions and you are going back and forth between JSON objects and clos objects, I would recommend cl-json. If you need to pay particular attention to the differences between null, nil and false, you should probably consider st-json. If you are going to be encoding lisp data but are not sure what the data will look like, cl-json may be your best choice because it handles different types of lisp data out of the box better than the other libraries. For example, handed a list of alists or a list of plists, cl-json correctly encoded an array of json objects and an array of JSON arrays respectively. Neither st-json nor yason could cope with that in one call. In the cases of st-json and yason, you would have to break it into loops and run the encoding function on elements.

A concern has been raised about memory usage for those libraries that use keywords as hash-symbols, particularly on unsanitized data. See the Security discussion for more information.

At the end of the day, your particular data and requirements will determine which library is best for any particular application and you need to pay attention to what your data will look like and what the receiving end will do with it. For example, in one test, yason:encode choked on a list which included a keyword :NULL in an unexpected location. Cl-json just encoded it as the string "null" and st-json encoded it as 'null' (not a string). In testing for your use, deliberately feed badly formed data and see how the library reacts. Some will throw recoverable conditions (depending on the error) while others may actually lock up a thread. For a different look at these libraries, see this review at <http://hitecnologys.org/1>


<a id="libraries"></a>

# Libraries

| Library          | Author                                      | License    | Website                                        | Conclusion                                                             |
| cl-json          | Henrik Hjelte, Boris Smilga, Robert Goldman | MIT        | <http://common-lisp.net/project/cl-json>       | Best with uncertain data types and errors                              |
| st-json          | Marijn Haverbeke                            | zlib-style | <http://marijnhaverbeke.nl/st-json>            | Best with nil/null/false issues                                        |
| yason            | Hans Huebner                                | BSD        | <http://common-lisp.net/project/yason>         | Lighter than cl-json                                                   |
| jsown            | Aad Versteden                               | MIT        | <https://github.com/madnificent/jsown>         | The best at decoding and parsing json.                                 |
| jonathan         | Rudolph Miller                              | MIT        | <https://github.com/Rudolph-Miller/jonathan>   | Best at encoding. Close to jsown speek on parsing with smaller objects |
| json-streams     | Thomas Bakketun, Stian Sletner              | GPL3       | <http://github.com/copyleft/json-streams>      |                                                                        |
| gigamonkeys.json | Peter Seibel                                | see source | <https://github.com/gigamonkey/monkeylib-json> | Had trouble with some test data sets                                   |


<a id="1f87972a-83a6-42f4-8286-b3362b1e6a74"></a>

## Some Postgresql results

We had some interesting results from dealing with queries via postmodern against a postgresql database having jsonb objects stored in the database.

I have been doing a little bit of work with json objects stored in a postgresql database and thought I would share the results of decoding a sample selection result:

Using postmodern, the test selection result is a simple-string of the form:

```lisp
(defparameter *query-result* "{\"name\": \"Paint house\", \"tags\": [\"Improvements\", \"Office\"], \"finished\": true}")
```


<a id="cl-json"></a>

### cl-json

```lisp
(cl-json:decode-json-from-string *query-result*)

((:NAME . "Paint house") (:TAGS "Improvements" "Office") (:FINISHED . T))
```


<a id="st-json"></a>

### st-json

```lisp
(st-json:read-json-from-string *query-result*)

  #S(ST-JSON:JSO

:ALIST (("name" . "Paint house") ("tags" "Improvements" "Office")

("finished" . :TRUE)))

77
```


<a id="yason"></a>

### yason

Note that yason returns a hash, so I am showing the hash as an alist for this purpose.

```lisp
(alexandria:hash-table-alist (yason:parse *query-result*))

(("finished" . T) ("tags" "Improvements" "Office") ("name" . "Paint house"))
```


<a id="jsown"></a>

### jsown

```lisp
(jsown:parse *query-result*)

(:OBJ ("name" . "Paint house") ("tags" "Improvements" "Office")

("finished" . T))

```


<a id="jonathan"></a>

### jonathan

Jonathan has some issues with simple-strings

```lisp
(jonathan:parse *query-result*)

; Evaluation aborted on #<SB-KERNEL:NIL-ARRAY-ACCESSED-ERROR expected-type: (NOT (ARRAY NIL))

datum:

"{\"name\": \"Paint house\", \"tags\": [\"Improvements\", \"Office\"], \"finished\": true}">.


An attempt to access an array of element-type NIL was made.  Congratulations!

[Condition of type SB-KERNEL:NIL-ARRAY-ACCESSED-ERROR]

See also:

 Common Lisp Hyperspec, UPGRADED-ARRAY-ELEMENT-TYPE [:function]

 Common Lisp Hyperspec, 15.1.2.1 [:section]

 Common Lisp Hyperspec, 15.1.2.2 [:section]

```

If you coerce or otherwise force the simple-string to a normal string, jonathan is fine:

```lisp
(jonathan:parse (format nil "~a" *query-result*))


(:|finished| T :|tags| ("Improvements" "Office") :|name| "Paint house")
```


<a id="gigamonkeys.json"></a>

### gigamonkeys.json

```lisp
(com.gigamonkeys.json:parse-json *query-result*)

("name" "Paint house" "tags" #("Improvements" "Office") "finished" :TRUE)
```


<a id="dcb7f24a-e09d-4e83-8b83-bb3e8ca8d923"></a>

### Some test data.

For reference and testing purposes, I used a few different data dumps turned into parameters.

```lisp
(defparameter *country-a* '((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53) (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP . 205) (:CORRUPTION-INDEX . 69)))

(defparameter*countries-a*) ; which is a list of 248*country-a* type entries.

(defparameter *country-p* '(:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO "FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY "Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978 :POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90))

(defparameter *countries-p*) ; a list of 248 *country-p* type entries.
```

While this particular **country-a** and **country-p** parameters do not have :null results, the parameters **countries-a** and **countries-p** do have some :null results.

I have also stored the following nested json object "/home/sabra/json-test/json4.txt". I then use alexandria:read-file-into-string to provide a single string to the libraries for testing how they descend into a nested object.

```json
{

  "items": [

    {

      "index": 1,

      "index_start_at": 56,

      "integer": 29,

      "float": 16.8278,

      "name": "Milton",

      "surname": "Jensen",

      "fullname": "Sheryl Winters",

      "email": "denise@weiss.na",

      "bool": false

    }

  ]

}
```

Other data is created by making calls to public data sources such as:

```lisp
(drakma:http-request "http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/geography?formatjson")
```

Note: If you want to query Facebook API and process their JSON output, don't forget to execute

```lisp
(push (cons "application" "json") drakma:*text-content-types*)
```

or whatever the content-type Facebook uses or drakma will return data as byte arrays which IIRC no JSON library knows what to do with it.

Still other test data sets were generated by on-line json data generators such as <http://www.json-generator.com> and <http://jsongen.pykaso.net/>

Note: I have not tested for anything other than UTF-8 character sets.


<a id="1b5d183a-bcb7-4847-9b77-fa603ae996e6"></a>

## Quick JSON syntax refresher

First, a quick refresher on JSON syntax:

-   Data is represented by name/value pairs
-   Curly braces hold objects and each name is followed by a colon, with the name/value pairs separated by a comma
-   Square brackets hold arrays and values are separated by a comma


<a id="security"></a>

## Security

Getting JSON objects from another source is just as insecure as any other data you receive from another source. You are still responsible for ensuring that you have properly sanitized, validated or other checked the security of the data.

Redditor lokedhs has pointed out that "I'd be careful about using any JSON library that uses keywords for hash keys (like CL-JSON). The reason is that if you are using it to parse unchecked input, it can be used to cause a denial of service attack by sending maps that contain random keys. Every key will be interned into the keyword package, which are never garbage collected, causing an out of memory condition after a while." St-json, jsown and com.gigamonkeys.json do not intern the keywords. Yason doesn't intern the keywords in the library but the test files do, giving you the impression that it assumes that will be normal practice. Cl-json flags the issue and provides the function safe-json-intern - so at least you are warned and provided with an alternative. Jonathan does seem to have this as a potential issue.


<a id="f31d81ac-4c32-4c6e-9046-fc70c3ab45d3"></a>

## Mapping Data Structures from JSON to lisp

The following table sets out the mapping from JSON datastructures to lisp datastructures using the normal functions listed above.

| json      | cl-json                         | st-json            | yason                                                         | jsown                                          | jonathan                 | json-streams                      | com.gigamonkeys.json |
| functions | (decode-json x)\\\\             | (read-json x)      | (parse x)\\\\                                                 | (parse x)\\\\                                  | (parse x)\\\\            | (json-parse)(json-parse-multiple) | (parse-json x)       |
|           | (decode-json-strict x)\\\\      |                    | (**parse-json-arrays-as-vectors**)\\\\                        | Next Version:\\\\                              | (parse x :as :alist)\\\\ |                                   |                      |
|           | (decode-json-from-string x)\\\\ |                    | (**parse-json-booleans-as-symbols**)\\\\                      | (with-injective-reader (parse **json-string**) | (parse x :as jsown)\\\\  |                                   |                      |
|           | (decode-json-from-source x)     |                    | (**parse-json-null-as-keyword**)\\\\                          |                                                | (parse x :as :hashtable  |                                   |                      |
|           |                                 |                    | (**parse-object-key-fn**)                                     |                                                |                          |                                   |                      |
| number    | integer,float                   | number             | number                                                        | number                                         | integer,ratio            | integer, float or ratio           | number               |
| string    | string                          | string             | string                                                        | string                                         | string                   | string                            | string               |
| null      | nil                             | :null              | nill(null if \\\*parse-json-null-as-keyword\\\* is set)\\\\   | nil                                            | nil                      | :NULL                             | :NULL                |
| false     | nil                             | :false             | nil                                                           | nil                                            | nil                      | nil                               | :FALSE               |
| true      | t                               | :true              | t                                                             | t                                              | t                        | t                                 | :TRUE                |
| object    | alist (or clos object)          | st-json:jso object | hash-table                                                    | jsown:json object                              |                          | object                            | hash-table           |
| array     | list (or vector)                | list               | list (vectors if \\\*parse-json-arrays-as-vectors\\\* is set) | list                                           | list                     | array                             | vector               |


<a id="cb52878f-5771-418b-9c82-4c5aaf25a6fe"></a>

## Mapping Data Structures from lisp to JSON

The following table sets out the mapping in the encoding direction, that is, going from lisp datastructures to json datastructures.

| lisp            | cl-json -> json        | st-json -> json            | yason -> json                                   | jsown -> json                                                            | jonathan                                      | json-streams | com.gigamonkeys.json   |
| functions       | (encode-json x)        | (write-json x stream)      | (encode x)                                      | (to-json x)                                                              | (to-json x)                                   |              | (json x)               |
| integer         | number (no frac/exp)   | number (no frac/exp)       | number (no frac/exp)                            | number as string e.g "1"                                                 | number as string e.g "1"                      |              | number (no frac/exp)   |
| float           | number (with frac/exp) | number (with frac/exp)     | number (with frac/exp)                          | number as string e.g. "1.0"                                              | number as string e.g. "1.0"                   |              | number (with frac/exp) |
| rational        | number (with frac/exp) | number (with frac/exp)     | number (with frac/exp)                          | number as string e.g. "0.33333333"                                       | number as string e.g. "0.33333333"            |              | number                 |
| t               | true                   | true                       | true                                            | "true"                                                                   | "true"                                        |              | true                   |
| nil             | null                   | [] (danger - true in json) | null                                            | "[]". You can write json's false by writing lisp's keywords :false or :f | "[]"                                          |              | {}                     |
| symbol          | string                 | N/A                        | N/A                                             | N/A (Next version)                                                       | string                                        |              | N/A                    |
| character       | string                 | N/A                        | N/A                                             | N/A                                                                      | N/A                                           |              |                        |
| string          | string                 | string                     | string                                          | string                                                                   | string\*                                      |              | string                 |
| list            | array                  | array                      | array                                           | array in a string                                                        | array in a string                             |              | object                 |
| plist           | array                  | array                      | array (with encode), object (with encode-plist) | array in a string e.g\\\\                                                | array in a string e.g\\\\                     |              |                        |
|                 |                        |                            |                                                 | "[\\"a\\",\\"b\\"]"                                                      | "[\\"a\\",\\"b\\"]"                           |              |                        |
| alist           | object                 | array                      | object (with encode-alist)                      | string enclosing nested arrays                                           | "{\\"A\\":\\"B\\",\\"C\\":\\"D\\"}"\\\\       |              | Error                  |
|                 |                        |                            |                                                 |                                                                          | \*\*                                          |              |                        |
| other sequence  | array                  |                            |                                                 |                                                                          |                                               |              |                        |
| array           | array                  | N/A                        | array                                           | array                                                                    |                                               |              | array                  |
| hash-table      | object                 | object                     | object                                          | object                                                                   | object e.g. "{\\"foo\\":1,\\"bar\\":[7,8,9]}" |              | object                 |
| standard-object |                        |                            |                                                 |                                                                          |                                               |              |                        |
| object          | object                 | N/A                        | N/A                                             | N/A                                                                      | N/A                                           |              | N/A                    |

Note: Jonathan expects simple-strings. Postmodern, for example, does not return text fields as simple-strings, so the fields have to be coerced to something that jonathan will accept.

See, eg.<http://ircbrowse.net/browse/lisp?id9282910&timestamp1442474317#t1442474317>. As jonathan gets used in other libraries, this needs to be kept in mind. It might trip you up in render-json in caveman2, for example.

Note: Jonathan does not like cons cells in the alist.

So far, the only library which can encode a clos object is cl-json. With the other libraries you would either have to convert the clos object into another type (hash-table or alist etc.) or write a specific function for that particular class of object.


<a id="fd75b757-6074-483e-87b1-0b9e6ed6dd4c"></a>

## Encodes Correctly using sbcl 1.2.13 (Some of these results have changed

from earlier versions). In particular, the st-json results are puzzling and I am re-runing those tests.\*

|            | cl-json | st-json  | yason    | jsown | jonathan | json-streams | com.gigamonkeys.json |
| list       | Yes     | Yes      | NO       | Yes   | Yes      |              | Yes                  |
| alist      | Yes     | NO       | Yes\*    | NO    | NO       |              | Yes                  |
| plist      | Yes     | NO       | Yes\*    | Yes   | Yes      |              | Yes                  |
| array      | Yes     | NO       | Yes      | Yes   | Yes      |              | Yes                  |
| hash-table | Yes     | Some\*\* | Some\*\* | Yes   | Yes      |              | Yes                  |
| object     | Yes     | NO       | NO       | NO    | NO       |              | NO                   |
|            |         |          |          |       |          |              |                      |

Requires special function (e.g. encode-alist or encode-plist)

1.  hash-table key must be a string


<a id="encoding-a-hash-table"></a>

### Encoding a hash-table

Generally (we will see an exception) each library handled a simple hash table well. To demonstrate, consider a hash table generated by the following function:

```lisp
(alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'eq)
```

| function                      | result                                           |
| cl-json:encode-json           | {"foo":1,"bar":[7,8,9]}                          |
| cl-json:encode-json-to-string | "{\\"foo\\":1,\\"bar\\":[7,8,9]}"                |
| st-json:write-json            | {"foo":1,"bar":[7,8,9]}                          |
| yason:encode                  | {"foo":1,"bar":[7,8,9]}                          |
| jsown:to-json                 | "{\\"bar\\":[7,8,9],\\"foo\\":1}"                |
| jonathan:to-json              | "{\\"foo\\":1,\\"bar\\":[7,8,9]}"                |
| com.gigamonkeys.json:json     | "{\\"foo\\":1,\\"bar\\":{\\"7\\":8,\\"9\\":{}}}" |

Except for the differences between generating a string or not, cl-json, st-json, yason and jonathan all generate the same result. jsown has the result in reverse. Gigamonkeys seemed to get confused on the embedded list.

Now consider what happens if the key is not a string

```lisp
(alexandria:plist-hash-table '(:FOO 1 "bar" (7 8 9)) :test #'eq)
```

| function                      | result                                                                                                                                                                                   |
| cl-json:encode-json           | {"foo":1,"bar":[7,8,9]}                                                                                                                                                                  |
| cl-json:encode-json-to-string | "{\\"foo\\":1,\\"bar\\":[7,8,9]}"                                                                                                                                                        |
| st-json:write-json            | ; Evaluation aborted on #<SB-KERNEL:CASE-FAILURE expected-type: (MEMBER NIL T :TRUE :FALSE :NULL :UNDEFINED) datum: :FOO>.                                                               |
| yason:encode                  | There is no applicable method for the generic function#<STANDARD-GENERIC-FUNCTION YASON:ENCODE (12)>when called with arguments(:FOO #<SWANK-BACKEND::SLIME-OUTPUT-STREAM {10076FBDF3}>). |
| jsown:to-json                 | "{\\"bar\\":[7,8,9],\\"FOO\\":1}"                                                                                                                                                        |
| jonathan:to-json              | "{\\"FOO\\":1,\\"bar\\":[7,8,9]}"                                                                                                                                                        |
| com.gigamonkeys.json:json     | "{\\"foo\\":1,\\"bar\\":{\\"7\\":8,\\"9\\":{}}}"\\\\                                                                                                                                     |

Note: Additional specialist encoding functions available

-   (cl-json:encode-json-to-string x)
-   (cl-json:encode-json-alist x)
-   (cl-json:encode-json-alist-to-string x)
-   (cl-json:encode-json-plist x)
-   (cl-json:encode-json-plist-to-string x)
-   (st-json:write-json-to-string x stream)


<a id="2f81bcf5-cf14-4f88-968d-74bbdecbae95"></a>

# Mapping library functions against each other

| Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | cl-json                           | st-json               | yason                 | jsown                                                       | jonathan             | json-streams | com.gigamonkeys.json |
| Encode from lisp to json object. These are generic functions so you can write your own for particular objects.jsown:to-json will return an object if you wrap the data in '(:OBJ data-here).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | encode-json                       | write-json(1)         | encode (1)            | to-json (2)                                                 |                      |              | write-json           |
| Encode to json object as string (yason needs to wrap with write-to-string macro. jsown:to-json normally writes to string.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | encode-json-to-string             | write-json-to-string  |                       | to-json (2)                                                 | to-json (3)          |              | json                 |
| Non-generic variation of to-json. Not as smart, but faster.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                   |                       |                       | to-json\*                                                   |                      |              |                      |
| Write the JSON representation (Object) of ALIST to STREAM (or to **JSON-OUTPUT**). Return NIL.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | encode-json-alist                 |                       | encode-alist          |                                                             | \\\\                 |              |                      |
| Return the JSON representation (Object) of ALIST as a string.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | encode-json-alist-to-string       |                       |                       |                                                             | to-json :from :alist |              |                      |
| Write the JSON representation (Object) of PLIST to STREAM (or to **JSON-OUTPUT**). Return NIL.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | encode-json-plist                 |                       | encode-plist          |                                                             |                      |              |                      |
| Return the JSON representation (Object) of PLIST as a string.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | encode-json-plist-to-string       |                       |                       |                                                             |                      |              |                      |
| Encode OBJECT, presumably a CLOS object as a JSON object, invoking the ENCODE-SLOTS method as appropriate. Cl-jason's basic encode-json function can handle objects. None of the other libraries basic encoding function will handle objects. Yason has this special function to encode objects.                                                                                                                                                                                                                                                                                                                                                                                                      | encode-json                       |                       | encode-object         | to-json (but no default implementation for generic objects) |                      |              |                      |
| Generic function to encode objects. Every class in a hierarchy implements a method for ENCODE-OBJECT that serializes its slots. It is a PROGN generic function so that for a given instance, all slots are serialized by invoking the ENCODE-OBJECT method for all classes that it inherits from.                                                                                                                                                                                                                                                                                                                                                                                                     |                                   |                       | encode-slots          |                                                             |                      |              |                      |
| (cl-json)Encode KEY and VALUE as a Member pair of the innermost JSON Object opened with WITH-OBJECT in the dynamic context. KEY and VALUE are encoded using the ENCODE-JSON generic function, so they both must be of a type for which an ENCODE-JSON method is defined. If KEY does not encode to a String, its JSON representation (as a string) is encoded over again.\\\\                                                                                                                                                                                                                                                                                                                         | encode-object-member              | write-json-element    | encode-object-element |                                                             |                      |              |                      |
| (st-json)Method used for writing values of a specific type. You can specialise this for your own types.\\\\                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                   |                       |                       |                                                             |                      |              |                      |
| (yason) EncodeKEY and VALUE as object element to the last JSON object opened with WITH-OBJECT in the dynamic context. KEY and VALUE are encoded using the ENCODE generic function, so they both must be of a type for which an ENCODE method is defined.                                                                                                                                                                                                                                                                                                                                                                                                                                              |                                   |                       |                       |                                                             |                      |              |                      |
| (cl-json) Encode OBJECT as the next Member of the innermost JSON Array opened with WITH-ARRAY in the dynamic context. OBJECT is encoded using the ENCODE-JSON generic function, so it must be of a type for which an ENCODE-JSON method is defined.\\\\                                                                                                                                                                                                                                                                                                                                                                                                                                               | encode-array-member               |                       | encode-array-element  |                                                             |                      |              |                      |
| (yason) Encode OBJECT as next array element to the last JSON array opened with WITH-ARRAY in the dynamic context. OBJECT is encoded using the ENCODE generic function, so it must be of a type for which an ENCODE method is defined.                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                                   |                       |                       |                                                             |                      |              |                      |
| Encode OBJECTS, a list of JSON encodable objects, as array elements.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |                                   |                       | encode-array-elements |                                                             |                      |              |                      |
| Decode from json object (generic functions)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | decode-json                       | read-json             | parse                 | parse                                                       | parse                |              | parse-json           |
| Decodes from a string (so all strings internal to the json object must be escaped.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | decode-json-from-string           | read-json-from-string |                       | parse                                                       |                      | json-parse   |                      |
| allows pulling from string, stream or file                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | decode-json-from-source           |                       |                       |                                                             |                      |              |                      |
| Read a JSON value and assert the result to be of a given type. Raises a json-type-error when the type is wrong.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |                                   | read-json-as-type     |                       |                                                             |                      |              |                      |
| Parses the keywords which have been specified in the container from the json string json-string. For most cases you can just use the parse function without a special key container. This is only here to support some cases where the building of the key container takes too much time. See #'parse for the normal variant. See #'build-key-container for a way to build new keyword containers.                                                                                                                                                                                                                                                                                                    |                                   |                       |                       | parse-with-container                                        |                      |              |                      |
| Convert a generalized boolean to a :true/:false keyword                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | json-bool                         | as-json-bool          |                       | as-js-bool                                                  |                      |              |                      |
| Convert :true/:false to lisp boolean                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |                                   | from-json-bool        |                       |                                                             |                      |              |                      |
| fetch value from a json object (st-json) or (jsown)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |                                   | getjso                |                       | val                                                         |                      |              |                      |
| store value in an st-json:json object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                                   | (setf getjso)         |                       | (setf val)                                                  |                      |              |                      |
| iterate over key/value pairs of json object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                   | mapjso                |                       |                                                             |                      |              |                      |
| Traverse a tree of json objects and interpret its results.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |                                   |                       |                       | filter                                                      |                      |              |                      |
| Macro allowing you totraverse over all keywords                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |                                   |                       |                       | do-json-keys                                                |                      |              |                      |
| see which keywords have been defined in a parsed json object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |                                   |                       |                       | keywords                                                    |                      |              |                      |
| writes json null                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |                                   |                       |                       | as-js-null                                                  |                      |              |                      |
| Return true if OBJECT is a NULL, and NIL otherwise                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |                                   |                       | null                  |                                                             |                      |              |                      |
| Not-safe:Intern STRING in the current JSON-SYMBOLS-PACKAGE.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | json-intern                       |                       |                       |                                                             |                      |              |                      |
| Returns a non-nil value as itself, or a nil value as a json null-value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | json-or-null                      |                       |                       |                                                             |                      |              |                      |
| Return a function which takes an argument and encodes it to STREAM as a Member of an Array. The encoding function is taken from the value of ENCODER (default is #'ENCODE-JSON).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | stream-array-member-encoder       |                       |                       |                                                             |                      |              |                      |
| Return a function which takes two arguments and encodes them to STREAM as a Member of an Object (String : Value pair).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | stream-object-member-encoder      |                       |                       |                                                             |                      |              |                      |
| Builds an internal structure to speed up the keywords which you can read. This should be used when the keywords needed are not known at compile time, but you still want to parse those keywords of a lot of documents. If the keywords you are interested in are known at compile time, the use of #'parse will automatically expand the keywords at compile time. parse-with-container takes the result of this function and will return the keywords which have been inserted here.                                                                                                                                                                                                                |                                   |                       |                       | build-key-container                                         |                      |              |                      |
| Set the decoder semantics to the following: \* Strings and Numbers are decoded naturally, reals becoming floats. \* The literal name true is decoded to T, false and null to NIL. \* Arrays are decoded to sequences of the type **JSON-ARRAY-TYPE**. \* Objects are decoded to alists. Object keys are converted by the function **JSON-IDENTIFIER-NAME-TO-LISP** and then interned in the package **JSON-SYMBOLS-PACKAGE**.                                                                                                                                                                                                                                                                         | set-decoder-simple-list-semantics |                       |                       |                                                             |                      |              |                      |
| Set the decoder semantics to the following: \* Strings and Numbers are decoded naturally, reals becoming floats. \* The literal name true is decoded to T, false and null to NIL. \* Arrays are decoded to sequences of the type **JSON-ARRAY-TYPE**. \* Objects are decoded to CLOS objects. Object keys are converted by the function **JSON-IDENTIFIER-NAME-TO-LISP**. If a JSON Object has a field whose key matches **PROTOTYPE-NAME**, the class of the CLOS object and the package wherein to intern slot names are inferred from the corresponding value which must be a valid prototype. Otherwise, a FLUID-OBJECT is constructed whose slot names are interned in **JSON-SYMBOLS-PACKAGE**. | set-decoder-simple-clos-semantics |                       |                       |                                                             |                      |              |                      |
| Only intern symbols that already exist. See discussion on security                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | safe-json-intern                  |                       |                       |                                                             |                      |              |                      |
| Signal an UNENCODABLE-VALUE-ERROR.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | unencodable-value-error           |                       |                       |                                                             |                      |              |                      |
| Signal a JSON-SYNTAX-ERROR condition.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | json-syntax-error                 |                       |                       |                                                             |                      |              |                      |
| Take a string with Lisp-style hyphentation and convert it to camel case. This is an inverse of CAMEL-CASE-TO-LISP.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | lisp-to-camel-case                |                       |                       |                                                             |                      |              |                      |
| Take a camel-case string and convert it into a string with Lisp-style hyphenation.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | camel-case-to-lisp                |                       |                       |                                                             |                      |              |                      |

1.  Does not work with alists or plists

2.  Handled sample plist data but threw a memory error on sample alist

data

1.  Handled sample plist data but threw a type error on sample alist data


<a id="testing-with-unexpected-data"></a>

# Testing with Unexpected Data

You thought you were going to get clean data? What happens when you don't? The errors that were thrown were of many different types. Sometimes more surprising is when it doesn't throw an error. Compare the results for st-json and jsown on "[.5]". There are some valid inputs to compare to some particular invalid inputs.

| JSON string\\\\         | cl-json                                  | st-json                                    | yason                                             | jsown                                         | jonathan                          | com.gigamonkeys.json              |                                   |        |   |
| ""                      | errorEND-OF-FILE                         | errorUnexpected end of input               | errorEND-OF-FILE                                  | errorBOUNDING-INDICES-BAD-ERROR expected-type | nil                               | nil                               |                                   |        |   |
| "not a value"\\\\       | errorInvalid JSON literal name           | errorUnrecognized value in JSON data       | errrorinvalid constant                            | nil                                           | Incomplete json string            | nil                               |                                   |        |   |
| "[]"                    | nil                                      | nil\\\\                                    | nil                                               | nil\\\\                                       | nil                               | #()\\\\                           |                                   |        |   |
| "[1.1]"                 | (1.1)                                    | (1.1)                                      | (1.1)                                             | (11/10)                                       | (11/10)                           | #(1.1)                            |                                   |        |   |
| "[-1E4]"                | (-10000.0)                               | (-10000.0)                                 | (-10000.0)                                        | (-10000)                                      | incomplete json string            | #(-10000.0)\\\\                   |                                   |        |   |
| "[100.0e-2]"            | (1.0)                                    | (1.0)                                      | (1.0)                                             | (1)                                           | incomplete json string            | #(1.0)                            |                                   |        |   |
| "[.5]"                  | errorInvalid char on JSON input          | (0.5)                                      | errorCASE-FAILURE expected-type                   | (5)                                           | incomplete json string            | nil                               |                                   |        |   |
| "[5.]"                  | errorInvalid JSON number: no Frac digits | (5)                                        | (5)                                               | errorno non-whitespace characters             | errorno non-whitespace characters | #(5)                              |                                   |        |   |
| "[.]"                   | errorInvalid char on JSON input          | errorUnrecognized value in JSON data       | errorCASE-FAILURE expected-type                   | nil                                           | incomplete json string            | nil                               |                                   |        |   |
| "[5..5]"                | errorInvalid JSON number: no Frac digits | errorUnrecognized value in JSON data       | (                                                 | 5..5                                          | )                                 | errorjunk in string               | errorno non-whitespace characters | nil    |   |
| "[10e]"                 | errorInvalid JSON number: no Exp digits  | errorUnrecognized value in JSON data       | (                                                 | 10E                                           | )                                 | errorno non-whitespace characters | incomplete json string            | #(}10E | ) |
| "[e10]"                 | errorInvalid JSON literal name           | error Unrecognized value in JSON data      | error CASE-FAILURE                                | (1)\\\\                                       | incomplete json string            | nil                               |                                   |        |   |
| "[010e2]"               | errorToken out of place in Array         | (1000.0)                                   | (1000.0)\\\\                                      | (1000)                                        | incomplete json string            | #(1000.0)                         |                                   |        |   |
| "*\* comment\** 1"\\\\  | errorInvalid char on JSON input          | errorUnrecognized value in JSON data       | errorCASE-FAILURE expected-type                   | nil                                           | incomplete json string            | nil                               |                                   |        |   |
| "1 *\* comment \**"\\\\ | 1                                        | error Unused characters at end of input    | 1                                                 | 1                                             | 1                                 | 1                                 |                                   |        |   |
| "[0xFF]"                | errorInvalid JSON literal name           | errorUnrecognized value in JSON data       | errorCASE-FAILURE expected-type                   | errorjunk in string                           | incomplete json string            | nil                               |                                   |        |   |
| "[true]"                | (T)\\\\                                  | (T)                                        | (T)                                               | (T)                                           | (T)                               | #(:TRUE)                          |                                   |        |   |
| "[TRUE]"                | errorInvalid JSON literal name           | errorUnrecognized value in JSON data       | errorCASE-FAILURE expected-type                   | nil                                           | incomplete json string            | nil                               |                                   |        |   |
| "[null]"                | (NIL)                                    | (:NULL)                                    | (NIL)                                             | (NIL)                                         | (NIL)                             | #(:NULL)                          |                                   |        |   |
| "[NULL]"                | errorInvalid JSON literal name           | errorUnrecognized value in JSON data       | errorCASE-FAILURE expected-type                   | nil                                           | incomplete json string            | nil                               |                                   |        |   |
| "[\\"a\\" \\"b\\"]"     | error Token out of place in Array        | errorComma or end of ~A expected, found ~A | errorCASE-FAILURE expected-type                   | ("a")                                         | ("a" "b")                         | nil                               |                                   |        |   |
| "[{}]"                  | (NIL)                                    | (#S(ST-JSON:JSO :ALIST NIL))               | (#<HASH-TABLE :TEST EQUAL :COUNT 0 {100E14BEB3}>) | ((:OBJ))                                      | unexpected EOF found              | #(NIL)                            |                                   |        |   |


<a id="benchmarking"></a>

# Benchmarking


<a id="bb9d4e06-2c5b-4850-8a48-41a480d872e0"></a>

## Encoding Overview

The following were some simple benchmarking numbers comparing the five libraries. Tests were run on sbcl-1.10.1 running on a linux box. First, simple encoding functions.

```lisp
(defun test21 () "St-json encoding a hash table"

 (let ((*standard-output* (make-broadcast-stream)))

  (st-json:write-json (alexandria:plist-hash-table

             '("sEcho" 1 "iTotalRecords" 57 "iTotalDisplayRecords" 57

              "aaData" ("a" "b" ("d" "e" "f")

                   ("alpha" "beta" "nuna")

                   "c")))

            nil)))


(defun test21a () "St-json encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (st-json:write-json

  '("sEcho" 1 "iTotalRecords" 57 "iTotalDisplayRecords" 57

   "aaData" ("a" "b" ("d" "e" "f")

        ("alpha" "beta" "nuna")

        "c"))

  nil)))


(defun test22 () "cl-json encoding a plist"

 (let ((*standard-output*

    (make-broadcast-stream)))

  (cl-json:encode-json-plist '("sEcho" 1

                 "iTotalRecords" 57 "iTotalDisplayRecords" 57

                 "aaData" ("a" "b" ("d" "e" "f")

                      ("alpha" "beta" "nuna")

                      "c")))))


(defun test32 () "cl-json encoding a list"

 (let ((*standard-output*

    (make-broadcast-stream)))

  (cl-json:encode-json '("sEcho" 1

              "iTotalRecords" 57 "iTotalDisplayRecords" 57

              "aaData" ("a" "b" ("d" "e" "f")

                   ("alpha" "beta" "nuna")

                   "c")))))


(defun test23 () "Yason encoding a plist"

 (let ((*standard-output* (make-broadcast-stream)))

  (yason:encode-plist '("sEcho" 1 "iTotalRecords" 57

             "iTotalDisplayRecords" 57

             "aaData" ("a" "b" ("d" "e" "f")

                  ("alpha" "beta" "nuna")

                  "c")))))


(defun test33 () "Yason encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (yason:encode '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))


(defun test34 () "com.gigamonkeys.json encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (com.gigamonkeys.json:write-json '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))





(defun test35 () "jsown.to-json encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (jsown:to-json '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))


(defun test35a () "jsown.to-json encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (jsown:to-json* '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))


(defun test36 () "jonathan.to-json encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (jonathan:to-json '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))


(defun test37 () "json-streams.json-stringify encoding a list"

 (let ((*standard-output* (make-broadcast-stream)))

  (json-streams:json-stringify '("sEcho" 1 "iTotalRecords" 57

          "iTotalDisplayRecords" 57

          "aaData" ("a" "b" ("d" "e" "f")

               ("alpha" "beta" "nuna")

               "c")))))

```

Running and timing these functions 300,000 times returned the following results using sbcl 1.2.10. Jonathan was the clear winner by a huge margin.

Library

cl-json-plist (22)

cl-json list (32)

st-json -hash (21)

st-json -list (21a)

yason -plist (23)

yason -list (33)

jsown:to-json (35)

jsown:to-json\* (35a)

jonathan (36)

json-streams

com.gigamonkeys.json (34)

Time (secs)

5.868

4.650

1.865

1.390

3.126

3.146

8.077

2.630

0.432

error - could not stringify d e f

More details:

    "jonathan"
    Evaluation took:
      0.482 seconds of real time
      0.480000 seconds of total run time (0.480000 user, 0.000000 system)
      99.59% CPU
      1,026,978,032 processor cycles
      276,006,960 bytes consed


    "21a st-json-list"
    Evaluation took:
      1.291 seconds of real time
      1.310000 seconds of total run time (1.310000 user, 0.000000 system)
      101.47% CPU
      2,754,828,672 processor cycles
      33,587,136 bytes consed"23"
    Evaluation took:
      3.126 seconds of real time
      3.193333 seconds of total run time (3.193333 user, 0.000000 system)
      102.14% CPU
      6,667,408,280 processor cycles
      33,619,504 bytes consed
    "32"
    Evaluation took:
      4.650 seconds of real time
      4.609999 seconds of total run time (4.609999 user, 0.000000 system)
      99.14% CPU
      9,922,267,752 processor cycles
      1,233,625,056 bytes consed


    "21"
    Evaluation took:
      1.865 seconds of real time
      1.936667 seconds of total run time (1.936667 user, 0.000000 system)
      103.86% CPU
      37 lambdas converted
      3,979,265,480 processor cycles
      414,789,136 bytes consed
    "33"
    Evaluation took:
      3.146 seconds of real time
      3.183333 seconds of total run time (3.183333 user, 0.000000 system)
      101.18% CPU
      30 lambdas converted
      6,712,495,312 processor cycles
      36,694,432 bytes consed


    "22"
    Evaluation took:
      5.868 seconds of real time
      5.829999 seconds of total run time (5.829999 user, 0.000000 system)
      99.35% CPU
      12,517,856,448 processor cycles
      1,823,992,896 bytes consed
    "com.gigamonkeys.json test34"
    Error: can't stringify (alpha beta nuna)


<a id="decoding"></a>

## Decoding Overview

Before getting to the benchmarks themselves, consider the following partial results from the decoding. You will get different lisp data-types from the decoding. Also, while st-json, yason, cl-json and com.gigamonkeys.json all were symmetrical (decoding after encoding would return you to the original, that is deliberately not the case with jsown.

```lisp
(first (test-decode-s)) ; st-json

#S(ST-JSON:JSO :ALIST (("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69)))


(first (test-decode-c)) ; cl-json

((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53) (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07")

(:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP . 205) (:CORRUPTION-INDEX . 69))


(alexandria:hash-table-alist (aref (yason:parse *encoded-countries*) 0)) ; yason

(("corruptionIndex" . 69) ("gdp" . 205) ("population" . 4588252) ("currencyNumericCode" . 978) ("currencyAlphabetCode" . "EUR") ("currency" . "Euro") ("updatedAt" . "2005-09-11 00:15:40-07") ("permissionId" . 1) ("iso" . "IE") ("longitude" . -8) ("latitude" . 53) ("regionId" . 4) ("name" . "Ireland") ("id" . 11))


(first (test-decode-j)) ; jsown

(:OBJ ("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69))


(aref (test-decode-gm) 0) ; com.gigamonkeys.json.parser

("id" 11 "name" "Ireland" "regionId" 4 "latitude" 53 "longitude" -8 "iso" "IE" "permissionId" 1 "updatedAt" "2005-09-11 00:15:40-07" "currency" "Euro" "currencyAlphabetCode" "EUR" "currencyNumericCode" 978 "population" 4588252 "gdp" 205 "corruptionIndex" 69)

```

Running a decoding function against a cl-json encoded dump of **countries** for 100 runs had the following results:

| Library | cl-json   | st-json   | yason     | jsown     | jonathan | json-streams | com.gigamonkeys.json |
| Time    | 6.573 sec | 2.346 sec | 3.184 sec | 0.308 sec |          |              | 2.051 sec.           |
| Time    | 7.222 sec | 1.804 sec | 4.053     | 0.367     | 3.677    | 6.827        | 1.818                |

Running a decoding function against a large nested json object for 200 runs had the following results:

| Library | cl-json   | st-json   | yason     | jsown     | jonathan | com.gigamonkeys.json |
| Time    | 4.043 sec | 1.669 sec | 2.063 sec | 0.268 sec |          | 1.535 sec.           |
|         |           |           |           |           |          |                      |

Running a decoding function against a smaller nested json object for 200,000 runs had the following result

| Library | cl-json   | st-json   | yason      | jsown     | jonathan  | com.gigamonkeys.json |
| Time    | 6.267 sec | 5.044 sec | 21.017 sec | 1.529 sec | 1.575 sec | 8.247 sec            |

Yes, those number for jsown are right. The more complicated the object, the faster jsown is compared to everything else. On smaller objects, jonathan is in the same league, sometimes faster, sometimes slightly slower. On larger objects, jsown has a decided advantage.

Running a decoding function against a different nested json object for 100,000 runs had the following result

| Library | cl-json    | st-json    | yason      | jsown     | jonathan  | json-streams | com.gigamonkeys.json |
| Time    | 28.988 sec | 11.724 sec | 15.246 sec | 1.486 sec | 2.344 sec | 25.956 sec.  | 9.105 sec            |
|         |            |            |            |           |           |              |                      |

1.  Countries Test

    st-json-read-json

    Evaluation took:

    2.346 seconds of real time

    2.343333 seconds of total run time (2.343333 user, 0.000000 system)

    [ Run times consist of 0.157 seconds GC time, and 2.187 seconds non-GC time. ]

    99.87% CPU

    5,002,895,672 processor cycles

    428,328,288 bytes consed

    cl-json:decode-json-from-string

    Evaluation took:

    6.573 seconds of real time

    6.566665 seconds of total run time (6.519999 user, 0.046666 system)

    [ Run times consist of 0.150 seconds GC time, and 6.417 seconds non-GC time. ]

    99.91% CPU

    14,022,642,800 processor cycles

    462,734,864 bytes consed

    yason:parse

    Evaluation took:

    3.184 seconds of real time

    3.183333 seconds of total run time (3.130000 user, 0.053333 system)

    [ Run times consist of 0.152 seconds GC time, and 3.032 seconds non-GC time. ]

    99.97% CPU

    29 lambdas converted

    6,793,157,192 processor cycles

    352,362,192 bytes consed

    jsown:parse

    Evaluation took:

    0.308 seconds of real time

    0.306666 seconds of total run time (0.306666 user, 0.000000 system)

    [ Run times consist of 0.020 seconds GC time, and 0.287 seconds non-GC time. ]

    99.68% CPU

    657,041,136 processor cycles

    49,472,800 bytes consed

    com.gigamonkey.json:parse-json

    Evaluation took:

    2.051 seconds of real time

    2.050000 seconds of total run time (2.040000 user, 0.010000 system)

    [ Run times consist of 0.140 seconds GC time, and 1.910 seconds non-GC time. ]

    99.95% CPU

    82 lambdas converted

    4,374,634,560 processor cycles

    329,166,160 bytes consed

    New Runs under sbcl 1.2.10

    cl-json

    Evaluation took:

    7.222 seconds of real time

    7.223333 seconds of total run time (7.166666 user, 0.056667 system)

    [ Run times consist of 0.269 seconds GC time, and 6.955 seconds non-GC time. ]

    100.01% CPU

    15,406,811,656 processor cycles

    731,784,080 bytes consed

    st-json

    Evaluation took:

    1.804 seconds of real time

    1.806667 seconds of total run time (1.796667 user, 0.010000 system)

    [ Run times consist of 0.081 seconds GC time, and 1.726 seconds non-GC time. ]

    100.17% CPU

    3,847,869,072 processor cycles

    302,944,944 bytes consed

    jsown

    Evaluation took:

    0.367 seconds of real time

    0.370000 seconds of total run time (0.360000 user, 0.010000 system)

    [ Run times consist of 0.043 seconds GC time, and 0.327 seconds non-GC time. ]

    100.82% CPU

    782,610,904 processor cycles

    67,320,544 bytes consed

    yason

    Evaluation took:

    4.053 seconds of real time

    4.073333 seconds of total run time (4.073333 user, 0.000000 system)

    [ Run times consist of 0.033 seconds GC time, and 4.041 seconds non-GC time. ]

    100.49% CPU

    8,645,612,248 processor cycles

    220,434,896 bytes consed

    jonathan

    Evaluation took:

    3.677 seconds of real time

    3.683333 seconds of total run time (3.579999 user, 0.103334 system)

    [ Run times consist of 0.793 seconds GC time, and 2.891 seconds non-GC time. ]

    100.16% CPU

    7,844,548,864 processor cycles

    2,668,150,608 bytes consed

    js-streams

    Evaluation took:

    6.827 seconds of real time

    6.813332 seconds of total run time (6.789999 user, 0.023333 system)

    [ Run times consist of 0.088 seconds GC time, and 6.726 seconds non-GC time. ]

    99.79% CPU

    14,565,486,608 processor cycles

    661,025,280 bytes consed

    gigamonkeys

    Evaluation took:

    1.818 seconds of real time

    1.826667 seconds of total run time (1.826667 user, 0.000000 system)

    [ Run times consist of 0.070 seconds GC time, and 1.757 seconds non-GC time. ]

    100.50% CPU

    3,879,537,456 processor cycles

    218,344,512 bytes consed

2.  Nested Test

    cl-json decode nested

    Evaluation took:

    4.043 seconds of real time

    4.053333 seconds of total run time (4.053333 user, 0.000000 system)

    [ Run times consist of 0.077 seconds GC time, and 3.977 seconds non-GC time. ]

    100.25% CPU

    8,625,907,640 processor cycles

    283,397,296 bytes consed

    st-json decode nested

    Evaluation took:

    1.669 seconds of real time

    1.660000 seconds of total run time (1.633333 user, 0.026667 system)

    [ Run times consist of 0.087 seconds GC time, and 1.573 seconds non-GC time. ]

    99.46% CPU

    3,562,216,224 processor cycles

    239,431,488 bytes consed

    yason-decode-nested

    Evaluation took:

    2.051 seconds of real time

    2.053333 seconds of total run time (2.050000 user, 0.003333 system)

    [ Run times consist of 0.054 seconds GC time, and 2.0000 seconds non-GC time. ]

    100.10% CPU

    4,375,103,664 processor cycles

    190,517,456 bytes consed

    jsown-decode-nested

    Evaluation took:

    0.268 seconds of real time

    0.270000 seconds of total run time (0.270000 user, 0.000000 system)

    [ Run times consist of 0.017 seconds GC time, and 0.253 seconds non-GC time. ]

    100.75% CPU

    573,028,112 processor cycles

    34,328,000 bytes consed

    gigamonkey-decode-nested

    Evaluation took:

    1.535 seconds of real time

    1.530000 seconds of total run time (1.420000 user, 0.110000 system)

    [ Run times consist of 0.354 seconds GC time, and 1.176 seconds non-GC time. ]

    99.67% CPU

    3,274,726,152 processor cycles

    168,174,768 bytes consed

    Second run, using sbcl 1.2.10 at 200,000 iterations on a smaller nested object as a string

    cl-json decode

    Evaluation took:

    6.267 seconds of real time

    6.276666 seconds of total run time (6.260000 user, 0.016666 system)

    [ Run times consist of 0.227 seconds GC time, and 6.050 seconds non-GC time. ]

    100.16% CPU

    13,368,762,472 processor cycles

    783,999,440 bytes consed

    st-json decode

    Evaluation took:

    5.044 seconds of real time

    5.039999 seconds of total run time (5.039999 user, 0.000000 system)

    [ Run times consist of 0.145 seconds GC time, and 4.895 seconds non-GC time. ]

    99.92% CPU

    10,760,952,120 processor cycles

    777,562,688 bytes consed

    jsown decode

    Evaluation took:

    1.529 seconds of real time

    1.530000 seconds of total run time (1.520000 user, 0.010000 system)

    [ Run times consist of 0.084 seconds GC time, and 1.446 seconds non-GC time. ]

    100.07% CPU

    3,260,608,000 processor cycles

    278,407,312 bytes consed

    yason decode

    Evaluation took:

    21.017 seconds of real time

    21.079997 seconds of total run time (21.079997 user, 0.000000 system)

    [ Run times consist of 0.030 seconds GC time, and 21.050 seconds non-GC time. ]

    100.30% CPU

    44,838,111,472 processor cycles

    595,200,528 bytes consed

    jonathan decode

    Evaluation took:

    1.575 seconds of real time

    1.610000 seconds of total run time (1.610000 user, 0.000000 system)

    102.22% CPU

    3,360,091,520 processor cycles

    275,212,608 bytes consed

    gigamonkey decode

    Evaluation took:

    8.247 seconds of real time

    8.233333 seconds of total run time (8.233333 user, 0.000000 system)

    99.83% CPU

    17,593,273,656 processor cycles

    627,159,584 bytes consed

    For another, more granular look at benchmarking, please see a different review at <http://hitecnologys.org/1#benchmarks>


<a id="cl-json-1"></a>

## [cl-json](https://www.google.com/url?q=https%3A%2F%2Fsites.google.com%2F&sa=D&sntz=1&usg=AFQjCNHxY5wfuxXyAi0nLvqSnNdtxyHyWw)


<a id="encoding"></a>

### Encoding

Basic encoding functionality is provided by the generic function encode-json. This can be customised with an entire series of macros as listed in the documentation. Some examples follow:

Using the **country-a** data:

```lisp
(cl-json:encode-json *country-a*)

{"id":11,"name":"Ireland","regionId":4,"latitude":53,"longitude":-8,"iso":"IE","permissionId":1,"updatedAt":"2005-09-11 00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":4588252,"gdp":205,"corruptionIndex":69}

```

list

```lisp
(cl-json:encode-json '("a" "alpha" "b" "beta"))

["a","alpha","b","beta"]


(cl-json:encode-json *country-a*)

{"id":7,"name":"Finland","regionId":108,"latitude":64,"longitude":26,"iso":"FI","permissionId":1,"updatedAt":"2005-09-11 00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":5430670,"gdp":247,"corruptionIndex":90}


(cl-json:encode-json *country-p*)

["id",7,"name","Finland","regionId",108,"latitude",64,"longitude",26,"iso","FI","permissionId",1,"updatedAt","2005-09-11 00:15:40-07","currency","Euro","currencyAlphabetCode","EUR","currencyNumericCode",978,"population",5430670,"gdp",247,"corruptionIndex",90]

```

Notice how cl-json's basic encoding function returns an object when handed an alist and returns an array when handed a plist. Now consider the behavior when handed a list of plists or list of alists:

```lisp
(cl-json:encode-json (list *country-p*))

[["id",7,"name","Finland","regionId",108,"latitude",64,"longitude",26,"iso","FI","permissionId",1,"updatedAt","2005-09-11 00:15:40-07","currency","Euro","currencyAlphabetCode","EUR","currencyNumericCode",978,"population",5430670,"gdp",247,"corruptionIndex",90]]

NIL

(cl-json:encode-json (list *country-a*))

[{"id":7,"name":"Finland","regionId":108,"latitude":64,"longitude":26,"iso":"FI","permissionId":1,"updatedAt":"2005-09-11 00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":5430670,"gdp":247,"corruptionIndex":90}]


```

In both instances, cl-json:encode-json returned an array, but the list of plists returned an array of arrays and the list of alists returned an array of objects.

1.  alist

    Notice the difference between encoding-json-alist and encoding-json in the examples below

    ```lisp
    (cl-json:encode-json-alist '((:a "alpha") ("b" "beta")))

    {"a":["alpha"],"b":["beta"]}

    (cl-json:encode-json '((:a "alpha") ("b" "beta")))

    [["a","alpha"],["b","beta"]]
    ```

    In the first specialized example, cl-json is told that this is an alist and returns a json object.

    In the second more general example, cl-json is not told that this is an alist and returns a json array.

        plist(cl-json:encode-json-plist '("a" "alpha" "b" "beta")){"a":"alpha","b":"beta"}

2.  array

    ```lisp
    (cl-json:encode-json (make-array 2 :initial-element "a" ))

    ["a","a"]
    ```

3.  object

    Now consider what happens if I just instantiate a clos object and hand it to cl-json to encode. Cl-json handles a simple clos object without any issues:

    ```lisp
    (cl-json:encode-json (make-instance 'country :id 237 :name "Sealand" :region-id 12 :currency "bitcoin" :currency-alphabet-code "bc" :currency-numeric-code 278 :iso "sl" :population 8 :gdp 10 :corruption-index 32 :permission-id 232))



    {"id":237,"name":"Sealand","updatedAt":{"day":4929,"sec":18745,"nsec":564132000},"regionId":12,"latitude":0,"longitude":0,"iso":"sl","currency":"bitcoin","currencyAlphabetCode":"bc","currencyNumericCode":278,"population":8,"gdp":10,"corruptionIndex":32,"permissionId":232}

    ```

4.  Encode-json-to-string

    Simple example:

    ```lisp
    (cl-json:encode-json-to-string

     (alexandria:plist-hash-table '("foo" 1"bar" (7 8 9)) :test #'eq))


    "{\"foo\":1,\"bar\":[7,8,9]}"

    Slightly more complex example using the *country-a* data:


    (cl-json:encode-json-to-string *country-a*)

    "{\"id\":11,\"name\":\"Ireland\",\"regionId\":4,\"latitude\":53,\"longitude\":-8,\"iso\":\"IE\",\"permissionId\":1,\"updatedAt\":\"2005-09-11 00:15:40-07\",\"currency\":\"Euro\",\"currencyAlphabetCode\":\"EUR\",\"currencyNumericCode\":978,\"population\":4588252,\"gdp\":205,\"corruptionIndex\":69}"

    ```


<a id="decoding-1"></a>

### Decoding

Cl-json decoding generally just does what it says on the box. Note: There was a point made by loke\_ on IRC that parsing the keys into keywords could effectively create a DoS attack by overloading the system with the interning of keywords. This is an interesting point that deserves more thought. I had only thought in terms of pull on decoding, but even there an unexpected amount of data can create an issue. Just another reminder to worry about sanitizing inputs.

```lisp
(with-input-from-string

  (s "{\"foo\": [1, 2, 3], \"bar\": true, \"baz\": \"!\"}")

        (json:decode-json s))

((:FOO 1 2 3) (:BAR . T) (:BAZ . "!"))

```

You may need to pay attention to whether you are decoding a json object in the form of a string (decode-json-from-string x) or not in the form of a string (decode-json x).While cl-json normally returns json objects as arrays, you could tell it to return the json object as an object.

```lisp
(cl-json:set-decoder-simple-clos-semantics)
```

You can reset the decoder back to lists with the function:

```lisp
(set-decoder-simple-list-semantics)
```

Testing arrays embedded in arrays and arrays embedded in objects

```lisp
(defparameter *short-encoded-items-A*

"[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"AndrewA Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")


(cl-json:decode-json-from-string *short-encoded-items-A*)

("items"

((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 12) (:FLOAT . 19.2041)

 (:NAME . "Jennifer") (:SURNAME . "Snow") (:FULLNAME . "Andrew Vaughan")

 (:EMAIL . "sherri@ritchie.zw") (:BOOL))

((:INDEX . 2) (:INDEX--START--AT . 57) (:INTEGER . 14) (:FLOAT . 14.9888)

 (:NAME . "Alfred") (:SURNAME . "Pitts") (:FULLNAME . "Barry Weiner")

 (:EMAIL . "cheryl@craven.re") (:BOOL)))


```

Now for some invalid JSON code

```lisp
; JSON array embedded in an array embedded in an object

(defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"AndrewB Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")


(cl-json:decode-json-from-string *short-encoded-items-B*)

; Evaluation aborted on #<JSON:JSON-SYNTAX-ERROR "Expected a key String in Object on JSON input ~

          but found `~A'" {1011D14AC3}>.

```

Back to valid code

```lisp
; JSON array embedded in array

(defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"AndrewC Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")


(cl-json:decode-json-from-string *short-encoded-items*)

(("items"

 ((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 12) (:FLOAT . 19.2041)

 (:NAME . "Jennifer") (:SURNAME . "Snow") (:FULLNAME . "Andrew Vaughan")

 (:EMAIL . "sherri@ritchie.zw") (:BOOL))

 ((:INDEX . 2) (:INDEX--START--AT . 57) (:INTEGER . 14) (:FLOAT . 14.9888)

 (:NAME . "Alfred") (:SURNAME . "Pitts") (:FULLNAME . "Barry Weiner")

 (:EMAIL . "cheryl@craven.re") (:BOOL))))

```


<a id="nested-json-objects"></a>

### Nested JSON Objects

Taking the nested json object that I stored in json4.txt, how could we get an alist out of the innermost nested object? As a reminder, the json object was as follows:

```json

{

  "items": [

    {

      "index": 1,

      "index_start_at": 56,

      "integer": 29,

      "float": 16.8278,

      "name": "Milton",

      "surname": "Jensen",

      "fullname": "Sheryl Winters",

      "email": "denise@weiss.na",

      "bool": false

    }

  ]

}

```

What can we do with it incl-json? The basic decoding would result in this:

```lisp
(with-open-file

(stream "/home/sabra/json-test/json4.txt")

(cl-json:decode-json-from-string (slurp-stream4 stream)))

((:ITEMS ((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 29) (:FLOAT . 16.8278) (:NAME . "Milton") (:SURNAME . "Jensen") (:FULLNAME . "Sheryl Winters") (:EMAIL . "denise@weiss.na") (:BOOL))))

```

This tells us that we are looking a series of nested lists. One way of getting to the value associated with the keyword float would be something like this:

```lisp
(let ((x (with-open-file (stream "/home/sabra/json-test/json4.txt")

            (cl-json:decode-json-from-string

             (slurp-stream4 stream)))))

 (cdr

 (assoc :FLOAT

     (second

     (assoc :ITEMS x)))))

16.8278
```

Now consider a different way of doing this. Reset the decoding from lists to clos classes.

```lisp
(cl-json:set-decoder-simple-clos-semantics)
```

Now look at this variation, trying to get the value associated with the keyword surname for this particular object:

```lisp
(let ((x (with-open-file (stream "/home/sabra/json-test/json4.txt")

            (cl-json:decode-json-from-string

             (slurp-stream4 stream)))))

 (slot-value

 (aref (slot-value x ':items) 0)

 ':surname))

"Jensen"
```

As you can tell, you still have to deal with the fact that there is an array in the middle between the objects. No one said json objects could not be messy.


<a id="other-information"></a>

### Other Information

1.  Error Conditions

    Cl-json has several error conditions, some of them recoverable and some of them not recoverable. These include "unrecoverable-value-error", "json-syntax-error", "no-char-for-code", "cell-error" "type-error", errors for calling functions in the wrong environment and others. Please read the user-manual for more details.

    Cl-json has a lot of other capabilities. The documentation is excellent and you should seriously consider the security considerations section of the user manual if you are going to be instantiating clos classes based on uncontrolled JSON objects.


<a id="st-json-1"></a>

## st-json

ST-JSON ('ST' because it originated at [Streamtech](http://www.google.com/url?q=http%3A%2F%2Fstreamtech.nl%2F&sa=D&sntz=1&usg=AFQjCNGqmjvG8loSuKlSTpv5LwlVdXj3ww)) is a Common Lisp library for encoding and decoding JSON values (as specified on [json.org](http://www.google.com/url?q=http%3A%2F%2Fjson.org%2F&sa=D&sntz=1&usg=AFQjCNEwm-AJDoBUou4pAH5E_gCsMD14HA)).


<a id="encoding-1"></a>

### Encoding

The basic encoding function is write-json.

1.  write-json

2.  list

        (st-json:write-json  '("sEcho" 1 "iTotalRecords" 57  "iTotalDisplayRecords" 57
        "f") ("alpha" "beta" "nuna") "c") ) *standard-output*)

        ["sEcho",1,"iTotalRecords",  "aaData" ("a" "b" ("d" "e"
         57,"iTotalDisplayRecords",57,
        ,["alpha","beta","nuna"],"c"]]"aaData",["a","b",["d","e","f"
        ]Now consider the encoding using the *country-a* and *country-p* parameters listed in the beginning of this page:(st-json:write-json *country-p* *standard-output*)
        Error :ID fell through ECASE expression.
        (st-json:write-json *country-a* *standard-output*)
        Errror :ID fell through ECASE expression.
        Notice the errors when handed the *country-a* and *country-p* alist and plist parameters.

3.  alist

    ```lisp
    (st-json:write-json '(("a" "alpha") ("b" "beta")) *standard-output*)

    [["a","alpha"],["b","beta"]]

    ```

    Consider the errors shown above under list

4.  plist

    Consider the errors shown above under list

5.  hash-table

    St-json can handle encoding hash tables. As noted in the examples below, it will normally result in writing out a json object

        (st-json:write-json (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal) *standard-output*)
        {"foo":1,"bar":[7,8,9]}

6.  write-json-to-string

    Similarly to cl-json, st-json has a function for writing json output to string as well as to a stream.

    (st-json:write-json-to-string

    (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal))

    "{\\"foo\\":1,\\"bar\\":[7,8,9]}"


<a id="decoding-2"></a>

### Decoding

In decoding, st-json creates instances of a jso object "jso" which wraps an alist.

    read-json

    (let ((item (st-json:jso "a" "alph" "b" "beta")))
      (st-json:read-json (st-json:write-json-to-string  item)))#S(ST-JSON:JSO :ALIST (("a" . "alph") ("b" . "beta")))

Testing arrays in arrays and arrays in arrays in objects

```lisp
(defparameter *short-encoded-items-A*
  "[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew1 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")


(st-json:read-json-from-string *short-encoded-items-A*)

("items"

#S(ST-JSON:JSO

  :ALIST (("index" . 1) ("index_start_at" . 56) ("integer" . 12)

      ("float" . 19.2041) ("name" . "Jennifer") ("surname" . "Snow")

      ("fullname" . "Andrew1 Vaughan") ("email" . "sherri@ritchie.zw")

      ("bool" . :NULL)))

#S(ST-JSON:JSO

  :ALIST (("index" . 2) ("index_start_at" . 57) ("integer" . 14)

      ("float" . 14.9888) ("name" . "Alfred") ("surname" . "Pitts")

      ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re")

      ("bool" . :NULL))))

```

Now for some invalid JSON code

; JSON array embedded in an array embedded in an object

```lisp
(defparameter *short-encoded-items-B*
 "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew2 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")


(st-json:read-json-from-string *short-encoded-items-B*)

; Evaluation aborted on #<ST-JSON:JSON-PARSE-ERROR "Invalid slot name in object literal: ~A" {10116DA673}>.

```

Back to valid code

```lisp
; JSON array embedded in array

(defparameter *short-encoded-items*
 "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew3 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")


(st-json:read-json-from-string *short-encoded-items*)

(("items"

 #S(ST-JSON:JSO

  :ALIST (("index" . 1) ("index_start_at" . 56) ("integer" . 12)

      ("float" . 19.2041) ("name" . "Jennifer") ("surname" . "Snow")

      ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw")

      ("bool" . :NULL)))

 #S(ST-JSON:JSO

  :ALIST (("index" . 2) ("index_start_at" . 57) ("integer" . 14)

      ("float" . 14.9888) ("name" . "Alfred") ("surname" . "Pitts")

      ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re")

      ("bool" . :NULL)))))

```


<a id="other-information-1"></a>

### Other Information

Error Conditions.

St-json provides error conditions for json-type-error, json-parse error, json-error and json-eof-error. These are undocumented, so you will have to look at the source code for how to use them.

1.  as-json-bool

    | lisp | t     | nil    | ()     | :NULL | (make-array 5 :initial-element nil) |
    | json | :TRUE | :FALSE | :FALSE | :TRUE | :TRUE                               |

        from-json-booljson

    :TRUE

    :FALSE

    NULL

    lisp

    t

    nil

    :NULL

        jso

        (st-json:jso "a" "alph" "b" "beta")#S(ST-JSON:JSO :ALIST (("a" . "alph") ("b" . "beta")))
        (let ((item (st-json:jso "a" "alph" "b" "beta"))) (format t "~a" item))

        (#S(ST-JSON:JSO :ALIST ((a . alph) (b . beta)))

        getjso(let ((item (st-json:jso "a" "alph" "b" "beta"))) (st-json:getjso "a" item))"alph"T

2.  mapjso

    (let ((item (st-json:jso "a" "alph" "b" "beta")))

    (st-json:mapjso #'(lambda (x y) (format t " key: ~a :value ~a" x y)) item))

    key: a :value alph key: b :value beta


<a id="nested-json-objects-1"></a>

### Nested JSON Objects

Now consider the nested json object which I referred to in the data section. As a reminder, it looks like this.

{

"items": [

{

"index": 1,

"index<sub>start</sub><sub>at</sub>": 56,

"integer": 29,

"float": 16.8278,

"name": "Milton",

"surname": "Jensen",

"fullname": "Sheryl Winters",

"email": "denise@weiss.na",

"bool": false

}

]

}

What can we do with it in st-json? First, let's try to get the value where the key is "integer". In order to do that, you need to descend into the json tree. The following code is not what you would use, but gets the point across.

GETJSO

(st-json:getjso "integer"

(car

(st-json:getjso\* "items"

(with-open-file

(stream "/home/sabra/json-test/json4.txt")

(st-json:read-json

(slurp-stream4 stream))))))

29

GETJSO\*

The getjso\* function in theory allows you to take a key in the form of "a.b.c" and st-json will generate a series of getjso calls to go down each level and return the value for key c. This, however, does not seem to work in the above piece of json data because read-json does not result in jso objects all the way down. What we have is a jso object which wraps a cons which wraps a jso object. As a result, the intermediary cons prevents getjso\* from walking down the nested list.


<a id="yason-1"></a>

## [Yason](file:///)

From the author: "the major difference between YASON and the other JSON libraries that were available when I wrote it is that YASON does not require the user to come up with a Lisp data structure that reflects the JSON data structure that should be generated. Rather, I wanted a way to generate JSON directly from my internal data structures.

The reason for that desire was that I had to generate different JSON format in different contexts. That is, a class instance would sometimes be generated including all its attributes, sometimes just with a select set of attributes and sometimes as a reference. Thus, there was no right way to render an object as JSON, and I found the approach to first generate a data structure that would then be rendered as JSON to be wasteful and, as CL has no literal syntax for hash tables, ugly.

Instead of going through an intermediate data structure, YASON allows you to encode to a JSON stream on the fly (<http://common-lisp.net/project/yason/#stream-encoder>."


<a id="encoding-2"></a>

### Encoding

The yason:encode functions returns first the json-encoded item and second the lisp item. Note, in one test, yason:encode choked on a list which included a keyword :NULL in an unexpected location.

1.  list

    Really basic example.

        (yason:encode '("a" "alpha"))["a","alpha"]
        ("a" "alpha")

    Note that in the next call to encode, **country-a** is an alist. This throws an error. In the same fashion, calling the basic encode function on a plist also throws an error, demonstrating that you either need to know your data or maybe you need to be using something like cl-json which can cope better with the unexpected.

        (yason:encode *country-a*)
        Error: There is no applicable method for the generic function
        (yason:encode *country-p*)
        Error ...

2.  alist

    Whereas yason's basic encoding function choked on the alist, yason's encode-alist function works. (yason:encode-alist '((:a "alpha") ("b" "beta"))){"A":["alpha"],"b":["beta"]}((:A "alpha") ("b" "beta")) (yason:encode-alist '((:a . "alpha") ("b" "beta"))) {"A":"alpha","b":["beta"]}((:A . "alpha") ("b" "beta")) (yason:encode-alist **country-a**) {"ID":7,"NAME":"Finland","REGION-ID":108,"LATITUDE":64,"LONGITUDE":26,"ISO":"FI","PERMISSION-ID":1,"UPDATED-AT":"2005-09-11 00:15:40-07","CURRENCY":"Euro","CURRENCY-ALPHABET-CODE":"EUR","CURRENCY-NUMERIC-CODE":978,"POPULATION":5430670,"GDP":247,"CORRUPTION-INDEX":90} ((:ID . 7) (:NAME . "Finland") (:REGION-ID . 108) (:LATITUDE . 64) (:LONGITUDE . 26) (:ISO . "FI") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 5430670) (:GDP . 247) (:CORRUPTION-INDEX . 90))

3.  plist

    Whereas yason's basic encoding function choked on the plist, yason's encode-plist function works on a plist.

        (yason:encode-plist  '("sEcho" 1 "iTotalRecords" 57                           "iTotalDisplayRecords" 57                            "aaData" ("a" "b" ("d" "e" "f")                                     ("alpha" "beta" "nuna")                                     "c")))
        Records":57,"aaData":["a","b",["d","e","f"],["alpha","beta","nuna"],"c"]}
        {"sEcho":1,"iTotalRecords":57,"iTotalDispla
        y("sEcho" 1 "iTotalRecords" 57 "iTotalDisplayRecords" 57 "aaData" ("a" "b" ("d" "e" "f") ("alpha" "beta" "nuna") "c"))

        (yason:encode-plist *country-p*)

        {"ID":7,"NAME":"Finland","REGION-ID":108,"LATITUDE":64,"LONGITUDE":26,"ISO":"FI","PERMISSION-ID":1,"UPDATED-AT":"2005-09-11 00:15:40-07","CURRENCY":"Euro","CURRENCY-ALPHABET-CODE":"EUR","CURRENCY-NUMERIC-CODE":978,"POPULATION":5430670,"GDP":247,"CORRUPTION-INDEX":90}

        (:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO "FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY "Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978 :POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90)

4.  array

    Yason's basic encoding function will return a JSON array.

    (yason:encode (make-array 2 :initial-element "a" ))

    ["a","a"]

    \#("a" "a")

5.  hash-table

    Yason can also encode a hash-table, which it will return a json object:

        (yason:encode (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal))
        {"foo":1,"bar":[7,8,9]}
        #<HASH-TABLE :TEST EQUAL :COUNT 2 {100965BA03}>

6.  object

    For objects, you will have to write your own method to extend yason to encode objects.

7.  output-to-string macro

    Unlike cl-json and st-json which provide functions specifically for encoding to string (encode-json-to-string and write-json-to-string, respectively), yason relies on an output-to-string macro. I have to admit I had difficulty getting the right syntax on yason's own with-output-to-string\* macro working properly.

        (defun test3 ()
          (with-output-to-string (s)
                                 (yason:encode '("sEcho" 1 "iTotalRecords" 57
                                                 "aaData" ("a" "b" ("d" "e" "f")
                                                           "iTotalDisplayRecords" 57
                                                           ("alpha" "beta" "nuna")
                                                           "c"))
                                               s)))
        > (test3)
        "[\"sEcho\",1,\"iTotalRecords\",57,\"aaData\",[\"a\",\"b\",[\"d\",\"e\",\"f\"],\"iTotalDisplayRecords\",57,[\"alpha\",\"beta\",\"nuna\"],\"c\"]]"


<a id="decoding-3"></a>

### Decoding

Yason uses its generic function parse to generate a hash-table of the received json-object. It is possible to set the special variable **parse-object-as** :hash-table, :plist or :alist to specify the data structure that objects are parsed into. The default is :hash-table. It is also possible to set the special variable **parse-json-arrays-as-vectors** to t, in which case the json arrays will be parsed as vectors and not as lists.In the benchmark test where it received an array of country json objects, yason parsed the array into an array of hash-tables. Thus, where the original entry encoded into json took the form of:

```lisp
((:ID . 7) (:NAME . "Finland") (:REGION-ID . 108) (:LATITUDE . 64) (:LONGITUDE . 26) (:ISO . "FI") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 5430670) (:GDP . 247) (:CORRUPTION-INDEX . 90))
```

To get back to the same alist format would require something akin to:

```lisp
(nreverse (alexandria:hash-table-alist (aref (yason:parse *encoded-countries*) 0)))
```

resulting in a form such as:

```lisp
(("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69))

```

The following are decoding tests on JSON arrays, arrays in arrays and embedded arrays in an object and the results. I am using the additional yason parameters to show the alist representation instead of a hash-table and to return JSON arrays as vectors rather than lists.

; JSON array

```lisp
(defparameter *short-encoded-items-A*

"[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew4 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")


(yason:parse *short-encoded-items-A* :json-arrays-as-vectors t :object-as :alist)

#("items"

 (("bool") ("email" . "sherri@ritchie.zw") ("fullname" . "Andrew Vaughan")

 ("surname" . "Snow") ("name" . "Jennifer") ("float" . 19.2041)

 ("integer" . 12) ("index_start_at" . 56) ("index" . 1))

 (("bool") ("email" . "cheryl@craven.re") ("fullname" . "Barry Weiner")

 ("surname" . "Pitts") ("name" . "Alfred") ("float" . 14.9888)

 ("integer" . 14) ("index_start_at" . 57) ("index" . 2)))

```

Now for some invalid JSON code

; JSON array embedded in an array embedded in an object

```lisp
(defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew5 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")


(yason:parse *short-encoded-items-B* :json-arrays-as-vectors t :object-as :alist)

; Evaluation aborted on #<YASON::EXPECTED-COLON {1010C45823}>.


```

Back to valid code

```lisp
; JSON array embedded in array

(defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew6 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")


(yason:parse *short-encoded-items* :json-arrays-as-vectors t :object-as :alist)

#(#("items"

  (("bool") ("email" . "sherri@ritchie.zw") ("fullname" . "Andrew Vaughan")

  ("surname" . "Snow") ("name" . "Jennifer") ("float" . 19.2041)

  ("integer" . 12) ("index_start_at" . 56) ("index" . 1))

  (("bool") ("email" . "cheryl@craven.re") ("fullname" . "Barry Weiner")

  ("surname" . "Pitts") ("name" . "Alfred") ("float" . 14.9888)

  ("integer" . 14) ("index_start_at" . 57) ("index" . 2))))

```


<a id="nested-json-objects-2"></a>

### Nested JSON Objects

Taking the nested JSON object that I stored in json4.txt, how could we get an alist out of the innermost nested object? As a reminder, the JSON object was as follows:

```json
{

  "items": [

    {

      "index": 1,

      "index_start_at": 56,

      "integer": 29,

      "float": 16.8278,

      "name": "Milton",

      "surname": "Jensen",

      "fullname": "Sheryl Winters",

      "email": "denise@weiss.na",

      "bool": false

    }

  ]

}
```

What can we do with it in yason? Remember that it is a JSON object which keyword "items"contains an array which contains a JSON object.

Remembering that yason decodes objects as hash-tables,I can see how to get there. Just using the following ugly code just for sake of example (your code will look better than this):

```lisp
(alexandria:hash-table-alist

(second

 (first

 (alexandria:hash-table-alist

  (with-open-file (stream "/home/sabra/json-test/json4.txt")

          (yason:parse (slurp-stream4 stream)))))))

(("bool") ("email" . "denise@weiss.na") ("fullname" . "Sheryl Winters") ("surname" . "Jensen") ("name" . "Milton") ("float" . 16.8278) ("integer" . 29) ("index_start_at" . 56) ("index" . 1))

```

The point is that you need to know your data structure so that you can figure out how to walk the tree.

How would we get the value of the key "integer"?We can descend the json tree in this particular example something like this:

```lisp
(gethash "integer"

    (first

     (gethash "items"

         (with-open-file

          (stream "/home/sabra/json-test/json4.txt")

          (yason:parse (slurp-stream4 stream))))))

29
```


<a id="other-information-2"></a>

### Other Information

1.  Error Conditions

    Yason has an error condition for "no-json-output-context".


<a id="jsown-1"></a>

## [Jsown](file:///)

Jsown uses an MIT license. In decoding, it is fantastically fast. So much so that I first questioned whether it actually decoded anything.


<a id="encoding-3"></a>

### Encoding

Jsown encodes to a string.

The next version has encoding of hash-tables, arrays and :keywords.

-   (to-json x) is a generic function which you can specialize on your own

types. This allows you to nest lisp objects in a jsown object and serialize them in a suitable way.- (to-json\* x) is the non-generic function variant of the same thing. It isn't as smart, but it is faster. As you can see from the performance table above, jsown:to-json turned in an encoding time of 9.556 seconds while jsown:to-json\* turned in an encoding time of 2.97 seconds.

As noted in the table matching library functions against each other, json:to-json returns an array or returns an object depending on what you tell it to do. Thus:

```lisp
(jsown:to-json* '(("a" "alpha") ("b" "beta")))


"[[\"a\",\"alpha\"],[\"b\",\"beta\"]]"


(jsown:to-json '(:obj (("a" "alpha") ("b" "beta"))))


"{[\"a\",\"alpha\"]:[[\"b\",\"beta\"]]}"

```

When to-json is called, jsown will internally call to-json each step of the way. This has a performance downside, yet it seems to provide the least surprises in the output. If you need more performance, jsown\* offers that, at the cost of flexibility."

As noted in the table matching library functions against each other, to-json is a generic function, allowing you to provide specific implementations for your own objects so those can easily be converted to JSON too (and it will ensure that nested objects are correctly transformed to json).

    (jsown:to-json '(:obj ("bingo" . 24.93) ("bang" 1 2 3 "foo" 4 5) ("foo" . "bar")))
    "{\"bingo\":24.93,\"bang\":[1,2,3,\"foo\",4,5],\"foo\":\"bar\"}"

When reading JSON objects, jsown converts their content to the most lispy translation of what was in there. As such, JSON's false will be translated to nil, which coincidentally also be the translation of JSON's []. When writing objects lisp's nil is translated to the empty JSON list []. You can write JSON's false by writing lisp's keywords :false or :f.

       (jsown:to-json (jsown:new-js                   ("items" nil)                   ("falseIsEmptyList" :f)                   ("success" t)))
      "{\"success\":true,\"falseIsEmptyList\":false,\"items\":[]}"
      (jsown:to-json 1)


    "1"


    (jsown:to-json 1.0)

    "1.0"


    (jsown:to-json t)

    "true"


    (jsown:to-json nil)

    "[]"


    (jsown:to-json (/ 1 3))

    "0.33333334f0"


    (jsown:to-json 'j)

    Error


    (jsown:to-json #\b)

    Error


    (jsown:to-json '("a" "b"))

    "[\"a\",\"b\"]"


    (jsown:to-json '(("a" "b") (:f 12)))

    "[[\"a\",\"b\"],[false,12]]"


    (jsown:to-json '(("a" "b") (:k 12)))

    Error (notice the only difference is the prior keyword :f translated to nil and here the keyword is :k


    (jsown:to-json (make-array 3 :initial-element "f"))

    Error


    (jsown:to-json *customer-hash*)

    Error


    (jsown:to-json (make-instance 'country))

    Error

If you are constructing json objects, consider using the jsown:js-new and jsowon:extend-js functions.jsown:js-new has a clean and clear interface for building content. It works together with jsown:extend-js if [you] need to split up the object creation.

The latest version of jsown has a setf-expander on (setf jsown:val) which automatically creates a jsown-object if no such object was available at the designated place. An example should clarify:

```lisp
(let (doc)

 (setf (jsown:val (jsown:val (jsown:val doc "properties") "color") "paint") "red")

 (jsown:to-json doc))

"{\"properties\":{\"color\":{\"paint\":\"red\"}}}"

```

It turns out to be a handy little feature when you need to build deeply nested json documents.

-   Warning: jsown:to-json did not handle the alist sample data **country-a**. It did successfully handle the plist sample data **country-p**.\*

```lisp
(jsown:to-json *country-a*)

; Evaluation aborted on #<SB-SYS:MEMORY-FAULT-ERROR {100AA82233}>.


(jsown:to-json *country-p*)

"[\"ID\",7,\"NAME\",\"Finland\",\"REGION-ID\",108,\"LATITUDE\",64,\"LONGITUDE\",26,\"ISO\",\"FI\",\"PERMISSION-ID\",1,\"UPDATED-AT\",\"2005-09-11 00:15:40-07\",\"CURRENCY\",\"Euro\",\"CURRENCY-ALPHABET-CODE\",\"EUR\",\"CURRENCY-NUMERIC-CODE\",978,\"POPULATION\",5430670,\"GDP\",247,\"CORRUPTION-INDEX\",90]"

```

Decoding

This is where jsown really shines. Jsown is by far the fastest decoder. It not only decodes, it also makes it easy to pull out elements of the json-object. For example, consider the **encoded-countries** special variable. Jsown can pull out the id and name of the first object in the encoded array thusly:

```lisp
(jsown:parse *encoded-countries* "id" "name")

(:OBJ ("id" . 11) ("name" . "Ireland"))

Just some decoding examples:


(jsown:parse "{\"id\": \"null\"}")

(:OBJ ("id" . "null"))


(jsown:parse "[\"id\": \"null\"]")

("id")
```

; Where the JSON document is an array in a string:

```lisp
(defparameter *short-encoded-items-A*"[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew7 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")



(jsown:parse *short-encoded-items-A*)

("items"

(:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 12)

 ("float" . 192041/10000) ("name" . "Jennifer") ("surname" . "Snow")

 ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw")
("bool"))

(:OBJ ("index" . 2) ("index_start_at" . 57) ("integer" . 14)

 ("float" . 9368/625) ("name" . "Alfred") ("surname" . "Pitts")

 ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re")
("bool")))


; Where the JSON document is an arrays embedded in another array


(defparameter
*short-encoded-items*"[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew8
Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\", \"surname\":\"Pitts\",\"fullname\":\"Barry
Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")


(jsown:parse *short-encoded-items*)

(("items"

 (:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 12)

 ("float" . 192041/10000) ("name" . "Jennifer") ("surname" . "Snow")

 ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw") ("bool"))

 (:OBJ ("index" . 2) ("index_start_at" . 57) ("integer" . 14)

 ("float" . 9368/625) ("name" . "Alfred") ("surname" . "Pitts")

 ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re") ("bool"))))

```


<a id="nested-json-objects-3"></a>

### Nested JSON Objects

Now consider the nested JSON object which I keep referring to. Again, as a reminder, here is the object.

```json
{

  "items": [

    {

      "index": 1,

      "index_start_at": 56,

      "integer": 29,

      "float": 16.8278,

      "name": "Milton",

      "surname": "Jensen",

      "fullname": "Sheryl Winters",

      "email": "denise@weiss.na",

      "bool": false

    }

  ]

}
```

What can we do with it in jsown? It is a JSON object which keyword "items"contains an array which contains a JSON object. I've limited the dataset size simply for explanation purposes. Also note that several of the following functions require that we have parsed the JSON object first.

1.  PARSE

    ```lisp
    (with-open-file (stream "/home/sabra/json-text/json4.txt") (jsown:parse (slurp-stream4 stream)))


    (:OBJ ("items" (:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 29) ("float" . 84139/5000) ("name" . "Milton") ("surname" . "Jensen") ("fullname" . "Sheryl Winters") ("email" . "denise@weiss.na") ("bool"))))

    ```

    That was the generic parse. Now, if you add keywords to the parse function, you can get specific items back. This is easy to see in the case of an unested object:

    ```lisp
    (jsown:parse "{\"foo\":\"bar\",\"baz\":100.25}")

    (:OBJ ("foo" . "bar") ("baz" . 401/4))


    (jsown:parse "{\"foo\":\"bar\",\"baz\":100.25}" "baz")

    (:OBJ ("baz" . 401/4))



    (jsown:val

    (first

     (jsown:val

     (with-open-file (stream "/home/sabra/json-test/json4.txt")

             (jsown:parse (slurp-stream4 stream)

                    "items"))

     "items"))

    "integer")

    29
    ```

    What happens if the data stream provides all values as string? Well, again, you need to know how your data source is providing the data and deal with it correctly.

    Jsown does have a great function called filter that allows you to traverse a tree of json objects and interpret its results.

    ```lisp
    (jsown:filter (jsown:new-js

           ("one" 100)

           ("two" (jsown:new-js

               ("three" (list (jsown:new-js ("four" (jsown:new-js

                                  ("five" "result-one"))))

                       (jsown:new-js ("four" (jsown:new-js

                                  ("five" "result-two"))))

                       (jsown:new-js ("four" (jsown:new-js

                                  ("five" "result-three")))))))))

           "two" "three" map "four" "five")

    > ("result-one" "result-two" "result-three")

    ```

    KEYWORDS

    ```lisp
    (with-open-file (stream "/home/sabra/json-test/json4.txt") (jsown:keywords  (jsown:parse (slurp-stream4 stream))))


    ("items")

    ```

    DO-JSON-KEYS

    ```lisp
    (with-open-file (stream "/home/sabra/json-test/json4.txt") (jsown:do-json-keys (keyword value) (jsown:parse (slurp-stream4 stream)) (format t "~A > ~A~%" keyword value)))


    items > ((OBJ (index . 1) (index_start_at . 56) (integer . 29) (float . 84139/5000) (name . Milton) (surname . Jensen) (fullname . Sheryl Winters) (email . denise@weiss.na) (bool)))

    ```

    VAL

    ```lisp
    jsown:val (jsown:parse "{\"foo\":\"bar\",\"baz\":100.25}" "baz") "baz")

    401/4

    ```

    If you have a nested object/array/object JSON document and you want some value that is buried deep in the tree, then just like any other library you need to walk the tree and get it. It is not that difficult.

    ```lisp
    (jsown:val

    (first

     (jsown:val

     (with-open-file (stream "/home/sabra/projects/lisp-tax-test/json4.txt")

             (jsown:parse (slurp-stream4 stream)))

     "items"))

    "float")

    84139/5000

    ```


<a id="jonathan-1"></a>

## Jonathan

While jsown is the winner in the decoding stakes, Jonathan is the clear winner in encoding.


<a id="encoding-4"></a>

### Encoding

The basic encoding function is to-json. Jonathan can return either a string or octets.

to-json

list

(jonathan:to-json '("sEcho" 1 "iTotalRecords" 57 "iTotalDisplayRecords" 57 "f" ("alpha" "beta" "nuna") "c"))

"[\\"sEcho\\",1,\\"iTotalRecords\\",57,\\"iTotalDisplayRecords\\",57,\\"f\\",[\\"alpha\\",\\"beta\\",\\"nuna\\"],\\"c\\"]"

Now consider the encoding using the **country-a** and **country-p** parameters listed in the beginning of this page:

(jonathan:to-json **country-p**)

"{\\"ID\\":7,\\"NAME\\":\\"Finland\\",\\"REGION-ID\\":108,\\"LATITUDE\\":64,\\"LONGITUDE\\":26,\\"ISO\\":\\"FI\\",\\"PERMISSION-ID\\":1,\\"UPDATED-AT\\":\\"2005-09-11 00:15:40-07\\",\\"CURRENCY\\":\\"Euro\\",\\"CURRENCY-ALPHABET-CODE\\":\\"EUR\\",\\"CURRENCY-NUMERIC-CODE\\":978,\\"POPULATION\\":5430670,\\"GDP\\":247,\\"CORRUPTION-INDEX\\":90}"

While to-json can handle a plist without any additional parameters, to-json will throw an error if handed an alist without warning.

Jonathan expects simple-strings. Postmodern, for example, does not return text fields as simple-strings, so the fields have to be coerced to something that jonathan will accept.

See, eg.<http://ircbrowse.net/browse/lisp?id9282910&timestamp1442474317#t1442474317>.

1.  alist

    Consider the following:

    ```lisp
    (jonathan:to-json '((a . "1") (b . "2")))

    ; Evaluation aborted on #<TYPE-ERROR expected-type: "LIST"datum: "1">.


    (jonathan:to-json '((a "1") (b "2")))

    "[[\"A\",\"1\"],[\"B\",\"2\"]]"


    (jonathan:to-json '((:a "1") (:b "2")))

    "[{\"A\":\"1\"},{\"B\":\"2\"}]"

    ```

    Jonathan does not like cons cells. Remember **country-a** has cons cells:

    ```lisp
    ((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53)
    (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT .
    "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE
    . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP .
    205) (:CORRUPTION-INDEX . 69))

    ```

    Earlier tests with jonathan encoding **country-a** were successful. The most recent tests failed with a type error. I have not been able to get a response on follow-ups.

    ```lisp
    (jonathan:to-json *country-a* :from :alist)

    "{\"ID\":11,\"NAME\":\"Ireland\",\"REGION-ID\":4,\"LATITUDE\":53,\"LONGITUDE\":-8,\"ISO\":\"IE\",\"PERMISSION-ID\":1,\"UPDATED-AT\":\"2005-09-11 00:15:40-07\",\"CURRENCY\":\"Euro\",\"CURRENCY-ALPHABET-CODE\":\"EUR\",\"CURRENCY-NUMERIC-CODE\":978,\"POPULATION\":4588252,\"GDP\":205,\"CORRUPTION-INDEX\":69}"


    (jonathan:to-json '(("a" "alpha") ("b" "beta")))

    "[[\"a\",\"alpha\"],[\"b\",\"beta\"]]"

    ```

2.  plist

    Similar to alists

    (jonathan:to-json '(:A "1" :B "2"))

    "{\\"A\\":\\"1\\",\\"B\\":\\"2\\"}"

    (jonathan:to-json '(A "1" B "2"))

    "[\\"A\\",\\"1\\",\\"B\\",\\"2\\"]"

3.  hash-table

    jonathan can handle encoding hash tables. As noted in the examples below, it will normally result in writing out a json object as a string:

    (jonathan:to-json (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal))

    "{\\"foo\\":1,\\"bar\\":[7,8,9]}"


<a id="decoding-4"></a>

### Decoding

Testing arrays in arrays and arrays in arrays in objects

```lisp
(defparameter *short-encoded-items-A*

"[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew9 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")



(jonathan:parse *short-encoded-items-A*)

("items" (:|bool| NIL :|email| "sherri@ritchie.zw" :|fullname| "Andrew Vaughan" :|surname| "Snow" :|name| "Jennifer" :|float| 192041/10000 :|integer| 12 :|index_start_at| 56 :|index| 1) (:|bool| NIL :|email| "cheryl@craven.re" :|fullname| "Barry Weiner" :|surname| "Pitts" :|name| "Alfred" :|float| 9368/625 :|integer| 14 :|index_start_at| 57 :|index| 2))


(jonathan:parse *short-encoded-items-A* :as :alist)

("items" (("bool") ("email" . "sherri@ritchie.zw") ("fullname" . "Andrew Vaughan") ("surname" . "Snow") ("name" . "Jennifer") ("float" . 192041/10000) ("integer" . 12) ("index_start_at" . 56) ("index" . 1)) (("bool") ("email" . "cheryl@craven.re") ("fullname" . "Barry Weiner") ("surname" . "Pitts") ("name" . "Alfred") ("float" . 9368/625) ("integer" . 14) ("index_start_at" . 57) ("index" . 2)))


(jonathan:parse *short-encoded-items-A* :as :array)

("items" (:|bool| NIL :|email| "sherri@ritchie.zw" :|fullname| "Andrew Vaughan" :|surname| "Snow" :|name| "Jennifer" :|float| 192041/10000 :|integer| 12 :|index_start_at| 56 :|index| 1) (:|bool| NIL :|email| "cheryl@craven.re" :|fullname| "Barry Weiner" :|surname| "Pitts" :|name| "Alfred" :|float| 9368/625 :|integer| 14 :|index_start_at| 57 :|index| 2))


(jonathan:parse *short-encoded-items-A* :as :hashtable)

("items" (:|bool| NIL :|email| "sherri@ritchie.zw" :|fullname| "Andrew Vaughan" :|surname| "Snow" :|name| "Jennifer" :|float| 192041/10000 :|integer| 12 :|index_start_at| 56 :|index| 1) (:|bool| NIL :|email| "cheryl@craven.re" :|fullname| "Barry Weiner" :|surname| "Pitts" :|name| "Alfred" :|float| 9368/625 :|integer| 14 :|index_start_at| 57 :|index| 2))

```

1.  Now for some invalid JSON

    code

    :CUSTOM<sub>ID</sub>: now-for-some-invalid-json-code

    ; JSON array embedded in an array embedded in an object

    ```lisp
    (defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew10 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")


    (jonathan:parse *short-encoded-items-B*)

    Evaluation aborted on #<JONATHAN.ERROR:<JONATHAN-INCOMPLETE-JSON-ERROR>

    Incomplete JSON string:

    {[["items",{"index":1,"index_start_at":56,"integer":12,"float":19.2041,"name":"Jennifer","surname":"Snow","fullname":"Andrew11 Vaughan","email":"sherri@ritchie.zw","bool":null},{"index":2,"index_start_at":57,"integer":14,"float":14.9888,"name":"Alfred","surname":"Pitts","fullname":"Barry Weiner","email":"cheryl@craven.re","bool":null}]]}

     [Condition of type JONATHAN.ERROR:<JONATHAN-INCOMPLETE-JSON-ERROR>]

    ```

2.  Back to valid code

    ```lisp

    (defparameter *short-encoded-items* "[[\"items\", {\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew12 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null}, {\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry   Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")


    (jonathan:parse *short-encoded-items*)

    (("items" (:|bool| NIL :|email| "sherri@ritchie.zw" :|fullname| "Andrew Vaughan" :|surname| "Snow" :|name| "Jennifer" :|float| 192041/10000 :|integer| 12 :|index_start_at| 56 :|index| 1) (:|bool| NIL :|email| "cheryl@craven.re" :|fullname| "Barry Weiner" :|surname| "Pitts" :|name| "Alfred" :|float| 9368/625 :|integer| 14 :|index_start_at| 57 :|index| 2)))

    ```

    ; JSON array embedded in array


<a id="other-attributes"></a>

### Other attributes

Jonathan can emit either as a string or as octets. It also has the ability to compile the encoder.


<a id="json-streams"></a>

## [Json-streams](https://www.google.com/url?q=https%3A%2F%2Fgithub.com%2Fcopyleft%2Fjson-streams&sa=D&sntz=1&usg=AFQjCNHNholdktw4j7XPK4_JLIHhE56mzQ)

The author describes Json-streams advantages as:

-   Separation of low-level and high-level functionality. json-streamsprovides a basic tokenizer for json, so that it's easy to build any datastructure mapping one wants on top of it. All the other json librariescould in theory be built on it. The problem with many of them is thatthey've chosen a particular data mapping that very often isnon-deterministic (False and [] both map to NIL, for example), or simplydoesn't suit a particular use case. With json-streams you have fullcontrol of these things. But it also comes with a high-level API with achosen mapping so it's ready to use.
-   The API is streaming, so you don't have to process more than you wantto, and you can process files of any size.
-   Both Unicode and numbers are properly handled.

I will note that decoding with json-streams is not fast. It is faster decoding than cl-json, but slower than all the rest of the libraries.


<a id="encoding-5"></a>

### Encoding

[Still needs to be done]

Note that unlike the other libraries, json-streams does not have a generic function which handles many different types of lisp types. For example, encoding the alist variable **country-a** would look like:

(json-streams:json-stringify(list\* :object (mapcar (lambda (obj) (cons (symbol-name (car obj)) (cdr obj))) **country-a**)))

Obviously this works for an alist, but not for a plist. I do expect that future versions will have a more generic function.


<a id="decoding-incomplete"></a>

### Decoding (Incomplete)

```lisp
(defparameter *short-encoded-items-A*

"[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew13 Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")

(json-parse *short-encoded-items-a*)

(:ARRAY "items"

(:OBJECT ("index" . 1) ("index_start_at" . 56) ("integer" . 12)

 ("float" . 19.2041d0) ("name" . "Jennifer") ("surname" . "Snow")

 ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw")

 ("bool" . :NULL))

(:OBJECT ("index" . 2) ("index_start_at" . 57) ("integer" . 14)

 ("float" . 14.9888d0) ("name" . "Alfred") ("surname" . "Pitts")

 ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re")

 ("bool" . :NULL)))

```


<a id="other-comments"></a>

### Other Comments


<a id="com.gigamonkeys.json"></a>

## Com.gigamonkeys.json


<a id="ea11f064-801c-4dce-899d-f4d94abb052b"></a>

### Encoding

```lisp
(com.gigamonkeys.json:to-json *country-a*)

((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53) (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP . 205) (:CORRUPTION-INDEX . 69))

(com.gigamonkeys.json:to-json *country-p*)

(:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO
"FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY
"Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978
:POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90)

(json x) is supposed to be the top level function for converting lisp
objects into a string in the json format. However, running (json x)
against the *country-a* and *country-p* arrays and plists triggered an
error

(com.gigamonkeys.json:json *country-a*)

Can't stringify (ID . 11)

 [Condition of type SIMPLE-ERROR]

```

I ran into a similar issue with (write-json) which is intended to write a lisp object to a stream.

hash-tables

Gigamonkeys had no trouble encoding hash tables. Here is a simple example.

```lisp
(com.gigamonkeys.json:to-json (alexandria:plist-hash-table *country-p*))

(:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO "FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY "Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978 :POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90)

```


<a id="decoding-5"></a>

### Decoding

Hash tables are used to represent Javascript objects and vectors to represent arrays.

The following is the result from calling parse-json on two slightly different encodings of **country-a**. Notice that the first I used cl-json:encode-json-to-string and the second used cl-json:encode-json

```lisp
(com.gigamonkeys.json:parse-json (cl-json:encode-json-to-string
*country-a*))

("id" 11 "name" "Ireland" "regionId" 4 "latitude" 53 "longitude" -8
"iso" "IE" "permissionId" 1 "updatedAt" "2005-09-11 00:15:40-07"
"currency" "Euro" "currencyAlphabetCode" "EUR" "currencyNumericCode" 978
"population" 4588252 "gdp" 205 "corruptionIndex" 69)


(com.gigamonkeys.json:parse-json (cl-json:encode-json *country-a*))

{"id":11,"name":"Ireland","regionId":4,"latitude":53,"longitude":-8,"iso":"IE","permissionId":1,"updatedAt":"2005-09-11
00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":4588252,"gdp":205,"corruptionIndex":69}

```
