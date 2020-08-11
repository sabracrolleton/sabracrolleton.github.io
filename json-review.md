---
layout: default
title: The original 2016 review of Common Lisp Json Libraries
---
# Table of Contents

1.  [Comparing cl-json, st-json, yason, jsown and com.gigamonkeys.json](#4582c0db-3069-4662-b4ba-4d924a6c67f2)
    1.  [Quick Summary](#555e1e34-b6c9-4a30-9fc3-ee43b2d13bd9)
        1.  [Libraries](#e8be9525-2b29-4651-9808-367beeeda248)
        2.  [Quick JSON syntax refresher](#fda5c8cd-eaab-4f94-a78a-6e2b9d63eab8)
        3.  [Reminder on Security](#6b83a031-e1c0-410a-a452-b23657947627)
    2.  [Mapping Data Structures from JSON to lisp](#b3cbb240-045a-49db-8079-b1ff1cd1e7df)
    3.  [Mapping Data Structures from lisp to JSON](#18c05971-2485-429a-9eff-46ffcbd490dc)
    4.  [Mapping library functions against each other](#c3772dc5-64ce-4174-b2f9-fa13accf1071)
    5.  [Benchmarking](#8d97448d-ab64-4567-918b-4ee9c7d9156f)
        1.  [Encoding](#342127f9-ef96-4bf4-9d21-1ec2be4d1cec)
    6.  [Decoding](#7492f7a8-2d64-4bff-b0e8-c04fa0aceb7f)
    7.  [cl-json](#733af192-a0ff-4f7d-bade-aea6dfec38bd)
        1.  [Encoding](#0360a723-90f3-45ae-806f-5e0b8aba6645)
        2.  [Encode-json-to-string](#04c13e20-c243-4a4b-ba2a-e05ac838b199)
        3.  [Decoding](#29cad98b-04f6-4461-b277-7cd2dc2b08ee)
        4.  [Nested JSON Objects](#1fa98b08-39f4-4a2f-abe9-4a7ea4ba7272)
        5.  [Other Information](#cda26baa-e882-4ef8-8263-0981c3d0b334)
    8.  [st-json](#4483b932-bdd4-4251-a86e-736a1c0ca8be)
        1.  [Encoding](#887758c0-6365-421c-a146-eb023f25fcf5)
        2.  [Decoding](#e7e75f4c-a665-48d5-b83d-3d33b1d315fe)
        3.  [Other Information](#5769e4e4-d5b0-423e-9bfb-c334b23a7476)
        4.  [Nested JSON Objects](#861f43e0-6742-453a-8068-d0f98ffc0e6b)
    9.  [Yason](#f4006ee2-82d8-4693-8dc6-8b0b7e76013a)
        1.  [Encoding](#743bc712-4e34-46ce-a15d-cb654ea37e61)
        2.  [Decoding](#f3541c99-2640-45bc-b376-e9fa8599e613)
        3.  [Nested JSON Objects](#32399c86-31cf-4443-a0a9-15b0af75532b)
        4.  [Other Information](#b0de07fe-0399-45a5-8fc9-1fc7c143d51b)
    10. [Jsown](#70c41b15-eefe-420c-9e2c-1bbc92062fab)
        1.  [Encoding](#5604415e-fc22-423d-a3d7-19ebc9520066)
        2.  [Nested JSON Objects](#cfe19cf9-ae03-432e-a55b-5cfde40e4da4)
    11. [Com.gigamonkeys.json](#bc02ce38-5c93-4fac-b46a-8e455335b556)
        1.  [Encoding](#ebc86a54-5992-425c-a5ea-3ba4a52d273f)
        2.  [Decoding](#1700394f-cee7-4785-b622-17ec332005c0)
    12. [Further tests](#3492fe0c-e37d-4875-806a-59ecf5283ded)

<a id="4582c0db-3069-4662-b4ba-4d924a6c67f2"></a>

# Comparing cl-json, st-json, yason, jsown and com.gigamonkeys.json

JSON (JavaScript Object Notation) is a lightweight data-interchange format, codified in rfc4627. Common lisp currently has five libraries that address importing and exporting json data. (For purposes of this comparison, I will refer to "encoding" as converting from common lisp to JSON and "decoding" as converting from JSON to common lisp. This note comparescl-json, st-json, yason, jsown and com.gigamonkeys.json. Cl-json has by far the biggest feature list, allowing for library internal mapping between JSON objects and clos objects. Yason was created as a lighter weight alternative. St-json was also created to be simpler than cl-json but also to be more precise about types (distinguishing between boolean false, the empty array and the empty object. Jsown appears to be incredibly good at decoding. If you use the generic encoding function (jsown:to-json), it is slower at encoding, but if you use the non-generic function (jsown:to-json\*) it is competitive. The generic function is likely to be faster if you specialize the generic functions for your data. All the libraries write to streams; cl-json and yason generally allowing the stream specification as an optional parameter and st-json requiring that you specify the stream. Jsown encodes to string and it is your job to get it to whatever stream you desire. com.monkeylib.json encodes to a stream or a string. Obviously, any of these libraries can read from a file using the normal common-lisp with-open-file macro.


<a id="555e1e34-b6c9-4a30-9fc3-ee43b2d13bd9"></a>

## Quick Summary

As can be expected, the libraries do much of the same if you have basic needs. However, significant differences exist and should be considered in choosing which library is right for any particular project. Many applications are asymmetric in how they will use these libraries. If you are mostly getting JSON data from somewhere else, parsing it and using it, then jsown will likely be your best choice. It is focused on decoding JSON data and is an order of magnitude faster than any other library at this task. If you do not want to write your own methods for generic functions and you are going back and forth between JSON objects and clos objects, I would recommend cl-json. If you need to pay particular attention to the differences between null, nil and false, you should probably consider st-json. If you are going to be encoding lisp data but are not sure what the data will look like, cl-json may be your best choice because it handles different types of lisp data out of the box better than the other libraries. For example, handed a list of alists or a list of plists, cl-json correctly encoded an array of json objects and an array of JSON arrays respectively. Neither st-json nor yason could cope with that in one call. In the cases of st-json and yason, you would have to break it into loops and run the encoding function on elements.

At the end of the day, your particular data and requirements will determine which library is best for any particular application and you need to pay attention to what your data will look like and what the receiving end will do with it. For example, in one test, yason:encode choked on a list which included a keyword :NULL in an unexpected location. Cl-json just encoded it as the string "null" and st-json encoded it as 'null' (not a string). In testing for your use, deliberately feed badly formed data and see how the library reacts. Some will throw recoverable conditions (depending on the error) while others may actually lock up a thread.


<a id="e8be9525-2b29-4651-9808-367beeeda248"></a>

### Libraries

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">Library</td>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">com.gigamonkey.json</td>
</tr>


<tr>
<td class="org-left">Author</td>
<td class="org-left">Henrik Hjelte, Boris Smilga, Robert Goldman</td>
<td class="org-left">Marijn Haverbeke</td>
<td class="org-left">Hans Huebner</td>
<td class="org-left">Aad Versteden</td>
<td class="org-left">Peter Seibel</td>
</tr>


<tr>
<td class="org-left">License</td>
<td class="org-left">MIT</td>
<td class="org-left">zlib-style</td>
<td class="org-left">BSD</td>
<td class="org-left">MIT</td>
<td class="org-left">See source code</td>
</tr>


<tr>
<td class="org-left">Website</td>
<td class="org-left"><http://common-lisp.net/project/cl-json/></td>
<td class="org-left"><http://marijnhaverbeke.nl/st-json/>  <http://common-lisp.net/project/yason/></td>
<td class="org-left"><https://github.com/madnificent/jsown></td>
<td class="org-left"><https://github.com/gigamonkey/monkeylib-json></td>
</tr>


<tr>
<td class="org-left">Conclusion</td>
<td class="org-left">Best with uncertain data types and errors</td>
<td class="org-left">Best with nil/null/false issues</td>
<td class="org-left">Lighter than cl-json</td>
<td class="org-left">The best at decoding and parsing json</td>
<td class="org-left">Had trouble with test data sets</td>
</tr>
</tbody>
</table>

1.  Some test data.

    For reference and testing purposes, I used a few different data dumps turned into parameters.

        (defparameter *country-a* ((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53) (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP . 205) (:CORRUPTION-INDEX . 69)))

        (defparameter *countries-a*) ; which is a list of 217 *country-a* type entries.

        (defparameter *country-p* '(:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO "FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY "Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978 :POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90))

        (defparameter *countries-p*) ; a list of 217 *country-p* type entries.

    While this particular **country-a** and **country-p** parameters do not have :null results, the parameters **countries-a** and **countries-p** do have some :null results.

    I have also stored the following nested json object "/home/sabra/json-test/json4.txt". I then use a function called slurp-stream4 from <http://www.ymeme.com/slurping-a-file-common-lisp-83.html> to slurp the entire file into a single string to provide to the libraries for testing how they descend into a nested object.

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

    Other data is created by making calls to public data sources such as:

        (drakma:http-request "http://www.broadbandmap.gov/broadbandmap/api/dictionary/v1/geography?format=json")

    Note: If you want to query Facebook API and process their JSON output, don't forget to execute

        (push (cons "application" "json") drakma:*text-content-types*)

    or whatever the content-type Facebook uses or drakma will return data as byte arrays which IIRC no JSON library knows what to do with it.

    Still other test data sets were generated by on-line json data generators such as <http://www.json-generator.com/> and <http://jsongen.pykaso.net/>.

    Note: I have not tested for anything other than UTF-8 character sets.


<a id="fda5c8cd-eaab-4f94-a78a-6e2b9d63eab8"></a>

### Quick JSON syntax refresher

First, a quick refresher on JSON syntax:

-   Data is represented by name/value pairs
-   Curly braces hold objects and each name is followed by a colon, with the name/value pairs separated by a comma
-   Square brackets hold arrays and values are separated by a comma


<a id="6b83a031-e1c0-410a-a452-b23657947627"></a>

### Reminder on Security

Anyone reading this is likely not to need this reminder, but I will put it here to remind myself, if no-one else. Getting JSON objects from another source is just as insecure as any other data you receive from another source. You are still responsible for ensuring that you have properly sanitized, validated or other checked the security of the data.


<a id="b3cbb240-045a-49db-8079-b1ff1cd1e7df"></a>

## Mapping Data Structures from JSON to lisp

The following table sets out the mapping from JSON datastructures to lisp datastructures using the normal functions listed above.

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">json</td>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">com.gigamonkeys.json</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">functions</td>
<td class="org-left">(cl-json:decode-json x)(cl-json:decode-json-strict x) (cl-json:decode-json-from-string x)(cl-json:decode-json-from-source x)</td>
<td class="org-left">(st-json:read-json x)</td>
<td class="org-left">(yason:parse x) (yason:\*parse-json-arrays-as-vectors\*)(yason:\*parse-json-booleans-as-symbols\*) (yason:\*parse-json-null-as-keyword\*)(yason:\*parse-object-key-fn\*)</td>
<td class="org-left">(jsown:parse x)</td>
<td class="org-left">(com.gigamonkeys.json:parse-json x)</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">number</td>
<td class="org-left">integer,float</td>
<td class="org-left">number</td>
<td class="org-left">number</td>
<td class="org-left">number</td>
<td class="org-left">number</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">null</td>
<td class="org-left">nil</td>
<td class="org-left">:null</td>
<td class="org-left">nill</td>
<td class="org-left">(null if \\\*parse-json-null-as-keyword\\\* is set)</td>
<td class="org-left">nil</td>
<td class="org-left">:NULL</td>
</tr>


<tr>
<td class="org-left">false</td>
<td class="org-left">nil</td>
<td class="org-left">:false</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
<td class="org-left">:FALSE</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">true</td>
<td class="org-left">t</td>
<td class="org-left">:true</td>
<td class="org-left">t</td>
<td class="org-left">t</td>
<td class="org-left">:TRUE</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">object</td>
<td class="org-left">alist (or clos object)</td>
<td class="org-left">st-json:jso object</td>
<td class="org-left">hash-table</td>
<td class="org-left">jsown:json object</td>
<td class="org-left">hash-table</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">array</td>
<td class="org-left">list (or vector)</td>
<td class="org-left">list</td>
<td class="org-left">list (vectors if \\\*parse-json-arrays-as-vectors\\\* is set)</td>
<td class="org-left">list</td>
<td class="org-left">vector</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="18c05971-2485-429a-9eff-46ffcbd490dc"></a>

## Mapping Data Structures from lisp to JSON

The following table sets out the mapping in the encoding direction, that is, going from lisp datastructures to json datastructures.

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">lisp</td>
<td class="org-left">cl-json -> json</td>
<td class="org-left">st-json -> json</td>
<td class="org-left">yason -> json</td>
<td class="org-left">jsown -> json</td>
<td class="org-left">com.gigamonkeys.json</td>
</tr>


<tr>
<td class="org-left">functions</td>
<td class="org-left">(cl-json:encode-json x)</td>
<td class="org-left">(st-json:write-json x stream)</td>
<td class="org-left">(yason:encode x)</td>
<td class="org-left">(jsown:to-json x)</td>
<td class="org-left">(com.gigamonkeys.json:json x)</td>
</tr>


<tr>
<td class="org-left">integer</td>
<td class="org-left">number (no frac/exp)</td>
<td class="org-left">number (no frac/exp)</td>
<td class="org-left">number (no frac/exp)</td>
<td class="org-left">number as string e.g "1"</td>
<td class="org-left">number (no frac/exp)</td>
</tr>


<tr>
<td class="org-left">float</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number as string e.g. "1.0"</td>
<td class="org-left">number (with frac/exp)</td>
</tr>


<tr>
<td class="org-left">rational</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number (with frac/exp)</td>
<td class="org-left">number as string e.g. "0.33333333"</td>
<td class="org-left">number</td>
</tr>


<tr>
<td class="org-left">t</td>
<td class="org-left">true</td>
<td class="org-left">true</td>
<td class="org-left">true</td>
<td class="org-left">"true"</td>
<td class="org-left">true</td>
</tr>


<tr>
<td class="org-left">nil</td>
<td class="org-left">null</td>
<td class="org-left">[] (danger - true in json)</td>
<td class="org-left">null</td>
<td class="org-left">"[]". You can write json's false by writing lisp's keywords :false or :f</td>
<td class="org-left">{}</td>
</tr>


<tr>
<td class="org-left">symbol</td>
<td class="org-left">string</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
</tr>


<tr>
<td class="org-left">character</td>
<td class="org-left">string</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
<td class="org-left">string</td>
</tr>


<tr>
<td class="org-left">list</td>
<td class="org-left">array</td>
<td class="org-left">array</td>
<td class="org-left">array</td>
<td class="org-left">array in a string</td>
<td class="org-left">object</td>
</tr>


<tr>
<td class="org-left">plist</td>
<td class="org-left">array</td>
<td class="org-left">array</td>
<td class="org-left">array (with encode), object (with encode-plist)</td>
<td class="org-left">array in a string e.g "[\\"a\\",\\"b\\"]"</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">alist</td>
<td class="org-left">object</td>
<td class="org-left">array</td>
<td class="org-left">object (with encode-alist)</td>
<td class="org-left">string enclosing nested arrays</td>
<td class="org-left">Error</td>
</tr>


<tr>
<td class="org-left">other sequence</td>
<td class="org-left">array</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">array</td>
<td class="org-left">array</td>
<td class="org-left">N/A</td>
<td class="org-left">array</td>
<td class="org-left">N/A</td>
<td class="org-left">array</td>
</tr>


<tr>
<td class="org-left">hash-table</td>
<td class="org-left">object</td>
<td class="org-left">object</td>
<td class="org-left">object</td>
<td class="org-left">N/A</td>
<td class="org-left">object</td>
</tr>


<tr>
<td class="org-left">standard-object</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">object</td>
<td class="org-left">object</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">N/A</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

Note: Additional specialist functions available

-   (cl-json:encode-json-to-string x)
-   (cl-json:encode-json-alist x)
-   (cl-json:encode-json-alist-to-string x)
-   (cl-json:encode-json-plist x)
-   (cl-json:encode-json-plist-to-string x)
-   (st-json:write-json-to-string x stream)


<a id="c3772dc5-64ce-4174-b2f9-fa13accf1071"></a>

## Mapping library functions against each other

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">com.gigamonkeys.json</td>
<td class="org-left">Description</td>
</tr>


<tr>
<td class="org-left">encode-json</td>
<td class="org-left">write-json</td>
<td class="org-left">encode</td>
<td class="org-left">to-json</td>
<td class="org-left">write-json</td>
<td class="org-left">Encode from lisp to json object. These are generic functions so you can write your own for particular objects. jsown:to-json will return an object if you wrap the data in '(:OBJ data-here).</td>
</tr>


<tr>
<td class="org-left">encode-json-to-string</td>
<td class="org-left">write-json-to-string</td>
<td class="org-left">to-json</td>
<td class="org-left">json</td>
<td class="org-left">Encode to json object as string (yason needs to wrap with write-to-string macro. jsown:to-json normally writes to string.</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">to-json\*</td>
<td class="org-left">Non-generic variation of to-json. Not as smart, but faster.</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">encode-json-alist</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-alist</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Write the JSON representation (Object) of ALIST to STREAM (or to **JSON-OUTPUT**). Return NIL.</td>
</tr>


<tr>
<td class="org-left">encode-json-alist-to-string</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Return the JSON representation (Object) of ALIST as a string.</td>
</tr>


<tr>
<td class="org-left">encode-json-plist</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-plist</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Write the JSON representation (Object) of PLIST to STREAM (or to **JSON-OUTPUT**). Return NIL.</td>
</tr>


<tr>
<td class="org-left">encode-json-plist-to-string</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Return the JSON representation (Object) of PLIST as a string.</td>
</tr>


<tr>
<td class="org-left">encode-json</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-object</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Encode OBJECT, presumably a CLOS object as a JSON object, invoking the ENCODE-SLOTS method as appropriate. Cl-jason's basic encode-json function can handle objects. None of the other libraries basic encoding function will handle objects. Yason has this special function to encode objects.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-slots</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Generic function to encode objects. Every class in a hierarchy implements a method for ENCODE-OBJECT that serializes its slots. It is a PROGN generic function so that for a given instance, all slots are serialized by invoking the ENCODE-OBJECT method for all classes that it inherits from.</td>
</tr>


<tr>
<td class="org-left">encode-object-member</td>
<td class="org-left">write-json-element</td>
<td class="org-left">encode-object-element</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(cl-json) Encode KEY and VALUE as a Member pair of the innermost JSON Object opened with WITH-OBJECT in the dynamic context. KEY and VALUE are encoded using the ENCODE-JSON generic function, so they both must be of a type for which an ENCODE-JSON method is defined. If KEY does not encode to a String, its JSON representation (as a string) is encoded over again. (st-json) Method used for writing values of a specific type. You can specialise this for your own types. (yason) Encode KEY and VALUE as object element to the last JSON object opened with WITH-OBJECT in the dynamic context. KEY and VALUE are encoded using the ENCODE generic function, so they both must be of a type for which an ENCODE method is defined.</td>
</tr>


<tr>
<td class="org-left">encode-array-member</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-array-element</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(cl-json) Encode OBJECT as the next Member of the innermost JSON Array opened with WITH-ARRAY in the dynamic context. OBJECT is encoded using the ENCODE-JSON generic function, so it must be of a type for which an ENCODE-JSON method is defined. (yason) Encode OBJECT as next array element to the last JSON array opened with WITH-ARRAY in the dynamic context. OBJECT is encoded using the ENCODE generic function, so it must be of a type for which an ENCODE method is defined.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">encode-array-elements</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Encode OBJECTS, a list of JSON encodable objects, as array elements.</td>
</tr>


<tr>
<td class="org-left">decode-json</td>
<td class="org-left">read-json</td>
<td class="org-left">parse</td>
<td class="org-left">parse</td>
<td class="org-left">parse-json</td>
<td class="org-left">Decode from json object (generic functions)</td>
</tr>


<tr>
<td class="org-left">decode-json-from-string</td>
<td class="org-left">read-json-from-string</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Decodes from a string (so all strings internal to the json object must be escaped.</td>
</tr>


<tr>
<td class="org-left">decode-json-from-source</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">allows pulling from string, stream or file</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">read-json-as-type</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Read a JSON value and assert the result to be of a given type. Raises a json-type-error when the type is wrong.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">parse-with-container</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Parses the keywords which have been specified in the container from the json string json-string. For most cases you can just use the parse function without a special key container. This is only here to support some cases where the building of the key container takes too much time. See #'parse for the normal variant. See #'build-key-container for a way to build new keyword containers.</td>
</tr>


<tr>
<td class="org-left">json-bool</td>
<td class="org-left">as-json-bool</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Convert a generalized boolean to a :true/:false keyword</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">from-json-bool</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Convert :true/:false to lisp boolean</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">getjso</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">val</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">fetch value from a json object (st-json) or (jsown)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">(setf getjso)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(setf val)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">store value in an st-json:json object</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">mapjso</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">iterate over key/value pairs of json object</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">filter</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">function allows you to traverse a tree of json objects and interpret its results.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">do-json-keys</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Macro allowing you to traverse over all keywords</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">keywords</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">see which keywords have been defined in a parsed json object</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">null</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Return true if OBJECT is a NULL, and NIL otherwise</td>
</tr>


<tr>
<td class="org-left">json-intern</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Not-safe: Intern STRING in the current JSON-SYMBOLS-PACKAGE.</td>
</tr>


<tr>
<td class="org-left">json-or-null</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Returns a non-nil value as itself, or a nil value as a json null-value</td>
</tr>


<tr>
<td class="org-left">stream-array-member-encoder</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Return a function which takes an argument and encodes it to STREAM as a Member of an Array. The encoding function is taken from the value of ENCODER (default is #'ENCODE-JSON).</td>
</tr>


<tr>
<td class="org-left">stream-object-member-encoder</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Return a function which takes two arguments and encodes them to STREAM as a Member of an Object (String : Value pair).</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">build-key-container</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Builds an internal structure to speed up the keywords which you can read. This should be used when the keywords needed are not known at compile time, but you still want to parse those keywords of a lot of documents. If the keywords you are interested in are known at compile time, the use of #'parse will automatically expand the keywords at compile time. parse-with-container takes the result of this function and will return the keywords which have been inserted here.</td>
</tr>


<tr>
<td class="org-left">set-decoder-simple-list-semantics</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Set the decoder semantics to the following: \* Strings and Numbers are decoded naturally, reals becoming floats. \* The literal name true is decoded to T, false and null to NIL. \* Arrays are decoded to sequences of the type **JSON-ARRAY-TYPE**. \* Objects are decoded to alists. Object keys are converted by the function **JSON-IDENTIFIER-NAME-TO-LISP** and then interned in the package **JSON-SYMBOLS-PACKAGE**.</td>
</tr>


<tr>
<td class="org-left">set-decoder-simple-clos-semantics</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Set the decoder semantics to the following: \* Strings and Numbers are decoded naturally, reals becoming floats. \* The literal name true is decoded to T, false and null to NIL. \* Arrays are decoded to sequences of the type **JSON-ARRAY-TYPE**. \* Objects are decoded to CLOS objects. Object keys are converted by the function **JSON-IDENTIFIER-NAME-TO-LISP**. If a JSON Object has a field whose key matches **PROTOTYPE-NAME**, the class of the CLOS object and the package wherein to intern slot names are inferred from the corresponding value which must be a valid prototype. Otherwise, a FLUID-OBJECT is constructed whose slot names are interned in **JSON-SYMBOLS-PACKAGE**.</td>
</tr>


<tr>
<td class="org-left">safe-json-intern</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Only allows symbols that already exist</td>
</tr>


<tr>
<td class="org-left">unencodable-value-error</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Signal an UNENCODABLE-VALUE-ERROR.</td>
</tr>


<tr>
<td class="org-left">json-syntax-error</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Signal a JSON-SYNTAX-ERROR condition.</td>
</tr>


<tr>
<td class="org-left">lisp-to-camel-case</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Take a string with Lisp-style hyphentation and convert it to camel case. This is an inverse of CAMEL-CASE-TO-LISP.</td>
</tr>


<tr>
<td class="org-left">camel-case-to-lisp</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Take a camel-case string and convert it into a string with Lisp-style hyphenation.</td>
</tr>
</tbody>
</table>

1.  Encoding a hash-table

    Assume hash tables with the following key/values

    -   hash1 'foo:'quux 'bar:23
    -   hash2 :foo:quux 'bar:23
    -   hash3 :foo:"jeff" 'bar:23

    Running the basic encoding functions from the libraries across these hashes generated the following results:

    <table border="2" rules="all" frame="border">


    <colgroup>
    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left">Hash</td>
    <td class="org-left">cl-json</td>
    <td class="org-left">st-json</td>
    <td class="org-left">yason</td>
    <td class="org-left">jsown</td>
    <td class="org-left">com.gigamonkeys.json</td>
    </tr>


    <tr>
    <td class="org-left">hash1</td>
    <td class="org-left">{"foo":"quux","bar":23}</td>
    <td class="org-left">CASE-FAILURE expected-type:</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">Only keywords allowed in JSON-EXPs</td>
    </tr>


    <tr>
    <td class="org-left">hash2</td>
    <td class="org-left">{"foo":"quux","bar":23}</td>
    <td class="org-left">CASE-FAILURE expected-type:</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">Only keywords allowed in JSON-EXPs</td>
    </tr>


    <tr>
    <td class="org-left">hash3</td>
    <td class="org-left">{"foo":"jeff","bar":23}</td>
    <td class="org-left">CASE-FAILURE expected-type:</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">no applicable method</td>
    <td class="org-left">{"foo":"jeff","bar":23}</td>
    </tr>
    </tbody>
    </table>


<a id="8d97448d-ab64-4567-918b-4ee9c7d9156f"></a>

## Benchmarking


<a id="342127f9-ef96-4bf4-9d21-1ec2be4d1cec"></a>

### Encoding

The following were some simple benchmarking numbers comparing the five libraries. Tests were run on sbcl-1.10.1 running on a linux box. First, simple encoding functions.

    (defun test21 ()  "St-json encoding a hash table"
      (let ((*standard-output* (make-broadcast-stream)))
        (st-json:write-json  (alexandria:plist-hash-table
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
        (cl-json:encode-json-plist  '("sEcho" 1
                                      "iTotalRecords" 57 "iTotalDisplayRecords" 57
                                      "aaData" ("a" "b" ("d" "e" "f")
                                                ("alpha" "beta" "nuna")
                                                "c")))))

    (defun test32 () "cl-json encoding a list"
      (let ((*standard-output*
             (make-broadcast-stream)))
        (cl-json:encode-json  '("sEcho" 1
                                "iTotalRecords" 57 "iTotalDisplayRecords" 57
                                "aaData" ("a" "b" ("d" "e" "f")
                                          ("alpha" "beta" "nuna")
                                          "c")))))

    (defun test23 () "Yason encoding a plist"
      (let ((*standard-output* (make-broadcast-stream)))
        (yason:encode-plist  '("sEcho" 1 "iTotalRecords" 57
                               "iTotalDisplayRecords" 57
                               "aaData" ("a" "b" ("d" "e" "f")
                                         ("alpha" "beta" "nuna")
                                         "c")))))

    (defun test33 () "Yason encoding a list"
      (let ((*standard-output* (make-broadcast-stream)))
        (yason:encode  '("sEcho" 1 "iTotalRecords" 57
                         "iTotalDisplayRecords" 57
                         "aaData" ("a" "b" ("d" "e" "f")
                                   ("alpha" "beta" "nuna")
                                   "c")))))

    (defun test34 () "com.gigamonkeys.json encoding a list"
      (let ((*standard-output* (make-broadcast-stream)))
        (com.gigamonkeys.json:write-json  '("sEcho" 1 "iTotalRecords" 57
                         "iTotalDisplayRecords" 57
                         "aaData" ("a" "b" ("d" "e" "f")
                                   ("alpha" "beta" "nuna")
                                   "c")))))

    (defun test35 () "jsown.to-json encoding a list"
      (let ((*standard-output* (make-broadcast-stream)))
        (jsown:to-json  '("sEcho" 1 "iTotalRecords" 57
                         "iTotalDisplayRecords" 57
                         "aaData" ("a" "b" ("d" "e" "f")
                                   ("alpha" "beta" "nuna")
                                   "c")))))

    (defun test35a () "jsown.to-json encoding a list"
      (let ((*standard-output* (make-broadcast-stream)))
        (jsown:to-json*  '("sEcho" 1 "iTotalRecords" 57
                         "iTotalDisplayRecords" 57
                         "aaData" ("a" "b" ("d" "e" "f")
                                   ("alpha" "beta" "nuna")
                                   "c")))))

Running and timing these functions 300,000 times returned the following results.

<table>


<colgroup>
<col  class="org-left">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-right">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">Library</td>
<td class="org-right">cl-json-plist (22)</td>
<td class="org-right">cl-json list (32)</td>
<td class="org-right">st-json -hash (21)</td>
<td class="org-right">st-json -list (21a)</td>
<td class="org-right">yason -plist (23)</td>
<td class="org-right">yason -list (33)</td>
<td class="org-right">jsown:to-json (35)</td>
<td class="org-right">jsown:to-json\* (35a)</td>
<td class="org-left">com.gigamonkeys.json (34)</td>
</tr>


<tr>
<td class="org-left">Time (secs)</td>
<td class="org-right">6.902</td>
<td class="org-right">5.710</td>
<td class="org-right">2.140</td>
<td class="org-right">1.551</td>
<td class="org-right">3.220</td>
<td class="org-right">3.305</td>
<td class="org-right">9.556</td>
<td class="org-right">2.970</td>
<td class="org-left">Error</td>
</tr>
</tbody>
</table>

More details:


    "cl-json-plist test22"
    Evaluation took:
      6.902 seconds of real time
      6.896666 seconds of total run time (6.886666 user, 0.010000 system)
      [ Run times consist of 0.816 seconds GC time, and 6.081 seconds non-GC time. ]
      99.93% CPU
      14,725,840,848 processor cycles
      1,751,982,064 bytes consed


    "cl-json-list test32"
    Evaluation took:
      5.710 seconds of real time
      5.710000 seconds of total run time (5.640000 user, 0.070000 system)
      [ Run times consist of 0.549 seconds GC time, and 5.161 seconds non-GC time. ]
      100.00% CPU
      12,181,210,800 processor cycles
      1,137,593,472 bytes consed


    "st-json hash test21"
    Evaluation took:
      2.140 seconds of real time
      2.140000 seconds of total run time (2.136667 user, 0.003333 system)
      [ Run times consist of 0.147 seconds GC time, and 1.993 seconds non-GC time. ]
      100.00% CPU
      25 lambdas converted
      4,566,017,440 processor cycles
      415,136,352 bytes consed


    "st-json-list test21a"
    Evaluation took:
      1.551 seconds of real time
      1.543334 seconds of total run time (1.543334 user, 0.000000 system)
      [ Run times consist of 0.023 seconds GC time, and 1.521 seconds non-GC time. ]
      99.48% CPU
      3,307,883,976 processor cycles
      33,587,008 bytes consed


    "yason-plist test23"
    Evaluation took:
      3.220 seconds of real time
      3.220000 seconds of total run time (3.220000 user, 0.000000 system)
      100.00% CPU
      6,870,549,112 processor cycles
      33,659,232 bytes consed


    "yason-list test33"
    Evaluation took:
      3.305 seconds of real time
      3.306666 seconds of total run time (3.306666 user, 0.000000 system)
      [ Run times consist of 0.026 seconds GC time, and 3.281 seconds non-GC time. ]
      100.06% CPU
      7,051,552,040 processor cycles
      33,654,592 bytes consed

    "jsown test35 with generic to-json"
    Evaluation took:
      9.556 seconds of real time
      9.543332 seconds of total run time (9.413332 user, 0.130000 system)
      [ Run times consist of 1.372 seconds GC time, and 8.172 seconds non-GC time. ]
      99.86% CPU
      20,385,337,800 processor cycles
      3,628,799,200 bytes consed


    "jsown test35a with non-generic to-json*"
    Evaluation took:
      2.970 seconds of real time
      2.970000 seconds of total run time (2.963333 user, 0.006667 system)
      [ Run times consist of 0.166 seconds GC time, and 2.804 seconds non-GC time. ]
      100.00% CPU
      6,336,518,168 processor cycles
      417,597,104 bytes consed


    "com.gigamonkeys.json test34"
    Error: can't stringify (alpha beta nuna)


<a id="7492f7a8-2d64-4bff-b0e8-c04fa0aceb7f"></a>

## Decoding

Before getting to the benchmarks themselves, consider the following partial results from the decoding. You will get different lisp data-types from the decoding. Also, while st-json, yason, cl-json and com.gigamonkeys.json all were symmetrical (decoding after encoding would return you to the original, that is deliberately not the case with jsown.

    (first (test-decode-s)) ; st-json
    #S(ST-JSON:JSO :ALIST (("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69)))

    (first (test-decode-c)) ; cl-json
    ((:ID . 11) (:NAME . "Ireland") (:REGION-ID . 4) (:LATITUDE . 53) (:LONGITUDE . -8) (:ISO . "IE") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07")
    (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 4588252) (:GDP . 205) (:CORRUPTION-INDEX . 69))

    (alexandria:hash-table-alist (aref (yason:parse *encoded-countries*) 0))  ; yason
    (("corruptionIndex" . 69) ("gdp" . 205) ("population" . 4588252) ("currencyNumericCode" . 978) ("currencyAlphabetCode" . "EUR") ("currency" . "Euro") ("updatedAt" . "2005-09-11 00:15:40-07") ("permissionId" . 1) ("iso" . "IE") ("longitude" . -8) ("latitude" . 53) ("regionId" . 4) ("name" . "Ireland") ("id" . 11))

    (first (test-decode-j)) ; jsown
    (:OBJ ("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69))

    (aref (test-decode-gm) 0) ; com.gigamonkeys.json.parser
    ("id" 11 "name" "Ireland" "regionId" 4 "latitude" 53 "longitude" -8 "iso" "IE" "permissionId" 1 "updatedAt" "2005-09-11 00:15:40-07" "currency" "Euro" "currencyAlphabetCode" "EUR" "currencyNumericCode" 978 "population" 4588252 "gdp" 205 "corruptionIndex" 69)

Running a decoding function against a cl-json encoded dump of **countries** for 100 runs had the following results:

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">Library</td>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">com.gigamonkeys.json</td>
</tr>


<tr>
<td class="org-left">Time</td>
<td class="org-left">6.573 sec</td>
<td class="org-left">2.346 sec</td>
<td class="org-left">3.184 sec</td>
<td class="org-left">0.308 sec</td>
<td class="org-left">2.051 sec.</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

Running a decoding function against a large nested json object for 200 runs had the following results:

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">Library</td>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">com.gigamonkeys.json</td>
</tr>


<tr>
<td class="org-left">Time</td>
<td class="org-left">4.043 sec</td>
<td class="org-left">1.669 sec</td>
<td class="org-left">2.063 sec</td>
<td class="org-left">0.268 sec</td>
<td class="org-left">1.535 sec.</td>
</tr>
</tbody>
</table>

Yes, those number for jsown are right. Almost an order of magnitude faster.
Countries Test


    "st-json-read-json"
    Evaluation took:
      2.346 seconds of real time
      2.343333 seconds of total run time (2.343333 user, 0.000000 system)
      [ Run times consist of 0.157 seconds GC time, and 2.187 seconds non-GC time. ]
      99.87% CPU
      5,002,895,672 processor cycles
      428,328,288 bytes consed

    "cl-json:decode-json-from-string"
    Evaluation took:
      6.573 seconds of real time
      6.566665 seconds of total run time (6.519999 user, 0.046666 system)
      [ Run times consist of 0.150 seconds GC time, and 6.417 seconds non-GC time. ]
      99.91% CPU
      14,022,642,800 processor cycles
      462,734,864 bytes consed

    "yason:parse"
    Evaluation took:
      3.184 seconds of real time
      3.183333 seconds of total run time (3.130000 user, 0.053333 system)
      [ Run times consist of 0.152 seconds GC time, and 3.032 seconds non-GC time. ]
      99.97% CPU
      29 lambdas converted
      6,793,157,192 processor cycles
      352,362,192 bytes consed

    "jsown:parse"
    Evaluation took:
      0.308 seconds of real time
      0.306666 seconds of total run time (0.306666 user, 0.000000 system)
      [ Run times consist of 0.020 seconds GC time, and 0.287 seconds non-GC time. ]
      99.68% CPU
      657,041,136 processor cycles
      49,472,800 bytes consed

    "com.gigamonkey.json:parse-json"
    Evaluation took:
      2.051 seconds of real time
      2.050000 seconds of total run time (2.040000 user, 0.010000 system)
      [ Run times consist of 0.140 seconds GC time, and 1.910 seconds non-GC time. ]
      99.95% CPU
      82 lambdas converted
      4,374,634,560 processor cycles
      329,166,160 bytes consed

    Nested Test


    "cl-json-decode-nested"
    Evaluation took:
      4.043 seconds of real time
      4.053333 seconds of total run time (4.053333 user, 0.000000 system)
      [ Run times consist of 0.077 seconds GC time, and 3.977 seconds non-GC time. ]
      100.25% CPU
      8,625,907,640 processor cycles
      283,397,296 bytes consed

    "st-json-decode-nested"
    Evaluation took:
      1.669 seconds of real time
      1.660000 seconds of total run time (1.633333 user, 0.026667 system)
      [ Run times consist of 0.087 seconds GC time, and 1.573 seconds non-GC time. ]
      99.46% CPU
      3,562,216,224 processor cycles
      239,431,488 bytes consed

    "yasson-decode-nested"
    Evaluation took:
      2.051 seconds of real time
      2.053333 seconds of total run time (2.050000 user, 0.003333 system)
      [ Run times consist of 0.054 seconds GC time, and 2.0000 seconds non-GC time. ]
      100.10% CPU
      4,375,103,664 processor cycles
      190,517,456 bytes consed

    "jsown-decode-nested"
    Evaluation took:
      0.268 seconds of real time
      0.270000 seconds of total run time (0.270000 user, 0.000000 system)
      [ Run times consist of 0.017 seconds GC time, and 0.253 seconds non-GC time. ]
      100.75% CPU
      573,028,112 processor cycles
      34,328,000 bytes consed

    "gigamonkey-decode-nested"
    Evaluation took:
      1.535 seconds of real time
      1.530000 seconds of total run time (1.420000 user, 0.110000 system)
      [ Run times consist of 0.354 seconds GC time, and 1.176 seconds non-GC time. ]
      99.67% CPU
      3,274,726,152 processor cycles
      168,174,768 bytes consed


<a id="733af192-a0ff-4f7d-bade-aea6dfec38bd"></a>

## cl-json


<a id="0360a723-90f3-45ae-806f-5e0b8aba6645"></a>

### Encoding

Basic encoding functionality is provided by the generic function encode-json. This can be customised with an entire series of macros as listed in the documentation. Some examples follow:

1.  list



        (cl-json:encode-json '("a" "alpha" "b" "beta"))
        ["a","alpha","b","beta"]

        (cl-json:encode-json *country-a*)
        {"id":7,"name":"Finland","regionId":108,"latitude":64,"longitude":26,"iso":"FI","permissionId":1,"updatedAt":"2005-09-11 00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":5430670,"gdp":247,"corruptionIndex":90}

        (cl-json:encode-json *country-p*)
        ["id",7,"name","Finland","regionId",108,"latitude",64,"longitude",26,"iso","FI","permissionId",1,"updatedAt","2005-09-11 00:15:40-07","currency","Euro","currencyAlphabetCode","EUR","currencyNumericCode",978,"population",5430670,"gdp",247,"corruptionIndex",90]

    Notice how cl-json's basic encoding function returns an object when handed an alist and returns an array when handed a plist. Now consider the behavior when handed a list of plists or list of alists:



        (cl-json:encode-json (list *country-p*))
        [["id",7,"name","Finland","regionId",108,"latitude",64,"longitude",26,"iso","FI","permissionId",1,"updatedAt","2005-09-11 00:15:40-07","currency","Euro","currencyAlphabetCode","EUR","currencyNumericCode",978,"population",5430670,"gdp",247,"corruptionIndex",90]]
        NIL
        (cl-json:encode-json (list *country-a*))
        [{"id":7,"name":"Finland","regionId":108,"latitude":64,"longitude":26,"iso":"FI","permissionId":1,"updatedAt":"2005-09-11 00:15:40-07","currency":"Euro","currencyAlphabetCode":"EUR","currencyNumericCode":978,"population":5430670,"gdp":247,"corruptionIndex":90}]

    In both instances, cl-json:encode-json returned an array, but the list of plists returned an array of arrays and the list of alists returned an array of objects.

2.  alist

    Notice the difference between encoding-json-alist and encoding-json in the examples below



        (cl-json:encode-json-alist '((:a "alpha") ("b" "beta")))
        {"a":["alpha"],"b":["beta"]}

        (cl-json:encode-json '((:a "alpha") ("b" "beta")))
        [["a","alpha"],["b","beta"]]

    In the first specialized example, cl-json is told that this is an alist and returns a json object.
    In the second more general example, cl-json is not told that this is an alist and returns a json array.

3.  plist



        (cl-json:encode-json-plist '("a" "alpha" "b" "beta"))
        {"a":"alpha","b":"beta"}

4.  array

        (cl-json:encode-json (make-array 2 :initial-element "a" ))
        ["a","a"]

5.  object

    Now consider what happens if I just instantiate a clos object and hand it to cl-json to encode. Cl-json handles a simple clos object without any issues:


        (cl-json:encode-json (make-instance 'country :id 237 :name "Sealand" :region-id 12 :currency "bitcoin" :currency-alphabet-code "bc" :currency-numeric-code 278 :iso "sl" :population 8 :gdp 10 :corruption-index 32 :permission-id 232))

        {"id":237,"name":"Sealand","updatedAt":{"day":4929,"sec":18745,"nsec":564132000},"regionId":12,"latitude":0,"longitude":0,"iso":"sl","currency":"bitcoin","currencyAlphabetCode":"bc","currencyNumericCode":278,"population":8,"gdp":10,"corruptionIndex":32,"permissionId":232}


<a id="04c13e20-c243-4a4b-ba2a-e05ac838b199"></a>

### Encode-json-to-string

Simple example:



    (cl-json:encode-json-to-string
       (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'eq))

    "{\"foo\":1,\"bar\":[7,8,9]}"


<a id="29cad98b-04f6-4461-b277-7cd2dc2b08ee"></a>

### Decoding

Cl-json decoding generally just does what it says on the box.



    (with-input-from-string
        (s "{\"foo\": [1, 2, 3], \"bar\": true, \"baz\": \"!\"}")
                     (json:decode-json s))
    ((:FOO 1 2 3) (:BAR . T) (:BAZ . "!"))

You may need to pay attention to whether you are decoding a json object in the form of a string (decode-json-from-string x) or not in the form of a string (decode-json x). While cl-json normally returns json objects as arrays, you could tell it to return the json object as an object.

    (cl-json:set-decoder-simple-clos-semantics)

You can reset the decoder back to lists with the function:

    (set-decoder-simple-list-semantics)

Testing arrays embedded in arrays and arrays embedded in objects



    (defparameter *short-encoded-items-A*
    "[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")

    (cl-json:decode-json-from-string *short-encoded-items-A*)
    ("items"
     ((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 12) (:FLOAT . 19.2041)
      (:NAME . "Jennifer") (:SURNAME . "Snow") (:FULLNAME . "Andrew Vaughan")
      (:EMAIL . "sherri@ritchie.zw") (:BOOL))
     ((:INDEX . 2) (:INDEX--START--AT . 57) (:INTEGER . 14) (:FLOAT . 14.9888)
      (:NAME . "Alfred") (:SURNAME . "Pitts") (:FULLNAME . "Barry Weiner")
      (:EMAIL . "cheryl@craven.re") (:BOOL)))

Now for some invalid JSON code



    ; JSON array embedded in an array embedded in an object
    (defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")

    (cl-json:decode-json-from-string *short-encoded-items-B*)
    ; Evaluation aborted on #<JSON:JSON-SYNTAX-ERROR "Expected a key String in Object on JSON input ~
                        but found `~A'" {1011D14AC3}>.

Back to valid code



    ; JSON array embedded in array
    (defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")

    (cl-json:decode-json-from-string *short-encoded-items*)
    (("items"
      ((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 12) (:FLOAT . 19.2041)
       (:NAME . "Jennifer") (:SURNAME . "Snow") (:FULLNAME . "Andrew Vaughan")
       (:EMAIL . "sherri@ritchie.zw") (:BOOL))
      ((:INDEX . 2) (:INDEX--START--AT . 57) (:INTEGER . 14) (:FLOAT . 14.9888)
       (:NAME . "Alfred") (:SURNAME . "Pitts") (:FULLNAME . "Barry Weiner")
       (:EMAIL . "cheryl@craven.re") (:BOOL))))


<a id="1fa98b08-39f4-4a2f-abe9-4a7ea4ba7272"></a>

### Nested JSON Objects

Taking the nested json object that I stored in json4.txt, how could we
get an alist out of the innermost nested object? As a reminder, the
json object was as follows:

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

What can we do with it in cl-json? The basic decoding would result in this:



    (with-open-file
     (stream "/home/sabra/json-test/json4.txt")
     (cl-json:decode-json-from-string (slurp-stream4 stream)))
    ((:ITEMS ((:INDEX . 1) (:INDEX--START--AT . 56) (:INTEGER . 29) (:FLOAT . 16.8278) (:NAME . "Milton")
     (:SURNAME . "Jensen") (:FULLNAME . "Sheryl Winters") (:EMAIL . "denise@weiss.na") (:BOOL))))

This tells us that we are looking a series of nested lists. One way of
getting to the value associated with the keyword float would be
something like this:



    (let ((x (with-open-file (stream "/home/sabra/json-test/json4.txt")
                             (cl-json:decode-json-from-string
                              (slurp-stream4 stream)))))
      (cdr
       (assoc :FLOAT
              (second
               (assoc :ITEMS x)))))
    16.8278

Now consider a different way of doing this. Reset the decoding from lists to clos classes.



    (cl-json:set-decoder-simple-clos-semantics)

    Now look at this variation, trying to get the value associated with the keyword surname for this particular object:

    (let ((x (with-open-file (stream "/home/sabra/json-test/json4.txt")
                             (cl-json:decode-json-from-string
                              (slurp-stream4 stream)))))
      (slot-value
       (aref (slot-value  x ':items) 0)
       ':surname))
    "Jensen"

As you can tell, you still have to deal with the fact that there is an array in the middle between the objects. No one said json objects could not be messy.


<a id="cda26baa-e882-4ef8-8263-0981c3d0b334"></a>

### Other Information

1.  Error Conditions

    Cl-json has several error conditions, some of them recoverable and some of them not recoverable. These include "unrecoverable-value-error", "json-syntax-error", "no-char-for-code", "cell-error" "type-error", errors for calling functions in the wrong environment and others. Please read the user-manual for more details.

    Cl-json has a lot of other capabilities. The documentation is excellent and you should seriously consider the security considerations section of the user manual if you are going to be instantiating clos classes based on uncontrolled JSON objects.


<a id="4483b932-bdd4-4251-a86e-736a1c0ca8be"></a>

## st-json

ST-JSON ('ST' because it originated at Streamtech) is a Common Lisp library for encoding and decoding JSON values (as specified on json.org).


<a id="887758c0-6365-421c-a146-eb023f25fcf5"></a>

### Encoding

The basic encoding function is write-json.

1.  write-json

    1.  list


            (st-json:write-json  '("sEcho" 1 "iTotalRecords" 57
              "iTotalDisplayRecords" 57
            "f") ("alpha" "beta" "nuna") "c") ) *standard-output*)

            ["sEcho",1,"iTotalRecords",
              "aaData" ("a" "b" ("d" "e"
             57,"iTotalDisplayRecords",57,
            ,["alpha","beta","nuna"],"c"]]
            "aaData",["a","b",["d","e","f"
            ]

        Now consider the encoding using the **country-a** and **country-p** parameters listed in the beginning of this page:


            (st-json:write-json *country-p* *standard-output*)

            Error :ID fell through ECASE expression.

            (st-json:write-json *country-a* *standard-output*)

            Errror :ID fell through ECASE expression.

        Notice the errors when handed the **country-a** and **country-p** alist and plist parameters.

2.  alist



        (st-json:write-json '(("a" "alpha") ("b" "beta")) *standard-output*)
        [["a","alpha"],["b","beta"]]

    Consider the errors shown above under list

3.  plist

    Consider the errors shown above under list

4.  hash-table

    St-json can handle encoding hash tables. As noted in the examples below, it will normally result in writing out a json object

        (st-json:write-json (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal) *standard-output*)

        {"foo":1,"bar":[7,8,9]}

5.  write-json-to-string

    Similarly to cl-json, st-json has a function for writing json output to string as well as to a stream.

        (st-json:write-json-to-string
         (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal))

        "{\"foo\":1,\"bar\":[7,8,9]}"


<a id="e7e75f4c-a665-48d5-b83d-3d33b1d315fe"></a>

### Decoding

In decoding, st-json creates instances of a jso object "jso" which wraps an alist.

1.  read-json


        (let ((item (st-json:jso "a" "alph" "b" "beta")))
          (st-json:read-json (st-json:write-json-to-string  item)))

        #S(ST-JSON:JSO :ALIST (("a" . "alph") ("b" . "beta")))

    Testing arrays in arrays and arrays in arrays in objects

        (defparameter *short-encoded-items-A*
        "[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")

        (st-json:read-json-from-string *short-encoded-items-A*)
        ("items"
         #S(ST-JSON:JSO
            :ALIST (("index" . 1) ("index_start_at" . 56) ("integer" . 12)
                    ("float" . 19.2041) ("name" . "Jennifer") ("surname" . "Snow")
                    ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw")
                    ("bool" . :NULL)))
         #S(ST-JSON:JSO
            :ALIST (("index" . 2) ("index_start_at" . 57) ("integer" . 14)
                    ("float" . 14.9888) ("name" . "Alfred") ("surname" . "Pitts")
                    ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re")
                    ("bool" . :NULL))))

    Now for some invalid JSON code
    ; JSON array embedded in an array embedded in an object

        (defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")

        (st-json:read-json-from-string *short-encoded-items-B*)
        ; Evaluation aborted on #<ST-JSON:JSON-PARSE-ERROR "Invalid slot name in object literal: ~A" {10116DA673}>.

    Back to valid code
    ; JSON array embedded in array

        (defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")

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


<a id="5769e4e4-d5b0-423e-9bfb-c334b23a7476"></a>

### Other Information

1.  Error Conditions.

    St-json provides error conditions for json-type-error, json-parse error, json-error and json-eof-error. These are undocumented, so you will have to look at the source code for how to use them.

2.  as-json-bool

    <table>


    <colgroup>
    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left">lisp</td>
    <td class="org-left">t</td>
    <td class="org-left">nil</td>
    <td class="org-left">()</td>
    <td class="org-left">:NULL</td>
    <td class="org-left">(make-array 5 :initial-element nil)</td>
    </tr>


    <tr>
    <td class="org-left">json</td>
    <td class="org-left">:TRUE</td>
    <td class="org-left">:FALSE</td>
    <td class="org-left">:FALSE</td>
    <td class="org-left">:TRUE</td>
    <td class="org-left">:TRUE</td>
    </tr>
    </tbody>
    </table>

3.  from-json-bool

    <table>


    <colgroup>
    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">

    <col  class="org-left">
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left">json</td>
    <td class="org-left">:TRUE</td>
    <td class="org-left">:FALSE</td>
    <td class="org-left">NULL</td>
    </tr>


    <tr>
    <td class="org-left">lisp</td>
    <td class="org-left">t</td>
    <td class="org-left">nil</td>
    <td class="org-left">:NULL</td>
    </tr>
    </tbody>
    </table>

4.  jso



        (st-json:jso "a" "alph" "b" "beta")
        #S(ST-JSON:JSO :ALIST (("a" . "alph") ("b" . "beta")))

        (let ((item (st-json:jso "a" "alph" "b" "beta"))) (format t "~a" item))

        (#S(ST-JSON:JSO :ALIST ((a . alph) (b . beta)))

5.  getjso

        (let ((item (st-json:jso "a" "alph" "b" "beta"))) (st-json:getjso "a" item))
        "alph"
        T

6.  mapjso

        (let ((item (st-json:jso "a" "alph" "b" "beta")))
              (st-json:mapjso #'(lambda (x y) (format t " key: ~a :value ~a" x y))  item))
         key: a :value alph key: b :value beta


<a id="861f43e0-6742-453a-8068-d0f98ffc0e6b"></a>

### Nested JSON Objects

Now consider the nested json object which I referred to in the data section. As a reminder, it looks like this.



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

What can we do with it in st-json? First, let's try to get the value where the key is "integer". In order to do that, you need to descend into the json tree. The following code is not what you would use, but gets the point across.

1.  GETJSO



        (st-json:getjso "integer"
                         (car
                          (st-json:getjso* "items"
                                           (with-open-file
                                            (stream "/home/sabra/json-test/json4.txt")
                                            (st-json:read-json
                                             (slurp-stream4 stream))))))
        29

2.  GETJSO\*

    The getjso\* function in theory allows you to take a key in the form of
    "a.b.c" and st-json will generate a series of getjso calls to go down
    each level and return the value for key c. This, however, does not
    seem to work in the above piece of json data because read-json does
    not result in jso objects all the way down. What we have is a jso
    object which wraps a cons which wraps a jso object. As a result, the
    intermediary cons prevents getjso\* from walking down the nested list.


<a id="f4006ee2-82d8-4693-8dc6-8b0b7e76013a"></a>

## Yason

From the author: "the major difference between YASON and the other
JSON libraries that were available when I wrote it is that YASON does
not require the user to come up with a Lisp data structure that
reflects the JSON data structure that should be generated.  Rather, I
wanted a way to generate JSON directly from my internal data
structures.

The reason for that desire was that I had to generate different JSON
format in different contexts.  That is, a class instance would
sometimes be generated including all its attributes, sometimes just
with a select set of attributes and sometimes as a reference.  Thus,
there was no right way to render an object as JSON, and I found the
approach to first generate a data structure that would then be
rendered as JSON to be wasteful and, as CL has no literal syntax for
hash tables, ugly.

Instead of going through an intermediate data structure, YASON allows
you to encode to a JSON stream on the fly
(<http://common-lisp.net/project/yason/#stream-encoder>). "


<a id="743bc712-4e34-46ce-a15d-cb654ea37e61"></a>

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

    Whereas yason's basic encoding function choked on the alist, yason's encode-alist function works.

        (yason:encode-alist '((:a "alpha") ("b" "beta")))


        {"A":["alpha"],"b":["beta"]}((:A "alpha") ("b" "beta"))


        (yason:encode-alist '((:a . "alpha") ("b" "beta")))


        {"A":"alpha","b":["beta"]}
        ((:A . "alpha") ("b" "beta"))
        (yason:encode-alist *country-a*)
        {"ID":7,"NAME":"Finland","REGION-ID":108,"LATITUDE":64,"LONGITUDE":26,"ISO":"FI","PERMISSION-ID":1,"UPDATED-AT":"2005-09-11 00:15:40-07","CURRENCY":"Euro","CURRENCY-ALPHABET-CODE":"EUR","CURRENCY-NUMERIC-CODE":978,"POPULATION":5430670,"GDP":247,"CORRUPTION-INDEX":90}

        ((:ID . 7) (:NAME . "Finland") (:REGION-ID . 108) (:LATITUDE . 64) (:LONGITUDE . 26) (:ISO . "FI") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 5430670) (:GDP . 247) (:CORRUPTION-INDEX . 90))

3.  plist

    Whereas yason's basic encoding function choked on the plist, yason's encode-plist function works on a plist.

        (yason:encode-plist  '("sEcho" 1 "iTotalRecords" 57
                                   "iTotalDisplayRecords" 57
                                   "aaData" ("a" "b" ("d" "e" "f")
                                             ("alpha" "beta" "nuna")
                                             "c")))
        Records":57,"aaData":["a","b",["d","e","f"],["alpha","beta","nuna"],"c"]}
        {"sEcho":1,"iTotalRecords":57,"iTotalDispla
        y
        ("sEcho" 1 "iTotalRecords" 57 "iTotalDisplayRecords" 57 "aaData" ("a" "b" ("d" "e" "f") ("alpha" "beta" "nuna") "c"))
        (yason:encode-plist *country-p*)
        {"ID":7,"NAME":"Finland","REGION-ID":108,"LATITUDE":64,"LONGITUDE":26,"ISO":"FI","PERMISSION-ID":1,"UPDATED-AT":"2005-09-11 00:15:40-07","CURRENCY":"Euro","CURRENCY-ALPHABET-CODE":"EUR","CURRENCY-NUMERIC-CODE":978,"POPULATION":5430670,"GDP":247,"CORRUPTION-INDEX":90}
        (:ID 7 :NAME "Finland" :REGION-ID 108 :LATITUDE 64 :LONGITUDE 26 :ISO "FI" :PERMISSION-ID 1 :UPDATED-AT "2005-09-11 00:15:40-07" :CURRENCY "Euro" :CURRENCY-ALPHABET-CODE "EUR" :CURRENCY-NUMERIC-CODE 978 :POPULATION 5430670 :GDP 247 :CORRUPTION-INDEX 90)

4.  array

    Yason's basic encoding function will return a JSON array.

        (yason:encode (make-array 2 :initial-element "a" ))
        ["a","a"]
        #("a" "a")

5.  hash-table

    Yason can also encode a hash-table, which it will return a json object:

        (yason:encode (alexandria:plist-hash-table '("foo" 1 "bar" (7 8 9)) :test #'equal))
        {"foo":1,"bar":[7,8,9]}
        #<HASH-TABLE :TEST EQUAL :COUNT 2 {100965BA03}>

6.  object

    For objects, you will have to write your own method to extend yason to encode objects.
    output-to-string macro

    Unlike cl-json and st-json which provide functions specifically for
    encoding to string (encode-json-to-string and write-json-to-string,
    respectively), yason relies on an output-to-string macro. I have to
    admit I had difficulty getting the right syntax on yason's own
    with-output-to-string\* macro working properly.

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


<a id="f3541c99-2640-45bc-b376-e9fa8599e613"></a>

### Decoding

Yason uses its generic function parse to generate a hash-table of the
received json-object. It is possible to set the special variable
 **parse-object-as** :hash-table, :plist or :alist to specify the data
structure that objects are parsed into. The default is :hash-table. It
is also possible to set the special variable
 **parse-json-arrays-as-vectors** to t, in which case the json arrays
will be parsed as vectors and not as lists.In the benchmark test where
it received an array of country json objects, yason parsed the array
into an array of hash-tables. Thus, where the original entry encoded
into json took the form of:

    ((:ID . 7) (:NAME . "Finland") (:REGION-ID . 108) (:LATITUDE . 64) (:LONGITUDE . 26) (:ISO . "FI") (:PERMISSION-ID . 1) (:UPDATED-AT . "2005-09-11 00:15:40-07") (:CURRENCY . "Euro") (:CURRENCY-ALPHABET-CODE . "EUR") (:CURRENCY-NUMERIC-CODE . 978) (:POPULATION . 5430670) (:GDP . 247) (:CORRUPTION-INDEX . 90))

To get back to the same alist format would require something akin to:

    (nreverse (alexandria:hash-table-alist (aref (yason:parse *encoded-countries*) 0)))

resulting in a form such as:

    (("id" . 11) ("name" . "Ireland") ("regionId" . 4) ("latitude" . 53) ("longitude" . -8) ("iso" . "IE") ("permissionId" . 1) ("updatedAt" . "2005-09-11 00:15:40-07") ("currency" . "Euro") ("currencyAlphabetCode" . "EUR") ("currencyNumericCode" . 978) ("population" . 4588252) ("gdp" . 205) ("corruptionIndex" . 69))

The following are decoding tests on JSON arrays, arrays in arrays and embedded arrays in an object and the results. I am using the additional yason parameters to show the alist representation instead of a hash-table and to return JSON arrays as vectors rather than lists.
; JSON array


    (defparameter *short-encoded-items-A*
    "[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]")

    (yason:parse *short-encoded-items-A* :json-arrays-as-vectors t :object-as :alist)
    #("items"
      (("bool") ("email" . "sherri@ritchie.zw") ("fullname" . "Andrew Vaughan")
       ("surname" . "Snow") ("name" . "Jennifer") ("float" . 19.2041)
       ("integer" . 12) ("index_start_at" . 56) ("index" . 1))
      (("bool") ("email" . "cheryl@craven.re") ("fullname" . "Barry Weiner")
       ("surname" . "Pitts") ("name" . "Alfred") ("float" . 14.9888)
       ("integer" . 14) ("index_start_at" . 57) ("index" . 2)))

Now for some invalid JSON code

; JSON array embedded in an array embedded in an object

    (defparameter *short-encoded-items-B* "{[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]}")

    (yason:parse *short-encoded-items-B* :json-arrays-as-vectors t :object-as :alist)
    ; Evaluation aborted on #<YASON::EXPECTED-COLON {1010C45823}>.

Back to valid code
; JSON array embedded in array


    (defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")

    (yason:parse *short-encoded-items* :json-arrays-as-vectors t :object-as :alist)
    #(#("items"
        (("bool") ("email" . "sherri@ritchie.zw") ("fullname" . o"Andrew Vaughan")
         ("surname" . "Snow") ("name" . "Jennifer") ("float" . 19.2041)
         ("integer" . 12) ("index_start_at" . 56) ("index" . 1))
        (("bool") ("email" . "cheryl@craven.re") ("fullname" . "Barry Weiner")
         ("surname" . "Pitts") ("name" . "Alfred") ("float" . 14.9888)
         ("integer" . 14) ("index_start_at" . 57) ("index" . 2))))


<a id="32399c86-31cf-4443-a0a9-15b0af75532b"></a>

### Nested JSON Objects

Taking the nested JSON object that I stored in json4.txt, how could we get an alist out of the innermost nested object? As a reminder, the JSON object was as follows:

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

What can we do with it in yason? Remember that it is a JSON object which keyword "items" contains an array which contains a JSON object.
Remembering that yason decodes objects as hash-tables, I can see how to get there. Just using the following ugly code just for sake of example (your code will look better than this):

    (alexandria:hash-table-alist
     (second
      (first
       (alexandria:hash-table-alist
        (with-open-file (stream "/home/sabra/json-test/json4.txt")
                        (yason:parse (slurp-stream4 stream)))))))
    (("bool") ("email" . "denise@weiss.na") ("fullname" . "Sheryl Winters") ("surname" . "Jensen") ("name" . "Milton") ("float" . 16.8278) ("integer" . 29) ("index_start_at" . 56) ("index" . 1))

The point is that you need to know your data structure so that you can figure out how to walk the tree.

How would we get the value of the key "integer"? We can descend the json tree in this particular example something like this:

    (gethash "integer"
             (first
              (gethash "items"
                       (with-open-file
                        (stream "/home/sabra/json-test/json4.txt")
                        (yason:parse (slurp-stream4 stream))))))
    29


<a id="b0de07fe-0399-45a5-8fc9-1fc7c143d51b"></a>

### Other Information

1.  Error Conditions

    Yason has an error condition for "no-json-output-context".


<a id="70c41b15-eefe-420c-9e2c-1bbc92062fab"></a>

## Jsown

Jsown uses an MIT license. In decoding, it is fantastically fast. So much so that I first questioned whether it actually decoded anything.


<a id="5604415e-fc22-423d-a3d7-19ebc9520066"></a>

### Encoding

Jsown encodes to a string.

-   (to-json x) is a generic function which you can specialize on your own types. This allows you to nest lisp objects in a jsown object and serialize them in a suitable way.
-   (to-json\* x) is the non-generic function variant of the same thing. It isn't as smart, but it is faster. As you can see from the performance table above, jsown:to-json turned in an encoding time of 9.556 seconds while jsown:to-json\* turned in an encoding time of 2.97 seconds.

As noted in the table matching library functions against each other, json:to-json returns an array or returns an object depending on what you tell it to do. Thus:

    (jsown:to-json*  '(("a" "alpha") ("b" "beta")))

    "[[\"a\",\"alpha\"],[\"b\",\"beta\"]]"

    (jsown:to-json  '(:obj (("a" "alpha") ("b" "beta"))))

    "{[\"a\",\"alpha\"]:[[\"b\",\"beta\"]]}"

When to-json is called, jsown will internally call to-json each step of the way.  This has a performance downside, yet it seems to provide the least surprises in the output.  If you need more performance, jsown\* offers that, at the cost of flexibility."
As noted in the table matching library functions against each other, to-json is a generic function, allowing you to provide specific implementations for your own objects so those can easily be converted to JSON too (and it will ensure that nested objects are correctly transformed to json).

    (jsown:to-json '(:obj ("bingo" . 24.93) ("bang" 1 2 3 "foo" 4 5) ("foo" . "bar")))

    "{\"bingo\":24.93,\"bang\":[1,2,3,\"foo\",4,5],\"foo\":\"bar\"}"

When reading JSON objects, jsown converts their content to the most lispy translation of what was in there. As such, JSON’s false will be translated to nil, which coincidentally also be the translation of JSON’s []. When writing objects lisp’s nil is translated to the empty JSON list []. You can write JSON’s false by writing lisp’s keywords :false or :f.

     (jsown:to-json (jsown:new-js
                       ("items" nil)
                       ("falseIsEmptyList" :f)
                       ("success" t)))

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

If you are constructing json objects, consider using the jsown:js-new and jsowon:extend-js functions. jsown:js-new has a clean and clear interface for building content. It works together with jsown:extend-js if [you] need to split up the object creation.

The latest version of json has a setf-expander on (setf jsown:val) which automatically creates a jsown-object if no such object was available at the designated place.  An example should clarify:


    (let (doc)
      (setf (jsown:val (jsown:val (jsown:val doc "properties") "color") "paint") "red")
      (jsown:to-json doc))
    "{\"properties\":{\"color\":{\"paint\":\"red\"}}}"

It turns out to be a handy little feature when you need to build deeply nested json documents

1.  Decoding

    This is where jsown really shines. Jsown is by far the fastest decoder. It not only decodes, it also makes it easy to pull out elements of the json-object. For example, consider the **encoded-countries** special variable. Jsown can pull out the id and name of the first object in the encoded array thusly:

        (jsown:parse *encoded-countries* "id" "name")
        (:OBJ ("id" . 11) ("name" . "Ireland"))
        ; Just some decoding examples:

        (jsown:parse "{\"id\": \"null\"}")
        (:OBJ ("id" . "null"))

        (jsown:parse "[\"id\": \"null\"]")
        ("id")

    ; Where the JSON document is an array in a string:

        (defparameter *short-encoded-items-A* "[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]"

        (jsown:parse *short-encoded-items-A*)
        ("items"
         (:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 12)
          ("float" . 192041/10000) ("name" . "Jennifer") ("surname" . "Snow")
          ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw") ("bool"))
         (:OBJ ("index" . 2) ("index_start_at" . 57) ("integer" . 14)
          ("float" . 9368/625) ("name" . "Alfred") ("surname" . "Pitts")
          ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re") ("bool")))

    ; Where the JSON document is an arrays embedded in another array


        (defparameter *short-encoded-items* "[[\"items\",{\"index\":1,\"index_start_at\":56,\"integer\":12,\"float\":19.2041,\"name\":\"Jennifer\",\"surname\":\"Snow\",\"fullname\":\"Andrew Vaughan\",\"email\":\"sherri@ritchie.zw\",\"bool\":null},{\"index\":2,\"index_start_at\":57,\"integer\":14,\"float\":14.9888,\"name\":\"Alfred\",\"surname\":\"Pitts\",\"fullname\":\"Barry Weiner\",\"email\":\"cheryl@craven.re\",\"bool\":null}]]")

        (jsown:parse *short-encoded-items*)
        (("items"
          (:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 12)
           ("float" . 192041/10000) ("name" . "Jennifer") ("surname" . "Snow")
           ("fullname" . "Andrew Vaughan") ("email" . "sherri@ritchie.zw") ("bool"))
          (:OBJ ("index" . 2) ("index_start_at" . 57) ("integer" . 14)
           ("float" . 9368/625) ("name" . "Alfred") ("surname" . "Pitts")
           ("fullname" . "Barry Weiner") ("email" . "cheryl@craven.re") ("bool"))))


<a id="cfe19cf9-ae03-432e-a55b-5cfde40e4da4"></a>

### Nested JSON Objects

Now consider the nested JSON object which I keep referring to. Again, as a reminder, here is the object.

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

What can we do with it in jsown? It is a JSON object which keyword "items" contains an array which contains a JSON object. I've limited the dataset size simply for explanation purposes. Also note that several of the following functions require that we have parsed the JSON object first.

1.  PARSE

        (with-open-file (stream "/home/sabra/json-text/json4.txt") (jsown:parse (slurp-stream4 stream)))

        (:OBJ ("items" (:OBJ ("index" . 1) ("index_start_at" . 56) ("integer" . 29) ("float" . 84139/5000) ("name" . "Milton") ("surname" . "Jensen") ("fullname" . "Sheryl Winters") ("email" . "denise@weiss.na") ("bool"))))

    That was the generic parse. Now, if you add keywords to the parse function, you can get specific items back. This is easy to see in the case of an unested object:

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

    What happens if the data stream provides all values as string? Well, again, you need to know how your data source is providing the data and deal with it correctly.

2.  KEYWORDS

        (with-open-file (stream "/home/sabra/json-test/json4.txt") (jsown:keywords   (jsown:parse (slurp-stream4 stream))))

        ("items")

    (with-open-file (stream "/home/sabra/json-test/json4.txt") (jsown:do-json-keys (keyword value)  (jsown:parse (slurp-stream4 stream)) (format t "~A => ~A~%" keyword value)))

    items => ((OBJ (index . 1) (index<sub>start</sub><sub>at</sub> . 56) (integer . 29) (float . 84139/5000) (name . Milton) (surname . Jensen) (fullname . Sheryl Winters) (email . denise@weiss.na) (bool)))

    VAL
    jsown:val (jsown:parse "{\\"foo\\":\\"bar\\",\\"baz\\":100.25}" "baz") "baz")
    401/4

3.  DO-JSON-KEYS



    If you have a nested object/array/object JSON document and you want some value that is buried deep in the tree, then just like any other library you need to walk the tree and get it. It is not that difficult.

        (jsown:val
         (first
          (jsown:val
           (with-open-file (stream "/home/sabra/projects/lisp-tax-test/json4.txt")
                           (jsown:parse (slurp-stream4 stream)))
           "items"))
         "float")
        84139/5000


<a id="bc02ce38-5c93-4fac-b46a-8e455335b556"></a>

## Com.gigamonkeys.json

I have to admit I had more issues with this library in encoding complex lisp objects than with the other libraries. I have a lot of respect for Peter Seibel, the author, so I may have just been misusing the library.


<a id="ebc86a54-5992-425c-a5ea-3ba4a52d273f"></a>

### Encoding

(com.gigamonkeys.json:json x) ;The top-level function for converting Lisp objects into a string in the JSON format. It can convert any object that can be converted to a json-exp via the to-json generic function.

(com.gigamonkeys.json:write-json data &optional (stream **standard-output**)); Writes a lisp object to a stream.
As noted earlier, you need to test these functions against your datatypes. For example, trying to run com.gigamonkeys.json:write-json against a clos object resulted in a hang that required aborting the evaluation.


<a id="1700394f-cee7-4785-b622-17ec332005c0"></a>

### Decoding

(com.gigamonkeys.json:parse-json x) ; Parse json text into Lisp
objects. Hash tables are used to represent Javascript objects and
vectors to represent arrays.


<a id="3492fe0c-e37d-4875-806a-59ecf5283ded"></a>

## Further tests

<table border="2" rules="all" frame="border">


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">json</td>
<td class="org-left">cl-json</td>
<td class="org-left">st-json</td>
<td class="org-left">yason</td>
<td class="org-left">jsown</td>
<td class="org-left">com.gigamonkeys.json</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">""</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">"\\"\\""</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"not a value"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">nil</td>
<td class="org-left">error</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[]"</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
<td class="org-left">"\\"[]\\""</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[1]"</td>
<td class="org-left">(1)</td>
<td class="org-left">(1)</td>
<td class="org-left">(1)</td>
<td class="org-left">(1)</td>
<td class="org-left">"\\"[1]\\""</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[1.1]"</td>
<td class="org-left">(1.1)</td>
<td class="org-left">(1.1)</td>
<td class="org-left">(1.1)</td>
<td class="org-left">(11/10)</td>
<td class="org-left">"\\"[1.1]\\""</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[-1E4]"</td>
<td class="org-left">(-10000.0)</td>
<td class="org-left">(-10000.0)</td>
<td class="org-left">(-10000.0)</td>
<td class="org-left">(-10000)</td>
<td class="org-left">"\\"[-1E4]\\""</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[100.0e-2]"</td>
<td class="org-left">(1.0)</td>
<td class="org-left">(1.0)</td>
<td class="org-left">(1.0)</td>
<td class="org-left">(1)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[.5]"</td>
<td class="org-left">error</td>
<td class="org-left">(0.5)</td>
<td class="org-left">error</td>
<td class="org-left">(5)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[5.]"</td>
<td class="org-left">error</td>
<td class="org-left">(5)</td>
<td class="org-left">(5)</td>
<td class="org-left">error</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[.]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">nil</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[5..5]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">(\\</td>
<td class="org-left">5..5\\</td>
<td class="org-left">)</td>
<td class="org-left">error</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[10e]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">(\\</td>
<td class="org-left">10E\\</td>
<td class="org-left">)</td>
<td class="org-left">error</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[e10]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">(10)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[010e2]"</td>
<td class="org-left">error</td>
<td class="org-left">(1000.0)</td>
<td class="org-left">(1000.0)</td>
<td class="org-left">(1000)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"***comment***[1]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">nil</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"1 ***comment***"</td>
<td class="org-left">1</td>
<td class="org-left">error</td>
<td class="org-left">1</td>
<td class="org-left">1</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[0xFF]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[true]"</td>
<td class="org-left">(T)</td>
<td class="org-left">(T)</td>
<td class="org-left">(T)</td>
<td class="org-left">(T)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[TRUE]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">nil</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[null]"</td>
<td class="org-left">(NIL)</td>
<td class="org-left">(:NULL)</td>
<td class="org-left">(NIL</td>
<td class="org-left">(NIL)</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">"[NULL]"</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">error</td>
<td class="org-left">nil</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>
