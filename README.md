# Kit 
Kit encapsulates some gadgets.

# Interface
## Format Output
**printf** *(object &optional (stream *standard-output*) (indent 0)*
```
CL-USER> data
#<HASH-TABLE :TEST EQL :COUNT 2 {1004398943}>
CL-USER> (kit:printf data)
 {
  "a": (1 2 3 4 5 ), 
  "b": {},}
2
CL-USER> 
```

## Number
**parse-float** *(string)*
Parse floating-point numbers from strings.
```
CL-USER> (kit:parse-float "1.23")
1.23d0
CL-USER> (kit:parse-float "1.2322 ")
1.2322d0
CL-USER> (kit:parse-float "1.2322.123 ")
1.2322d0
CL-USER> 
```

**digit-string-p** *(string)*
Determine whether a string is composed entirely of numbers.
```
CL-USER> (kit:digit-string-p "1a.asda")
NIL
CL-USER> (kit:digit-string-p "112312312")
T
CL-USER> (kit:digit-string-p "112312312.23")
NIL
CL-USER> 
```

**integer-to-bits** *(integer)*
Numbers are converted to binary.
```
CL-USER> (kit:digit-string-p "1a.asda")
NIL
CL-USER> (kit:digit-string-p "112312312")
T
CL-USER> (kit:digit-string-p "112312312.23")
NIL
CL-USER> 
```

**bits-to-integer** *(bit-vector)*
Convert binary to number.
```
CL-USER> (kit:bits-to-integer (kit:integer-to-bits 1))
1
CL-USER> (kit:bits-to-integer (kit:integer-to-bits 1123123))
1123123
CL-USER> 
```

**integer-to-bytes** *(integer length &key (order :big) (signed nil))*
Number to byte array.
```
CL-USER> (kit:integer-to-bytes 123 16)
#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 123)
CL-USER> (kit:integer-to-bytes 123 8)
#(0 0 0 0 0 0 0 123)
CL-USER> (kit:integer-to-bytes 1212313 8)
#(0 0 0 0 0 18 127 153)
CL-USER> (kit:integer-to-bytes 1212313 8 :order :little)
#(153 127 18 0 0 0 0 0)
CL-USER> 
```

**bytes-to-integer** *(array &key (order :big) (signed nil))*
Byte array to number.
```
CL-USER> (kit:bytes-to-integer (kit:integer-to-bytes 123 16))
123
CL-USER> 
```

##String
**string-join** *(sequences &key (sep ","))*
String splicing.
```
CL-USER> (kit:string-join '("a" "b" "c" "d"))
"a,b,c,d"
CL-USER> 
```

##Parse
**parse-ts** *(timestamp)*
Parsing timestamps generates the corresponding day stamps, hour stamps, minute stamps, hours, minutes, and seconds.
```
CL-USER> (kit:parse-ts 1702213775)
19701
472837
28370229
22
1269
76175
CL-USER> 
```

**parse-xml** *(filename &key skip-root-p auto-parse-type-p (auto-parse-type-func 'read-from-string))*
Parsing the XML file generates a hash structure.  
skip-root-p 是否跳过根节点  
auto-parse-type-p 是否自动解析属性类型  
auto-parse-type-func 自动解析属性类型函数(单参数)  

**parse-csv** *(filename key-row value-row &key (external-format :utf-8) skip-first-p auto-parse-type-p (auto-parse-type-func 'read-from-string))*
Parsing the CSV file generates a hash structure.  
key-row 可以是 number 或 list, 从 1 开始  
value-row 可以是 number 或 list 或 plist, 从 1 开始  
skip-first-p 是否跳过表头  
auto-parse-type-p 是否自动解析属性类型  
auto-parse-type-func 自动解析属性类型函数(单参数)  
  
```
(parse-vcsv "xx.csv" 1 1)
(parse-vcsv "xx.csv" 1 '(1 2 3))
(parse-vcsv "xx.csv" 1 '("a" 1 "b" 2 "c" 3))
(parse-vcsv "xx.csv" 1 '("a" (1 2 3) "b" 2 "c" 3))
(parse-vcsv "xx.csv" '(1 2) '("a" (1 2 3) "b" 2 "c" 3))
```
