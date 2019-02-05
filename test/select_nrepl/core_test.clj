(ns select-nrepl.core-test
  (:require
   [midje.sweet :refer :all]
   [select-nrepl.test-helpers :refer :all]))

(facts "about selecting elements"
  (tabular
    (select "whole" "element" ?input) => ?output
    ?input             ?output
    "<hello>"          "<hello>"
    "  hello/<>world"  "  <hello/world>"
    " :foo/ba<>r"      " <:foo/bar>"
    " \\new<>line"     " <\\newline>"
    " \"a s<t>ring\" " " <\"a string\"> "
    " 4200<>0  x"      " <42000>  x"
    "++ #\"<>x\" ;;-"  "++ <#\"x\"> ;;-"
    " #fo<>o \"h\" "   " <#foo \"h\"> "
    " \"h<>ello\nw\" " " <\"hello\nw\"> "

    "( he<>llo there)" "( <hello> there)"
    "h <>"             "h "
    "<> h ello"        " <h> ello"
    "h <> (ello t)"    "h  (<ello> t)"
    "h <> ([ello t])"  "h  ([<ello> t])")

  (tabular
    (select "inside" "element" ?input) => ?output
    ?input ?output
    " \"a s<t>ring\" "  " \"<a string>\" "
    " #\"a <>regex\" x" " #\"<a regex>\" x"
    " #fo<>o \"hi\" "   " #foo \"<hi>\" "))
