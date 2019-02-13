(ns select-nrepl.core-test
  (:require
   [midje.sweet :refer :all]
   [select-nrepl.test-helpers :refer :all]))

(facts "about selecting elements"
  (facts "about selecting whole elements"
    (fact "when the cursor is inside the element, it will expand the selection to the whole element"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "  hello/<>world"  "  <hello/world>"
        " :foo/ba<>r"      " <:foo/bar>"
        " \\new<>line"     " <\\newline>"
        " \"a s<t>ring\" " " <\"a string\"> "
        " 4200<>0  x"      " <42000>  x"
        "++ #\"<>x\" ;;-"  "++ <#\"x\"> ;;-"
        " #fo<>o \"h\" "   " <#foo \"h\"> "
        " \"h<>ello\nw\" " " <\"hello\nw\"> "
        "a ^S foo<>b y"    "a <^S foob> y"
        "a #^S foo<>b y"   "a <#^S foob> y"

        "( he<>llo there)" "( <hello> there)"
        "h <>"             "h "
        "<> h ello"        " <h> ello"
        "h <> (ello t)"    "h  (<ello> t)"
        "h <> ([ello t])"  "h  ([<ello> t])"))

    (fact "when a whole element is already selected, it will select the next one"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "<foo> -42 :baz"   "foo <-42> :baz"
        "foo <-42> :baz"   "foo -42 <:baz>"

        "<foo> (-42) :baz" "foo (<-42>) :baz"
        "foo (<-42>) :baz" "foo (-42) <:baz>"

        "<foo> (-42 :baz)" "foo (<-42> :baz)"
        "foo (<-42> :baz)" "foo (-42 <:baz>)"))

    (fact "when the last whole element is already selected, it will select nothing"
      (select "whole" "element" "foo (-42 <:baz>)") => "foo (-42 :baz)"))

  (facts "about selecting the inside of an element"
    (tabular
      (select "inside" "element" ?input) => ?output
      ?input ?output
      " \"a s<t>ring\" "  " \"<a string>\" "
      " #\"a <>regex\" x" " #\"<a regex>\" x"
      " #fo<>o \"hi\" "   " #foo \"<hi>\" "
      " \"he\nt<>here \"" " \"<he\nthere >\"")))

(facts "about selecting forms"
  (facts "about selecting whole forms"
    (fact "when the cursor is inside the form, it will expand the selection to the whole form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "x (he<>l wo) 4"      "x <(hel wo)> 4"
        "x [he<>l wo] 4"      "x <[hel wo]> 4"
        "x {he<>l wo} 4"      "x <{hel wo}> 4"
        "x #{he<>l wo} 4"     "x <#{hel wo}> 4"
        "x #:foo{:b<>ar 4} 4" "x <#:foo{:bar 4}> 4"
        "x `(foo ~b<>ar) 4"   "x <`(foo ~bar)> 4"
        "x ~(foo ~b<>ar) 4"   "x <~(foo ~bar)> 4"
        "x ~@(foo ~b<>ar) 4"  "x <~@(foo ~bar)> 4"
        "x ^S {he<>l wo} 4"   "x <^S {hel wo}> 4"
        "x #^S {he<>l wo} 4"  "x <#^S {hel wo}> 4"
        "x '(he<>l wo) 4"     "x <'(hel wo)> 4"
        "x #f/b (he<>l wo) 4" "x <#f/b (hel wo)> 4"

        "x (he (ll<>o wo) l)" "x (he <(llo wo)> l)"
        "x (h<>e (llo wo) l)" "x <(he (llo wo) l)>"))

    (fact "when a whole form is already selected, it selects the next-wider form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "( (he <(wo)> th) x)" "( <(he (wo) th)> x)"
        "( <(he (wo) th)> x)" "<( (he (wo) th) x)>")))

  (facts "about selecting inside forms"
    (tabular
      (select "inside" "form" ?input) => ?output
      ?input                ?output
      "x (he<>l wo) 4"      "x (<hel wo>) 4"
      "x [he<>l wo] 4"      "x [<hel wo>] 4"
      "x {he<>l wo} 4"      "x {<hel wo>} 4"
      "x #{he<>l wo} 4"     "x #{<hel wo>} 4"
      "x #:foo{:b<>ar 4} 4" "x #:foo{<:bar 4>} 4"
      "x `(foo ~b<>ar) 4"   "x `(<foo ~bar>) 4"
      "x ~(foo ~b<>ar) 4"   "x ~(<foo ~bar>) 4"
      "x ~@(foo ~b<>ar) 4"  "x ~@(<foo ~bar>) 4"

      "x (he (ll<>o wo) l)" "x (he (<llo wo>) l)"
      "x (h<>e (llo wo) l)" "x (<he (llo wo) l>)")))

(facts "about selecting toplevel forms"
  (fact "when inside a top-level, that top-level is selected"
    (tabular
      (select "whole" "toplevel" ?input) => ?output
      ?input                ?output
      "x (he (ll<>o wo) l)" "x <(he (llo wo) l)>"
      "x (h<>e (llo wo) l)" "x <(he (llo wo) l)>"))
  (fact "when not inside a top-level, the following top-level is selected"
    (select "whole" "toplevel" "x<> (he (llo wo) l)") => "x <(he (llo wo) l)>"))
