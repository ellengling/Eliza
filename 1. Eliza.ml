#use "/course/cs017/src/ocaml/CS17setup.ml" ;;

(* TYPE DEFINITIONS: *)

(* a regexp_element can be a literal (a word represented by a string)
	or one of the two wild cards One and Any *)
type regexp_element =
| Lit of string
| One 
| Any ;;

(* examples of regexp_element : *)
Lit "example" ;;
Lit "eliza" ;;
Lit "Hello" ;;
Lit "you" ;;
One ;;
Any ;;

(* a response_element is represented as a sequence of words (text) 
	represented as a string, or a place which indicating a blank space
	 where a string could be placed *)
type response_element =
| Text of string
| Place of int ;;

(* examples of response_element : *)
Text "Are you feeling" ;; 
Place 2 ;; 
Text "today?" ;; 
Place 1 ;; 
Text "My name is Eliza" ;;
Place 3 ;;

(* a phrase is defined as a list of strings *)
type phrase = Phrase of string list ;;

(* examples of phrase : *)
Phrase ["How" ; "are" ; "you?"] ;;
Phrase ["I" ; "am" ; "well" ; "today."] ;;
Phrase ["Hello!"] ;;
Phrase ["My" ; "name" ; "is" ; "Eliza."] ;;
Phrase ["I" ; "am" ; "very" ; "worried" ; "today."] ;;
Phrase ["good" ; "morning."] ;; 

(* a pattern is defined as a list of regexp_elements (namely, a list of 
	literals, One and/or Any) *)
type pattern = Pattern of regexp_element list ;;

(* examples of pattern : *)
Pattern [Lit("I") ; Lit("feel") ; Any ; Lit("today")] ;;
Pattern [Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")] ;;
Pattern [Any] ;;
Pattern [Lit("my") ; One ; Lit("is") ; Any ; Lit("me")] ;;
Pattern [One] ;;
Pattern [Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")] ;;
Pattern [Lit("My") ; Any ; Lit("me")] ;;
Pattern [Lit("My") ; Lit("mother") ; Any)] ;;
Pattern [Lit("I") ; Lit("want") ; Lit("to") ; Any] ;;
Pattern [Lit("I") ; Lit("am") ; Lit("worried") ; Lit("that") ; Lit("my") ; 
	One ; Lit("is") ; Any ; Lit("me")] ;;
Pattern [Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")] ;;
Pattern [Lit("I") ; Any ; Lit("hate you")] ;;


(* a response_template is a list of response elements (namely a list
	of strings and/or places) *)
type response_template = Response_template of response_element list ;;

(* examples of response_template : *)
Response_template [Text("Why do you") ; Place(1) ; Text("hate me?")] ;;
Response_template [Text("Why do you think that your") ; Place(1) ; 
	Text("is") ; Place(2) ; Text("you?")] ;;
Response_template [Place(1)] ;;
Response_template [Text("Why do you feel that way?")] ;;
Response_template (Text("Have you spoken with your") ; Place(1) ; 
	Text("since?")] ;;
Response_template [Text("Tell me about your mother?")] ;;
Response_template [Text("Why are you feeling") ; Place(1) ; Text("today?")] ;;
Response_template [Text("Your") ; Place(1) ; Text("you?")] ;;
Response_template [Text("Can you be more specific?")] ;;
Response_template [(Text("Have you spoken with your") ; Place(1) ; 
	Text("since?")] ;;
Response_template [Text("Why do you want to") ; Place(1) ; Text("?")] ;;
Response_template [Text("Why don't you") ; Place(1) ; Text("?")] ;;
Response_template [Text("What did you say to your") ; Place(1) ; Text("?")] ;;
Response_template [Lit("I") ; Lit("want") ; Lit("to") ; Any] ;;

(* a rule is made up of a pattern and a response template *)
type rule = Rule of pattern * response_template ;;

(* examples of rule : *)
Rule ([Lit("I") ; Lit("feel") ; Any ; Lit("today")], 
[Text("Why are you feeling") ; Place(1) ; Text("today?")]) ;;

Rule ([Lit("My") ; Any ; Lit("me")], 
[Text("Your") ; Place(1) ; Text("you?")]) ;;

Rule ([Any], [Text("Can you be more specific?")]) ;;

Rule ([Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")], 
[(Text("Have you spoken with your") ; Place(1) ; Text("since?")]) ;;

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
[Text("Why do you want to") ; Place(1) ; Text("?")]) ;;

(* a rule_list is a list of rules, examples: *)
let rule_list_1 = 
[Rule ([Lit("I") ; Lit("feel") ; Any ; Lit("today")], 
	[Text("Why are you feeling") ; Place(1) ; Text("today?")]) ;

Rule ([Lit("My") ; Any ; Lit("me")], 
	[Text("Your") ; Place(1) ; Text("you?")]) ;

Rule ([Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")], 
	[(Text("Have you spoken with your") ; Place(1) ; Text("since?")]) ;

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
	[Text("Why do you want to") ; Place(1) ; Text("?")]) ;

Rule ([Lit("My") ; Lit("mother") ; Any)], 
	[Text("Tell me about your mother?")]) ;

Rule ([Lit("I") ; Lit("don't") ; Any], 
[Text("Why don't you") ; Place(1) ; Text("?")]) ;

Rule ([Lit("I") ; Lit("am") ; Lit("worried") ; Lit("that") ; Lit("my") ; 
	One ; Lit("is") ; Any ; Lit("me")], 
	[Text("Why do you think that your") ; Place(1) ; Text("is") ; Place(2) ; 
	Text("you?")]) ;

Rule ([Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")], 
	[Text("What did you say to your") ; Place(1) ; Text("?")]) ;

Rule ([Lit("I") ; Any ; Lit("hate you")], 
	[Text("Why do you") ; Place(1) ; Text("hate me?")])

Rule ([Any], [Text("Can you be more specific?")])] ;;

let rule_list_2 = 
[Rule([Lit("I"); Any; Lit("my"); Any],
	[Text("Why do you"); Place(1); Text("your"); Place(2); Text("?")]);
		  
Rule([One; Lit("hit"); Lit("me")],
	[Text("Why were you hit by"); Place(1); Text("?")]);
		  
Rule([Any; Lit("sued"); Any],
 	[Text("Why was"); Place(2); Text("sued by"); Place(1); Text("?")]);
		  
Rule([Any; Lit("mother"); Any],
 	[Text("Tell me more about your mother.")]);
		  
Rule([One; Lit("hates"); Lit("me")],
 	[Text("Do you think"); Place(1); Text("hates you because of something that you did?")]);
		  
Rule([Lit("I"); Any; Lit("hate"); Lit("you")],
 	[Text("Why do you"); Place(1); Text("hate me?")]);
		  
Rule([Any],
 	[Text("What have you been thinking about recently?")])] ;;

(* PROCEDURES: *)

(* helper_place : *)

(* Input:
	n, a positive integer
	alod, a list of any atomic data type *)
(* Output: 
	the nth element of alod *)

let rec helper_place (n: int) (alod: 'a list) : 'a =
	match n, alod with
	| _,[] -> []
	| 1, hd::tl -> hd 
	| _, hd::tl -> helper_place (n - 1) tl ;;

(* Test cases for helper_place : *)
check_expect (helper_place 1 [1 ; 2 ; 3]) 1 ;;
check_expect (helper_place 5 ["how" ; "are" ; "you" ; "doing" ; "today?"])
	"today?" ;;
check_expect (helper_place 3 []) [] ;;
check_expect (helper_place 10 [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11])
	10 ;;
check_expect (helper_place 1 ["I" ; "love" ; "my" ; "cat"]) "I" ;;
check_expect (helper_place 20 []) [] ;;
check_expect (helper_place 1 [["How"] ; ["are" ; "you" ; "today"] ; ["?"]]) 
	["How"] ;;
check_expect (helper_place 2 [["How"] ; ["are" ; "you" ; "today"] ; ["?"]]) 
	["are" ; "you" ; "today"] ;;
check_expect (helper_place 3 ["how" ; "are" ; "you" ; "doing" ; "today?"])
	"you" ;;
check_expect (helper_place 3 [["I"] ; ["love"] ; ["my"] ; ["cat"]) ["my"] ;;
check_expect (helper_place 4 [1 ; 2 ; 3]) [] ;;

(* extract : *)

(* Input: 
	user_query, a phrase that is the user's input question or statement
	a_pattern, a pattern *)
(* Output:
	extract returns the option Some followed by the portions of the user's
	 input that match each of the wild cards in the regular expression of 
	 the pattern in such a way that the whole matches the pattern 
	 (a phrase list), and the option None otherwise *)

let rec extract (user_query: phrase) (a_pattern: pattern) 
	: ((phrase list) option) = 
	match user_query, a_pattern with  
	| [], hd::tl -> None
	| hd::tl, [] -> None
	| hd1::tl1, Lit::tl2 -> (* does hd1 of user_query match Lit? if it
		does return Some[hd1] :: recur on tl1 and tl2, if it doesn't, 
		return None *) 
	| hd1::tl1, One::tl2 -> (* does hd1 match One? if it does return Some[hd1]
		:: recur on tl1 and tl2, if it doesn't return None *) 
	| hd1::tl1, Any::tl2 -> (* the idea: assume Any is just the first element of 
		user_query, recur through the  rest of both, if it returns Some you're
		done, if it returns None try again assuming that the first two elements
		of user_query are equal to Any, and so on, if you go through every 
		combination and nothing works return None - maybe use a nested 
		pattern? *) ;;  

(* Test cases for extract ; *)
check_expect (extract ["cs" ; "17" ; "is" ; "really" ; "fun"] 
		[Lit("cs") ; One ; Lit("is") ; One ; Lit("fun")]) 
	Some [["17"] ; ["really"]] ;;
check_expect (extract ["The" ; "right" ; "answer" ; "is" ; "right"]
		[Any ; Lit("right") ; Any]) 
	Some [["The"] ; ["answer" ; "is" ; "right"]] ;;
check_expect (extract ["I" ; "am" ; "not" ; "me" ; "today"] 
		[Lit("I") ; Any ; Lit("not") ; One]) 
	None ;; 
check_expect (extract ["I" ; "feel" ; "sick" ; "today"] 
		[Lit("I") ; Lit("feel") ; Any ; Lit("today")])
	Some [["sick"]] ;;
check_expect (extract ["My" ; "brother" ; "is" ; "mad" ; "at" ; "me"] 
		[Lit("My") ; Any ; Lit("me")])
	Some [["brother" ; "is" ; "mad" ; "at"]] ;;
check_expect (extract ["I" ; "got" ; "in" ; "a" ; "fight" ; "with" ; "my" ; 
		"mom" ; "today"] 
		[Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")])
	Some [["mom"]] ;;
check_expect (extract ["I" ; "want" ; "to" ; "go" ; "to" ; "the" ; "beach"]
		[Lit("I") ; Lit("want") ; Lit("to") ; Any])
	Some [["go" ; "to" ; "the" ; "beach"]] ;;
check_expect (extract ["I" ; "want" ; "to" ; "cry"] 
		[Lit("I") ; Lit("want") ; Lit("to") ; Any])
	Some [["cry"]] ;;
check_expect (extract ["My" ; "mother" ; "got" ; "so" ; "mad" ; "at" ; "me"]
 		[Lit("My") ; Lit("mother") ; Any)])
	Some [["got" ; "so" ; "mad" ; "at" ; "me"]] ;;
check_expect (extract ["I" ; "don't" ; "know"] [Lit("I") ; Lit("don't") ; Any])
	Some [["know"]] ;;
check_expect (extract ["I" ; "am" ; "worried" ; "that" ; "my" ; "sister" ; "is" ; 
		"angry" ; "with" ; "me"] 
		[Lit("I") ; Lit("am") ; Lit("worried") ; Lit("that") ; Lit("my") ; One ; 
		Lit("is") ; Any ; Lit("me")])
	Some [["sister"] ; ["angry" ; "with"]] ;;
check_expect (extract ["I" ; "saw" ; "my" ; "friends" ; "yesterday"] 
		[Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")])
	Some [["friends"]] ;;
check_expect (extract ["I" ; "hate" ; "to" ; "hate" ; "you"] 
		[Lit("I") ; Any ; Lit("hate you")])
	Some [["hate" ; "to"]] ;;
check_expect (extract ["I" ; "hate" ; "you"] 
		[Lit("I") ; Any ; Lit("hate you")])
	Some [[]] ;;
check_expect (extract ["I'm" ; "not" ; "well" ; "today"] [Any])
	Some [["I'm" ; "not" ; "well" ; "today"]] ;;
check_expect (extract ["I" ; "am" ; "not" ; "me" ; "today"] 
		[Lit("I") ; Lit("don't") ; Any])
	None ;;
check_expect (extract ["My" ; "mother" ; "got" ; "so" ; "mad" ; "at" ; "me"]
		[Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")])
	None ;;

(* make_response : *)

(* Input:
	extracted_phrases, the phrase list that is output by extract 
 	a_response_template, a response template *)
(* Output:
	a response constructed by substituting the extracted literals 
	(extracted_phrases) into their positions in the input response template *)

let rec make_response (extracted_phrases: (phrase list)) 
		(a_response_template: response_template) : phrase = 
	match extracted_phrases, my_response_template with 
	| [], hd::tl -> []
	| hd::tl ,[] -> []
	| hd1::tl1, Text::tl2 -> (* Text :: recur on extracted_phrases and tl2 *)
	| hd1::tl1, Place::tl2 -> (* call helper_place to find the nth element of 
		extracted_phrases that corresponds to Place(n), take that string ::
		recur on exctracted_phrases and the tl2 *) ;;

(* Test cases for make_response : *)
check_expect (make_response [["Eric"] ; ["Alex"]] 
	[Text("Why was") ; Place(2) ; Text("hit by") ; Place(1) ; Text("?")])
	["Why was" ; "Alex" ; "hit by" ; "Eric" ; "?"] ;;
check_expect (make_response [["cookies" ; "and" ; "cakes"]] 
	[Text("What do you like about") ; Place(1) ; Text("?")])
	["What do you like about" ; "cookies" ; ("?")] ;;
check_expect (make_response [["I'm" ; "not" ; "well" ; "today"]] 
		[Text("Can you be more specific?")])
	["Can you be more specific?"] ;;
check_expect (make_response [["hate" ; "to"]] 
		[Text("Why do you") ; Place(1) ; Text("hate me?")])
	["Why do you" ; "hate to" ; "hate me?"] ;;
check_expect (make_response [["friends"]] 
		[Text("What did you say to your") ; Place(1) ; Text("?")])
	["What did you say to your" ; "friends" ; "?"] ;;
[check_expect (make_response ["sister"] ; ["angry" ; "with"]] 
		[Text("Why do you think that your") ; Place(1) ; Text("is") ; Place(2)
		 ; Text("you?")])
	["Why do you think that you" ; "sister" ; "is" ; "angry with" ; "you?"] ;;
check_expect (make_response [["know"]] 
		[Text("Why don't you") ; Place(1) ; Text("?")])
	["Why don't you" ; "know" ; "?"] ;;
check_expect (make_response [["got" ; "so" ; "mad" ; "at" ; "me"]] 
		[Text("Tell me about your mother.")])
	["Tell me about your mother."] ;;
check_expect (make_response [["cry"]] 
		[Text("Why do you want to") ; Place(1) ; Text("?")])
	["Why do you want to" ; "cry" ; "?"] ;;
check_expect (make_response [["go" ; "to" ; "the" ; "beach"]] 
		[Text("Why do you want to") ; Place(1) ; Text("?")])
	["Why do you want to" ; "go to the beach" ; "?"] ;;
check_expect (make_response [["mom"]] 
		[(Text("Have you spoken with your") ; Place(1) ; Text("since?")])
	["Have you spoken with your" ; "mom" ; "since?"] ;;
check_expect (make_response [["brother" ; "is" ; "mad" ; "at"]] 
		[Text("Your") ; Place(1) ; Text("you?")])
	["Your" ; "brother is made at" ; "you?"] ;;
check_expect (make_response [["sick"]]
		[Text("Why are you feeling") ; Place(1) ; Text("today?")])
	["Why are you feeling" ; "sick" ; "today?"] ;;
check_expect (make_response [["the"] ; ["answer" ; "is" ; "right"]] 
		[Text("How do you know that?") ; Place(1) ; Text("?")])
	["How do you know that" ; "the answer is right"] ;;

(* eliza_respond : *)
let eliza_respond (user_query: phrase) (rule_list: (rule list)) : 
		(string list) =
	match user_query, rule_list with
	| hd::tl, [] -> []
	| [], hd::tl -> []
	| hd1::tl1, hd2::tl2 -> (* call extract on user_query and the pattern of the first 
		rule of rule_list, if extract returns Some call make_response on that result
		and the corresponding response template and return this result, if extract
		returns None recur on user_query and the rest of rule_list which will either
		produce a response using the steps above or eliza_respond will reach the 
		end of the rule list with no results in which case the empty list will be 
		returned, eliza_respond will likely require a helper procedure to match
		the phrase with the right rule *) ;;

(* Test cases for eliza_respond : *)
check_expect (eliza_respond ["I" ; "feel" ; "sick" ; "today"] rule_list_1)
	["Why are you feeling" ; "sick" ; "today?"] ;;
check_expect (eliza_respond ["My" ; "brother" ; "is" ; "mad" ; "at" ; "me"]
 		rule_list_1) 
	["Your" ; "brother is made at" ; "you?"] ;;
check_expect (eliza_respond ["I" ; "got" ; "in" ; "a" ; "fight" ; "with" ;
		 "my" ; "mom" ; "today"] rule_list_1)
	["Have you spoken with your" ; "mom" ; "since?"] ;;
check_expect (eliza_respond ["I" ; "want" ; "to" ; "go" ; "to" ; "the" ; 
		"beach"] rule_list_1) 
	["Why do you want to" ; "go to the beach" ; "?"] ;;
check_expect (eliza_respond ["I" ; "want" ; "to" ; "cry"] rule_list_1)
	["Why do you want to" ; "cry" ; "?"] ;;
check_expect (eliza_respond ["My" ; "mother" ; "got" ; "so" ; "mad" ;
		 "at" ; "me"] rule_list_1) 
	["Tell me about your mother?"] ;;
check_expect (eliza_respond ["I" ; "don't" ; "know"] rule_list_1)
	["Why don't you" ; "know" ; "?"] ;;
check_expect (eliza_respond ["I" ; "am" ; "worried" ; "that" ; "my" ; 
		"sister" ; "is" ; "angry" ; "with" ; "me"] rule_list_1)
	["Why do you think that your" ; "sister" ; "is" ; "angry with" ; "you?"] ;;
check_expect (eliza_respond ["I" ; "saw" ; "my" ; "friends" ; "yesterday"] 
		rule_list_1) 
	["What did you say to your" ; "friends" ; "?"] ;;
check_expect (eliza_respond ["I" ; "hate" ; "to" ; "hate" ; "you"] rule_list_1)
	["Why do you" ; "hate" ; "to" ; "hate me?"] ;;
check_expect (eliza_respond ["I" ; "hate" ; "you"] rule_list_1)
	["Why do you" ; "hate me?"] ;;
check_expect (eliza_respond ["I'm" ; "not" ; "well" ; "today"] rule_list_1)
	["Can you be more specific?"] ;;
check_expect (eliza_respond ["I" ; "really" ; "love" ; "my" ; "cat"] 
		rule_list_2) 
	["Why do you" ; "really" ; "love" ; "your" ; "cat" ; "?"] ;;
check_expect (eliza_respond ["Lucy" ; "hit" ; "me"] rule_list_2)
	["Why were you hit by" ; "Lucy" ; "?"] ;;
check_expect (eliza_respond ["Michael" ; "sued" ; "Andy"] rule_list_2)
	["Why was" ; "Andy" ; "sued by" ; "Michael" ; "?"] ;;
check_expect (eliza_respond ["My" ; "mother" ; "is" ; "wonderful"] rule_list_2)
	["Tell me more about your mother."] ;;
check_expect (eliza_respond ["Laura" ; "hates" ; "me"] rule_list_2)
	["Do you think" ; "Laura" ; "hates you because of something that
		 you did?"] ;;
check_expect (eliza_respond ["I" ; "hate" ; "to" ; "hate" ; "you"] rule_list_2)
	["Why do you" ; "hate" ; "to" ; "hate me?"] ;;
check_expect (eliza_respond ["I" ; "hate" ; "you"] rule_list_2)
	["Why do you" ; "hate me?"] ;;
check_expect (eliza_respond ["Hello" ; "Eliza"] rule_list_2)
	["What have you been thinking about recently?"] ;;
















