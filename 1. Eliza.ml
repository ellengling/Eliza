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
Phrase ["My" ; "name" ; "is" ; "Eliza."]
Phrase ["I" ; "am" ; "very" ; "worried" ; "today."]
Phrase ["good" ; "morning."]

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
[Text("Why are you feeling") ; Place(1) ; Text("today?")])

Rule ([Lit("My") ; Any ; Lit("me")], 
[Text("Your") ; Place(1) ; Text("you?")])

Rule ([Any], [Text("Can you be more specific?")])

Rule ([Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")], 
[(Text("Have you spoken with your") ; Place(1) ; Text("since?")])

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
[Text("Why do you want to") ; Place(1) ; Text("?")])

Rule ([Lit("My") ; Lit("mother") ; Any)], 
[Text("Tell me about your mother?")])

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
[Text("Why don't you") ; Place(1) ; Text("?")])

Rule ([Lit("I") ; Lit("am") ; Lit("worried") ; Lit("that") ; Lit("my") ; 
	One ; Lit("is") ; Any ; Lit("me")], 
[Text("Why do you think that your") ; Place(1) ; Text("is") ; Place(2) ; 
	Text("you?")])

Rule ([Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")], 
[Text("What did you say to your") ; Place(1) ; Text("?")])

Rule ([Lit("I") ; Any ; Lit("hate you")], 
[Text("Why do you") ; Place(1) ; Text("hate me?")])

let rule_list_1 = 
[Rule ([Lit("I") ; Lit("feel") ; Any ; Lit("today")], 
	[Text("Why are you feeling") ; Place(1) ; Text("today?")]) ;

Rule ([Lit("My") ; Any ; Lit("me")], 
	[Text("Your") ; Place(1) ; Text("you?")]) ;

Rule ([Any], [Text("Can you be more specific?")]) ;

Rule ([Lit("I") ; Lit("got") ; Lit("in") ; Lit("a") ; Lit("fight") ; 
	Lit("with") ; Lit("my") ; One ; Lit("today")], 
	[(Text("Have you spoken with your") ; Place(1) ; Text("since?")]) ;

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
	[Text("Why do you want to") ; Place(1) ; Text("?")]) ;

Rule ([Lit("My") ; Lit("mother") ; Any)], 
	[Text("Tell me about your mother?")]) ;

Rule ([Lit("I") ; Lit("want") ; Lit("to") ; Any], 
	[Text("Why don't you") ; Place(1) ; Text("?")]) ;

Rule ([Lit("I") ; Lit("am") ; Lit("worried") ; Lit("that") ; Lit("my") ; 
	One ; Lit("is") ; Any ; Lit("me")], 
	[Text("Why do you think that your") ; Place(1) ; Text("is") ; Place(2) ; 
	Text("you?")]) ;

Rule ([Lit("I") ; Lit("saw") ; Lit("my") ; One ; Lit("yesterday")], 
	[Text("What did you say to your") ; Place(1) ; Text("?")]) ;

Rule ([Lit("I") ; Any ; Lit("hate you")], 
	[Text("Why do you") ; Place(1) ; Text("hate me?")])] 

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
	| [],_ -> None
	| _,[] -> None
	| _,[Lit ; _] -> (* does the first element of user_query match Lit? if it
		 does return Some[that element] :: recur on the rest of both, 
		if it doesn't, return None *) 
	| _,[One ; _] -> (* does the first element of user_query match One? if it
		 does return Some[that element] :: recur on the rest of both, if
		 it doesn't return None *) 
	| _,[Any ; _] -> (* the idea: assume Any is just the first element of 
		user_query, recur through the  rest of both, if it returns Some you're
		done, if it returns None try again assuming that the first two elements
		of user_query are equal to Any, and so on, if you go through every 
		combination and nothing works return None *) ;;

(* Test cases for extract ; *)
check_expect (extract ["cs" ; "17" ; "is" ; "really" ; "fun"] 
	[Lit("cs") ; One ; Lit("is") ; One ; Lit("fun")]) 
	Some [["17"] ; ["really"]] ;;
check_expect (extract ["The" ; "right" ; "answer" ; "is" ; "right"] 
	[Any ; Lit("right") ; Any]) 
	Some [["The"] ; ["answer" ; "is" ; "right"]]
check_expect (extract ["I" ; "am" ; "not" ; "me" ; "today"] 
	[Lit("I") ; Any ; Lit("not") ; One]) None ;; 

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
	| [],_ -> []
	| _,[] -> []
	| _,[Text ; _] -> (* Text :: recur on extracted_phrases and the rest of 
		a_response_template *)
	| _,[Place ; _] -> (* call helper_place to find the nth element of 
		extracted_phrases that corresponds to Place(n), take that string ::
		recur on exctracted_phrases and the rest of a_response_template *) ;;

(* Test cases for make_response : *)
check_expect (make_response [["Eric"] ; ["Alex"]] 
	[Text("Why was") ; Place(2) ; Text("hit by") ; Place(1) ; Text("?")])
	["Why was" ; "Alex" ; "hit by" ; "Eric" ; "?"] ;;
check_expect (make_response [["cookies" ; "and" ; "cakes"]] 
	[Text("What do you like about") ; Place(1) ; Text("?")])
	["What do you like about" ; "cookies" ; ("?")] ;;

(* eliza_respond : *)
let eliza_respond (user_query: phrase) (rule_list: (rule list)) : 
		(string list) =
	match user_query, rule_list with
	| _,[] -> []
	| [],_ -> []
	| hd::tl, _ -> (* call extract on user_query and the pattern of the first 
		rule of rule_list, if extract returns Some call make_response on that result
		and the corresponding response template and return this result, if extract
		returns None recur on user_query and the rest of rule_list which will either
		produce a response using the steps above or eliza_respond will reach the 
		end of the rule list with no results in which case the empty list will be 
		returned *) ;;

(* Test cases for eliza_respond : *)
check_expect (eliza_respond ["I" ; "feel" ; "sick" ; "today"] rule_list_1)
	["Why are you feeling" ; "sick" ; "today?"] ;; 













