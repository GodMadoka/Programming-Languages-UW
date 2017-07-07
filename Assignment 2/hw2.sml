fun same_string(s1 : string, s2 : string) =
  s1 = s2
fun all_except_option(s, ss) =
  case ss of
      [] => NONE
   |  x::ss' =>
      if same_string(x,s) then SOME ss'
      else
	  case all_except_option(s, ss') of
	      NONE => NONE
	   |  SOME y => SOME (x :: y) 
	  
fun get_substitutions1(slists, s) =
  case slists of
      [] => []
   |  ss::slists' =>
      case all_except_option(s,ss) of
	  NONE => get_substitutions1(slists',s)
       |  SOME x => x  @ get_substitutions1(slists',s)
		 
fun get_substitutions2(slists, s) =
  let
      fun get_s(slists, s, acc) =
	case slists of
	    [] => acc
	 |  ss::slists' =>
	    case all_except_option(s,ss) of
		NONE => get_s(slists',s,acc)
	     |  SOME x => get_s(slists',s,acc @ x)
  in
      get_s(slists, s, [])
  end

fun similar_names(slists, name) =
  let
      val {first = f, middle = m , last = l} = name	  
      fun helper(f_names, name) =
	case f_names of
	    [] => []
	 |  x::f_names' => {first = x, middle = m, last = l} ::
			   helper(f_names', name)
  in
      name::helper(get_substitutions2(slists,f),name)
  end
      
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit,rank) =
  case suit of
      Clubs => Black
   |  Spades => Black
   |  _ => Red 

fun card_value(suit,rank) =
  case rank of
      Ace => 11
   |  Num i => i 
   |  _ => 10  

fun remove_card(cs,c,e) =
  case cs of
      [] => raise e
     |x::cs'  => if x=c then cs' else x::remove_card(cs',c,e)
						     
fun all_same_color(cs) =
  case cs of
      [] => true
   |  _::[] => true
   |  x::y::cs' => (card_color(x)=card_color(y)) andalso all_same_color(y::cs')
		       
fun sum_cards(cs) =
  let fun helper(cs,acc) =
	case cs of
	    [] => acc
	 |  c::cs' => helper(cs',acc + card_value(c))
  in
      helper(cs,0)
  end
      
fun score(cs,goal) =
  let val sum = sum_cards(cs)
      val prel = if sum > goal then (sum - goal) *3 else goal - sum
  in
      if all_same_color(cs) then prel div 2
      else prel
  end
      
fun officiate(cl,ml,goal) =
  let fun helper(hc,cl,ml,goal,e) =
	case ml of
	    [] => score(hc,goal)
	  | a::ml' =>
	    case a of
	    Draw => (case cl of
		        [] => score(hc,goal)
		      | b::cl' => let val sum = sum_cards(hc)+card_value(b)
			          in if sum > goal then score(b::hc,goal)
				     else helper(b::hc,cl',ml',goal,e)
			          end)
	  |Discard c => helper(remove_card(hc,c,e),cl,ml',goal,e)
  in
      helper([],cl,ml,goal,IllegalMove)
  end
      

  
				     
				     
				     
				  
      
