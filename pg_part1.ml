type ('k, 'v) trie = Trie of 'v option * (('k * ('k, 'v) trie) list)


(* val empty : ('k, 'v) trie *)
let empty=Trie (None,[])

(* function is_empty to check if trie is empty or not *)
let rec is_empty t = match t with
  |Trie (a, [])->begin
    match a with
    None -> true
    |Some a -> false
  end
  |Trie (_, (_, Trie (_, _))::_)->false

  (* insert function *)
 
    

(* function find *)
let rec find t v = match (t, v) with

|  Trie (None,_) , [] -> raise Not_found
|  Trie (Some v,_), [] -> v
|  Trie (_,m) , (x::r as b) -> find2 b m
and
find2 a b = match b with 
(k, v)::t -> begin
  match  a with
  u::s -> if u=k then find v s else find2 a t
  |[]->failwith "Not found"
end
|[]-> failwith "not found"

let rec insert (t :('k, 'v) trie) ls  v = match (t, ls) with
    | Trie (_,m), [] -> Trie (Some v,m)
    | Trie (a,m) , x::r-> begin
      match m with
       []->Trie(a, [(x, (insert empty r v))])
       |(key, value)::l -> if key=a then 
       Trie(a, [(key, (insert value r v))]@l)
       else
       Trie(a, (key, value):: (ins l (x::r)  v))
    end
      and
      ins tls (r::s)  vl = match tls with
      |[] ->failwith "Error can not insert"
      |(k, v)::l -> if k= r then
      ([k, insert  v s vl]@l)
      else
      (k, v):: ins l (r::s)  vl 
  (* delete function *)
  (* let rec delete  t l = match (l,t) with
  | [], Trie (_,m) -> Trie (None,m)
  | x::r, Trie (v,m) ->begin
    match m with
    (k, vl)::ts -> if k = x then
    Trie(None, vl::ts
    else
    delete vl l
    |[]->failwith "Can not delete from empty trie"
  end *)

