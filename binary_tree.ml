type 'a bt =
  | Leaf
  | Node of 'a * 'a bt * 'a bt

  (** does the tree contain an element equal to the given one? *)

  let rec contains  (b:'a bt) x = match b with
  |  Leaf -> false
  | Node (num, left, right)-> num =x || contains left x || contains right x


  (** produce elements in the left-to-right order they appear in
the tree *)
let rec to_list=function
Leaf->[]
|Node(number,left,right)->number::(to_list left)@(to_list right)

let v=Node(9,Node(2,Leaf,Leaf),Node(3,Leaf,Leaf))

let z=Node('a',Node('e',Leaf,Leaf),Node('c',Node('g', Leaf, Leaf),Leaf))
 




  (** zip together two binary trees with same shape; fail if not
same shape *)

  let rec is_same_shape a b = match a with
  Leaf ->begin 
    match b with
    Leaf-> true
    |Node(data, left, right)-> false
  end
  |Node(data1, left1, right1)->begin
    match b with
    Leaf -> false
    |Node(data2,left2,right2)->true&& is_same_shape left1 left2 && is_same_shape right1 right2
  end

  let rec zip a b = match is_same_shape a b with
   | true-> begin
    match a with
      |Node(data1, left,right)->begin
        match b with
        |Node(data2, left1,right1)-> Node((data1,data2), zip left left1, zip right right1)
        |Leaf-> Leaf
      end
    | Leaf-> Leaf
    end
    |false -> failwith "trees not of same shape"

    (** produce tree of same shape, every element is obtained by
applying f *)
let rec map a f =  match a with
    |Leaf -> Leaf
    |Node(data, left, right)->begin
      Node(f data, map  left f ,  map right f )
    end



(** produce the mirror image of a tree; elements are in reverse
order *)
let rec rev = function
Leaf -> Leaf
|Node(data, left, right)-> Node(data, rev right, rev left)


(** list List.fold but for binary trees *)
(* let rec fold a = *)


(* path *)
type dir = Left | Right

(** get the element found by following given path *)
let get_element = function
Leaf -> failwith "fail"
| Node(data, left, right)-> data

let rec get a b= match a with
Leaf -> failwith "fail"
|Node(data, left, right)->begin 
  match b with
  [] ->failwith "fail"
  | head::tail -> begin
    match head with 
    Left-> begin
      match tail with
      |[]->get_element left
      |h::t-> get left tail
    end
    | Right ->begin
      match tail with
      |[]->get_element left
      |h::t-> get right tail
    end 
  end
end