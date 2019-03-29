(** Main file for Mutables library *)

open Core
(* a simple mutable object *)
module MutableObject : sig
  type 'a t
  val set : 'a t -> 'a -> unit
  val get : 'a t -> 'a
end = struct
  type 'a t = {mutable contents: 'a}
  let set self a = 
    self.contents <- a
  let get self = 
    self.contents
end

(** A module for a mutable stack *)
module MutableStack : sig
  type 'a t

  (** push item into stack *)
  val push : 'a t -> 'a -> unit

  (** pop item from stack *)
  val pop : 'a t -> 'a option

  (** pop item from stack. Raise error if empty *)
  val pop_exn : 'a t -> 'a 

  (** get an empty stack *)
  val empty : unit -> 'a t

  (** peek the top of stack *)
  val peek : 'a t -> 'a option

  (** peek the top of stack. Raise error if empty *)
  val peek_exn : 'a t -> 'a

  (** A exception for empty stack *)
  exception StackEmptyException
end = struct
  type 'a node = {value: 'a; next: 'a node option}
  type 'a t = {mutable top: 'a node option}
  exception StackEmptyException

  let empty () = {top = None}

  let push s x = 
    s.top <- Some {value = x; next = s.top}

  let pop s = 
    match s.top with
    | Some node -> s.top <- node.next; Some node.value
    | None -> None

  let pop_exn s = 
    match s.top with
    | Some node -> s.top <- node.next; node.value
    | None -> raise StackEmptyException

  let peek s = 
    match s.top with
    | Some node -> Some node.value
    | None -> None

  let peek_exn s = 
    match s.top with
    | Some node -> node.value
    | None -> raise StackEmptyException
end

(** A module for a mutable que *)
module MutableQue : sig 
  type 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val init : 'a list -> 'a t

  (** get an empty que *)
  val empty : unit -> 'a t
  val exists : 'a t -> 'a -> bool

  (* An exception for empty que *)
  exception QueEmptyException
end = struct
  type 'a t = {mutable contents: 'a array}
  exception QueEmptyException
  let push self a = 
    self.contents <- Array.concat [[|a|]; self.contents]
  let pop self = 
    let n = Array.length self.contents in
    if n <> 0 then 
      let ret = self.contents.(0) in
      self.contents <- Array.sub ~pos:0 ~len:(Array.length self.contents - 1) self.contents;
      ret
    else raise QueEmptyException
  let init (l: 'a list) = 
    {contents = Array.of_list l}
  let empty () = 
    {contents = [||]}
  let exists self a = 
    Array.exists self.contents ~f:(fun x -> x = a) 
end

(** A module for a mutable dictionary *)
module MutableDict : sig
  type ('a , 'b) t
  val init : ('a * 'b) list -> ('a, 'b) t
  val get : ('a, 'b) t -> 'a -> 'b option
  val get_exn : ('a, 'b) t -> 'a -> 'b 
  val get_with_default : ('a, 'b) t -> 'a -> default:'b -> 'b
  val exists_key : ('a, 'b) t -> key:'a -> bool
  val exists_value : ('a, 'b) t -> value:'b -> bool
  val add : ('a, 'b) t -> key:'a -> value:'b -> unit
  val length : ('a, 'b) t -> int
  val for_all : ('a, 'b) t -> f:(('a * 'b) -> bool) -> bool
  val for_alli : ('a, 'b) t -> f:(int -> ('a * 'b) -> bool) -> bool
end = struct
  type ('a , 'b) t = {mutable contents: ('a * 'b) list}
  let init l = {contents = l}
  let get self a = 
    List.Assoc.find self.contents ~equal:(=) a
  let get_exn self a = 
    List.Assoc.find_exn self.contents ~equal:(=) a
  let get_with_default self a ~default = 
    match get self a with
    | Some x -> x
    | None -> default
  let exists_key self ~key = 
    List.exists self.contents ~f:(fun (k, _) -> k = key)
  let exists_value self ~value = 
    List.exists self.contents ~f:(fun (_, v) -> v = value)
  let add self ~key ~value = 
    self.contents <- List.Assoc.add self.contents ~equal:(=) key value
  let length self = 
    List.length self.contents
  let for_all self ~(f:('a * 'b) -> bool) = 
    List.for_all self.contents ~f
  let for_alli self ~(f: int -> ('a * 'b) -> bool) = 
    List.for_alli self.contents ~f
end

(** Module for mutable Binary Search Trees *)
module MutableBST : sig 
  type 'a t

  (** check if data x exists *)
  val exists : 'a t -> 'a -> bool

  (** insert data x into bst *)
  val insert : 'a t -> 'a -> unit

  (** delete data x *)
  val delete : 'a t -> 'a -> unit

  (** get height of tree *)
  val height : 'a t -> int

end = struct
  type 'a bst = 
    | Node of 'a * 'a bst * 'a bst
    | Leaf of 'a
    | Empty
  type 'a t = {mutable contents: 'a bst; comp: 'a -> 'a -> int} 

  let exists self x = 
    let tree = self.contents in
    let cmp = self.comp in
    let rec loop t x = 
      match t with
      | Node(v, l, r) -> 
        if cmp v x >= 0 then loop l x
        else loop r x
      | Leaf v -> v = x
      | Empty -> false
    in
    loop tree x

  let insert self x = 
    let tree = self.contents in
    let cmp = self.comp in
    let rec loop t x = 
      match t with
      | Node(v, l, r) -> 
        if cmp v x = 0 then t
        else if cmp v x >= 0 then loop l x
        else loop r x
      | Leaf v -> 
        if v = x then t
        else if cmp v x >= 0 
        then Node(v, Leaf x, Empty)
        else Node(v, Empty, Leaf x)
      | Empty -> Leaf x
    in
    self.contents <- loop tree x

  let rec append t1 t2 cmp = 
    match t1, t2 with
    | tx, Empty | Empty, tx -> tx
    | Leaf v1, Leaf v2 -> if cmp v1 v2 >= 0 then Node(v2, Empty, Leaf v1) else Node(v1, Leaf v2, Empty)
    | Leaf vl, Node(vn, l, r) | Node(vn, l, r), Leaf vl -> 
      if cmp vn vl >= 0 then append l (Leaf vl) cmp
      else append r (Leaf vl) cmp
    | Node(v1, l, r), Node(v2, _, _) -> 
      if cmp v1 v2 >= 0 then append l t2 cmp
      else append r t2 cmp

  let delete self x = 
    if exists self x 
    then 
      let tree = self.contents in
      let cmp = self.comp in
      let rec loop t= 
        match t with
        | Node(v, l, r) -> 
          if cmp v x > 0 then loop l
          else if cmp v x < 0 then loop r
          else append l r cmp
        | Leaf v -> if v = x then Empty else Leaf x
        | Empty -> Leaf x
      in self.contents <- loop tree
    else ()

  let height self = 
    let tree = self.contents in
    let rec loop t =
      match t with
      | Node(_, l, r) -> max (loop l) (loop r) + 1
      | Leaf _ -> 1
      | Empty -> 0
    in loop tree
end


(** A mutable Trie *)
module MutableTrie : sig
  (** a type for mutable trie  *)
  type 'a t

  (** a type for trie *)
  type 'a trie

  (** produces an empty trie *)
  val empty : unit -> 'a trie

  (** check if value exists in trie *)
  val exists : 'a trie t -> 'a sexp_list -> bool

  (** add element to trie *)
  val add : 'a trie t -> 'a sexp_list -> unit
end = struct
  type 'a trie = 
    | Node of ('a, 'a trie) MutableDict.t
    | Empty
  type 'a t = {mutable contents: 'a}
  let empty () = Empty
  let exists self xl = 
    let trie = self.contents in
    let rec loop t l =
      match l with
      | hd :: tl -> 
        (match t with
         | Node dict -> 
           (match MutableDict.get dict hd with
            | Some subtrie -> loop subtrie tl
            | None -> false)
         | Empty -> false)
      | [] -> true in
    loop trie xl
  let add self xl = 
    let trie = self.contents in
    let rec loop t cxl = 
      match cxl with
      | hdx :: tlx -> 
        (match t with
         | Node dict -> 
           (match MutableDict.get dict hdx with
            | Some subtrie -> loop subtrie tlx
            | None -> MutableDict.add dict ~key:hdx ~value:Empty)
         | Empty -> loop (Node (MutableDict.init [(hdx, Empty)])) tlx)
      | [] -> () in
    loop trie xl
end


(* graph structures *)
(* module DirectedEdge : sig
   type 'a t
   end = struct
   type 'a t = {} *)


module DirectedGraph : sig
  type 'a t

  (** initialize direct graph *)
  val init : (string * string * int) sexp_list -> 'a t

  (** get all neighboring vertexes *)
  val get_all_neighbors : 'a t -> s:string -> string sexp_list

  (** get all paths from s to t *)
  val get_all_paths : 'a t -> s:string sexp_list -> t:string option -> string sexp_list sexp_list

  (** get total distance of a given path *)
  val get_distance : 'a t -> string sexp_list -> int
end = struct
  type 'a arrow = {mutable length: int; vertex: string}
  type 'a t = {mutable contents : (string, 'a arrow list) MutableDict.t}
  let init edges = 
    let content = MutableDict.init [] in
    List.iter edges ~f:(fun (e1, e2, l) -> 
        MutableDict.add content 
          ~key:e1 
          ~value:({length = l; vertex = e2}::MutableDict.get_with_default content e1 ~default:[]));
    {contents = content}
  let get_all_neighbors self ~s = 
    let g = self.contents in
    List.map (MutableDict.get_with_default g s ~default:[]) ~f:(fun {length = _; vertex = s} -> s)
  let get_all_paths self ~s ~t = 
    let rec incr_path (paths: string list list) = 
      let new_path_ll = 
        List.map paths
          ~f:(fun path -> List.map (get_all_neighbors self ~s:(List.hd_exn path)) 
                 ~f:(fun neigh -> 
                     if List.exists path ~f:(fun x -> x = neigh)
                     then []
                     else neigh:: path))
      in let new_paths = List.concat new_path_ll in
      let is_valid = List.fold new_paths ~init:true ~f:(fun accum a -> accum && List.hd a = t) in
      if is_valid then new_paths
      else incr_path new_paths
    in incr_path [s]
  let get_distance self p = 
    let rec loop cp accum = 
      match cp with
      | [_] -> accum
      | hd:: tl -> 
        let neighbors = MutableDict.get_exn self.contents hd in
        let arw = List.find_exn neighbors ~f:(fun {length = _; vertex = v1} -> v1 = (List.hd_exn tl)) in
        loop tl (accum + arw.length)
      | _ -> accum in
    loop p 0
end
