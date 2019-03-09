open Core

module MutableStack : sig
  type 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val empty : unit -> 'a t
end = struct
  type 'a node = {value: 'a; next: 'a node option}
  type 'a t = {mutable top: 'a node option}
  exception StackBottomException
  let empty () = {top = None}
  let push s x = 
    s.top <- Some {value = x; next = s.top}
  let pop s = 
    match s.top with
    | Some node -> s.top <- node.next; node.value
    | None -> raise StackBottomException
end

module MutableQue : sig 
  type 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val init : 'a list -> 'a t
  val empty : unit -> 'a t
  val exists : 'a t -> 'a -> bool
end = struct
  type 'a t = {mutable contents: 'a array}
  exception QueEmptyError
  let push self a = 
    self.contents <- Array.concat [[|a|]; self.contents]
  let pop self = 
    let n = Array.length self.contents in
    if n <> 0 then 
      let ret = self.contents.(0) in
      self.contents <- Array.sub ~pos:0 ~len:(Array.length self.contents - 1) self.contents;
      ret
    else raise QueEmptyError
  let init (l: 'a list) = 
    {contents = Array.of_list l}
  let empty () = 
    {contents = [||]}
  let exists self a = 
    Array.exists self.contents ~f:(fun x -> x = a) 
end

module MutableDict : sig
  type ('a , 'b) t
  val get : ('a, 'b) t -> 'a -> 'b option
  val get_exn : ('a, 'b) t -> 'a -> 'b 
  val add : ('a, 'b) t -> key:'a -> value:'b -> unit
end = struct
  type ('a , 'b) t = {mutable contents: ('a * 'b) list}
  let get self a = 
    List.Assoc.find self.contents ~equal:(=) a
  let get_exn self a = 
    List.Assoc.find_exn self.contents ~equal:(=) a
  let add self ~key ~value = 
    self.contents <- List.Assoc.add self.contents ~equal:(=) key value
end
