(* Lemonade_Stream -- Monadic streams

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

exception Empty

module type S =
sig
  type 'a t
  type (+ 'a) monad
  val from : (int -> 'a option monad) -> 'a t
  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val of_string : string -> char t
  val to_list : 'a t -> 'a list monad
  val to_string : char t -> string monad
  val peek : 'a t -> 'a option monad
  val npeek : int -> 'a t -> 'a list monad
  val get : 'a t -> 'a option monad
  val nget : int -> 'a t -> 'a list monad
  val get_while : ('a -> bool) -> 'a t -> 'a list monad
  val next : 'a t -> 'a monad
  val junk : 'a t -> unit monad
  val njunk : int -> 'a t -> unit monad
  val junk_while : ('a -> bool) -> 'a t -> unit monad
  val is_empty : 'a t -> bool monad
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_list : ('a -> 'b list) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b monad
  val iter : ('a -> unit) -> 'a t -> unit monad
  val find : ('a -> bool) -> 'a t -> 'a option monad
  val find_map : ('a -> 'b option) -> 'a t -> 'b option monad
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val flatten : 'a list t -> 'a t
end

module Make(Monad:Lemonade_Type.S) =
struct
  open Monad.Infix

  type 'a monad =
    'a Monad.t

  type 'a t =
    'a cell option monad
  and 'a cell = {
    mutable count : int;
    mutable data : 'a data;
  }
  and 'a data =
    | Sempty
    | Scons of 'a * 'a data
    | Sgen of 'a gen
  and 'a gen = {
    mutable curr : 'a option option;
    func : int -> 'a option monad;
  }

  let _count = function
    | None -> 0
    | Some { count } -> count

  let _data = function
    | None -> Sempty
    | Some { data } -> data

  let from f =
    Monad.return(Some {
        count = 0;
        data = Sgen { curr = None; func = f };
      })

  let of_list lst =
    Monad.return (Some {
        count = 0;
        data = List.fold_right (fun x l -> Scons (x, l)) lst Sempty;
      })

  let of_array a =
    let count = ref 0 in
    from
      (fun _ ->
         let c = !count in
         if c < Array.length a
         then (incr count; Monad.return(Some a.(c)))
         else Monad.return None)

  let of_string a =
    let count = ref 0 in
    from
      (fun _ ->
         let c = !count in
         if c < String.length a
         then (incr count; Monad.return(Some a.[c]))
         else Monad.return None)

  let rec get_data : type v. int -> v data -> v data monad = fun count d ->
    match d with
    (* Returns either Sempty or Scons(a, _) even when d is a generator
       or a buffer. In those cases, the item a is seen as extracted from
       the generator/buffer.
       The count parameter is used for calling `Sgen-functions'.  *)
    | Sempty
    | Scons (_, _) -> Monad.return d
    | Sgen {curr = Some None; func = _ } -> Monad.return Sempty
    | Sgen({curr = Some(Some x); func = f } as g) ->
        g.curr <- None;
        Monad.return (Scons(x, d))
    | Sgen({ curr = None; func = f} as g) ->
        Monad.bind (g.func count)
          (function
            | None -> g.curr <- Some None; Monad.return Sempty
            | Some x -> Monad.return(Scons(x, d)))

  let rec peek_data : type v. v cell -> v option monad = fun s ->
    (* consult the first item of s *)
    match s.data with
    | Sempty -> Monad.return None
    | Scons (x, _) -> Monad.return (Some(x))
    | Sgen {curr = Some x} -> Monad.return x
    | Sgen g ->
        Monad.bind (g.func s.count)
          (fun x -> g.curr <- Some x; Monad.return x)

  let peek m = Monad.bind m
      (function
        | None -> Monad.return None
        | Some s -> peek_data s)

  let rec junk_data : type v. v cell -> unit monad = fun s ->
    match s.data with
    | Scons (_, d) ->
        Monad.return(s.count <- (succ s.count); s.data <- d)
    | Sgen ({curr = Some _} as g) ->
        Monad.return(s.count <- (succ s.count); g.curr <- None)
    | _ ->
        Monad.bind (peek_data s)
          (function
            | None -> Monad.return ()
            | Some _ -> junk_data s)

  let junk m = Monad.bind m
      (function
        | None -> Monad.return ()
        | Some data -> junk_data data)


  let get m =
    Monad.bind (peek m)
      (function
        | Some(a) -> junk m >>= fun () -> Monad.return(Some(a))
        | None -> Monad.return None)

  let rec nget_data n s =
    if n <= 0 then
      Monad.return ([], s.data, 0)
    else
      Monad.bind (peek_data s)
        (function
          | None ->
              Monad.return([], s.data, 0)
          | Some a ->
              junk_data s >>=
              fun () ->
              nget_data (pred n) s >>=
              fun (al, d, k) ->
              Monad.return(a :: al, Scons (a, d), succ k))

  let nget n m =
    Monad.bind m
      (function
        | None -> Monad.return []
        | Some d ->
            nget_data n d
            >>= fun (al, _, len) ->
            if len < n then
              raise Empty
            else
              Monad.return al)

  let npeek_data n s =
    nget_data n s
    >>= fun (al, d, len) ->
    s.count <- (s.count - len);
    s.data <- d;
    Monad.return al

  let npeek n m =
    Monad.bind m
      (function
        | None -> Monad.return []
        | Some d -> npeek_data n d)

  let next s =
    peek s >>= function
    | Some a -> (junk s >>= fun () -> Monad.return a)
    | None -> raise Empty

  let get_while p m =
    let rec loop ax =
      Monad.bind (peek m)
        (function
          | Some a ->
              (junk m >>= fun () ->
               if p a then loop (a :: ax) else Monad.return (List.rev ax))
          | None -> Monad.return (List.rev ax))
    in
    loop []

  let junk_while p m =
    let rec loop () =
      Monad.bind (peek m)
        (function
          | Some a ->
              if p a then junk m >>= loop else Monad.return ()
          | None ->
              Monad.return ())
    in
    loop ()

  let is_empty m =
    Monad.bind (peek m)
      (function
        | Some(_) -> Monad.return false
        | None -> Monad.return true)

  let map f m =
    let f _ =
      peek m >>= function
      | Some a -> (junk m >>= fun () -> Monad.return(Some (f a)))
      | None -> Monad.return None
    in
    from f

  let map_list f m =
    let page = ref [] in
    let rec loop n =
      match !page with
      | [] ->
          Monad.bind (get m)
            (function
              | Some(a) -> page := f a; loop n
              | None -> Monad.return None)
      | hd :: tl -> page := tl; Monad.return (Some hd)
    in
    from loop

  let filter p m =
    let not_p x =
      not(p x)
    in
    from (fun _ -> junk_while not_p m >>= fun () -> peek m)

  let filter_map f m =
    let rec next serial =
      Monad.bind (get m)
        begin function
          | Some(a) -> begin match f a with
              | Some(x) -> Monad.return(Some x)
              | None -> next serial
            end
          | None -> Monad.return None
        end
    in
    from next

  let flatten m =
    map_list (fun lst -> lst) m

  let append m1 m2 =
    let m = ref m1 in
    let rec loop n =
      Monad.bind (get !m)
        (function
          | (Some _) as x -> Monad.return x
          | None ->
              if !m == m2 then
                Monad.return None
              else
                (m := m2; loop n))
    in
    from loop

  let concat m_top =
    let m = ref (from (fun _ -> Monad.return None)) in
    let rec loop n =
      Monad.bind (get !m)
        (function
          | (Some _) as x -> Monad.return x
          | None ->
              Monad.bind (get m_top)
                (function
                  | Some(nextm) ->
                      (m := nextm; loop n)
                  | None -> Monad.return None))
    in
    from loop


  let combine m1 m2 =
    let rec loop _ =
      Monad.bind (get m1)
        (function
          | Some a ->
              Monad.bind (get m2)
                (function
                  | Some b -> Monad.return (Some(a, b))
                  | None -> Monad.return None)
          | None -> Monad.return None)
    in
    from loop

  let fold f m ax0 =
    let rec loop ax =
      Monad.bind (get m)
        (function
          | Some(x) -> loop (f x ax)
          | None -> Monad.return ax)
    in
    loop ax0

  let iter f m =
    let rec loop () =
      Monad.bind (get m)
        (function
          | Some(a) -> f a; loop ()
          | None -> Monad.return ())
    in
    loop ()

  let find p m =
    let rec loop () =
      Monad.bind (get m)
        (function
          | Some(a) -> if p a then Monad.return(Some a) else loop()
          | None -> Monad.return None)
    in
    loop ()

  let find_map f m =
    let rec loop () =
      Monad.bind (get m)
        (function
          | Some(a) ->
              (match f a with
               | Some _ as x -> Monad.return x
               | None -> loop ())
          | None -> Monad.return None)
    in
    loop ()

  let rec njunk n m =
    if n <= 0 then
      Monad.return ()
    else
      Monad.bind (junk m)
        (fun () -> njunk (pred n) m)

  let to_list m =
    let rec loop ax =
      Monad.bind (get m)
        (function
          | Some x -> loop (x :: ax)
          | None -> Monad.return(List.rev ax))
    in
    loop []

  let to_string m =
    let b = Buffer.create 100 in
    let rec loop () =
      Monad.bind (get m)
        (function
          | Some c -> Buffer.add_char b c; loop ()
          | None -> Monad.return (Buffer.contents b))
    in
    loop ()
end
