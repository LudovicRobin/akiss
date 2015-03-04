(** Compatibility with systems without Nproc *)

module Lwt = struct
  let return x = x
  let bind x f = f x
  let (>>=) = bind
  let wrap1 x = x
end

module Nproc = struct
  let create n = (), ()
  let submit () f x =
    try Some (f x)
    with e ->
      Printexc.print_backtrace stderr;
      Printf.eprintf
        "[err] Exception raised by task: %s\n"
        (Printexc.to_string e);
      None
end

module Lwt_unix = struct
  let run x = x
end

module Lwt_list = struct
  let map_p = List.map
  let filter_p = List.filter
end
