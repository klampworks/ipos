open Printf

type id3_header = {
  ma_ver : int; 
  mi_ver : int;
  flag_a : bool;
  flag_b : bool;
  flag_c : bool;
  flag_d : bool;
  size : int}

let parse_id3_header bits =
  bitmatch bits with
  | { ("ID3") : 3*8 : string;
    major_version : 8;
    minor_version : 8;
    flag_a        : 1; (* Unsynchronisation *)
    flag_b        : 1; (* Extended header *)
    flag_c        : 1; (* Experimental indicator *)
    flag_d        : 1; (* Footer present *)
    _             : 4;
    size          : 32 : bigendian
    } -> (bits, 
          { ma_ver = major_version; 
            mi_ver = minor_version; 
            flag_a = flag_a; 
            flag_b = flag_b; 
            flag_c = flag_c; 
            flag_d = flag_d; 
            size = (Int32.to_int size)});;

let get_id3_header_size head =
  let top_size = 0 in
  let footer_size = 10 in
    if head.flag_b
      then head.size + top_size + footer_size
      else head.size + top_size;;

let print_id3_header head =
  printf "Version:                     %d.%d\n" head.ma_ver head.mi_ver;
  printf "Flag Unsynchronisation:      %b\n" head.flag_a;
  printf "Flag Extended header:        %b\n" head.flag_b;
  printf "Flag Experimental indicator: %b\n" head.flag_c;
  printf "Flag Footer present:         %b\n" head.flag_d;
  printf "Header Size:                 %d\n" head.size;
  printf "Full Size:                   %d\n" (get_id3_header_size head);;

let () =
  if Array.length Sys.argv <= 1 then
    failwith "usage: input.mp3";
  let filename = Sys.argv.(1) in
  let bits = Bitstring.bitstring_of_file filename in
  print_id3_header(snd (parse_id3_header bits));;

