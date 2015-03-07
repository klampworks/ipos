open Printf

let () =
  if Array.length Sys.argv <= 1 then
    failwith "usage: input.mp3";
  let filename = Sys.argv.(1) in
  let bits = Bitstring.bitstring_of_file filename in

  bitmatch bits with
  | { ("ID3") : 3*8 : string;
      major_version : 8;
      minor_version : 8;

      flag_a        : 1 ; (* Unsynchronisation *)
      flag_b        : 1 ; (* Extended header *)
      flag_c        : 1 ; (* Experimental indicator *)
      flag_d        : 1 ; (* Footer present *)
      _             : 4 ;
      size          : 32 : bigendian
      } ->
      printf "Filename:                    %s\n" filename;
      printf "Version:                     %d.%d\n" major_version minor_version;
      printf "Flag Unsynchronisation:      %b\n" flag_a;
      printf "Flag Extended header:        %b\n" flag_b;
      printf "Flag Experimental indicator: %b\n" flag_c;
      printf "Flag Footer present:         %b\n" flag_d;
      printf "Header Size:                 %d\n" (Int32.to_int size);
