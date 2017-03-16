(* a root is a pair of a device and a block ref *)

module Root = struct 
  type t = { 
    name:string; 
    dev:Device.id; 
    blkref: int;
    type_: string;
  } [@@deriving yojson]
end

module Root_map = struct
  type t = {
    devices:Device.devices;
    roots: Root.t list;
  } [@@deriving yojson]
end

let from_file fn = (
  fn |> Yojson.Safe.from_file |> Root_map.of_yojson 
)

let main () = (
  Root_map.{
    devices=[Device.example_];
    roots=Root.[
        {name="example_root"; 
         dev=Device.example_.id; 
         blkref=2;
         type_="virtual block device";
        }]
  }
  |> Root_map.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline
)
      
            
let _ = main ()

(*
{
  "devices": [
    {
      "name": "example_device",
      "id": 1,
      "path": "/tmp/example.store",
      "block_size": 4096
    }
  ],
  "roots": [ { "name": "example_root", "dev": 1, "blkref": 2; "type_":"virtual block device" } ]
}

*)
