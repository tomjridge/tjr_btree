(* representation of devices *)
type id = int [@@deriving yojson]

type device = {
  name: string;
  id: id;
  path: string;
  block_size: int
} [@@deriving yojson]

let example_ = {name="example_device"; id=1; path="/tmp/example.store"; block_size=4096 }

let _ = Test.test (fun () ->
    Printf.printf "example device: %s\n" (example_ |> device_to_yojson |> Yojson.Safe.to_string);
    ()
  )

(* {"name":"example_device","path":"/tmp/example.store","block_size":4096} *)

type devices = device list [@@deriving yojson]

let load_devices fn = (
  fn |> Yojson.Safe.from_file |> devices_of_yojson
)


