open Prelude
open Small_string
(* TODO let default_filename = "/tmp/store" *)

(* https://www.npmjs.com/package/human-readable-ids *)
let strings = {|
strong-wolverine-94
nervous-dog-21
angry-lizard-51
jolly-cheetah-52
mean-insect-50
moody-eel-54
spotty-gecko-64
spicy-swan-55
great-bobcat-38
rare-bulldog-27
blue-wasp-24
red-falcon-19
pink-rat-17
tiny-snail-71
afraid-ladybug-58
happy-catfish-77
nice-chipmunk-91
clever-eagle-69
lucky-goose-76
little-sloth-8
calm-starfish-61
rotten-chicken-96
modern-yak-30
gentle-liger-92
light-cobra-0
terrible-dragon-46
dangerous-seahorse-67
white-zebra-80
bright-robin-86
tough-donkey-39
chilly-shrimp-87
heavy-elephant-23
nasty-skunk-93
fluffy-badger-9
new-dingo-98
giant-newt-95
orange-rattlesnake-63
tricky-kangaroo-15
unlucky-grasshopper-88
sharp-penguin-40
witty-jellyfish-16
lovely-emu-11
splendid-cat-82
good-pug-34
empty-mole-7
evil-quail-25
horrible-dragonfly-28
neat-ape-81
tidy-lion-43
warm-impala-72
stupid-turkey-35
slimy-panda-22
big-duck-48
curvy-puma-2
weak-otter-62
serious-goat-53
fat-cougar-12
silly-fish-78
proud-bullfrog-6
pretty-earwig-47
swift-deer-29
rude-monkey-14
average-mayfly-65
wise-turtle-57
massive-rabbit-75
grumpy-treefrog-56
short-bird-84
green-frog-18
selfish-squid-73
quick-walrus-99
ordinary-hound-68
silent-moose-66
shaggy-fox-90
breezy-husky-49
tasty-sheep-85
wicked-baboon-13
cuddly-lionfish-74
honest-tiger-37
odd-owl-60
fast-bear-26
fuzzy-stingray-97
smooth-dolphin-45
chatty-vampirebat-4
cold-mule-31
ugly-moth-5
stale-firefox-100
soft-warthog-83
sweet-fireant-10
quiet-mouse-33
bad-fly-20
dry-dodo-41
tall-horse-70
sour-crab-42
wonderful-cow-89
curly-bat-79
friendly-pig-44
tame-octopus-59
smart-panther-36
lazy-termite-3
helpless-snake-32 |} |> Tjr_string.split_on_all ~sub:"\n"

module Uncached = String_int_filestore.Uncached

let map_ops = Uncached.map_ops

let store = Uncached.from_file Test_config.default_filename true true

(* TODO use exhaustive *)
let test () = 
  Printf.printf "%s: " __MODULE__;
  flush_out();
  let xs = ref strings in
  let c = ref 1 in
  let m = ref Map_string.empty in
  let t = ref store in
  let _ = 
    while (!xs <> []) do
      print_string "."; flush_out();
      let (k,v) = (List.hd !xs, !c) in
      Test.log __LOC__;
      Test.log (Printf.sprintf "insert: %s %s" k (v|>string_of_int));
      begin 
        map_ops.insert (SS_.of_string k) v
        |> (fun f -> f !t) |> (function (t',Ok ()) -> t:=t') 
      end;
      m:=(Map_string.add k v !m);
      c:=!c+1;
      xs:=(List.tl !xs);
      ()
    done
  in
  (* check the bindings match *)
  !m|>Map_string.bindings|>List.iter (fun (k,v) ->
      (map_ops.find (SS_.of_string k) |> (fun f -> f !t) |> function (_,Ok res) -> assert (res = Some v));
      (map_ops.delete (SS_.of_string k) |>(fun f -> f !t) |> function (t',Ok ()) -> t:=t');
      ());
  Unix.close (!t).fd;
  ()
