# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit re core menhir yojson atdgen lwt git conduit cohttp irmin.0.9.3"
	 
ppa=avsm/ocaml42+opam12 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra libgmp3-dev opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
ocamlfind list
./build.sh



