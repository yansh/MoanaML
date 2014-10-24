# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit re core sqlite3"
	 
ppa=avsm/ocaml41+opam11 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
./build.sh
