build:
	nix build .#canteen -L

run:
	nix run .#execute

format:
	smlfmt --force $(find ./ -type f \( -iname \*.sml \))
