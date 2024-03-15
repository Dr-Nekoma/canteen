build:
	nix build .#canteen

run:
	nix run .#canteen

format:
	smlfmt --force $(find ./ -type f \( -iname \*.sml \))
