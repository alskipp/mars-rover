# mars-rover

[Nix](https://nixos.org/download.html#nix-install-linux) is a requirement for dependency management

---

### Live edit Mars rovers!

Ensure `mars-rover.hs` is executable with:

`chmod u+x mars-rover.hs`

##### Now run it with:

`./mars-rover.hs`

- On first run Nix will install all dependencies then run the file which will print the result of running tests.
- You can now edit the file `mars-rover.hs` and the tests will automatically re-run in the terminal every time you save the file.

---

### Compile binary and parse input files

Open a Nix shell with the required dependencies:

```
nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hspec p.hspec-megaparsec p.megaparsec])"
```

##### Then compile with:

`ghc mars-rover.hs`

- A binary (`mars-rover`) will be created.
- If you pass the file path as an arg to the binary it will parse and run the simulation.

##### Parse and run the example file with:

`./mars-rover $(readlink -f example.txt)`

Output should look like:

```
(4, 4, E)
(0, 4, W) Lost
(2, 3, W)
(1, 0, S) Lost
```
