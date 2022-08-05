#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

main :: IO ()
main = putStrLn "Hello Mars!"
