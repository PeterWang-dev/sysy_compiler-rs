let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
  rustOverlay = fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";
  pkgs = import nixpkgs {
    overlays = [( import rustOverlay )];
  };
  rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./toolchain.toml;
  cross = pkgs.pkgsCross.riscv64-embedded;
in

pkgs.mkShellNoCC {
  packages = ( with pkgs; [
    gcc
    binutils
    clang
  ]) ++ ( with cross; [
    buildPackages.gcc
    buildPackages.binutils
  ]) ++ [
    rustToolchain
  ];

  RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
}