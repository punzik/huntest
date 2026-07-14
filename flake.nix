{
  description = "Huntest - HDL testbench launcher";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      mkHuntest = pkgs:
        pkgs.stdenv.mkDerivation rec {
          pname = "huntest";
          version = "0.2.0";

          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./huntest
              ./source
            ];
          };

          strictDeps = true;

          nativeBuildInputs = [ pkgs.guile ];
          buildInputs = [ pkgs.guile ];

          dontConfigure = true;
          dontBuild = true;
          dontPatchELF = true;
          dontStrip = true;

          installPhase = ''
            runHook preInstall

            install -Dm755 huntest "$out/bin/huntest"
            install -Dm644 source/huntest.scm "$out/${pkgs.guile.siteDir}/huntest.scm"
            install -Dm644 source/huntest/iverilog.scm "$out/${pkgs.guile.siteDir}/huntest/iverilog.scm"
            install -Dm644 source/huntest/sby.scm "$out/${pkgs.guile.siteDir}/huntest/sby.scm"

            sed -i '5,7d' "$out/bin/huntest"
            sed -i '5i(set! %load-compiled-path (cons "'"$out/${pkgs.guile.siteCcacheDir}"'" %load-compiled-path))' "$out/bin/huntest"
            sed -i '5i(add-to-load-path "'"$out/${pkgs.guile.siteDir}"'")' "$out/bin/huntest"

            patchShebangs "$out/bin/huntest"
            sed -i '1c#!${pkgs.guile}/bin/guile --no-auto-compile' "$out/bin/huntest"

            export GUILE_AUTO_COMPILE=0
            install -dm755 "$out/${pkgs.guile.siteCcacheDir}/huntest"
            guild compile -L "$out/${pkgs.guile.siteDir}" \
              -o "$out/${pkgs.guile.siteCcacheDir}/huntest.go" \
              "$out/${pkgs.guile.siteDir}/huntest.scm"
            guild compile -L "$out/${pkgs.guile.siteDir}" \
              -o "$out/${pkgs.guile.siteCcacheDir}/huntest/iverilog.go" \
              "$out/${pkgs.guile.siteDir}/huntest/iverilog.scm"
            guild compile -L "$out/${pkgs.guile.siteDir}" \
              -o "$out/${pkgs.guile.siteCcacheDir}/huntest/sby.go" \
              "$out/${pkgs.guile.siteDir}/huntest/sby.scm"

            runHook postInstall
          '';

          meta = {
            description = "HDL testbench launcher and Guile library";
            homepage = "https://github.com/punzik/huntest";
            license = pkgs.lib.licenses.mit;
            mainProgram = "huntest";
            platforms = pkgs.lib.platforms.unix;
          };
        };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          huntest = mkHuntest pkgs;
        in
        {
          inherit huntest;
          default = huntest;
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.huntest}/bin/huntest";
          meta.description = "Run Huntest";
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          huntest = self.packages.${system}.huntest;
        in
        {
          default = pkgs.mkShell {
            packages = [
              huntest
              pkgs.guile
            ];

            # Allow .hut scripts to import the packaged Huntest modules.
            GUILE_LOAD_PATH = "${huntest}/${pkgs.guile.siteDir}";
          };
        });
    };
}
