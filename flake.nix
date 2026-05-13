{
  description = "Causal Forest OHIE replication and paper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          tex = pkgs.texlive.combine {
            inherit (pkgs.texlive)
              scheme-small
              latexmk
              biber
              biblatex
              csquotes
              collection-latexrecommended
              collection-fontsrecommended
              koma-script;
          };

          r = pkgs.rWrapper.override {
            packages = with pkgs.rPackages; [
              fs
              grf
              haven
              here
              knitr
              patchwork
              arrow
              rmarkdown
              recipes
              scales
              tidyverse
              xtable
            ];
          };
        in
        {
          tex = tex;
          r = r;
          default = tex;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          tex = self.packages.${system}.tex;
          r = self.packages.${system}.r;
          paperPdf = "artifacts/causal-forest-ohie-paper.pdf";
        in
        {
          paper = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "build-causal-forest-ohie-paper" ''
                set -euo pipefail
                export PATH="${tex}/bin:${pkgs.bash}/bin:${pkgs.coreutils}/bin:$PATH"
                latexmk -C paper/main.tex
                rm -f paper/main.run.xml
                mkdir -p artifacts
                cd paper
                latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
                cd ..
                cp paper/main.pdf ${paperPdf}
              ''
            );
          };

          analysis = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "run-causal-forest-ohie-analysis" ''
                set -euo pipefail
                export PATH="${r}/bin:${pkgs.bash}/bin:${pkgs.coreutils}/bin:$PATH"
                Rscript scripts/run-analysis.R
              ''
            );
          };

          data = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "prepare-causal-forest-ohie-data" ''
                set -euo pipefail
                export PATH="${r}/bin:${pkgs.bash}/bin:${pkgs.coreutils}/bin:$PATH"
                Rscript scripts/prepare-data.R
              ''
            );
          };

          check-analysis = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "check-causal-forest-ohie-analysis" ''
                set -euo pipefail
                export PATH="${r}/bin:${pkgs.bash}/bin:${pkgs.coreutils}/bin:$PATH"
                Rscript scripts/check-analysis.R
              ''
            );
          };

          notebooks = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "render-causal-forest-ohie-notebooks" ''
                set -euo pipefail
                export PATH="${r}/bin:${pkgs.bash}/bin:${pkgs.coreutils}/bin:$PATH"
                Rscript scripts/render-notebooks.R
              ''
            );
          };

          count-words = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "count-causal-forest-ohie-paper-words" ''
                set -euo pipefail
                export PATH="${pkgs.poppler-utils}/bin:${pkgs.coreutils}/bin:$PATH"
                pdftotext ${paperPdf} - | wc -w
              ''
            );
          };

          default = self.apps.${system}.paper;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              self.packages.${system}.tex
              self.packages.${system}.r
              pkgs.pandoc
              pkgs.poppler-utils
            ];
          };
        }
      );
    };
}
