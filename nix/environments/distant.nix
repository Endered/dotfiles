{ config ,lib, pkgs, ...}:
let
  distant-source = pkgs.fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = "distant";
    rev = "3fe1fba339403f6162730ad082aafe424a6795ad";
    sha256 = "sha256-FDDpiK9K7q+8M/1ue+GIbTjBP36gpJ6llb6fZICC3hg=";
  };
  distant = pkgs.rustPlatform.buildRustPackage rec {
    name = "distant";
    src = distant-source;
    cargoHash = "sha256-HEyPfkusgk8JEYAzIS8Zj5EU0MK4wt4amlsJqBEG/Kc=";
    nativeBuildInputs = [ pkgs.perl ];
    doCheck = false;
  };
  cfg = config.my-settings.distant;
in {
  options.my-settings.distant = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = {
    home.packages = lib.mkIf (!cfg.disable) [
      distant
    ];
  };
}
