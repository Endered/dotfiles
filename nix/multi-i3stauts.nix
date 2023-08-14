{
  lib,
  rustPlatform,
  fetchFromGitHub,
  ...
}:
rustPlatform.buildRustPackage rec {
  name = "multi-i3status";
  src = fetchFromGitHub {
    owner = "Endered";
    repo = name;
    rev = "09c4dfd981b6cf1cdb3e67c186b349b1e836c20d";
    sha256 = "sha256-BkLsVB+zeauWkojeQsX9UuSs7JPS2xAAF5bc++kUCuE=";
  };
  cargoHash = "sha256-WPfeXDA63KVzyW/V4xSnWlup4T9x/DFQ8dfzy7NI9hM=";
}
