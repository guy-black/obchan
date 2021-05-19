{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
let deps = obelisk.nixpkgs.thunkSet ./dep;
    haskellLib = obelisk.nixpkgs.haskell.lib;
in with obelisk;
project ./. ({ ... }: {
  packages = {
    database-id-class = deps.database-id + /class;
    database-id-obelisk = deps.database-id + /obelisk;
  };
  overrides = self: super: import dep/gargoyle self // {
    aeson-gadt-th = (self.callCabal2nix "aeson-gadt-th" deps.aeson-gadt-th {}).overrideAttrs (drv: {
      configureFlags = drv.configureFlags or [] ++ ["-f-build-readme"]; # Upstream issue: readme doesn't build on ios.
    });
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})



###default setup###
# with obelisk;
# project ./. ({ ... }: {
#   android.applicationId = "systems.obsidian.obelisk.examples.minimal";
#   android.displayName = "Obelisk Minimal Example";
#   ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
#   ios.bundleName = "Obelisk Minimal Example";
# })
