final: prev: efinal: eprev:
eprev.transmission.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/xFA25E/transmission/commit/a16a3516a84bb496da4b313f7185300c3def0f41.diff";
        hash = "sha256-IarmPjyESW5SQWLgrZ3GU6SNBm/q6/2akr26CJyj7hc=";
      }
    ];
})
