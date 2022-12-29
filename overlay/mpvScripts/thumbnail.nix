final: prev:
prev.mpvScripts.thumbnail.overrideAttrs ({postPatch ? "", ...}: {
  postPatch =
    postPatch
    + ''
      sed -r -i src/options.lua -e '/autogenerate = true/ s/true/false/g'
    '';
})
