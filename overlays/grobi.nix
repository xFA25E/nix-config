final: prev: grobi:
prev.grobi.overrideAttrs (_: {
  src = grobi;
  vendorHash = "sha256-3hyI5oHV8qEkIsF6pk1xx1H98Wx+Ug/Z2IswVbzIQLQ=";
  patches = map final.fetchpatch [
    {
      url = "https://github.com/fd0/grobi/pull/30.diff";
      hash = "sha256-eSCCfsRuBdNqqd8EF4lhRXCFS1WzXGeOJiP/+h7p1Vk=";
    }
  ];
})
