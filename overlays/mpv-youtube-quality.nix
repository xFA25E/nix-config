self: super: {
  mpvYoutubeQuality = super.fetchFromGitHub {
    name = "mpv-youtube-quality";
    owner = "jgreco";
    repo = "mpv-youtube-quality";
    rev = "1f8c31457459ffc28cd1c3f3c2235a53efad7148";
    sha256 = "09z6dkypg0ajvlx02270p3zmax58c0pkqkh6kh8gy2mhs3r4z0xy";
  };
}
