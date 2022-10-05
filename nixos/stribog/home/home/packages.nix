{pkgs, ...}: {
  home.packages = with pkgs; [
    acpi
    alsaUtils
    amded
    ascii
    bind
    binutils
    brave
    brave-incognito
    brightnessctl
    browser
    calibre
    cloc
    cpulimit
    discord
    djvulibre
    dmenu
    exiftool
    fd
    ffmpeg
    file
    filename_put_duration
    format_seconds
    ghostscript
    gimp
    go-mtpfs
    hunspell
    hunspellDicts.en_US-large
    hunspellDicts.it_IT
    hunspellDicts.ru_RU
    image-dired-external-viewer
    image_clipboard
    imagemagick
    iw
    ledger
    leiningen
    libjpeg
    libnotify
    libreoffice
    make_backup
    mediainfo
    mkpasswd
    mpc_cli
    mpvi
    nload
    notifiers
    p7zip
    pandoc
    parted
    pdftk
    perlPackages.JSONPP
    pinentry
    pueue
    pulsemixer
    pwgen
    qrencode
    qrshow
    rar
    recode_video
    resize_video
    ripgrep
    rsync
    scrot
    sdcv
    shellcheck
    simplescreenrecorder
    sort_videos_by_duration
    speedtest-cli
    stalonetray
    strip_video
    stumpwm
    sxiv
    teams
    tor-browser-bundle-bin
    transmission
    unzip
    video_seconds
    wget
    woof
    xclip
    xdg-user-dirs
    xkb-switch
    xterm
    xz
    yt-dlp
    ytdl
    ytdla
    ytdlam
    ytdli
    ytdlp
    ytdlpa
    ytdlpam
    zip
    zoom-us
  ];
}
