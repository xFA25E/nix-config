{writeShellScriptBin}:
writeShellScriptBin "preparehd" ''
  set -eu

  PS3="Select mode: "
  select mode in uefi mbr; do
      if [[ $mode =~ ^(uefi|mbr)$ ]]; then
          break
      fi
      echo "Please, select 1 (uefi) or 2 (mbr)"
  done

  swap=""
  until [[ $swap =~ ^[0-9]+$ ]]; do
      read -r -p "How many GiB of SWAP: " swap
  done


  echo ">>> Partitioning $mode with encrypted root and ''${swap}GiB swap"
  case "$mode" in
      uefi)
          echo ">>> Creating gpt table..."
          sudo parted /dev/sda -- mklabel gpt
          echo ">>> Creating root partition..."
          sudo parted /dev/sda -- mkpart primary 512MiB 100%
          echo ">>> Creating boot partition..."
          sudo parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
          echo ">>> Setting boot flag..."
          sudo parted /dev/sda -- set 2 esp on
          ;;
      mbr)
          echo ">>> Creating msdos table..."
          sudo parted /dev/sda -- mklabel msdos
          echo ">>> Creating root partition..."
          sudo parted /dev/sda -- mkpart primary 512MiB 100%
          echo ">>> Creating boot partition..."
          sudo parted /dev/sda -- mkpart primary fat32 1MiB 512MiB
          echo ">>> Setting boot flag..."
          sudo parted /dev/sda -- set 2 boot on
          ;;
      *)
          echo ">>> Something went terribly wrong! Abort!"
          exit 1
          ;;
  esac

  echo ">>> Encrypting root partition with luks..."
  sudo cryptsetup luksFormat /dev/sda1 --label luks
  echo ">>> Opening luks..."
  sudo cryptsetup luksOpen /dev/sda1 enc-pv

  echo ">>> Creating physical volume..."
  sudo pvcreate /dev/mapper/enc-pv
  echo ">>> Creating volume group..."
  sudo vgcreate vg /dev/mapper/enc-pv
  echo ">>> Creating logical volume for swap..."
  sudo lvcreate -L "''${swap}G" -n swap vg
  echo ">>> Creating logical volume for root..."
  sudo lvcreate -l '100%FREE' -n root vg

  echo ">>> Formatting boot as fat32..."
  sudo mkfs.fat -F 32 -n boot /dev/sda2
  echo ">>> Formatting root as ext4..."
  sudo mkfs.ext4 -L root /dev/vg/root
  echo ">>> Formatting swap as, well, swap..."
  sudo mkswap -L swap /dev/vg/swap

  echo ">>> Mounting root to /mnt..."
  sudo mount /dev/vg/root /mnt
  echo ">>> Making /mnt/boot directory..."
  sudo mkdir -p /mnt/boot
  echo ">>> Mounting boot to /mnt/boot..."
  sudo mount /dev/disk/by-label/boot /mnt/boot
  echo ">>> Enabling swap..."
  sudo swapon /dev/vg/swap

  echo ">>> Now you can run something like the following"
  echo ">>> # nixos-install --flake github:xFA25E/nix-config#hostname"
''
