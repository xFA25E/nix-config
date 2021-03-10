pkgs: ''
  defaults
  tls           on
  uidls_file    ~/.cache/mpop/%U_at_%H

  account       exys
  delivery      maildir ~/.mail/EXYS
  host          mail.exys.it
  user          valeriy@exys.it
  passwordeval  ${pkgs.pass}/bin/pass show mail/exys
  tls_certcheck off

  account       polimi
  delivery      maildir ~/.mail/POLIMI
  host          outlook.office365.com
  user          10622800@polimi.it
  passwordeval  ${pkgs.pass}/bin/pass show mail/polimi
''
