pkgs: dir: ''
  defaults
  tls           on
  uidls_file    ${dir.cache}/mpop/%U_at_%H

  account       exys
  delivery      maildir ${dir.mail}/EXYS
  host          mail.exys.it
  user          valeriy@exys.it
  passwordeval  ${pkgs.pass}/bin/pass show mail/exys
  tls_certcheck off

  account       polimi
  delivery      maildir ${dir.mail}/POLIMI
  host          outlook.office365.com
  user          10622800@polimi.it
  passwordeval  ${pkgs.pass}/bin/pass show mail/polimi
''
