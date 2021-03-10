pkgs: dir: ''
  defaults
  auth           on
  tls            on
  logfile        ${dir.cache}/msmtp.log

  account        exys
  host           mail.exys.it
  from           valeriy@exys.it
  user           valeriy@exys.it
  passwordeval   ${pkgs.pass}/bin/pass show mail/exys
  tls_certcheck  off

  account        polimi
  host           smtp.office365.com
  from           valeriy.litkovskyy@mail.polimi.it
  user           10622800@polimi.it
  passwordeval   ${pkgs.pass}/bin/pass show mail/polimi
''
