{
  config,
  pkgs,
  ...
}: let
  maildir = config.accounts.email.maildirBasePath;
in {
  programs.notmuch = {
    enable = true;
    new.tags = ["new"];
    search.excludeTags = ["trash" "spam" "deleted"];
    hooks = {
      preNew = ''
        export PATH="${pkgs.findutils}/bin''${PATH:+:}$PATH"

        notmuch search --format=text0 --output=files tag:deleted | xargs -r0 rm -v

        # polimi rules

        mkdir -p '${maildir}'/polimi/{inbox,sent,all}/cur

        ## all tags sent should be in polimi/sent
        notmuch search --output=files --format=text0 tag:polimi AND tag:sent AND NOT 'path:polimi/sent/cur/**' \
          | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/sent/cur'

        ## all tags archive/trash/spam should be in polimi/all
        notmuch search --output=files --format=text0 tag:polimi AND '(tag:archive OR tag:trash OR tag:spam)' AND NOT 'path:polimi/all/cur/**' \
          | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/all/cur'

        ## all tags inbox/flagged should be in polimi/inbox
        notmuch search --output=files --format=text0 tag:polimi AND '(tag:inbox OR tag:flagged)' AND NOT 'path:polimi/inbox/cur/**' \
          | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/inbox/cur'

      '';
      postNew = ''
        # general

        notmuch tag +draft -- 'path:drafts/**'

        # polimi rules

        notmuch tag +polimi -- 'path:polimi/**'
        notmuch tag +inbox -- 'path:polimi/inbox/**'
        notmuch tag +sent  -- 'path:polimi/sent/**'

        ## spam rules

        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:politamtam
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"[eventi/events]"
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"open day"

        # after processing remove tag new

        notmuch tag -new -- tag:new

      '';
    };
  };
}
