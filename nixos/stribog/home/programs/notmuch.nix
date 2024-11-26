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

        # Delete all mails with deleted tag
        notmuch search --format=text0 --output=files tag:deleted | xargs -r0 rm -v

        # Tag drafts with their accounts
        notmuch tag +litkov -- tag:drafts AND from:/litkov.one/ AND NOT tag:litkov
        notmuch tag +polimi -- tag:drafts AND from:/polimi.it/ AND NOT tag:polimi

        # Create all folders and move mails to it
        for account in litkov polimi; do
          for folder in archive drafts flagged inbox sent spam trash; do
            cur=$account/$folder/cur
            mkdir -p "${maildir}/$cur"
            notmuch search --output=files --format=text0 tag:"$account" AND tag:"$folder" AND NOT "path:$cur/**" \
              | xargs -r0 -I '{}' mv -v '{}' "${maildir}/$cur"
          done
        done

      '';
      postNew = ''
        notmuch tag +drafts -- 'path:drafts/**'

        # litkov rules
        notmuch tag +litkov -- 'path:litkov/**'
        notmuch tag +archive -- 'path:litkov/archive/**'
        notmuch tag +drafts  -- 'path:litkov/drafts/**'
        notmuch tag +flagged -- 'path:litkov/flagged/**'
        notmuch tag +inbox  -- 'path:litkov/inbox/**'
        notmuch tag +sent -- 'path:litkov/sent/**'
        notmuch tag +spam  -- 'path:litkov/spam/**'
        notmuch tag +trash  -- 'path:litkov/trash/**'

        ## polimi forwarding
        notmuch tag +polimi -litkov -- tag:new AND tag:litkov AND tag:inbox AND to:/polimi.it/
        notmuch tag +polimi -litkov -- tag:new AND tag:litkov AND tag:inbox AND from:/polimi.it/

        # polimi rules
        notmuch tag +polimi -- 'path:polimi/**'
        notmuch tag +inbox -- 'path:polimi/inbox/**'
        notmuch tag +sent  -- 'path:polimi/sent/**'

        ## spam rules
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:politamtam
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"[eventi/events]"
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"open day"
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"Career Service"
        notmuch tag +spam -inbox -- tag:new AND tag:polimi AND from:"Career Service"

        ## ignore radar imaging
        notmuch tag +trash -inbox -- tag:new AND tag:polimi AND subject:"[Registrazioni/Recordings]" AND body:"GEOPHYSICAL AND RADAR IMAGING"

        ## mark subjcts
        notmuch tag +statistica -- tag:new AND tag:polimi AND "elio piazza"
        notmuch tag +analisi -- tag:new AND tag:polimi AND "maristella galeazzi"

        # after processing remove tag new
        notmuch tag -new -- tag:new
      '';
    };
  };
}
