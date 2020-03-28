#!/usr/bin/awk -f

{
    key = $1 " " $2 " " $3 " " $4
    value = $5
    table[key] += value
}

END {
    OLD_FILENAME = FILENAME
    NEW_FILENAME = OLD_FILENAME ".bak"
    COMMAND = "mv " NEW_FILENAME " " OLD_FILENAME

    n = asorti(table, keys)

    for (i = 1; i <= n; i++) {
        print keys[i], table[keys[i]] > NEW_FILENAME
    }

    close(NEW_FILENAME)
    system(COMMAND)
}
