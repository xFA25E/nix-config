#!/usr/bin/awk -f

{
    table[$4] += $5
}

END {
    for (key in table) {
        data = table[key]
        hrs = int(data / 3600)
        data = data % 3600
        mins = int(data / 60)
        secs = data % 60
        printf("%s %02d:%02d:%02d\n", key, hrs, mins, secs)
    }
}
