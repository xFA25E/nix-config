#!/usr/bin/awk -f

BEGIN {
    while (getline < "study.marks") {
        if ($3) {
            cfu[$1] = $2
            mark[$1] = $3
        }
    }

    print "subj time cfu mark m/h diff"
}

$4 in cfu {
    time[$4] += $5
}

END {
    for (s in time) {
        t = time[s]
        m = mark[s]
        c = cfu[s]
        print s, fmt_tm(t), c, m, (m - 17)/(t/3600), ((m - 17)/(t*((1/c) * 10))) * 100000
    }
}

function fmt_tm(secs)
{
    hrs = int(secs / 3600)
    secs = secs % 3600
    mins = int(secs / 60)
    secs = secs % 60
    return sprintf("%02d:%02d:%02d", hrs, mins, secs)
}
