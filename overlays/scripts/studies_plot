#!/usr/bin/awk -f

BEGIN {
    DAY = 24 * 60 * 60
    HOUR = 3600.0
    PROGRAM = "gnuplot --persist"
    DATE_FORMAT = "%Y/%m/%d"

    BEGIN_STR = "\
set xdata time\n\
set timefmt \"" DATE_FORMAT "\"\n\
plot \"-\" using 1:2 title \"Study hours\" with linespoints"

    END_STR = "e\nquit"

    print BEGIN_STR | PROGRAM
}

NR == 1 {
    prev_sec = current_seconds()
    prev_val = $5
    next
}

prev_sec == current_seconds() {
    prev_val += $5
    next
}

{
    curr_sec = current_seconds()
    output_data(prev_sec, prev_val)

    for (i = prev_sec + DAY; i < curr_sec; i += DAY) {
        output_data(i, 0)
    }

    prev_sec = curr_sec
    prev_val = $5
}

END {
    output_data(prev_sec, prev_val)
    print END_STR | PROGRAM
    close(PROGRAM)
}

function current_seconds()
{
    return mktime($1 " " $2 " " $3 " 00 00 00")
}

function output_data(seconds, value_seconds)
{
    print strftime(DATE_FORMAT, seconds), value_seconds / HOUR | PROGRAM
}
