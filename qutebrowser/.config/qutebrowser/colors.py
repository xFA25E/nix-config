from subprocess import check_output

def get_color(resource):
    return (check_output(["xgetres", "qutebrowser." + resource])
            .strip().decode('utf-8'))

def make_colors():
    result = {}
    keys = ["BG", "BGH", "SEC", "FG", "FGH", "white", "red", "orange",
            "yellow", "green", "cyan", "blue", "violet", "magenta"]

    for key in keys:
        result[key] = get_color(key)

    return result

colors = make_colors()
