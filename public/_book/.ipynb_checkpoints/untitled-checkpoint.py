def replace_md(file, find, replace):
    with open(file, encoding="utf8") as f:
        s = f.read()
        s = s.replace(find, replace)
    with open(file,'w', encoding="utf8") as f:
        f.write(s)

import glob, os
for file in glob.glob("*.md"):
    replace_md(file = file, find = '<img src="', replace = '![](')
    replace_md(file = file, find = '" width="672" />', replace = ')\n\n')