'''
Created on 21. nov. 2013

@author: ermias
'''

import subprocess
def html2rst(html):
    p = subprocess.Popen(['pandoc', '--from=html', '--to=rst'],
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    return p.communicate(html)[0]


def html2rstFile(html, rst):
    args = ['pandoc', '-f', 'html', '-t', 'rst', html, '-o', rst]
    subprocess.Popen(args)
    return 'html converted to rst and saved to: ' + rst


try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass


'''
from transform import transform
from docutils.writers.html4css1 import Writer
from docutils.core import default_description
from directives import Pygments

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources. Uses `pygments` to colorize the content of'
               '"code-block" directives. Needs an adapted stylesheet' 
               + default_description)

def main():
    return transform(writer=Writer(), part='html_body')

if __name__ == '__main__':
    print(main())

'''