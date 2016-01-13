from ParseWikipediaXML import Parser
import sys
import psycopg2

from functools import cmp_to_key, reduce
import re

def myWriteToFile(self, dictBofw, title):

    docCount=sum(dictBofw.values())

    conn = psycopg2.connect("dbname=testdb host=localhost user=test password=test")
    cur = conn.cursor()

    code = False
    if docCount >= self.args.minw and docCount <= self.args.maxw:

        # Make string from list of tuples of bag-of-words
        cont = reduce(
            lambda freq, keyval: freq+keyval[0]+" => "+str(keyval[1])+",",
            [ (key, val) for key, val in dictBofw.items() if val >= self.args.minc ],
            ""
        ).rstrip(",")

        if len(cont) > 1:
            cur.execute(
                "INSERT INTO output (title, bagofwords) VALUES( '{0}', '{1}' )".format(
                    title.replace('\'','\'\''), cont.replace('\'','\'\'')
                )
            )
            conn.commit()
            code = True

    cur.close()
    conn.close()

    return code

parser = Parser(sys.argv).new()
parser.post_process = myWriteToFile
parser.readDictionary()
parser.startParse()

