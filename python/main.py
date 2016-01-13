from ParseWikipediaXML import Parser
import sys

import psycopg2
from functools import reduce

def myWriteIntoHstore(self, dictBofw, title):

    docCount=sum(dictBofw.values())

    if docCount >= self.args.minw and docCount <= self.args.maxw:

        # Make string from list of tuples of bag-of-words
        cont = reduce(
            lambda freq, keyval: freq+keyval[0]+" => "+str(keyval[1])+",",
            [ (key, val) for key, val in dictBofw.items() if val >= self.args.minc ],
            ""
        ).rstrip(",")

        if len(cont) > 1:

            cur = self.db_conn.cursor()
            cur.execute(
                "INSERT INTO output (title, bagofwords) VALUES( '{0}', '{1}' )".format(
                    title.replace('\'','\'\''), cont.replace('\'','\'\'')
                )
            )
            conn.commit()
            cur.close()

            return True

    return False

conn = psycopg2.connect("dbname=testdb host=localhost user=test password=test")

parser = Parser(sys.argv).new()
parser.db_conn = conn
parser.post_process = myWriteIntoHstore
parser.readDictionary()
parser.startParse()

