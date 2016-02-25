# -*- coding: utf-8 -*-

from __future__ import print_function
import re
import threading
from ParseWikipediaXML.funcs import *

class BofwThread(threading.Thread):

    # static variables
    lock = threading.Lock()
    pages = 0
    saved = 0

    def __init__(self, parser, idx):
        super(BofwThread,self).__init__()
        self.parser = parser
        self.idx = idx

    def run(self):

        args = self.parser.args
        queue = self.parser.queue

        while True:

            page = queue.get()

            # This may finish main thread before parseText()
            #queue.task_done()

            if page == "Finished":

                # Be sure that this can let queue.join() go
                queue.task_done()
                break

            elif re.search(args.recateg, page):

                titlema = re.search("<title[^<>]*>([^<>]+)</title>",page)
                textma = re.search("<text[^<>]*>([^<>]+)</text>",page)

                if titlema and textma:

                    title = titlema.group(1)
                    text = textma.group(1)

                    dictBofw = self.parser.parseText(text)

                    if self.parser.post_process(self.parser, dictBofw, title):
                        BofwThread.saved+=1

            self.report()

            # put() counts up and task_done() counts down
            queue.task_done()


    def report(self):
        m = " > Execute %(class)s" % { "class": self.__class__.__name__ }

        BofwThread.lock.acquire()
        BofwThread.pages += 1
        print( "%(message)s [#page(saved/parsed) %(saved)s/%(count)s @ thread %(index)s]" %
                { "message" : m,
                  "count"   : BofwThread.pages,
                  "saved"   : BofwThread.saved,
                  "index"   : self.idx
                },
                "\r", end="" )
        BofwThread.lock.release()

