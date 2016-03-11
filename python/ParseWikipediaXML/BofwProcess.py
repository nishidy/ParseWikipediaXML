# -*- coding: utf-8 -*-

from __future__ import print_function

from ParseWikipediaXML.util_funcs import *

import re

def run(parser,idx,lock,pages,saved):
    args = parser.args
    queue = parser.queue
    while True:
        page = queue.get()

        # XXX This may terminate main thread before the last parseText() ends
        #queue.task_done()

        if page == "Finished":
            # Be sure that this can let queue.join() go
            queue.task_done()
            break
        elif re.search(args.recateg, page):
            titlematch = re.search("<title[^<>]*>([^<>]+)</title>",page)
            textmatch = re.search("<text[^<>]*>([^<>]+)</text>",page)
            if titlematch and textmatch:
                title = titlematch.group(1)
                text = textmatch.group(1)
                dictBofw = parser.parseText(text)
                if parser.post_process(parser, dictBofw, title):
                    # multiprocessing Value object is thread safe
                    saved.value+=1

        report(parser,idx,lock,pages,saved)
        # put() counts up and task_done() counts down
        queue.task_done()

def report(parser,idx,lock,pages,saved):
    m = " > BofwProcess "

    pages.value += 1
    lock.acquire()
    print( "%(message)s [#page(saved/parsed) %(saved)s/%(count)s @ thread %(index)s]" %
            { "message" : m,
              "count"   : pages.value,
              "saved"   : saved.value,
              "index"   : idx
            },
            "\r", end="" )
    lock.release()

