# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import redis
import time

if sys.version_info[0] == 2:
    pass
else:
    from functools import cmp_to_key, reduce

def python_sorted(lst):
    if sys.version_info[0] == 2:
        return sorted(lst,cmp=cmp_dict)
    else:
        return sorted(lst,key=cmp_to_key(cmp_dict))

def cmp_dict(a,b):
    if a[1]>b[1]:
        return -1
    elif a[1]<b[1]:
        return 1
    else:
        if a[0]>b[0]:
            return 1
        else:
            return -1

def store_redis(func):
    import functools
    @functools.wraps(func)
    def wrapper(*args,**kwargs):
        try:
            if args[0].client == None:
                raise redis.exceptions.ConnectionError
            else:
                args[0].client.ping()
        except redis.exceptions.ConnectionError:
            result = func(*args,**kwargs)
        except Exception as e:
            print(e)
        else:
            args[0].client.set("start_time",time.time())
            result = func(*args,**kwargs)
            args[0].client.set("finish_time",time.time())
        return result
    return wrapper

def check_time(func):
    import functools
    @functools.wraps(func)
    def wrapper(*args,**kwargs):
        import time
        start = time.time()
        #print("Begin %s."%(func.__name__))
        result = func(*args,**kwargs)
        print(" > Finished %s in %.2f sec."%(func.__name__,time.time()-start))
        return result
    return wrapper

def writeToFile(self, dictBofw, title):

    docCount=sum(dictBofw.values())
    listTupleBofw = python_sorted(dictBofw.items())

    if docCount >= self.args.minw and docCount <= self.args.maxw:

        # Make string from list of tuples of bag-of-words
        cont = reduce(
            lambda _cont, _bofw: _cont+_bofw[0]+" "+str(_bofw[1])+" ",
            [ _bofw for _bofw in listTupleBofw if _bofw[1] >= self.args.minc ],
            ""
        ).rstrip()+"\n"

        if len(cont) > 1:
            self.lockb.acquire()
            if self.args.ofcont:
                with open(self.args.ofcont,'a') as f:
                    f.write(cont)
            else:
                print(cont)
            self.lockb.release()
            self.writeTitleToFile(title)

            return True

    return False

