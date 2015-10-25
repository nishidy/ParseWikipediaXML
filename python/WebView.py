import tornado.ioloop
import tornado.web
import os
import redis
import sys

class MainHandler(tornado.web.RequestHandler):
	def get(self):
		try:
			r = redis.Redis()

			st = r.get("start_time")
			if st == None:
				st = 0
			else:
				st = float(st)

			fn = r.get("finish_time")
			if fn == None:
				fn = 0
			else:
				fn = float(fn)

			if fn > st:
				duration = fn - st
			else:
				duration = float(".".join([str(x) for x in r.time()])) - st

			total=map(lambda x: (int(float(x[0])),int(x[1])),
					r.zrevrange("sorted_total",0,-1,withscores=True))

			num=map(lambda x: (int(float(x[0])),int(x[1])),
					r.zrevrange("sorted_num",0,-1,withscores=True))

			pages = reduce( lambda pages,line: pages+line[1], total, 0 )

			self.render("index.html",
				total=total,
				num=num,
				pages=pages,
				duration=duration,
			)

		except Exception as e:
			self.write("%s"%e)


if sys.version_info[0] > 2:
	from functools import reduce

app = tornado.web.Application(
	[("/", MainHandler)],
	template_path=os.path.join(os.getcwd(),"views"),
)

if __name__ == "__main__":
	app.listen(8888)
	tornado.ioloop.IOLoop.instance().start()


