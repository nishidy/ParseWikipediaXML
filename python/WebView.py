import tornado.ioloop
import tornado.web
import os
import redis

class MainHandler(tornado.web.RequestHandler):
	def get(self):
		try:
			r = redis.Redis()
			self.render("index.html",
				total=r.zrevrange("sorted_total",0,-1,withscores=True),
				num=r.zrevrange("sorted_num",0,-1,withscores=True),
			)
		except Exception as e:
			self.write("%s"%e)

app = tornado.web.Application(
	[("/", MainHandler)],
	template_path=os.path.join(os.getcwd(),"views"),
)

if __name__ == "__main__":
	app.listen(8888)
	tornado.ioloop.IOLoop.instance().start()

