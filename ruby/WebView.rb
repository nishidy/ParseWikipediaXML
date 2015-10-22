require 'sinatra/base'
require 'redis'

class App < Sinatra::Base
	set :redis, Redis.new
	get '/' do
		@total_num = settings.redis.zrevrange("total_num",0,-1,:withscores=>true)
		@num = settings.redis.zrevrange("num",0,-1,:withscores=>true)
		erb :index
	end
end

App.run!
