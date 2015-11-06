require 'sinatra/base'
require 'redis'

class App < Sinatra::Base
  set :redis, Redis.new
  set :environment, :production

  get '/' do
    @total_num = settings.redis.zrevrange('total_num', 0, -1, withscores: true)
    @num = settings.redis.zrevrange('num', 0, -1, withscores: true)

    st = settings.redis.get('start_time').to_f
    fn = settings.redis.get('finish_time').to_f
    if fn > st
      @duration = fn - st
    else
      @duration = settings.redis.time.join('.').to_f - st
    end

    erb :index
  end
end

App.run!
