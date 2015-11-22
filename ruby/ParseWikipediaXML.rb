require 'thor'
require 'redis'

class AbstParser

  attr_reader :options, :write_lock, :hdlr_bofw, :redis
  attr_reader :tospexp, :splexp, :termexp

  def initialize(options)
    @options = options
    @write_lock = Mutex.new
    @redis = Redis.new
    begin
      @redis.ping
    rescue
      puts 'Redis server is not running.'
    end
    begin
      @hdlr_bofw = File.open(options[:outBofwFile], 'a')
    rescue => e
      e.message
    end
  end

  def save_to_file(bofw)
    write_lock.lock
    begin
      @hdlr_bofw.write bofw
    ensure
      write_lock.unlock
    end
  end

  def set_exps()
    @tospexp = "[,.;]"
    @splexp = "[ \n]"
    @termexp = "^[a-z][0-9a-z'-]*[0-9a-z]$"
  end

end

class EngParser < AbstParser
  attr_reader :hash_dict, :stopwords

  def initialize(options)
    super(options)
    stopwords = 'a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your'
    @stopwords = stopwords.split(/,/)
  end

  def run_parse
    startflag = stopflag = false
    page = ''

    @redis.set 'start_time', Time.now.to_f if @redis.connected?

    File.readlines(@options[:inWikiFile]).each do |line|
      startflag = true if line.include? '<page>'
      stopflag = true if line.include? '</page>'
      page << line if startflag
      if startflag && stopflag
        fiber = Fiber.new do |page_|
          %r{<text[^>]*>([^<>]*)<\/text>} =~ page_
          text = Regexp.last_match(1)
          hash_bofw = {}
          total_num_of_words = 0

          text.gsub(/#{@tospexp}/," ")
		  .split(/#{@splexp}/)
		  .map(&:downcase)
		  .select{|w|!@stopwords.include?(w)}
		  .select{|w|w=~/#{@termexp}/}
		  .each do |word|
            word = @hash_dict[word] if @hash_dict.key? word
            hash_bofw.key?(word) ? hash_bofw[word] += 1 : hash_bofw[word] = 1
            total_num_of_words += 1
          end

          unless hash_bofw.empty? ||
                 total_num_of_words > @options[:"max-page-words"] ||
                 total_num_of_words < @options[:"min-page-words"]
            save_to_file(
              hash_bofw.sort do |(k1, v1), (k2, v2)|
                v1 == v2 ? k1 <=> k2 : v2 <=> v1
              end.inject('') do |bofw, arr|
                bofw + arr[0] + ' ' + arr[1].to_s + ' '
              end.rstrip + "\n"
            )
            if @redis.connected?
              num_of_words = hash_bofw.keys.size
              @redis.zincrby 'total_num', 1, get_set_val(total_num_of_words)
              @redis.zincrby 'num', 1, get_set_val(num_of_words)
            end
          end

        end

        begin
          fiber.resume page
        rescue => e
          p e.message
        end

        page = ''
        startflag = stopflag = false
      end
    end

    @redis.set 'finish_time', Time.now.to_f if @redis.connected?
  end

  def read_dictionary
    @hash_dict = {}
    unless @options[:inDictFile].nil?
      print 'Begin reading from dictionary... '
      File.readlines(@options[:inDictFile]).each do |line|
        items = line.split(/[ \t]/)
        trans = items[0]
        base = items[3]
        @hash_dict[trans] = base unless trans == base
      end
      print "Finshded reading from dictionary.\n"
    end
  end

  def get_set_val(num)
    (num / 100 * 100).to_s
  end
end

class JapParser < AbstParser
  def initialize(options)
    super(options)
  end

  def run_parse
  end
end

class CLI < Thor
  package_name 'ParseWikipediaXML'
  default_command :bagofwords

  option :inWikiFile,
         type: :string, aliases: '-i', required: true, desc: 'Input file of Wikipedia'

  option :inDictFile,
         type: :string, aliases: '-d', desc: 'Input file of dictionary'

  option :outBofwFile,
         type: :string, aliases: '-s', required: true, desc: 'Ouput file for bag-of-words'

  option :outTitleFile,
         type: :string, aliases: '-t', desc: 'Ouput file for title'

  option :"min-page-words",
         type: :numeric, aliases: '-m', default: 1, desc: 'How many terms at least a page should contain'

  option :"max-page-words",
         type: :numeric, aliases: '-x', default: 65_535, desc: 'How many terms at most a page should contain'

  option :"min-word-count",
         type: :numeric, aliases: '-c', default: 1, desc: 'How many times a term should appear in a page'

  option :workers,
         type: :numeric, aliases: '-w', default: 1, desc: '# of worker but thread is not implemented, this is only for the common test parameter.'

  method_option :japanese, aliases: '-j', desc: 'If it is in Japanese'

  desc 'generate bag-of-words usage', 'generate bag-of-words desc'
  def bagofwords
    if options[:japanese]
      parser = JapParser.new(options)
    else
      parser = EngParser.new(options)
      parser.read_dictionary
      parser.set_exps
    end
    parser.run_parse
  end

  desc 'convert bag-of-words according to TF-IDF usage', 'convert bag-of-words according to TF-IDF desc'
  def tfidf
  end
end

CLI.start
