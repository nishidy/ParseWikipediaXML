require 'thor'
require 'redis'

# Abstract Parser class
# Params:
# +options+:: command line arguments
class AbstParser
  attr_reader :options, :write_lock, :hdlr_bofw, :hdlr_title, :hdlr_tfidf, :redis
  attr_reader :tospexp, :splexp, :termexp

  def initialize(options)
    @options = options
    @write_lock = Mutex.new
    begin
      @hdlr_bofw = File.open(options[:outBofwFile], 'a') unless options[:outBofwFile].nil?
      @hdlr_title = File.open(options[:outTitleFile], 'a') unless options[:outTitleFile].nil?
      @hdlr_tfidf = File.open(options[:outTfIdfFile], 'a') unless options[:outTfIdfFile].nil?
    rescue => e
      e.message
    end
  end

  def set_redis
    @redis = Redis.new
    begin
      @redis.ping
    rescue
      puts 'Redis server is not running.'
      @redis = nil
    end
  end

  def save_bofw_to_file(bofw)
    write_lock.lock
    begin
      @hdlr_bofw.write bofw
    ensure
      write_lock.unlock
    end
  end

  def save_title_to_file(title)
    write_lock.lock
    begin
      @hdlr_title.write title
    ensure
      write_lock.unlock
    end
  end

  def set_exps
    @tospexp = '[,.;]'
    @splexp = "[ \n]"
    @termexp = "^[a-z][0-9a-z'-]*[0-9a-z]$"
  end

  def get_corpus_df
    return if options[:outTfIdfFile].nil?

    total_docs = 0
    corpus_df = {}
    File.readlines(@options[:outBofwFile]).each do |line|
      terms = line.split(" ").select.each_with_index{ |_, i| i.even? }
      terms.each{ |term|
        corpus_df.key?(term) ? corpus_df[term] += 1 : corpus_df[term] = 1
      }
      total_docs += 1
    end

    apply_tfidf corpus_df, total_docs
  end

  def apply_tfidf(corpus_df, total_docs)
    File.readlines(@options[:outBofwFile]).each do |line|
      terms = line.split(" ").select.each_with_index{ |_, i| i.even? }
      freqs = line.split(" ").select.each_with_index{ |_, i| i.odd? }
      total_terms = freqs.inject(:+)
      output = ""
      terms.zip(freqs) { |term, freq|
        output << " " if terms[0] != term
        tf  = freq.to_f/total_terms.to_f
        idf = Math.log2(total_docs/corpus_df[term].to_f)+1
        output << term + " " + sprintf("%.3f", tf*idf)
      }
      save_tfidf_to_file(output + "\n")
    end
  end

  def save_tfidf_to_file(output)
    write_lock.lock
    begin
      @hdlr_tfidf.write output
    ensure
      write_lock.unlock
    end
  end
end

# English Parser class inherited from Abstract Parser class
# Params:
# +options+:: command line arguments
class EngParser < AbstParser
  attr_reader :hash_dict, :stopwords

  def initialize(options)
    super(options)
    stopwords = 'a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your'
    @stopwords = stopwords.split(/,/)
  end

  def start_parse(page)
    fiber = Fiber.new do |page_|
      %r{<text[^>]*>([^<>]*)<\/text>} =~ page_
      text = Regexp.last_match(1)

      %r{<title[^>]*>([^<>]*)<\/title>} =~ page_
      title = Regexp.last_match(1)

      h, n = parse_bofw(text)
      post_parse(h, n, title)
    end

    begin
      fiber.resume page
    rescue => e
      p e.message
    end
  end

  def parse_bofw(text)
    hash_bofw = {}
    total_num_of_words = 0
    text.gsub(/#{@tospexp}/, ' ').split(/#{@splexp}/).map(&:downcase)
      .select { |w| !@stopwords.include?(w) }
      .select { |w| w =~ /#{@termexp}/ }.each do |word|
      word = @hash_dict[word] if @hash_dict.key? word
      hash_bofw.key?(word) ? hash_bofw[word] += 1 : hash_bofw[word] = 1
      total_num_of_words += 1
    end
    [hash_bofw, total_num_of_words]
  end

  def post_parse(hash_bofw, total_num_of_words, title)
    return if hash_bofw.empty? ||
              total_num_of_words > @options[:"max-page-words"] ||
              total_num_of_words < @options[:"min-page-words"]

    save_bofw_to_file(
      hash_bofw.sort do |(k1, v1), (k2, v2)|
        v1 == v2 ? k1 <=> k2 : v2 <=> v1
      end.inject('') do |bofw, arr|
        bofw + arr[0] + ' ' + arr[1].to_s + ' '
      end.rstrip + "\n"
    )

    save_title_to_file( title + "\n" )

    redis_save(total_num_of_words, hash_bofw.keys.size) unless @redis.nil?
  end

  def redis_save(t, n)
    @redis.zincrby 'total_num', 1, get_set_val(t)
    @redis.zincrby 'num', 1, get_set_val(n)
  end

  def run_parse
    @redis.set 'start_time', Time.now.to_f unless @redis.nil?

    startflag = stopflag = false, page = ''
    File.readlines(@options[:inWikiFile]).each do |line|
      startflag = true if line.include? '<page>'
      stopflag = true if line.include? '</page>'
      page << line if startflag
      next unless startflag && stopflag

      start_parse page

      page = ''
      startflag = stopflag = false
    end

    @redis.set 'finish_time', Time.now.to_f unless @redis.nil?
  end

  def read_dictionary
    @hash_dict = {}
    return if @options[:inDictFile].nil?

    print 'Begin reading from dictionary... '
    File.readlines(@options[:inDictFile]).each do |line|
      items = line.split(/[ \t]/)
      trans = items[0]
      base = items[3]
      @hash_dict[trans] = base unless trans == base
    end
    print "Finshded reading from dictionary.\n"
  end

  def get_set_val(num)
    (num / 100 * 100).to_s
  end
end

class JapParser < AbstParser
end

# Command Line Parse class
# Params:
class CLI < Thor
  package_name 'ParseWikipediaXML'
  default_command :bagofwords

  option :inWikiFile,
         type: :string,
         aliases: '-i',
         required: true,
         desc: 'Input file of Wikipedia'

  option :inDictFile,
         type: :string,
         aliases: '-d',
         desc: 'Input file of dictionary'

  option :outBofwFile,
         type: :string,
         aliases: '-s',
         required: true,
         desc: 'Ouput file for bag-of-words'

  option :outTitleFile,
         type: :string,
         aliases: '-t',
         desc: 'Ouput file for title'

  option :outTfIdfFile,
         type: :string,
         aliases: '-f',
         desc: 'Ouput file for bag-of-words normarized by tf-idf'

  option :"min-page-words",
         type: :numeric,
         aliases: '-m',
         default: 1,
         desc: 'How many terms at least a page should contain'

  option :"max-page-words",
         type: :numeric,
         aliases: '-x',
         default: 65_535,
         desc: 'How many terms at most a page should contain'

  option :"min-word-count",
         type: :numeric,
         aliases: '-c',
         default: 1,
         desc: 'How many times a term should appear in a page'

  option :workers,
         type: :numeric,
         aliases: '-w',
         default: 1,
         desc: '# of worker but thread is not implemented,\
               this is only for the common test parameter.'

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
    parser.get_corpus_df
  end

  desc 'convert bag-of-words according to TF-IDF usage',
       'convert bag-of-words according to TF-IDF desc'
  def tfidf
  end
end

CLI.start
