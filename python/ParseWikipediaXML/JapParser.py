# -*- coding: utf-8 -*-

from __future__ import print_function
from collections import defaultdict
import MeCab
from ParseWikipediaXML.AbstParser import AbstParser

class JapParser(AbstParser):

    def __init__(self,args):
        AbstParser.__init__(self,args)
        self.mecab = MeCab.Tagger()

        stopwords = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"
        self.stopwords=stopwords.split(",")

    def parseText(self,text):
        dictBofw=defaultdict(int) # python >= 2.5
        for word_info in self.mecab.parse(text).split("\n"):
            if word_info == "EOS": break
            word = word_info.split("\t")[0]
            if word in self.stopwords: continue
            info = word_info.split("\t")[1].split(",")
            speech = info[0]
            speech_info = info[1]
            baseform = info[6]
            if speech in ["形容詞","動詞","副詞"] or\
                (speech == "名詞" and speech_info == "サ変接続" ) :
                if baseform == "*": continue
                dictBofw[baseform] += 1 # defaultdict initializes the fist value of a key

        return dictBofw

