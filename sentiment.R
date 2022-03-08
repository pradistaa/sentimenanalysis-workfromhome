library(rtweet)
library(dplyr)
library(plyr)
library(stringr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(textclean)
library(katadasaR)
library(tokenizers)
library(wordcloud)
library(corpus)
library(tm)
library(wordcloud) 
library(RColorBrewer) 
library(SnowballC)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "oH4PXuhItHbrxepelJGaiEF3X"
api_secret_key <- "0udJUFZVwzhgXhefLoJHVRaFyOZDVpffOm1jwhW8yFytmMbTOU"

## authenticate via web browser
token <- create_token(
  app = "Irzan Research",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

#Cari tweet tentang topik pilihan Anda, 
#persempit jumlah tweet yang diinginkan dan putuskan untuk memasukkan retweet atau tidak.

#kata <- search_tweets("WFH", n=10000, include_rts = FALSE,lang="id")
kata <- read.csv("D:/Tugas/Big Data/TA/Dataset_Twitter/dataset_wfh.csv")
tweets <- kata$text %>% 
  as.character()

#Text Subbing
tweets <- gsub( "\n"," ",tweets)

#Replace HTML
tweets <- tweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank

#Replace Emoji
tweets <- tweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)

#Replace Hashtag and Mention
tweets <- tweets %>% 
  replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

#Replace Slang Words
# import Indonesian lexicon
spell.lex <- read.csv("D:/Tugas/Big Data/TA/Preproses/colloquial-indonesian-lexicon.csv")

# replace internet slang
tweets <- replace_internet_slang(tweets, slang = paste0("\\b",
                                                        spell.lex$slang, "\\b"),
                                 replacement = spell.lex$formal, ignore.case = TRUE)
#Text Stripping
tweets <- strip(tweets)

#Stemming
tweets <- tweets %>% 
  as.data.frame() %>% 
  distinct()

tweets <- as.character(tweets$.)

stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

tweets <- lapply(tokenize_words(tweets[]), stemming)

tweets <- as.character(tweets)
corpus = Corpus(VectorSource(tweets))

tdm = TermDocumentMatrix( 
  corpus, 
  control = list( 
    removePunctuation = TRUE, 
    stopwords  = 
      c(stopwords("english"),"ada","adalah","adanya","adapun","agak","agaknya","agar","akan","akankah","akhir","akhiri","akhirnya","aku","akulah",
        "amat","amatlah","anda","andalah","antar","antara","antaranya","apa","apaan","apabila","apakah","apalagi","apatah","artinya","asal",
        "asalkan","atas","atau","ataukah","ataupun","awal","awalnya","bagai","bagaikan","bagaimana","bagaimanakah","bagaimanapun","bagi",
        "bagian","bahkan","bahwa","bahwasanya","baik","bakal","bakalan","balik","banyak","bapak","baru","bawah","beberapa","begini","beginian",
        "beginikah","beginilah","begitu","begitukah","begitulah","begitupun","bekerja","belakang","belakangan","belum","belumlah","benar",
        "benarkah","benarlah","berada","berakhir","berakhirlah","berakhirnya","berapa","berapakah","berapalah","berapapun","berarti","berawal",
        "berbagai","berdatangan","beri","berikan","berikut","berikutnya","berjumlah","berkali-kali","berkata","berkehendak","berkeinginan",
        "berkenaan","berlainan","berlalu","berlangsung","berlebihan","bermacam","bermacam-macam","bermaksud","bermula","bersama","bersama-sama",
        "bersiap","bersiap-siap","bertanya","bertanya-tanya","berturut","berturut-turut","bertutur","berujar","berupa","besar","betul","betulkah",
        "biasa","biasanya","bila","bilakah","bisa","bisakah","boleh","bolehkah","bolehlah","buat","bukan","bukankah","bukanlah","bukannya","bulan",
        "bung","cara","caranya","cukup","cukupkah","cukuplah","cuma","dahulu","dalam","dan","dapat","dari","daripada","datang","dekat","demi",
        "demikian","demikianlah","dengan","depan","di","dia","diakhiri","diakhirinya","dialah","diantara","diantaranya","diberi","diberikan",
        "diberikannya","dibuat","dibuatnya","didapat","didatangkan","digunakan","diibaratkan","diibaratkannya","diingat","diingatkan","diinginkan",
        "dijawab","dijelaskan","dijelaskannya","dikarenakan","dikatakan","dikatakannya","dikerjakan","diketahui","diketahuinya","dikira","dilakukan",
        "dilalui","dilihat","dimaksud","dimaksudkan","dimaksudkannya","dimaksudnya","diminta","dimintai","dimisalkan","dimulai","dimulailah",
        "dimulainya","dimungkinkan","dini","dipastikan","diperbuat","diperbuatnya","dipergunakan","diperkirakan","diperlihatkan","diperlukan",
        "diperlukannya","dipersoalkan","dipertanyakan","dipunyai","diri","dirinya","disampaikan","disebut","disebutkan","disebutkannya","disini",
        "disinilah","ditambahkan","ditandaskan","ditanya","ditanyai","ditanyakan","ditegaskan","ditujukan","ditunjuk","ditunjuki","ditunjukkan",
        "ditunjukkannya","ditunjuknya","dituturkan","dituturkannya","diucapkan","diucapkannya","diungkapkan","dong","dua","dulu","empat","enggak",
        "enggaknya","entah","entahlah","guna","gunakan","hal","hampir","hanya","hanyalah","hari","harus","haruslah","harusnya","hendak","hendaklah",
        "hendaknya","hingga","ia","ialah","ibarat","ibaratkan","ibaratnya","ibu","ikut","ingat","ingat-ingat","ingin","inginkah","inginkan","ini",
        "inikah","inilah","itu","itukah","itulah","jadi","jadilah","jadinya","jangan","jangankan","janganlah","jauh","jawab","jawaban","jawabnya",
        "jelas","jelaskan","jelaslah","jelasnya","jika","jikalau","juga","jumlah","jumlahnya","justru","kala","kalau","kalaulah","kalaupun","kalian",
        "kami","kamilah","kamu","kamulah","kan","kapan","kapankah","kapanpun","karena","karenanya","kasus","kata","katakan","katakanlah","katanya",
        "ke","keadaan","kebetulan","kecil","kedua","keduanya","keinginan","kelamaan","kelihatan","kelihatannya","kelima","keluar","kembali","kemudian",
        "kemungkinan","kemungkinannya","kenapa","kepada","kepadanya","kesampaian","keseluruhan","keseluruhannya","keterlaluan","ketika","khususnya",
        "kini","kinilah","kira","kira-kira","kiranya","kita","kitalah","kok","kurang","lagi","lagian","lah","lain","lainnya","lalu","lama","lamanya",
        "lanjut","lanjutnya","lebih","lewat","lima","luar","macam","maka","makanya","makin","malah","malahan","mampu","mampukah","mana","manakala",
        "manalagi","masa","masalah","masalahnya","masih","masihkah","masing","masing-masing","mau","maupun","melainkan","melakukan","melalui",
        "melihat","melihatnya","memang","memastikan","memberi","memberikan","membuat","memerlukan","memihak","meminta","memintakan","memisalkan",
        "memperbuat","mempergunakan","memperkirakan","memperlihatkan","mempersiapkan","mempersoalkan","mempertanyakan","mempunyai","memulai",
        "memungkinkan","menaiki","menambahkan","menandaskan","menanti","menanti-nanti","menantikan","menanya","menanyai","menanyakan","mendapat",
        "mendapatkan","mendatang","mendatangi","mendatangkan","menegaskan","mengakhiri","mengapa","mengatakan","mengatakannya","mengenai",
        "mengerjakan","mengetahui","menggunakan","menghendaki","mengibaratkan","mengibaratkannya","mengingat","mengingatkan","menginginkan",
        "mengira","mengucapkan","mengucapkannya","mengungkapkan","menjadi","menjawab","menjelaskan","menuju","menunjuk","menunjuki","menunjukkan",
        "menunjuknya","menurut","menuturkan","menyampaikan","menyangkut","menyatakan","menyebutkan","menyeluruh","menyiapkan","merasa","mereka",
        "merekalah","merupakan","meski","meskipun","meyakini","meyakinkan","minta","mirip","misal","misalkan","misalnya","mula","mulai","mulailah",
        "mulanya","mungkin","mungkinkah","nah","naik","namun","nanti","nantinya","nyaris","nyatanya","oleh","olehnya","pada","padahal","padanya",
        "pak","paling","panjang","pantas","para","pasti","pastilah","penting","pentingnya","per","percuma","perlu","perlukah","perlunya","pernah",
        "persoalan","pertama","pertama-tama","pertanyaan","pertanyakan","pihak","pihaknya","pukul","pula","pun","punya","rasa","rasanya","rata",
        "rupanya","saat","saatnya","saja","sajalah","saling","sama","sama-sama","sambil","sampai","sampai-sampai","sampaikan","sana","sangat",
        "sangatlah","satu","saya","sayalah","se","sebab","sebabnya","sebagai","sebagaimana","sebagainya","sebagian","sebaik","sebaik-baiknya",
        "sebaiknya","sebaliknya","sebanyak","sebegini","sebegitu","sebelum","sebelumnya","sebenarnya","seberapa","sebesar","sebetulnya","sebisanya",
        "sebuah","sebut","sebutlah","sebutnya","secara","secukupnya","sedang","sedangkan","sedemikian","sedikit","sedikitnya","seenaknya","segala",
        "segalanya","segera","seharusnya","sehingga","seingat","sejak","sejauh","sejenak","sejumlah","sekadar","sekadarnya","sekali","sekali-kali",
        "sekalian","sekaligus","sekalipun","sekarang","sekarang","sekecil","seketika","sekiranya","sekitar","sekitarnya","sekurang-kurangnya",
        "sekurangnya","sela","selain","selaku","selalu","selama","selama-lamanya","selamanya","selanjutnya","seluruh","seluruhnya","semacam","semakin",
        "semampu","semampunya","semasa","semasih","semata","semata-mata","semaunya","sementara","semisal","semisalnya","sempat","semua","semuanya",
        "semula","sendiri","sendirian","sendirinya","seolah","seolah-olah","seorang","sepanjang","sepantasnya","sepantasnyalah","seperlunya",
        "seperti","sepertinya","sepihak","sering","seringnya","serta","serupa","sesaat","sesama","sesampai","sesegera","sesekali","seseorang","sesuatu",
        "sesuatunya","sesudah","sesudahnya","setelah","setempat","setengah","seterusnya","setiap","setiba","setibanya","setidak-tidaknya",
        "setidaknya","setinggi","seusai","sewaktu","siap","siapa","siapakah","siapapun","sini","sinilah","soal","soalnya","suatu","sudah",
        "sudahkah","sudahlah","supaya","tadi","tadinya","tahu","tahun","tak","tambah","tambahnya","tampak","tampaknya","tandas","tandasnya","tanpa",
        "tanya","tanyakan","tanyanya","tapi","tegas","tegasnya","telah","tempat","tengah","tentang","tentu","tentulah","tentunya","tepat","terakhir",
        "terasa","terbanyak","terdahulu","terdapat","terdiri","terhadap","terhadapnya","teringat","teringat-ingat","terjadi","terjadilah","terjadinya",
        "terkira","terlalu","terlebih","terlihat","termasuk","ternyata","tersampaikan","tersebut","tersebutlah","tertentu","tertuju","terus","terutama",
        "tetap","tetapi","tiap","tiba","tiba-tiba","tidak","tidakkah","tidaklah","tiga","tinggi","toh","tunjuk","turut","tutur","tuturnya","ucap",
        "ucapnya","ujar","ujarnya","umum","umumnya","ungkap","ungkapnya","untuk","usah","usai","waduh","wah","wahai","waktu","waktunya","walau",
        "walaupun","wong","yaitu","yakin","yakni","yang","aja","gue","gak","gini","wkwk","mulu","nggak","skrg","iya","tuh","klo","udh","ngga",
        "dah","nya","sih","udah","pas","kalo","nih","sampe","banget","bgt","wkwkwk","wfh"), 
    removeNumbers = TRUE, tolower = TRUE) 
)

m = as.matrix(tdm) 
word_freqs = sort(rowSums(m), decreasing = TRUE)  
dm = data.frame(word = names(word_freqs), freq = word_freqs) 

wordcloud(dm$word,  dm$freq,  max.words=100,  min.freq=20, 
          scale=c(5,0.4),  random.order  =  FALSE,rot.per=.5,  colors  = 
            brewer.pal(8, "Dark2"))

findFreqTerms(tdm, lowfreq = 3) 
findAssocs(tdm, terms = "freedom", corlimit = 0.3) 
head(dm, 10) 

barplot(dm[1:10,]$freq,  las  =  1,  names.arg  =  dm[1:10,]$word, 
        ylim=c(0,2100),  col  =  "blue",  border="white",  main  ="Kata  dengan 
Frekuensi Tertinggi",xlab = "Kata", ylab = "Frekuensi")

tweets <- tweets %>% 
  as.data.frame() %>% 
  distinct()

stopwordID <- "D:/Tugas/Big Data/TA/Preproses/stopword.txt"##membaca stopwordID perbaris
cStopwordID<-readLines(stopwordID);

corpus <- tm_map(corpus, removeWords, cStopwordID)
corpus <-tm_map(corpus,stripWhitespace)

tweets1 <-data.frame(text=unlist(sapply(corpus,`[`)))

pos = scan('D:/Tugas/Big Data/TA/LoOW/positive.txt', what='character', comment.char=':') 
neg = scan('D:/Tugas/Big Data/TA/LoOW/negative.txt', what='character', comment.char=':')

str(tweets1)

score.sentiment  =  function(sentences,  pos.words,  neg.words, .progress='none') 
{ 
  require(plyr) 
  require(stringr) 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  { 
    word.list = str_split(sentence, '\\s+') 
    words = unlist(word.list) 
    
    pos.matches = match(words, pos.words) 
    neg.matches = match(words, neg.words) 
    
    pos.matches = !is.na(pos.matches) 
    neg.matches = !is.na(neg.matches) 
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score) 
  }, pos.words, neg.words, .progress=.progress ) 
  
  scores.df = data.frame(score=scores, text=sentences) 
  return(scores.df) 
} 

#melakukan skoring text
hasil = score.sentiment(tweets1$text, pos, neg)
head(hasil)# melakukan labeling pada nilai yang kurang dari 0 sebagai negatif dan lebih dari = 0 adalah positif
hasil$klasifikasi<- ifelse(hasil$score== 0,"Netral",ifelse(hasil$score<0,"Negatif","Positif"))
View(hasil)

hist(hasil$score,  main="Klasifikasi  Sentimen  Cuitan  mengenai 
WFH",  xlab="Skor  Sentimen",  xlim=c(-14,8),  ylim=c(0,5000),  col  = 
       brewer.pal(n = 11, name = "RdBu"))

count(hasil$score)

data <- hasil
View(data)

write.csv(data, file = "dataset_klasifikasi_wfh_preproses.csv")
