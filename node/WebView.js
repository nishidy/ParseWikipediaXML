var express = require('express');
var client = require('redis').createClient();
var app = express();
var ECT = require('ect');
app.engine('ect', ECT({ watch: true, root: __dirname+'/views', ext: '.ect'}).render);
app.set('view engine','ect');

function is(type, obj) {
  var clas = Object.prototype.toString.call(obj).slice(8, -1);
  return obj !== undefined && obj !== null && clas === type;
}

function map_to_int(arr){
  var resarr = []
  arr.forEach(function(elem){
    resarr.push(parseInt(elem))
  })
  return resarr
}

function each_slice(arr,gather){
  if(is("Number",gather)){
    var cnt=0;
    var resarr = [];
    var elemarr = [];
    arr.forEach(function(elem){
      elemarr.push(elem)
      if(cnt%gather==1){
        resarr.push(elemarr);
        elemarr = [];
      }
      cnt += 1;
    })
  }
  return resarr;
}

function sum_with_gap(arr,start,gap){
  var cnt=0;
  var sum=0;
  arr.forEach(function(elem){
    if(cnt>=start && cnt%gap==start){
      sum += parseInt(elem)
    }
    cnt += 1
  })
  return sum
}

app.get('/', function (req, res) {
  client.zrevrange("total_num", 0, -1, 'withscores', function(err, total_num){
    client.zrevrange("num", 0, -1, 'withscores', function(err, num){
      client.get("start_time", function(err, start_time){
        client.get("stop_time", function(err, stop_time){
          pages = sum_with_gap(total_num,1,2)
          res.render('index', {
            pages: pages,
            duration: (stop_time-start_time)/1000,
            throughput: pages/((stop_time-start_time)/1000)+"",
            total_num: each_slice(map_to_int(total_num),2),
            num: each_slice(map_to_int(num),2),
          });
        });
      });
    });
  });
});

app.listen(3000);

