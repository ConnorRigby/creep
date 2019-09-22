var mqtt = require('mqtt')
var fs = require('fs');
var path = require('path')
var KEY = fs.readFileSync(path.join(__dirname, '../ssl/server.key'))
var CERT = fs.readFileSync(path.join(__dirname, '../ssl/server.crt'))
var TRUSTED_CA_LIST = fs.readFileSync(path.join(__dirname, '../ssl/ca.crt'))

// var client = mqtt.connect('ws://localhost:4000/mqtt')

var PORT = 4443
var HOST = 'localhost'

var options = {
  port: PORT,
  host: HOST,
  key: KEY,
  cert: CERT,
  rejectUnauthorized: true,
  // The CA list will be used to determine if server is authorized
  ca: TRUSTED_CA_LIST,
  protocol: 'mqtts'
}

var client = mqtt.connect(options)

client.on('connect', function () {
  console.log("connected")
  client.subscribe('presence', function (err) {
    if (!err) {
      client.publish('presence', 'Hello mqtt')
    } else {
      console.error(err)
    }
  })
})

client.on('message', function (topic, message) {
  // message is Buffer
  console.log(message.toString())
  client.end()
})

client.on('error', function (error) {
  console.error('mqtt error' + error);
})

client.on('offline', function () {
  console.error('mqtt offline')
})

console.log('running ws mqtt integration')