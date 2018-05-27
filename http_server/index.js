var express = require('express')
var bodyParser = require('body-parser');
var multer = require('multer')
var upload = multer({dest: 'uploads/sound.wav'});
var app = express()
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())
const { exec } = require('child_process')


app.post('/estimation', upload.single('file'), function (req, res) {
    console.log(req.file)
    console.log(req.body)
    const estimate = exec('pwd');

    estimate.stdout.on('data', data => {
        console.log("Shell returned:\n " + data);
        res.json({
            result: data
        })
    })

})

app.listen(3000)