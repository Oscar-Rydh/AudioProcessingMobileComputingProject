var express = require('express')
var bodyParser = require('body-parser');
var multer = require('multer')
var upload = multer({dest: 'uploads/'});
var app = express()
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())
const { exec } = require('child_process')


app.post('/estimation', upload.single('file'), function (req, res) {
    console.log(req.file)
    console.log(req.body)
    var fileName = req.file.filename;
    console.log(fileName)

    const pwd = exec('pwd');
    pwd.stdout.on('data', data => {
        console.log(data)
    })


    const estimate = exec('mv ./uploads/' + fileName + " ./uploads/sound.wav");

    estimate.stdout.on('data', data => {
        console.log("Shell returned:\n " + data);
        res.json({
            result: data
        })
    })
    res.status(200).send()

})

app.listen(3000)