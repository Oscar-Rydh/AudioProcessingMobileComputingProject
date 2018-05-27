var express = require('express')
var bodyParser = require('body-parser');
var multer = require('multer')
var upload = multer({dest: 'uploads/'});
var app = express()
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())
const { exec } = require('child_process')


app.post('/estimation', upload.single('file'), function (req, res) {
    var fileName = req.file.filename;

    const pwd = exec('pwd');
    pwd.stdout.on('data', data => {
        console.log(data)
    })
    const move = exec ('mv uploads/' + fileName + ' ./uploads/sound.wav')
    move.on('exit', (code, signal) => {
        console.log('Will start r script')
        const predict = exec('Rscript Predict.R uploads/sound.wav model.hdf5')
        predict.stdout.on('data', data => {
            const result = data.split(" ")[1];
            if (result !== undefined) {
                console.log('will send response: ' + result)
                res.json({
                    result
                })
            } else {
                console.log('Did not get a result from rscript')
            }
        })
    })

})

app.listen(3000)